library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)


INPUT_DIR   <- "E:\\gbd\\vid\\eapc"
NUM_FILE    <- file.path(INPUT_DIR, "1990+2023+num.xlsx")
RATE_FILE   <- file.path(INPUT_DIR, "1990+2023+rate.xlsx")
OUTPUT_FILE <- file.path(INPUT_DIR, "EAPC_ASR_65plus.xlsx")

START_YEAR <- 1990
END_YEAR   <- 2023
Z_VALUE    <- qnorm(0.975)                  

STUDY_AGES <- c("65-69 years", "70-74 years", "75-79 years", "80+ years")


STD_POP <- "who"
WHO_WEIGHTS <- tibble(age_name = STUDY_AGES, w = c(2.96, 2.21, 1.52, 1.51))
GBD_WEIGHTS <- tibble(age_name = STUDY_AGES, w = c(3.01, 2.10, 1.36, 1.17))
WEIGHTS <- if (STD_POP == "gbd") GBD_WEIGHTS else WHO_WEIGHTS

MEASURES <- list(
  list(name = "Incidence", count = "Incidence Cases", rate = "ASIR", sheet = "Incidence"),
  list(name = "Deaths",    count = "Deaths",          rate = "ASMR", sheet = "Deaths"),
  list(name = "DALYs (Disability-Adjusted Life Years)",
                           count = "DALYs",           rate = "ASDR", sheet = "DALYs")
)

OUT_COLS <- function(cnt, rt) {
  c("location",
    paste0("1990 ", cnt), paste0("1990 ", rt),
    paste0("2023 ", cnt), paste0("2023 ", rt),
    paste0(cnt, " Change Rate (%)"),
    paste0(rt, " Change Rate (%)"),
    paste0("EAPC of ", rt, " (95% CI)"))
}


COLS <- c("measure_name", "location_name", "sex_name", "age_name",
          "year", "val", "lower", "upper")
KEY  <- c("location_name", "sex_name", "age_name", "year")

num_all  <- read.xlsx(NUM_FILE)
rate_all <- read.xlsx(RATE_FILE)
stopifnot(nrow(num_all) > 0, nrow(rate_all) > 0)

num_all  <- num_all  %>% filter(metric_name == "Number") %>% select(all_of(COLS))
rate_all <- rate_all %>% filter(metric_name == "Rate")   %>% select(all_of(COLS))
message("Number rows: ", nrow(num_all), " | Rate rows: ", nrow(rate_all))


sum_cases <- function(d) {
  d <- d %>% filter(age_name %in% STUDY_AGES)
  tibble(v = sum(d$val,   na.rm = TRUE),
         l = sum(d$lower, na.rm = TRUE),
         u = sum(d$upper, na.rm = TRUE))
}

one_rate <- function(d) {
  d <- d %>% left_join(WEIGHTS, by = "age_name") %>% filter(!is.na(val), !is.na(w))
  if (nrow(d) == 0) return(tibble(v = NA_real_, l = NA_real_, u = NA_real_))
  sw <- sum(d$w)
  tibble(v = sum(d$w * d$val)   / sw,
         l = sum(d$w * d$lower) / sw,
         u = sum(d$w * d$upper) / sw)
}

calc_eapc <- function(year_vec, val_vec) {
  keep <- !is.na(year_vec) & !is.na(val_vec) & val_vec > 0
  y <- year_vec[keep]; v <- val_vec[keep]

  if (length(y) < 3) {
    return(tibble(EAPC = NA_real_, EAPC_lower = NA_real_, EAPC_upper = NA_real_))
  }
  fit <- tryCatch(lm(log(v) ~ y), error = function(e) NULL)
  if (is.null(fit)) {
    return(tibble(EAPC = NA_real_, EAPC_lower = NA_real_, EAPC_upper = NA_real_))
  }
  beta <- coef(fit)[2]
  se   <- summary(fit)$coefficients[2, "Std. Error"]
  if (is.na(beta) || is.na(se)) {
    return(tibble(EAPC = NA_real_, EAPC_lower = NA_real_, EAPC_upper = NA_real_))
  }
  tibble(EAPC       = 100 * (exp(beta) - 1),
         EAPC_lower = 100 * (exp(beta - Z_VALUE * se) - 1),
         EAPC_upper = 100 * (exp(beta + Z_VALUE * se) - 1))
}

fmt <- function(x) sprintf("%.2f (%.2f - %.2f)", x$v, x$l, x$u)

summarize_row <- function(df_num, df_rate, label, sex = "Both", age = NULL) {

  d_num  <- df_num  %>% filter(sex_name == sex)
  d_rate <- df_rate %>% filter(sex_name == sex)
  if (!is.null(age)) {
    d_num  <- d_num  %>% filter(age_name == age)
    d_rate <- d_rate %>% filter(age_name == age)
  }

  yr <- sort(unique(d_rate$year))
  if (length(yr) < 3) stop("EAPC needs at least 3 years for: ", label)
  if (length(yr) != (END_YEAR - START_YEAR + 1)) {
    message("  [warn] ", label, ": ", length(yr), " years instead of ",
            END_YEAR - START_YEAR + 1)
  }

  n90 <- sum_cases(d_num %>% filter(year == START_YEAR))
  n23 <- sum_cases(d_num %>% filter(year == END_YEAR))

  r90 <- one_rate(d_rate %>% filter(year == START_YEAR))
  r23 <- one_rate(d_rate %>% filter(year == END_YEAR))

  v_series <- sapply(yr, function(y) one_rate(d_rate %>% filter(year == y))$v)
  er <- calc_eapc(yr, v_series)

  num_chg  <- if (!is.na(n90$v) && n90$v != 0) (n23$v - n90$v) / n90$v * 100 else NA_real_
  rate_chg <- if (!is.na(r90$v) && r90$v != 0) (r23$v - r90$v) / r90$v * 100 else NA_real_

  tibble(
    location = label,
    fmt(n90), fmt(r90), fmt(n23), fmt(r23),
    if (is.na(num_chg))  "NA" else sprintf("%.2f%%", num_chg),
    if (is.na(rate_chg)) "NA" else sprintf("%.2f%%", rate_chg),
    if (is.na(er$EAPC))  "NA" else
      sprintf("%.2f%% (%.2f%% - %.2f%%)", er$EAPC, er$EAPC_lower, er$EAPC_upper)
  )
}


run_measure <- function(spec) {

  numI  <- num_all  %>% filter(measure_name == spec$name) %>% select(-measure_name)
  rateI <- rate_all %>% filter(measure_name == spec$name) %>% select(-measure_name)

  if (nrow(numI) == 0 || nrow(rateI) == 0) {
    message("[skip] ", spec$sheet, " - no rows for measure '", spec$name, "'")
    return(NULL)
  }
  message("[run ] ", spec$sheet, " - ", nrow(numI), " count rows / ",
          nrow(rateI), " rate rows")

  stopifnot(!any(duplicated(numI[,  KEY])))
  stopifnot(!any(duplicated(rateI[, KEY])))

  numI  <- numI  %>% filter(age_name %in% STUDY_AGES)
  rateI <- rateI %>% filter(age_name %in% STUDY_AGES)

  if (!("Both" %in% unique(numI$sex_name))) {
    message("  [warn] no 'Both' sex rows - the 31 sex-aggregated rows would be NA")
  }

  all_locs    <- sort(unique(numI$location_name))
  sdi_locs    <- sort(grep("SDI", all_locs, value = TRUE))
  global_loc  <- "Global"
  region_locs <- setdiff(all_locs, c(sdi_locs, global_loc))

  numG  <- numI  %>% filter(location_name == global_loc)
  rateG <- rateI %>% filter(location_name == global_loc)

  rows <- list()
  for (loc in region_locs) {                      
    rows[[length(rows) + 1]] <- summarize_row(
      numI %>% filter(location_name == loc),
      rateI %>% filter(location_name == loc), label = loc)
  }
  rows[[length(rows) + 1]] <- summarize_row(numG, rateG, label = global_loc) 
  for (loc in sdi_locs) {                         
    rows[[length(rows) + 1]] <- summarize_row(
      numI %>% filter(location_name == loc),
      rateI %>% filter(location_name == loc), label = loc)
  }
  rows[[length(rows) + 1]] <- summarize_row(numG, rateG, label = "Male",   sex = "Male")
  rows[[length(rows) + 1]] <- summarize_row(numG, rateG, label = "Female", sex = "Female")
  for (age in STUDY_AGES) {                        
    rows[[length(rows) + 1]] <- summarize_row(numG, rateG, label = age, age = age)
  }

  tab <- bind_rows(rows)
  names(tab) <- OUT_COLS(spec$count, spec$rate)

  cat("\n--- ", spec$sheet, " (", nrow(tab), " rows ) ---\n", sep = "")
  print(as.data.frame(tab), row.names = FALSE)
  tab
}


WRITE_COMBINED <- FALSE

results <- list()
for (spec in MEASURES) {
  t <- tryCatch(run_measure(spec),
                error = function(e) { message("[fail] ", spec$sheet, ": ",
                                              conditionMessage(e)); NULL })
  if (!is.null(t)) {
    results[[spec$sheet]] <- t

    out_path <- file.path(INPUT_DIR, paste0("EAPC_", spec$rate, "_65plus.xlsx"))
    write.xlsx(t, file = out_path)
    cat("\n>>> written: ", out_path, "  (", nrow(t), " rows x ", ncol(t), " cols )\n", sep = "")
  }
}

if (length(results) == 0) stop("No measure could be produced - check the input files.")

if (WRITE_COMBINED) {
  write.xlsx(results, file = OUTPUT_FILE)
  cat("\n>>> combined workbook: ", OUTPUT_FILE, "\n", sep = "")
}

cat("\nDone! Files written to ", INPUT_DIR, "\n", sep = "")
cat("Measures: ", paste(names(results), collapse = ", "), "\n")
cat("Scope   : population aged 65+ | standard population: ", STD_POP, "\n", sep = "")
