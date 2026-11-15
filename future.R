mirrors <- c("https://mirrors.tuna.tsinghua.edu.cn/CRAN/",
             "https://mirrors.aliyun.com/CRAN/",
             "https://cloud.r-project.org/")
options(install.packages.compile.from.source = "never")

ensure <- function(p){
  if (!requireNamespace(p, quietly = TRUE)) {
    cat("Installing:", p, "...\n")
    ok <- FALSE
    for (m in mirrors) {
      try(options(repos = c(CRAN = m)), silent = TRUE)
      ok <- tryCatch({ utils::install.packages(p, type = "win.binary")
                       requireNamespace(p, quietly = TRUE) },
                     error = function(e) FALSE, warning = function(w) FALSE)
      if (isTRUE(ok)) break
    }
    if (!isTRUE(ok)) stop(sprintf("Package '%s' cannot be installed.", p))
  }
  suppressPackageStartupMessages(library(p, character.only = TRUE))
  cat("loaded:", p, "\n")
}
pkgs <- c("tidyverse", "readxl", "writexl", "forecast")
invisible(lapply(pkgs, ensure))

setwd("E:\\gbd\\vid\\future\\2050")
cat("\n=== xlsx files ===\n"); print(list.files(pattern = "\\.xlsx$"))

file_rate   <- "future.xlsx"
file_pop    <- "pop.xlsx"
file_gbdpop <- "gbdpop.xlsx"

USE_LOG <- TRUE
B       <- 2000
set.seed(20260101)

AGES     <- c("65-69","70-74","75-79","80-84","85-89","90-94","95+")
SEXES    <- c("Male","Female")
MEASURES <- c("Incidence","Deaths")
LAST_YR  <- 2023
END_YR   <- 2050

std_cols <- function(df){
  syn <- list(
    sex_name      = c("sex","sex_name","gender"),
    age           = c("age","age_name","age_group_name","age_group","agegroup","age_range"),
    year          = c("year","year_id"),
    val           = c("val","value","mean","population","pop","pop_value"),
    location_name = c("location_name","location"),
    metric_name   = c("metric_name","metric"),
    measure_name  = c("measure_name","measure")
  )
  out <- df
  for (std in names(syn)) {
    if (std %in% names(out)) next
    hit <- intersect(syn[[std]], names(out))[1]
    if (!is.na(hit) && !is.null(hit)) names(out)[names(out) == hit] <- std
  }
  out
}
norm_age <- function(x){
  x <- trimws(as.character(x))
  x <- gsub("\\s*\u2013\\s*", "-", x)
  x <- gsub("\\s+to\\s+", "-", x, ignore.case = TRUE)
  x <- gsub("\\s+-\\s+", "-", x)
  x <- gsub("\\s*years?$", "", x, ignore.case = TRUE)
  x <- gsub("\\s*(plus|\\+)$", "+", x, ignore.case = TRUE)
  ifelse(grepl("^95", x), "95+", x)
}
norm_sex <- function(x){
  lx <- tolower(trimws(as.character(x)))
  dplyr::case_when(lx %in% c("male","males","m","1")     ~ "Male",
                   lx %in% c("female","females","f","2") ~ "Female",
                   TRUE ~ as.character(x))
}
read_std <- function(f){
  d <- std_cols(readxl::read_excel(f))
  d %>% mutate(age = norm_age(age), sex_name = norm_sex(sex_name))
}

cat("\nReading files ...\n")
rate <- read_std(file_rate)
poph <- read_std(file_gbdpop)
popf <- read_std(file_pop)

cat("[rate  cols] ", paste(names(rate), collapse=", "), "\n")
cat("[gbdpop cols] ", paste(names(poph), collapse=", "), "\n")
cat("[ihme  cols] ", paste(names(popf), collapse=", "), "\n")

rate_df <- rate
if ("location_name" %in% names(rate_df)) rate_df <- filter(rate_df, location_name == "Global")
rate_df <- rate_df %>%
  filter(metric_name == "Rate", measure_name %in% MEASURES,
         sex_name %in% SEXES, age %in% AGES, year >= 1990 & year <= LAST_YR) %>%
  select(sex_name, measure_name, age, year, val, any_of(c("lower","upper")))
cat("rate rows:", nrow(rate_df), "| ages:", paste(sort(unique(rate_df$age)), collapse=", "), "\n")

gbd_pop <- poph
if ("location_name" %in% names(gbd_pop)) gbd_pop <- filter(gbd_pop, location_name == "Global")
if ("metric_name"   %in% names(gbd_pop)) gbd_pop <- filter(gbd_pop, metric_name == "Number")
if ("measure_name"  %in% names(gbd_pop)) gbd_pop <- filter(gbd_pop, measure_name == "Population")
if ("population_group_name" %in% names(gbd_pop))
  gbd_pop <- filter(gbd_pop, population_group_name == "All Population")

gbd_pop_raw <- gbd_pop %>%
  filter(sex_name %in% c("Male","Female","Both"),
         age %in% AGES,
         year >= 1990 & year <= LAST_YR) %>%
  select(sex_name, age, year, pop = val)

dupchk <- gbd_pop_raw %>% count(sex_name, age, year) %>% filter(n > 1)
if (nrow(dupchk) > 0) { print(as.data.frame(dupchk)); stop("GBD population has duplicate rows, please check!") }

have_sex <- unique(gbd_pop_raw$sex_name)
cat("GBD population original sexes:", paste(sort(have_sex), collapse = " | "), "\n")

if (!"Male" %in% have_sex) {
  if (!all(c("Both","Female") %in% have_sex))
    stop("Neither Male nor Both/Female available to derive Male!")

  cat("!! Male missing -> derive Male = Both - Female\n")

  wide <- gbd_pop_raw %>%
    filter(sex_name %in% c("Both","Female")) %>%
    tidyr::pivot_wider(id_cols = c(age, year), names_from = sex_name, values_from = pop)

  male_pop <- wide %>%
    mutate(pop = Both - Female, sex_name = "Male") %>%
    select(sex_name, age, year, pop)

  if (any(male_pop$pop < 0, na.rm = TRUE)) {
    bad <- male_pop %>% filter(pop < 0)
    cat("!! Derived Male population has negative values:\n"); print(as.data.frame(head(bad, 10)))
    stop("Male = Both - Female produced negative values, please check the data!")
  }

  if (any(is.na(male_pop$pop))) stop("Derived Male population has NA (Both/Female not paired)!")

  gbd_pop_raw <- bind_rows(
    filter(gbd_pop_raw, sex_name %in% c("Male","Female")),
    male_pop
  )
}

gbd_pop <- filter(gbd_pop_raw, sex_name %in% c("Male","Female"))

miss_gbdp <- setdiff(AGES, unique(gbd_pop$age))
if (length(miss_gbdp) > 0) {
  cat("!! GBD population missing age groups:", paste(miss_gbdp, collapse=", "), "\n")
  stop("GBD population age groups incomplete")
}
n_need <- length(AGES) * 2 * (LAST_YR - 1990 + 1)
cat("GBD pop rows:", nrow(gbd_pop), " (expected", n_need, ")",
    "| sexes:", paste(sort(unique(as.character(gbd_pop$sex_name))), collapse="/"),
    "| years:", min(gbd_pop$year), "-", max(gbd_pop$year), "\n")
if (nrow(gbd_pop) != n_need) stop("GBD population row count differs from expected, please check!")

chk23 <- gbd_pop %>% filter(year == LAST_YR) %>%
  group_by(sex_name) %>% summarise(pop65 = sum(pop), .groups = "drop")
cat("2023 population aged 65+ (after derivation):\n"); print(as.data.frame(chk23))

ihme_pop <- popf
if ("location_name" %in% names(ihme_pop)) ihme_pop <- filter(ihme_pop, location_name == "Global")
if ("metric_name"   %in% names(ihme_pop)) ihme_pop <- filter(ihme_pop, metric_name == "Number")
if ("scenario_name" %in% names(ihme_pop)) {
  cat("scenario_name:", paste(unique(as.character(ihme_pop$scenario_name)), collapse=" | "), "\n")
  if (n_distinct(ihme_pop$scenario_name) > 1)
    ihme_pop <- filter(ihme_pop, grepl("reference", tolower(as.character(scenario_name))))
} else if ("scenario" %in% names(ihme_pop)) {
  cat("scenario:", paste(unique(as.character(ihme_pop$scenario)), collapse=" | "), "\n")
  if (n_distinct(ihme_pop$scenario) > 1) {
    ihme_pop <- if (is.numeric(ihme_pop$scenario)) filter(ihme_pop, scenario == 0)
                else filter(ihme_pop, grepl("reference", tolower(as.character(scenario))))
  }
}
ihme_pop <- ihme_pop %>%
  filter(sex_name %in% SEXES, age %in% AGES, year > LAST_YR & year <= END_YR) %>%
  select(sex_name, age, year, pop = val)
cat("IHME future pop rows:", nrow(ihme_pop), "| years:",
    if (nrow(ihme_pop)) paste(min(ihme_pop$year), "-", max(ihme_pop$year)) else "EMPTY", "\n")

ihme_2023 <- popf
if ("location_name" %in% names(ihme_2023)) ihme_2023 <- filter(ihme_2023, location_name == "Global")
if ("metric_name"   %in% names(ihme_2023)) ihme_2023 <- filter(ihme_2023, metric_name == "Number")
if ("scenario_name" %in% names(ihme_2023) && n_distinct(ihme_2023$scenario_name) > 1)
  ihme_2023 <- filter(ihme_2023, grepl("reference", tolower(as.character(scenario_name))))
ihme_2023 <- ihme_2023 %>%
  filter(sex_name %in% SEXES, age %in% AGES, year == LAST_YR) %>%
  select(sex_name, age, ihme23 = val)

gbd_2023 <- gbd_pop %>% filter(year == LAST_YR) %>% select(sex_name, age, gbd23 = pop)

cal <- inner_join(gbd_2023, ihme_2023, by = c("sex_name","age")) %>%
  mutate(k = gbd23 / ihme23)
cat("\n--- Calibration (GBD 2023 / IHME 2023) ---\n"); print(as.data.frame(cal))
if (nrow(cal) == 0) {
  cat("!! No overlap at 2023 -> proceed WITHOUT calibration\n")
  cal <- NULL
} else {
  cat("mean k =", round(mean(cal$k), 4), "| range:", round(min(cal$k),4), "-", round(max(cal$k),4), "\n")
}

if (!is.null(cal)) {
  ihme_pop <- ihme_pop %>%
    left_join(cal %>% select(sex_name, age, k), by = c("sex_name","age")) %>%
    mutate(k = ifelse(is.na(k), 1, k), pop = pop * k) %>%
    select(-k)
}

pop_df <- bind_rows(gbd_pop, ihme_pop)

nchk <- pop_df %>% count(year) %>% filter(n != 14)
if (nrow(nchk) > 0) { print(as.data.frame(nchk)); stop("Population row count per year is not 14!") }

sanity <- pop_df %>% group_by(year) %>% summarise(tot = sum(pop), .groups = "drop")
bad <- sanity %>% filter(tot < 3e8 | tot > 2e9)
if (nrow(bad) > 0) { print(as.data.frame(bad)); stop("Population magnitude abnormal (65+ should be 3e8-2e9)!") }
cat("Population check passed | 2023:", format(filter(sanity, year==2023)$tot, big.mark=","),
    "| 2050:", format(filter(sanity, year==2050)$tot, big.mark=","), "\n")
cat("\nCombined pop rows:", nrow(pop_df), "| years:", min(pop_df$year), "-", max(pop_df$year), "\n")

miss_rate <- setdiff(AGES, unique(rate_df$age))
miss_pop  <- setdiff(AGES, unique(pop_df$age))
cat("!! missing age in rate:", ifelse(length(miss_rate)==0,"none",paste(miss_rate,collapse=", ")), "\n")
cat("!! missing age in pop :", ifelse(length(miss_pop)==0, "none",paste(miss_pop, collapse=", ")), "\n")
cat("age sets identical?", setequal(unique(rate_df$age), unique(pop_df$age)), "\n")

chk <- rate_df %>% count(sex_name, measure_name, age) %>% filter(n != LAST_YR - 1990 + 1)
if (nrow(chk) > 0) { print(as.data.frame(chk)); stop("Some rate series lack 34 years.") }
stopifnot("pop lacks 2050" = END_YR %in% pop_df$year)
pdup <- pop_df %>% count(sex_name, age, year) %>% filter(n > 1)
stopifnot("pop duplicated years" = nrow(pdup) == 0)
stopifnot("pop has NA" = !any(is.na(pop_df$pop)))

CAP_ANNUAL <- 0.025
DAMP       <- TRUE
DAMP_TAU   <- 15

fit_project <- function(d, end_year = END_YR,
                        cap_annual = CAP_ANNUAL, damp = DAMP, damp_tau = DAMP_TAU){
  d <- d %>% arrange(year)
  if (nrow(d) < 3) return(NULL)
  last_yr <- max(d$year); h <- end_year - last_yr
  if (h < 1) return(NULL)

  x  <- d$year - mean(d$year)
  ly <- log(d$val)
  fm <- stats::lm(ly ~ x)

  b        <- stats::coef(fm)[2]
  b_raw    <- b
  b        <- max(min(b, cap_annual), -cap_annual)

  resid_sd <- summary(fm)$sigma
  if (!is.finite(resid_sd) || resid_sd <= 0)
    resid_sd <- sd(diff(ly), na.rm = TRUE)
  if (!is.finite(resid_sd) || resid_sd <= 0) resid_sd <- 1e-4

  h_seq <- seq_len(h)
  taper <- if (damp) pmin(damp_tau * (1 - exp(-h_seq/damp_tau)) / h_seq, 1) else rep(1, h)

  last_log <- ly[length(ly)]
  mu  <- last_log + b * h_seq * taper
  sdv <- resid_sd * sqrt(h_seq)

  tibble(year = (last_yr + 1):end_year,
         mu = mu, sdv = sdv, log_scale = TRUE,
         annual_pct_raw = (exp(b_raw) - 1) * 100,
         annual_pct     = (exp(b)     - 1) * 100)
}

combos <- rate_df %>% distinct(sex_name, measure_name, age) %>%
  arrange(sex_name, measure_name, age)
cat("Series to project:", nrow(combos), " (expected 28)\n")
stopifnot("combos should be 28" = nrow(combos) == 28)

out <- vector("list", nrow(combos))
for (i in seq_len(nrow(combos))) {
  s <- combos$sex_name[i]; m <- combos$measure_name[i]; a <- combos$age[i]
  d <- rate_df %>% filter(sex_name == s, measure_name == m, age == a)
  p <- fit_project(d)
  if (is.null(p) || nrow(p) == 0) { out[[i]] <- NULL; next }
  out[[i]] <- bind_cols(tibble(sex_name = s, measure_name = m, age = a), p)
}
proj_df <- bind_rows(out)

if (is.null(proj_df) || !is.data.frame(proj_df) || nrow(proj_df) == 0)
  stop("proj_df generation failed!")
cat("proj_df rows:", nrow(proj_df), " (expected 756)\n")

chk_drift <- proj_df %>% filter(year == END_YR) %>%
  transmute(sex_name, measure_name, age,
            annual_pct, sdv2050 = sdv, ci95_ratio = exp(1.96*sdv)) %>%
  arrange(desc(abs(annual_pct)))
cat("\n=== Annual drift (all should be within +/-2.5%) ===\n")
print(as.data.frame(chk_drift))
cat("\nsdv2050 range:", round(range(proj_df$sdv[proj_df$year==END_YR]), 3), "\n")
cat("2050 95% interval ratio range:", round(range(chk_drift$ci95_ratio), 2), "\n")

np <- as.integer(nrow(proj_df)); stopifnot(np > 0)
fp <- pop_df %>% filter(year > LAST_YR & year <= END_YR)
stopifnot("future population empty" = nrow(fp) > 0)

pt <- proj_df %>% filter(year > LAST_YR) %>%
  transmute(sex_name, measure_name, age, year, r_med = exp(mu)) %>%
  left_join(fp %>% select(sex_name, age, year, pop), by = c("sex_name","age","year")) %>%
  mutate(n_med = r_med/1e5*pop) %>%
  group_by(sex_name, measure_name, year) %>%
  summarise(num_mean = sum(n_med), pop_tot = sum(pop), .groups = "drop") %>%
  mutate(crude_mean = num_mean/pop_tot*1e5)

draws <- proj_df[rep(seq_len(np), each = B), ] %>% as_tibble() %>%
  mutate(b = rep(seq_len(B), times = np), z = rnorm(n(), 0, 1),
         r_draw = exp(mu + sdv*z)) %>%
  left_join(fp %>% select(sex_name, age, year, pop), by = c("sex_name","age","year")) %>%
  filter(!is.na(pop)) %>% mutate(num_draw = r_draw/1e5*pop)

agg_future <- draws %>%
  group_by(sex_name, measure_name, year, b) %>%
  summarise(num = sum(num_draw), .groups = "drop") %>%
  group_by(sex_name, measure_name, year) %>%
  summarise(num_lo50 = quantile(num, .25), num_hi50 = quantile(num, .75),
            num_lo95 = quantile(num, .025), num_hi95 = quantile(num, .975),
            .groups = "drop") %>%
  left_join(pt, by = c("sex_name","measure_name","year")) %>%
  mutate(crude_lo50 = num_lo50/pop_tot*1e5, crude_hi50 = num_hi50/pop_tot*1e5,
         crude_lo95 = num_lo95/pop_tot*1e5, crude_hi95 = num_hi95/pop_tot*1e5) %>%
  select(sex_name, measure_name, year, num_mean, crude_mean,
         num_lo50, num_hi50, num_lo95, num_hi95,
         crude_lo50, crude_hi50, crude_lo95, crude_hi95)

cat("agg_future rows:", nrow(agg_future), " (expected 108)\n")

hist_agg <- rate_df %>%
  left_join(pop_df %>% filter(year <= LAST_YR), by = c("sex_name","age","year")) %>%
  filter(!is.na(pop)) %>%
  mutate(num = val / 1e5 * pop) %>%
  group_by(sex_name, measure_name, year) %>%
  summarise(num = sum(num), pop = sum(pop), .groups="drop") %>%
  mutate(crude = num / pop * 1e5)
cat("historical burden years:", min(hist_agg$year), "-", max(hist_agg$year),
    "| NA rows:", sum(is.na(hist_agg$num)), "\n")

mk <- function(d, type) {
  d %>% mutate(type = type) %>%
    mutate(num_lo50   = if_else(type == "Observed", NA_real_, num),
           num_hi50   = if_else(type == "Observed", NA_real_, num),
           num_lo95   = if_else(type == "Observed", NA_real_, num),
           num_hi95   = if_else(type == "Observed", NA_real_, num),
           crude_lo50 = if_else(type == "Observed", NA_real_, crude),
           crude_hi50 = if_else(type == "Observed", NA_real_, crude),
           crude_lo95 = if_else(type == "Observed", NA_real_, crude),
           crude_hi95 = if_else(type == "Observed", NA_real_, crude)) %>%
    select(sex_name, measure_name, year, num, crude,
           num_lo50, num_hi50, num_lo95, num_hi95,
           crude_lo50, crude_hi50, crude_lo95, crude_hi95, type)
}

plot_df <- bind_rows(mk(hist_agg, "Observed"),
                     mk(hist_agg %>% filter(year == LAST_YR), "Projected"),
                     agg_future %>% transmute(sex_name, measure_name, year,
                       num = num_mean, crude = crude_mean,
                       num_lo50, num_hi50, num_lo95, num_hi95,
                       crude_lo50, crude_hi50, crude_lo95, crude_hi95, type = "Projected")) %>%
  mutate(sex_name = factor(sex_name, levels = SEXES),
         measure_name = factor(measure_name, levels = MEASURES))

cat("\n=========== DIAG ===========\n")
cat("gbd_pop  rows:", nrow(gbd_pop),
    "| years:", if (nrow(gbd_pop)) paste(min(gbd_pop$year),"-",max(gbd_pop$year)) else "EMPTY", "\n")
cat("ihme_pop rows:", nrow(ihme_pop),
    "| years:", if (nrow(ihme_pop)) paste(min(ihme_pop$year),"-",max(ihme_pop$year)) else "EMPTY", "\n")
cat("pop_df   years:", min(pop_df$year), "-", max(pop_df$year),
    "| 2023 included:", 2023 %in% pop_df$year, "\n")
cat("hist_agg rows:", nrow(hist_agg),
    "| years:", if (nrow(hist_agg)) paste(min(hist_agg$year),"-",max(hist_agg$year)) else "EMPTY", "\n")
if (nrow(hist_agg))
  cat("hist_agg rows at 2023:", nrow(filter(hist_agg, year == 2023)), "\n")
cat("agg_future rows:", nrow(agg_future), "| 2050 included:", 2050 %in% agg_future$year, "\n")
cat("============================\n")

base23 <- hist_agg %>% filter(year == LAST_YR) %>%
  group_by(sex_name, measure_name) %>%
  summarise(num_2023   = mean(num,   na.rm = TRUE),
            crude_2023 = mean(crude, na.rm = TRUE), .groups = "drop")

fut50 <- agg_future %>% filter(year == END_YR) %>%
  group_by(sex_name, measure_name) %>%
  summarise(num_2050   = mean(num_mean,   na.rm = TRUE),
            crude_2050 = mean(crude_mean, na.rm = TRUE), .groups = "drop")

cat("\n[base23]\n"); print(as.data.frame(base23))
cat("\n[fut50]\n");  print(as.data.frame(fut50))

res_tbl <- inner_join(base23, fut50, by = c("sex_name","measure_name")) %>%
  mutate(num_change_pct   = round((num_2050   / num_2023   - 1) * 100, 2),
         crude_change_pct = round((crude_2050 / crude_2023 - 1) * 100, 2)) %>%
  arrange(sex_name, measure_name)

cat("\n[res_tbl]\n"); print(as.data.frame(res_tbl))

r23 <- rate_df %>% filter(year==LAST_YR) %>% select(sex_name, measure_name, age, r23=val)
r50 <- proj_df %>% filter(year==END_YR) %>% mutate(r50 = ifelse(log_scale, exp(mu), mu)) %>%
  select(sex_name, measure_name, age, r50)
p23 <- pop_df %>% filter(year==LAST_YR) %>% select(sex_name, age, p23=pop)
p50 <- pop_df %>% filter(year==END_YR) %>% select(sex_name, age, p50=pop)

stopifnot("p23 should have 14 rows" = nrow(p23) == 14)
stopifnot("p50 should have 14 rows" = nrow(p50) == 14)
cat("P2023 Female:", format(sum(filter(p23, sex_name=="Female")$p23), big.mark=","),
    "| P2050 Female:", format(sum(filter(p50, sex_name=="Female")$p50), big.mark=","), "\n")

dec_tbl <- r23 %>% left_join(p23, by=c("sex_name","age")) %>%
  left_join(p50, by=c("sex_name","age")) %>%
  left_join(r50, by=c("sex_name","measure_name","age")) %>%
  filter(!is.na(p23), !is.na(p50), !is.na(r50)) %>%
  mutate(n23 = r23/1e5*p23, n_aging = r23/1e5*p50, n_2050 = r50/1e5*p50) %>%
  group_by(sex_name, measure_name) %>%
  summarise(N2023=sum(n23), P2023=sum(p23), P2050=sum(p50),
            N_size = sum(n23)*sum(p50)/sum(p23),
            N_aging = sum(n_aging), N2050 = sum(n_2050), .groups="drop") %>%
  mutate(eff_size=N_size-N2023, eff_aging=N_aging-N_size, eff_epi=N2050-N_aging,
         total=N2050-N2023,
         pct_size =round(eff_size /abs(N2023)*100,2),
         pct_aging=round(eff_aging/abs(N2023)*100,2),
         pct_epi  =round(eff_epi  /abs(N2023)*100,2),
         pct_total=round(total    /abs(N2023)*100,2))

theme_set(theme_bw(base_size = 12))
common <- theme(panel.grid = element_blank(),
                strip.background = element_rect(fill = "grey90"),
                strip.text = element_text(face = "bold"))

tag_df <- tidyr::expand_grid(
  sex_name     = factor(SEXES,    levels = SEXES),
  measure_name = factor(MEASURES, levels = MEASURES)
) %>% mutate(tag = c("a", "b", "c", "d"))

sex_lab  <- c(Male = "Male", Female = "Female")
meas_lab <- c(Incidence = "Incident cases", Deaths = "Deaths")
my_labeller <- labeller(sex_name = sex_lab, measure_name = meas_lab)

add_tag <- function(p) {
  p + geom_text(data = tag_df,
                aes(x = -Inf, y = Inf, label = tag),
                hjust = -0.5, vjust = 1.8,
                size = 7, fontface = "bold", inherit.aes = FALSE)
}

p_rate <- ggplot(plot_df, aes(x = year)) +
  geom_ribbon(aes(ymin = crude_lo95, ymax = crude_hi95), fill = "#cce0ff", alpha = .35, na.rm = TRUE) +
  geom_ribbon(aes(ymin = crude_lo50, ymax = crude_hi50), fill = "#7399dd", alpha = .45, na.rm = TRUE) +
  geom_line(aes(y = crude), linewidth = .8, na.rm = TRUE) +
  geom_point(data = filter(plot_df, type == "Observed"), aes(y = crude), size = 1.1, na.rm = TRUE) +
  geom_vline(xintercept = LAST_YR, linetype = "dashed", color = "gray50") +
  scale_x_continuous(breaks = seq(1990, 2050, 10)) +
  facet_grid(sex_name ~ measure_name, scales = "free_y", labeller = my_labeller) +
  labs(x = "Year", y = "Crude rate per 100,000 (ages 65+)",
       title = "Observed and projected 65+ burden, 1990-2050") + common
p_rate <- add_tag(p_rate)

p_num <- ggplot(plot_df, aes(x = year)) +
  geom_ribbon(aes(ymin = num_lo95, ymax = num_hi95), fill = "#ffd9cc", alpha = .35, na.rm = TRUE) +
  geom_ribbon(aes(ymin = num_lo50, ymax = num_hi50), fill = "#e07b53", alpha = .40, na.rm = TRUE) +
  geom_line(aes(y = num), linewidth = .8, na.rm = TRUE) +
  geom_point(data = filter(plot_df, type == "Observed"), aes(y = num), size = 1.1, na.rm = TRUE) +
  geom_vline(xintercept = LAST_YR, linetype = "dashed", color = "gray50") +
  scale_x_continuous(breaks = seq(1990, 2050, 10)) +
  scale_y_continuous(labels = scales::comma) +
  facet_grid(sex_name ~ measure_name, scales = "free_y", labeller = my_labeller) +
  labs(x = "Year", y = "Number of cases / deaths (ages 65+)",
       title = "Observed and projected counts, 1990-2050") + common
p_num <- add_tag(p_num)

p_pop <- pop_df %>% group_by(sex_name, year) %>% summarise(pop65 = sum(pop), .groups="drop") %>%
  ggplot(aes(x = year, y = pop65/1e6, color = sex_name)) + geom_line(linewidth = .9) +
  geom_vline(xintercept = LAST_YR, linetype = "dashed", color = "gray50") +
  scale_x_continuous(breaks = seq(1990, 2050, 10)) +
  labs(x = "Year", y = "Population aged 65+ (millions)", color = NULL,
       title = "Population aged 65+, 1990-2050 (GBD + calibrated IHME)") + common

ggsave("Fig1_65plus_crude_rate_2050.jpg",  p_rate, width=11, height=8, dpi=300)
ggsave("Fig1_65plus_crude_rate_2050.tiff", p_rate, width=11, height=8, dpi=300,
       device="tiff", compression="lzw")
ggsave("Fig2_65plus_number_2050.jpg",  p_num, width=11, height=8, dpi=300)
ggsave("Fig2_65plus_number_2050.tiff", p_num, width=11, height=8, dpi=300,
       device="tiff", compression="lzw")
ggsave("Fig3_65plus_population.jpg",   p_pop, width=9,  height=5.5, dpi=300)

cat("Figures written to:", getwd(), "\n")

write_xlsx(list(Result_2023_2050   = as.data.frame(res_tbl),
                Decomposition_2050 = as.data.frame(dec_tbl),
                Plot_data          = as.data.frame(plot_df),
                Age_specific_proj  = as.data.frame(proj_df),
                Calibration        = as.data.frame(cal %||% data.frame(note="none")),
                Population_used    = as.data.frame(pop_df)), "Results_65plus_2050.xlsx")

cat("\n=== 2023 vs 2050 ===\n"); print(as.data.frame(res_tbl))
cat("\n=== Decomposition (%) ===\n"); print(as.data.frame(dec_tbl))
cat("\nDone.\n")
