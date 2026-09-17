# =====================================================
# EAPC of ASIR


library(tidyverse)
library(readr)
library(writexl)

setwd("E:\\gbd\\vid\\eapc")


target_measure <- "Incidence"
target_sex <- "Both"


num_df <- read_csv("1990+2023+num.csv", show_col_types = FALSE)
rate_df <- read_csv("1990+2023+rate.csv", show_col_types = FALSE)

numI  <- num_df %>% filter(measure_name == target_measure)
rateI <- rate_df %>% filter(measure_name == target_measure)


calc_eapc <- function(year_vec, val_vec) {
  keep <- !is.na(year_vec) & !is.na(val_vec) & val_vec > 0
  y <- year_vec[keep]
  v <- val_vec[keep]

  if (length(y) < 3) {
    return(tibble(
      EAPC = NA_real_,
      EAPC_lower = NA_real_,
      EAPC_upper = NA_real_
    ))
  }

  fit <- tryCatch(
    lm(log(v) ~ y),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(tibble(
      EAPC = NA_real_,
      EAPC_lower = NA_real_,
      EAPC_upper = NA_real_
    ))
  }

  beta <- coef(fit)[2]
  se   <- summary(fit)$coefficients[2, "Std. Error"]

  eapc  <- 100 * (exp(beta) - 1)
  lower <- 100 * (exp(beta - 1.96 * se) - 1)
  upper <- 100 * (exp(beta + 1.96 * se) - 1)

  tibble(
    EAPC = eapc,
    EAPC_lower = lower,
    EAPC_upper = upper
  )
}


summarize_row <- function(df_num, df_rate, label, sex = "Both", age = NULL) {

  # -- Incidence cases --
  d_num <- df_num %>% filter(sex_name == sex)
  if (!is.null(age)) d_num <- d_num %>% filter(age_name == age)

  n90 <- d_num %>% filter(year == 1990) %>%
    summarise(v = sum(val, na.rm = TRUE),
              l = sum(lower, na.rm = TRUE),
              u = sum(upper, na.rm = TRUE))
  n23 <- d_num %>% filter(year == 2023) %>%
    summarise(v = sum(val, na.rm = TRUE),
              l = sum(lower, na.rm = TRUE),
              u = sum(upper, na.rm = TRUE))

  # -- ASIR --
  d_rate <- df_rate %>% filter(sex_name == sex)
  if (!is.null(age)) d_rate <- d_rate %>% filter(age_name == age)

  r90 <- d_rate %>% filter(year == 1990) %>%
    summarise(v = mean(val, na.rm = TRUE),
              l = mean(lower, na.rm = TRUE),
              u = mean(upper, na.rm = TRUE))
  r23 <- d_rate %>% filter(year == 2023) %>%
    summarise(v = mean(val, na.rm = TRUE),
              l = mean(lower, na.rm = TRUE),
              u = mean(upper, na.rm = TRUE))

  # -- EAPC --
  eapc_data <- d_rate %>%
    group_by(year) %>%
    summarise(v = mean(val, na.rm = TRUE), .groups = "drop")

  er <- calc_eapc(eapc_data$year, eapc_data$v)

  # -- Change rates --
  num_chg  <- if (n90$v != 0) (n23$v - n90$v) / n90$v * 100 else NA_real_
  rate_chg <- if (r90$v != 0) (r23$v - r90$v) / r90$v * 100 else NA_real_

  num_chg_str  <- if (is.na(num_chg))  "NA" else sprintf("%.2f%%", num_chg)
  rate_chg_str <- if (is.na(rate_chg)) "NA" else sprintf("%.2f%%", rate_chg)

  if (is.na(er$EAPC)) {
    eapc_str <- "NA"
  } else {
    eapc_str <- sprintf("%.2f%% (%.2f%% - %.2f%%)",
                        er$EAPC, er$EAPC_lower, er$EAPC_upper)
  }

  
  fmt <- function(val_df) {
    sprintf("%.2f (%.2f - %.2f)", val_df$v, val_df$l, val_df$u)
  }

  tibble(
    location = label,

    `1990 Incidence Cases`           = fmt(n90),
    `1990 ASIR`                      = fmt(r90),
    `2023 Incidence Cases`           = fmt(n23),
    `2023 ASIR`                      = fmt(r23),

    `Incidence Cases Change Rate (%)` = num_chg_str,
    `ASIR Change Rate (%)`            = rate_chg_str,
    `EAPC of ASIR (95%CI)`           = eapc_str
  )
}


all_locs  <- unique(numI$location_name) %>% sort()
sdi_locs  <- all_locs[grep("SDI", all_locs)]
global_loc <- "Global"
region_locs <- setdiff(all_locs, c(sdi_locs, global_loc))
age_groups  <- unique(rateI$age_name) %>% sort()

rows <- list()

# 21 regions + Global
for (loc in region_locs) {
  rows[[length(rows) + 1]] <- summarize_row(
    numI %>% filter(location_name == loc),
    rateI %>% filter(location_name == loc),
    label = loc)
}
rows[[length(rows) + 1]] <- summarize_row(
  numI %>% filter(location_name == global_loc),
  rateI %>% filter(location_name == global_loc),
  label = global_loc)

# 5 SDI
for (loc in sdi_locs) {
  rows[[length(rows) + 1]] <- summarize_row(
    numI %>% filter(location_name == loc),
    rateI %>% filter(location_name == loc),
    label = loc)
}

# Male / Female
rows[[length(rows) + 1]] <- summarize_row(numI, rateI, label = "Male",   sex = "Male")
rows[[length(rows) + 1]] <- summarize_row(numI, rateI, label = "Female", sex = "Female")

# 4 age groups
for (age in age_groups) {
  rows[[length(rows) + 1]] <- summarize_row(numI, rateI, label = age, age = age)
}


final_table <- bind_rows(rows)

cat("Total rows:", nrow(final_table), "\n")
print(final_table)

output_path <- "E:\\gbd\\vid\\eapc\\incidence_ASIR_analysis_result.xlsx"
write_xlsx(final_table, output_path)

cat("\nDone! File saved to:", output_path, "\n")



# =====================================================
# EAPC of ASMR


library(tidyverse)
library(readr)
library(writexl)

setwd("E:\\gbd\\vid\\eapc")


target_measure <- "Deaths"
target_sex <- "Both"


num_df <- read_csv("1990+2023+num.csv", show_col_types = FALSE)
rate_df <- read_csv("1990+2023+rate.csv", show_col_types = FALSE)

numD  <- num_df %>% filter(measure_name == target_measure)
rateD <- rate_df %>% filter(measure_name == target_measure)


calc_eapc <- function(year_vec, val_vec) {
  keep <- !is.na(year_vec) & !is.na(val_vec) & val_vec > 0
  y <- year_vec[keep]
  v <- val_vec[keep]

  if (length(y) < 3) {
    return(tibble(
      EAPC = NA_real_,
      EAPC_lower = NA_real_,
      EAPC_upper = NA_real_
    ))
  }

  fit <- tryCatch(
    lm(log(v) ~ y),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(tibble(
      EAPC = NA_real_,
      EAPC_lower = NA_real_,
      EAPC_upper = NA_real_
    ))
  }

  beta <- coef(fit)[2]
  se   <- summary(fit)$coefficients[2, "Std. Error"]

  eapc  <- 100 * (exp(beta) - 1)
  lower <- 100 * (exp(beta - 1.96 * se) - 1)
  upper <- 100 * (exp(beta + 1.96 * se) - 1)

  tibble(
    EAPC = eapc,
    EAPC_lower = lower,
    EAPC_upper = upper
  )
}


summarize_row <- function(df_num, df_rate, label, sex = "Both", age = NULL) {

  # -- Deaths cases (val / lower / upper) --
  d_num <- df_num %>% filter(sex_name == sex)
  if (!is.null(age)) d_num <- d_num %>% filter(age_name == age)

  n90 <- d_num %>% filter(year == 1990) %>%
    summarise(v = sum(val, na.rm = TRUE),
              l = sum(lower, na.rm = TRUE),
              u = sum(upper, na.rm = TRUE))
  n23 <- d_num %>% filter(year == 2023) %>%
    summarise(v = sum(val, na.rm = TRUE),
              l = sum(lower, na.rm = TRUE),
              u = sum(upper, na.rm = TRUE))

  # -- ASMR (val / lower / upper) --
  d_rate <- df_rate %>% filter(sex_name == sex)
  if (!is.null(age)) d_rate <- d_rate %>% filter(age_name == age)

  r90 <- d_rate %>% filter(year == 1990) %>%
    summarise(v = mean(val, na.rm = TRUE),
              l = mean(lower, na.rm = TRUE),
              u = mean(upper, na.rm = TRUE))
  r23 <- d_rate %>% filter(year == 2023) %>%
    summarise(v = mean(val, na.rm = TRUE),
              l = mean(lower, na.rm = TRUE),
              u = mean(upper, na.rm = TRUE))

  # -- EAPC --
  eapc_data <- d_rate %>%
    group_by(year) %>%
    summarise(v = mean(val, na.rm = TRUE), .groups = "drop")

  er <- calc_eapc(eapc_data$year, eapc_data$v)

  # -- Change rates --
  num_chg  <- if (n90$v != 0) (n23$v - n90$v) / n90$v * 100 else NA_real_
  rate_chg <- if (r90$v != 0) (r23$v - r90$v) / r90$v * 100 else NA_real_

  num_chg_str  <- if (is.na(num_chg))  "NA" else sprintf("%.2f%%", num_chg)
  rate_chg_str <- if (is.na(rate_chg)) "NA" else sprintf("%.2f%%", rate_chg)

  if (is.na(er$EAPC)) {
    eapc_str <- "NA"
  } else {
    eapc_str <- sprintf("%.2f%% (%.2f%% - %.2f%%)",
                        er$EAPC, er$EAPC_lower, er$EAPC_upper)
  }

  # -- 关键: 拼成 "value (lower - upper)" --
  fmt <- function(d) {
    sprintf("%.2f (%.2f - %.2f)", d$v, d$l, d$u)
  }

  tibble(
    location = label,

    `1990 Deaths Cases`             = fmt(n90),
    `1990 ASMR`                      = fmt(r90),
    `2023 Deaths Cases`             = fmt(n23),
    `2023 ASMR`                      = fmt(r23),

    `Deaths Cases Change Rate (%)`   = num_chg_str,
    `ASMR Change Rate (%)`            = rate_chg_str,
    `EAPC of ASMR (95%CI)`           = eapc_str
  )
}


all_locs  <- unique(numD$location_name) %>% sort()
sdi_locs  <- all_locs[grep("SDI", all_locs)]
global_loc <- "Global"
region_locs <- setdiff(all_locs, c(sdi_locs, global_loc))
age_groups  <- unique(rateD$age_name) %>% sort()

rows <- list()

# 21 regions + Global
for (loc in region_locs) {
  rows[[length(rows) + 1]] <- summarize_row(
    numD %>% filter(location_name == loc),
    rateD %>% filter(location_name == loc),
    label = loc)
}
rows[[length(rows) + 1]] <- summarize_row(
  numD %>% filter(location_name == global_loc),
  rateD %>% filter(location_name == global_loc),
  label = global_loc)

# 5 SDI
for (loc in sdi_locs) {
  rows[[length(rows) + 1]] <- summarize_row(
    numD %>% filter(location_name == loc),
    rateD %>% filter(location_name == loc),
    label = loc)
}

# Male / Female
rows[[length(rows) + 1]] <- summarize_row(numD, rateD, label = "Male",   sex = "Male")
rows[[length(rows) + 1]] <- summarize_row(numD, rateD, label = "Female", sex = "Female")

# 4 age groups
for (age in age_groups) {
  rows[[length(rows) + 1]] <- summarize_row(numD, rateD, label = age, age = age)
}


final_table <- bind_rows(rows)

cat("Total rows:", nrow(final_table), "\n")
print(final_table)

output_path <- "E:\\gbd\\vid\\eapc\\deaths_ASMR_analysis_result.xlsx"
write_xlsx(final_table, output_path)

cat("\nDone! File saved to:", output_path, "\n")



# =====================================================
# EAPC of ASDR


install.packages(c("tidyverse", "readr", "writexl"))
library(tidyverse)
library(readr)
library(writexl)

setwd("E:\\gbd\\vid\\eapc")


target_measure <- "DALYs (Disability-Adjusted Life Years)"
target_sex <- "Both"


num_df <- read_csv("1990+2023+num.csv", show_col_types = FALSE)
rate_df <- read_csv("1990+2023+rate.csv", show_col_types = FALSE)


numDA <- num_df %>% filter(measure_name == target_measure)
rateDA <- rate_df %>% filter(measure_name == target_measure)


calc_eapc <- function(year_vec, val_vec) {
  keep <- !is.na(year_vec) & !is.na(val_vec) & val_vec > 0
  y <- year_vec[keep]
  v <- val_vec[keep]

  if (length(y) < 3) {
    return(tibble(
      EAPC = NA_real_,
      EAPC_lower = NA_real_,
      EAPC_upper = NA_real_
    ))
  }

  fit <- tryCatch(
    lm(log(v) ~ y),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(tibble(
      EAPC = NA_real_,
      EAPC_lower = NA_real_,
      EAPC_upper = NA_real_
    ))
  }

  beta <- coef(fit)[2]
  se <- summary(fit)$coefficients[2, "Std. Error"]

  eapc  <- 100 * (exp(beta) - 1)
  lower <- 100 * (exp(beta - 1.96 * se) - 1)
  upper <- 100 * (exp(beta + 1.96 * se) - 1)

  tibble(
    EAPC = eapc,
    EAPC_lower = lower,
    EAPC_upper = upper
  )
}


summarize_row <- function(df_num, df_rate, label, sex = "Both", age = NULL) {
  # DALYs cases
  d_num <- df_num %>% filter(sex_name == sex)
  if (!is.null(age)) d_num <- d_num %>% filter(age_name == age)

  num_1990 <- d_num %>% filter(year == 1990) %>% pull(val) %>% sum(na.rm = TRUE)
  num_2023 <- d_num %>% filter(year == 2023) %>% pull(val) %>% sum(na.rm = TRUE)

  # ASDR: mean of age-group rates (proxy when Age-standardized not downloaded)
  d_rate <- df_rate %>% filter(sex_name == sex)
  if (!is.null(age)) d_rate <- d_rate %>% filter(age_name == age)

  rate_1990 <- d_rate %>% filter(year == 1990) %>% pull(val) %>% mean(na.rm = TRUE)
  rate_2023 <- d_rate %>% filter(year == 2023) %>% pull(val) %>% mean(na.rm = TRUE)

  # EAPC from full time series (group by year, mean across age groups)
  eapc_data <- d_rate %>%
    group_by(year) %>%
    summarise(v = mean(val, na.rm = TRUE), .groups = "drop")

  eapc_result <- calc_eapc(eapc_data$year, eapc_data$v)

  # Change rates (as percentages, formatted to 2 decimal places)
  num_chg <- if (num_1990 != 0) (num_2023 - num_1990) / num_1990 * 100 else NA_real_
  rate_chg <- if (rate_1990 != 0) (rate_2023 - rate_1990) / rate_1990 * 100 else NA_real_

  num_chg_str  <- if (is.na(num_chg))  "NA" else sprintf("%.2f%%", num_chg)
  rate_chg_str <- if (is.na(rate_chg)) "NA" else sprintf("%.2f%%", rate_chg)

  # Format EAPC string
  if (is.na(eapc_result$EAPC)) {
    eapc_str <- "NA"
  } else {
    eapc_str <- sprintf(
      "%.2f%% (%.2f%% - %.2f%%)",
      eapc_result$EAPC,
      eapc_result$EAPC_lower,
      eapc_result$EAPC_upper
    )
  }

  tibble(
    location = label,
    `1990 DALYs Cases` = round(num_1990, 2),
    `1990 ASDR` = round(rate_1990, 2),
    `2023 DALYs Cases` = round(num_2023, 2),
    `2023 ASDR` = round(rate_2023, 2),
    `DALYs Cases Change Rate (%)` = num_chg_str,
    `ASDR Change Rate (%)` = rate_chg_str,
    `EAPC of ASDR (95%CI)` = eapc_str
  )
}



# Identify location groups
all_locs <- unique(numDA$location_name) %>% sort()
sdi_locs <- all_locs[grep("SDI", all_locs)]
global_loc <- "Global"
region_locs <- setdiff(all_locs, c(sdi_locs, global_loc))
age_groups <- unique(rateDA$age_name) %>% sort()

rows <- list()

# 6a. 21 regions + Global (Both, all ages)
for (loc in region_locs) {
  rows[[length(rows) + 1]] <- summarize_row(
    numDA %>% filter(location_name == loc),
    rateDA %>% filter(location_name == loc),
    label = loc
  )
}

rows[[length(rows) + 1]] <- summarize_row(
  numDA %>% filter(location_name == global_loc),
  rateDA %>% filter(location_name == global_loc),
  label = global_loc
)

# 6b. 5 SDI (Both, all ages)
for (loc in sdi_locs) {
  rows[[length(rows) + 1]] <- summarize_row(
    numDA %>% filter(location_name == loc),
    rateDA %>% filter(location_name == loc),
    label = loc
  )
}

# 6c. Male summary (all locations, all ages)
rows[[length(rows) + 1]] <- summarize_row(numDA, rateDA, label = "Male", sex = "Male")

# 6d. Female summary (all locations, all ages)
rows[[length(rows) + 1]] <- summarize_row(numDA, rateDA, label = "Female", sex = "Female")

# 6e. 4 age groups (all locations, Both)
for (age in age_groups) {
  rows[[length(rows) + 1]] <- summarize_row(numDA, rateDA, label = age, age = age)
}


final_table <- bind_rows(rows)

cat("Total rows:", nrow(final_table), "\n")
print(final_table)

output_path <- "E:\\gbd\\vid\\eapc\\dalys_ASDR_analysis_result.xlsx"
write_xlsx(final_table, output_path)

cat("\nDone! File saved to:", output_path, "\n")
cat("Rows: 21 regions + Global + 5 SDI + Male + Female + 4 age groups = 33\n")
