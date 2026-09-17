options(repos = c(CRAN="https://mirrors.aliyun.com/CRAN/"))
library(tidyverse)
library(writexl)

setwd("E:\\gbd\\vid\\future")
df <- read.csv("future.csv") %>%
  filter(location_name == "Global", metric_name == "Rate")



pred_rwd <- function(dat){
  if(nrow(dat) == 0) stop("Input data is empty.")
  if(length(unique(dat$year)) < 3) stop("Need at least 3 years.")

  dat <- dat %>% arrange(year)
  sex_val  <- unique(dat$sex_name)[1]
  meas_val <- unique(dat$measure_name)[1]
  last_yr  <- max(dat$year)
  last_v   <- dat$val[dat$year == last_yr]   


  d <- diff(dat$val)
  drift <- mean(d)
  sigma <- sd(d)   
  cat(sprintf("  [%s / %s] drift=%.4f/yr, sigma=%.4f, last_v=%.4f\n",
              sex_val, meas_val, drift, sigma, last_v))

  
  h <- 2050 - last_yr
  future_yrs <- (last_yr + 1):2050
  pred_mean  <- last_v + cumsum(rep(drift, h))

  
  h_seq   <- 1:h
  se      <- sigma * sqrt(h_seq)
  l50     <- pred_mean - 0.6745 * se
  u50     <- pred_mean + 0.6745 * se
  l95     <- pred_mean - 1.96 * se
  u95     <- pred_mean + 1.96 * se

  
  ar_df <- data.frame(
    year   = c(last_yr, future_yrs),
    val_ar = c(last_v, pred_mean),
    ar_l50 = c(last_v, l50),
    ar_u50 = c(last_v, u50),
    ar_l95 = c(last_v, l95),
    ar_u95 = c(last_v, u95)
  )

  hist  <- dat %>% mutate(type = "Observed")
  total <- full_join(hist, ar_df, by = "year") %>%
    mutate(sex = sex_val, measure = meas_val)

  y23  <- filter(total, year == 2023) %>% pull(val)
  ar50 <- filter(total, year == 2050) %>% pull(val_ar)

  tbl <- tibble(
    Sex            = sex_val,
    Measure        = meas_val,
    ASR_2023       = round(y23, 2),
    ASR_2050       = round(ar50, 2),
    Change_Percent = round((ar50 / y23 - 1) * 100, 2)
  )
  return(list(data = total, table = tbl))
}

run_safe <- function(sex_lvl, meas_lvl){
  dat <- df %>% filter(sex_name == sex_lvl, measure_name == meas_lvl)
  cat(sprintf("[%s / %s] rows = %d\n", sex_lvl, meas_lvl, nrow(dat)))
  pred_rwd(dat)
}

m1 <- run_safe("Male",   "Incidence")
m2 <- run_safe("Male",   "Deaths")
f1 <- run_safe("Female", "Incidence")
f2 <- run_safe("Female", "Deaths")

result_table <- bind_rows(m1$table, m2$table, f1$table, f2$table)
write_xlsx(result_table, "Prediction_2050.xlsx")
cat("\n=== Result table ===\n"); print(result_table)
cat("Excel saved: Prediction_2050.xlsx\n")

plot_data <- bind_rows(m1$data, m2$data, f1$data, f2$data) %>%
  mutate(group = factor(case_when(
    sex == "Male"   & measure == "Incidence" ~ "a) Male Incidence",
    sex == "Male"   & measure == "Deaths"    ~ "b) Male Deaths",
    sex == "Female" & measure == "Incidence" ~ "c) Female Incidence",
    sex == "Female" & measure == "Deaths"    ~ "d) Female Deaths"
  ), levels = c("a) Male Incidence", "b) Male Deaths",
                 "c) Female Incidence", "d) Female Deaths")))

p <- ggplot(plot_data, aes(x = year)) +
  geom_ribbon(aes(ymin = ar_l95, ymax = ar_u95),
              fill = "#cce0ff", alpha = 0.3, na.rm = TRUE) +
  geom_ribbon(aes(ymin = ar_l50, ymax = ar_u50),
              fill = "#7399dd", alpha = 0.35, na.rm = TRUE) +
  geom_point(aes(y = val), color = "black", size = 1.1, na.rm = TRUE) +
  geom_line(aes(y = val_ar), color = "black", linewidth = 0.8, na.rm = TRUE) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "gray60") +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2030, 2040, 2050)) +
  facet_wrap(~ group, ncol = 2, scales = "free_y") +
  labs(x = "Year", y = "Age-standardized rate per 100,000") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave("Prediction_2050.jpg", plot = p, width = 13, height = 10, dpi = 300)
cat("Plot saved: Prediction_2050.jpg\n")
ggsave("Prediction_2050.tif", plot = p, width = 13, height = 10, dpi = 300)
cat("Plot saved: Prediction_2050.tif\n")
