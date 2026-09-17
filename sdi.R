library(ggplot2)
library(dplyr)
library(mgcv) 


rate_file <- "E:\\gbd\\vid\\sdi\\1990-2023-rate.csv"
sdi_file <- "E:\\gbd\\vid\\sdi\\sdi.csv"
out_path <- "E:\\gbd\\vid\\sdi\\"


rate <- read.csv(rate_file, stringsAsFactors = FALSE)
rate <- rate[rate$year >= 1990 & rate$year <= 2023, ]

rate$metric <- case_when(
  rate$measure_name == "Incidence" ~ "ASIR",
  rate$measure_name == "Deaths" ~ "ASMR",
  rate$measure_name == "DALYs (Disability-Adjusted Life Years)" ~ "ASDR"
)
rate <- rate[, c("location_name", "year", "metric", "val")]
colnames(rate) <- c("location", "year", "metric", "val")

sdi <- read.csv(sdi_file, stringsAsFactors = FALSE)
sdi <- sdi[, c("location_name", "year_id", "mean_value")]
colnames(sdi) <- c("location", "year", "sdi")
sdi <- sdi[sdi$year >= 1990 & sdi$year <= 2023, ]

df <- merge(rate, sdi, by = c("location", "year"))


all_loc <- unique(df$location)
df$location <- factor(df$location, levels = c("Global", setdiff(all_loc, "Global")))


plot_fun <- function(data, target, filename){
  d <- subset(data, metric == target)
  d_global <- subset(d, location == "Global")
  
  
  fit <- gam(val ~ s(sdi, k = 3), data = d_global)
  sdi_x <- seq(min(d$sdi), max(d$sdi), length.out = 100)
  pred_res <- predict(fit, newdata = data.frame(sdi = sdi_x), se.fit = TRUE)
  
  pred_df <- data.frame(
    sdi = sdi_x,
    pred_val = pred_res$fit,
    ci_low = pred_res$fit - 1.96*pred_res$se.fit,
    ci_up = pred_res$fit + 1.96*pred_res$se.fit
  )
  
  
  shape_vec <- c(16,17,15,18,7,8,9,10,11,12,13,0,1,2,3,4,5,6,14,21,22,23)
  
  p <- ggplot() +
   
    geom_ribbon(data = pred_df, aes(x = sdi, ymin = ci_low, ymax = ci_up), fill = "gray75", alpha = 0.4) +
   
    geom_line(data = pred_df, aes(x = sdi, y = pred_val), color = "#3355bb", linewidth = 1.1) +
    
    geom_line(data = d, aes(x = sdi, y = val, color = location, group = location), linewidth = 0.55) +
    geom_point(data = d, aes(x = sdi, y = val, color = location, shape = location), size = 1.7, alpha = 0.82) +
    scale_shape_manual(values = shape_vec) +
    labs(x = "Social Development Index (SDI)", y = target, color = "Region", shape = "Region") +
    theme_bw() +
    theme(legend.position = "right", panel.grid.minor = element_blank())
  
  ggsave(paste0(out_path, filename), p, width = 14, height = 10, dpi = 300)
  return(p)
}


plot_fun(df, "ASIR", "SDI_ASIR.jpg")
plot_fun(df, "ASIR", "SDI_ASIR.tif")
plot_fun(df, "ASMR", "SDI_ASMR.jpg")
plot_fun(df, "ASMR", "SDI_ASMR.tif")
plot_fun(df, "ASDR", "SDI_ASDR.jpg")
plot_fun(df, "ASDR", "SDI_ASDR.tif")

cat("Done!\n")

