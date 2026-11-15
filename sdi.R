need <- c("openxlsx", "mgcv", "ggplot2")
for (pkg in need) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(openxlsx)
library(mgcv)
library(ggplot2)


p <- "E:\\gbd\\vid\\sdi"

rate_file <- paste0(p, "1990-2023-rate.xlsx")
sdi_file  <- paste0(p, "sdi.xlsx")

cat("Working folder:", p, "\n")
cat("rate file exists:", file.exists(rate_file), "\n")
cat("sdi  file exists:", file.exists(sdi_file),  "\n")


rate <- read.xlsx(rate_file)
rate <- rate[rate$year >= 1990 & rate$year <= 2023, ]

cat("\n[rate] rows:", nrow(rate),
    "| locations:", length(unique(rate$location_name)),
    "| years:", length(unique(rate$year)), "\n")
cat("[rate] measure_name:", paste(unique(as.character(rate$measure_name)), collapse=" | "), "\n")
cat("[rate] sex_name    :", paste(unique(as.character(rate$sex_name)),     collapse=" | "), "\n")
cat("[rate] age_name    :", paste(unique(as.character(rate$age_name)),     collapse=" | "), "\n")
cat("[rate] metric_name :", paste(unique(as.character(rate$metric_name)),  collapse=" | "), "\n")


if (length(unique(rate$sex_name)) > 1) {
  rate <- rate[rate$sex_name == "Both", ]
  cat("[rate] after keeping Both:", nrow(rate), "rows\n")
}

rate$metric <- ifelse(rate$measure_name == "Incidence", "ASIR",
               ifelse(rate$measure_name == "Deaths", "ASMR", "ASDR"))
rate <- rate[, c("location_name", "year", "metric", "val")]
colnames(rate) <- c("location", "year", "metric", "val")


sdi <- read.xlsx(sdi_file)
sdi <- sdi[, c("location_name", "year_id", "mean_value")]
colnames(sdi) <- c("location", "year", "sdi")
sdi <- sdi[sdi$year >= 1990 & sdi$year <= 2023, ]
cat("\n[sdi] raw rows:", nrow(sdi),
    "| unique locations:", length(unique(sdi$location)), "\n")


locs <- unique(rate$location)
sdi  <- sdi[sdi$location %in% locs, ]
cat("[sdi] rows after keeping 22 regions:", nrow(sdi), "\n")


sdi <- aggregate(sdi ~ location + year, data = sdi, FUN = mean)
cat("[sdi] rows after de-duplication:", nrow(sdi), "(expect 748)\n")


df <- merge(rate, sdi, by = c("location", "year"))
cat("\n[df] rows:", nrow(df), "| per metric:", nrow(df) / 3, "(expect 748)\n")

miss <- setdiff(unique(rate$location), unique(df$location))
cat("[df] locations lost in merge:", length(miss), "\n")
if (length(miss) > 0) print(miss)


g23 <- df$val[df$location == "Global" & df$year == 2023 & df$metric == "ASIR"]
cat("[check] 2023 Global ASIR =", g23, "\n")
cat("        ~15.8 means ALL-AGES (correct for this section)\n")
cat("        ~89.3 means 65+ (wrong file for this section)\n")

df$location <- factor(df$location,
                      levels = c("Global", setdiff(unique(df$location), "Global")))


basis_check <- function(fit) {
  out <- tryCatch({
    chk <- as.data.frame(k.check(fit))
    ki  <- suppressWarnings(as.numeric(chk[1, 3]))
    pv  <- suppressWarnings(as.numeric(chk[1, 4]))
    c(k_index = ki, p_value = pv)
  }, error = function(e) c(k_index = NA, p_value = NA))
  out
}


cat("\n=== Basis dimension comparison ===\n")
for (mm in c("ASIR", "ASMR", "ASDR")) {
  d <- subset(df, metric == mm)
  d <- d[!is.na(d$sdi) & !is.na(d$val), ]
  for (kk in c(10, 20)) {
    f  <- gam(val ~ s(sdi, k = kk, bs = "cs"), data = d, method = "REML")
    sm <- summary(f)
    bc <- basis_check(f)
    cat(sprintf("%s | k=%2d | n=%d | edf=%5.2f | F=%7.1f | P=%-8s | k-index=%5.2f | p=%s\n",
                mm, kk, nrow(d), sm$edf, sm$s.table[1, 3],
                format.pval(sm$s.pv, digits = 3),
                bc["k_index"], format.pval(bc["p_value"], digits = 3)))
  }
}


plot_fun <- function(data, target, filename, kk = 20) {

  d <- subset(data, metric == target)
  d <- d[!is.na(d$sdi) & !is.na(d$val), ]

  fit <- gam(val ~ s(sdi, k = kk, bs = "cs"), data = d, method = "REML")
  sm  <- summary(fit)
  bc  <- basis_check(fit)

  cat(sprintf("[%s] k=%d | n=%d | edf=%.2f | F=%.1f | P=%s | k-index=%.2f | p=%s\n",
              target, kk, nrow(d), sm$edf, sm$s.table[1, 3],
              format.pval(sm$s.pv, digits = 3),
              bc["k_index"], format.pval(bc["p_value"], digits = 3)))

  sdi_x <- seq(min(d$sdi), max(d$sdi), length.out = 200)
  pr <- predict(fit, newdata = data.frame(sdi = sdi_x), se.fit = TRUE)

  pred_df <- data.frame(
    sdi     = sdi_x,
    pred_val = as.numeric(pr$fit),
    ci_low  = as.numeric(pr$fit - 1.96 * pr$se.fit),
    ci_up   = as.numeric(pr$fit + 1.96 * pr$se.fit))

  nl <- nlevels(droplevels(d$location))
  shape_vec <- rep(c(16,17,15,18,7,8,9,10,11,12,13,0,1,2,3,4,5,6,14,21,22,23,24,25),
                   length.out = nl)

  ylab <- paste0(target, " per 100,000 (all ages)")

  gg <- ggplot() +
    geom_ribbon(data = pred_df,
                aes(x = sdi, ymin = ci_low, ymax = ci_up),
                fill = "gray75", alpha = 0.4) +
    geom_line(data = pred_df,
              aes(x = sdi, y = pred_val),
              color = "#3355bb", linewidth = 1.1) +
    geom_line(data = d,
              aes(x = sdi, y = val, color = location, group = location),
              linewidth = 0.55) +
    geom_point(data = d,
               aes(x = sdi, y = val, color = location, shape = location),
               size = 1.7, alpha = 0.82) +
    scale_shape_manual(values = shape_vec) +
    labs(x = "Socio-demographic Index (SDI)", y = ylab,
         color = "Region", shape = "Region") +
    theme_bw() +
    theme(legend.position = "right", panel.grid.minor = element_blank())

  ggsave(paste0(p, filename), gg, width = 14, height = 10, dpi = 300)
  invisible(list(plot = gg, model = fit))
}


cat("\n=== Final models (k = 20) ===\n")
res_1 <- plot_fun(df, "ASIR", "SDI_ASIR.jpg",  kk = 20)
res_2 <- plot_fun(df, "ASIR", "SDI_ASIR.tiff", kk = 20)
res_3 <- plot_fun(df, "ASMR", "SDI_ASMR.jpg",  kk = 20)
res_4 <- plot_fun(df, "ASMR", "SDI_ASMR.tiff", kk = 20)
res_5 <- plot_fun(df, "ASDR", "SDI_ASDR.jpg",  kk = 20)
res_6 <- plot_fun(df, "ASDR", "SDI_ASDR.tiff", kk = 20)


out_list <- list()
for (mm in c("ASIR", "ASMR", "ASDR")) {
  d <- subset(df, metric == mm)
  f <- gam(val ~ s(sdi, k = 20, bs = "cs"), data = d, method = "REML")
  d$fit_val <- as.numeric(fitted(f))
  out_list[[mm]] <- d[, c("location", "year", "sdi", "val", "fit_val")]
}
openxlsx::write.xlsx(out_list, file = paste0(p, "SDI_GAM_fitted_k20.xlsx"))

cat("\nDone. Figures and fitted values written to:\n")
cat("  ", p, "\n")
