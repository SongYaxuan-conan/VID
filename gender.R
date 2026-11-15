WORK_DIR    <- "E:\\gbd\\vid\\gender"
NUM_FILE    <- NA     
                      
RATE_FILE   <- NA     
TARGET_YEAR <- 2023
LOCATION    <- "Global"
AGE_ORDER   <- c("65-69 years", "70-74 years", "75-79 years", "80+ years")
SEX_KEEP    <- c("Male", "Female")

OUT_PREFIX  <- "gender_2023"
FIG_W       <- 10       
PANEL_H     <- 5.2      
DPI         <- 300
UI_STYLE    <- "ribbon" 
SF_ADJUST   <- 0.85     
                        
EXPECT_ROWS <- 8        

PANELS <- list(
  list(num  = "Incidence", rate = "Incidence",
       ylab = "Age-specific incidence rate per 100,000"),
  list(num  = "Deaths",    rate = "Deaths",
       ylab = "Age-specific death rate per 100,000"),
  list(num  = "DALY",      rate = "DALY",
       ylab = "Age-specific DALY rate per 100,000")
)

colors <- list(
  female_bar  = "#F8B1B9",
  male_bar    = "#91C4E1",
  female_line = "#E64B65",
  male_line   = "#2A7BBB"
)

options(stringsAsFactors = FALSE)
pkgs <- c("ggplot2", "dplyr", "patchwork", "scales", "openxlsx",
          "readxl", "stringr", "data.table")
need <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(need)) install.packages(need, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, function(p)
  suppressPackageStartupMessages(library(p, character.only = TRUE))))

if (!dir.exists(WORK_DIR)) stop("Folder does not exist: ", WORK_DIR)
setwd(WORK_DIR)

all_tab <- list.files(WORK_DIR, pattern = "\\.(xlsx|xls|csv)$",
                      ignore.case = TRUE, full.names = TRUE)
if (!length(all_tab)) stop("No xlsx/xls/csv file found in: ", WORK_DIR)

if (is.na(NUM_FILE)) {
  f_num <- grep("num", basename(all_tab), ignore.case = TRUE, value = TRUE)
  if (!length(f_num))
    stop("No file with 'num' in its name. Files found:\n  ",
         paste(basename(all_tab), collapse = "\n  "))
  f_num <- f_num[1]
} else {
  f_num <- file.path(WORK_DIR, NUM_FILE)
  if (!file.exists(f_num)) stop("Number file not found: ", f_num)
}

if (is.na(RATE_FILE)) {
  f_rate <- grep("rate", basename(all_tab), ignore.case = TRUE, value = TRUE)
  if (!length(f_rate))
    stop("No file with 'rate' in its name. Files found:\n  ",
         paste(basename(all_tab), collapse = "\n  "))
  f_rate <- f_rate[1]
} else {
  f_rate <- file.path(WORK_DIR, RATE_FILE)
  if (!file.exists(f_rate)) stop("Rate file not found: ", f_rate)
}

message("Number file: ", basename(f_num))
message("Rate   file: ", basename(f_rate))


read_any <- function(f) {
  if (grepl("\\.csv$", f, ignore.case = TRUE)) {
    as.data.frame(data.table::fread(f, showProgress = FALSE))
  } else {
    as.data.frame(openxlsx::read.xlsx(f))
  }
}


std_cols <- function(d) {
  nms <- names(d)
  find <- function(pats) {
    for (p in pats) {
      i <- grep(p, nms, ignore.case = TRUE)
      if (length(i)) return(nms[i[1]])
    }
    NA_character_
  }
  mp <- c(
    measure_name  = find(c("^measure_name$", "^measure$")),
    location_name = find(c("^location_name$", "^location$")),
    sex_name      = find(c("^sex_name$", "^sex$")),
    age_name      = find(c("^age_name$", "^age$")),
    metric_name   = find(c("^metric_name$", "^metric$")),
    cause_name    = find(c("^cause_name$", "^cause$")),
    year          = find(c("^year$")),
    val           = find(c("^val$", "^mean$")),
    lower         = find(c("^lower$")),
    upper         = find(c("^upper$"))
  )
  mp <- mp[!is.na(mp)]
  d  <- dplyr::rename(d, dplyr::all_of(mp))
  for (v in c("val", "lower", "upper")) {
    if (v %in% names(d)) d[[v]] <- suppressWarnings(as.numeric(d[[v]]))
  }
  for (v in c("measure_name", "location_name", "sex_name", "age_name",
              "metric_name")) {
    if (v %in% names(d)) d[[v]] <- as.character(d[[v]])
  }
  if ("year" %in% names(d)) d$year <- suppressWarnings(as.numeric(d$year))
  d
}

if (identical(normalizePath(f_num), normalizePath(f_rate))) {
  raw <- std_cols(read_any(f_num))
  if ("metric_name" %in% names(raw)) {
    df_num_raw  <- dplyr::filter(raw, grepl("number", metric_name,
                                            ignore.case = TRUE))
    df_rate_raw <- dplyr::filter(raw, grepl("rate", metric_name,
                                            ignore.case = TRUE))
  } else {
    df_num_raw  <- raw
    df_rate_raw <- raw
    warning("Same file for num and rate, and no metric_name column. ",
            "Using identical data for both -- please confirm.")
  }
} else {
  df_num_raw  <- std_cols(read_any(f_num))
  df_rate_raw <- std_cols(read_any(f_rate))
}

for (v in c("measure_name", "location_name", "sex_name", "age_name",
            "val", "lower", "upper")) {
  if (!v %in% names(df_num_raw))  stop("Number table is missing column: ", v)
  if (!v %in% names(df_rate_raw)) stop("Rate table is missing column: ", v)
}


filter_metric <- function(x, want) {
  if (!"metric_name" %in% names(x)) return(x)
  mv <- unique(as.character(x$metric_name))
  if (want %in% mv) return(dplyr::filter(x, metric_name == want))
  i <- grep(want, mv, ignore.case = TRUE)
  if (length(i)) return(dplyr::filter(x, metric_name == mv[i[1]]))
  message("  Note: metric_name has no '", want, "', skipping metric filter. ",
          "Available: ", paste(mv, collapse = " | "))
  x
}

pick_measure <- function(d, key, label) {
  ms <- unique(as.character(d$measure_name))
  i  <- grep(key, ms, ignore.case = TRUE)
  if (!length(i))
    stop(label, " has no measure matching '", key, "'. Available: ",
         paste(ms, collapse = " | "))
  if (length(i) > 1)
    message("  Note: ", label, " matched several measures, using '",
            ms[i[1]], "'")
  ms[i[1]]
}

check_location <- function(d, label) {
  lv <- unique(as.character(d$location_name))
  if (!LOCATION %in% lv)
    stop(label, " has no location '", LOCATION, "'. Available (first 10): ",
         paste(head(lv, 10), collapse = " | "))
  invisible(TRUE)
}

check_year <- function(d, label) {
  if (!"year" %in% names(d)) {
    warning(label, " has no year column, skipping year filter")
    return(Inf)
  }
  yv <- unique(d$year[!is.na(d$year)])
  if (TARGET_YEAR %in% yv) return(TARGET_YEAR)
  y <- max(yv)
  warning(label, " has no year ", TARGET_YEAR, ", using ", y, " instead")
  y
}


norm_age <- function(x) {
  x <- trimws(as.character(x))
  x <- stringr::str_replace_all(x, "[\u2013\u2014]", "-")
  hit <- stringr::str_detect(x, stringr::regex("80\\s*\\+|80\\s*plus",
                                               ignore_case = TRUE))
  x[hit] <- "80+ years"
  x
}

age_start <- function(x) suppressWarnings(as.numeric(stringr::str_extract(x, "^[0-9]+")))


make_panel_data <- function(num_key, rate_key) {

  
  m_n <- pick_measure(df_num_raw,  num_key,  "Number table")
  m_r <- pick_measure(df_rate_raw, rate_key, "Rate table")

  check_location(df_num_raw,  "Number table")
  check_location(df_rate_raw, "Rate table")
  y_n <- check_year(df_num_raw,  "Number table")
  y_r <- check_year(df_rate_raw, "Rate table")

  n_sel <- df_num_raw %>%
    dplyr::filter(location_name == LOCATION, measure_name == m_n) %>%
    filter_metric("Number") %>%
    dplyr::filter(sex_name %in% SEX_KEEP)
  r_sel <- df_rate_raw %>%
    dplyr::filter(location_name == LOCATION, measure_name == m_r) %>%
    filter_metric("Rate") %>%
    dplyr::filter(sex_name %in% SEX_KEEP)

  if (is.finite(y_n)) n_sel <- dplyr::filter(n_sel, year == y_n)
  if (is.finite(y_r)) r_sel <- dplyr::filter(r_sel, year == y_r)

  n_sel <- n_sel %>% dplyr::mutate(age_name = norm_age(age_name)) %>%
    dplyr::select(age_name, sex_name,
                  cases = val, cases_lower = lower, cases_upper = upper)
  r_sel <- r_sel %>% dplyr::mutate(age_name = norm_age(age_name)) %>%
    dplyr::select(age_name, sex_name,
                  rate_val = val, rate_lower = lower, rate_upper = upper)

  
  ages_avail <- unique(c(n_sel$age_name, r_sel$age_name))
  need_80 <- ("80+ years" %in% AGE_ORDER) && !("80+ years" %in% ages_avail)
  if (need_80) {
    cand <- ages_avail[vapply(ages_avail, function(a) {
      s <- age_start(a); !is.na(s) && s >= 80
    }, logical(1))]
    if (length(cand)) {
      message("  Detailed 80+ subgroups found; collapsing to '80+ years' ",
              "(population weighted): ", paste(cand, collapse = ", "))
    } else {
      warning("AGE_ORDER contains '80+ years' but no 80+ group was found ",
              "in the data")
    }
  }
  age_final <- AGE_ORDER[AGE_ORDER %in%
                           unique(c(ages_avail, if (need_80) "80+ years"))]
  miss <- setdiff(AGE_ORDER, age_final)
  if (length(miss))
    warning("Age group(s) absent from data, skipped: ",
            paste(miss, collapse = ", "))

  
  key <- setdiff(intersect(names(n_sel), names(r_sel)),
                 c("cases", "cases_lower", "cases_upper",
                   "rate_val", "rate_lower", "rate_upper"))
  j <- dplyr::inner_join(n_sel, r_sel, by = key)
  if (!nrow(j))
    stop("Join of Number and Rate is empty. Check measure / metric / year ",
         "filters.")

  
  j <- j %>% dplyr::mutate(
    pop = ifelse(is.finite(rate_val) & rate_val > 0 & is.finite(cases),
                 cases / rate_val * 1e5, NA_real_))

 
  if (need_80) {
    j <- j %>% dplyr::mutate(
      age_name = ifelse(!is.na(age_start(age_name)) &
                          age_start(age_name) >= 80 &
                          age_name != "80+ years",
                        "80+ years", age_name))
  }

  
  n_before <- nrow(j)
  j <- j %>% dplyr::filter(age_name %in% age_final)
  if (n_before > nrow(j))
    message("  Excluded ", n_before - nrow(j),
            " non-age-group row(s) (e.g. Age-standardized)")

  
  df <- j %>%
    dplyr::group_by(age_name, sex_name) %>%
    dplyr::summarise(
      cases       = sum(cases,       na.rm = TRUE),
      cases_lower = sum(cases_lower, na.rm = TRUE),
      cases_upper = sum(cases_upper, na.rm = TRUE),
      pop         = sum(pop,         na.rm = TRUE),
      r_rep       = mean(rate_val,   na.rm = TRUE),
      r_rep_lo    = mean(rate_lower, na.rm = TRUE),
      r_rep_up    = mean(rate_upper, na.rm = TRUE),
      n_row       = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      rate_val   = ifelse(pop > 0, cases       / pop * 1e5, r_rep),
      rate_lower = ifelse(pop > 0, cases_lower / pop * 1e5, r_rep_lo),
      rate_upper = ifelse(pop > 0, cases_upper / pop * 1e5, r_rep_up),
      age_name   = factor(age_name, levels = age_final)
    )

  if (any(df$n_row > 1))
    message("  Note: age x sex had multiple rows; Number was summed and ",
            "Rate recomputed population-weighted")

  
  chk <- df %>% dplyr::filter(is.finite(rate_val), is.finite(r_rep),
                              r_rep > 0) %>%
    dplyr::mutate(rel = abs(rate_val - r_rep) / r_rep)
  if (nrow(chk)) {
    mx <- max(chk$rel, na.rm = TRUE)
    if (mx < 1e-6) {
      message(sprintf(paste("  [OK] Rate check passed: max relative difference",
                            "between recomputed and reported rate = %.2e"), mx))
    } else {
      warning(sprintf(paste("Recomputed rate differs from reported rate by up",
                            "to %.3g (>1e-6). Check for duplicated rows or",
                            "mismatched num / rate denominators."), mx))
    }
  }

 
  if (!is.na(EXPECT_ROWS) && nrow(df) != EXPECT_ROWS)
    warning(sprintf("Panel has %d rows, expected %d (%d age groups x 2 sexes). ",
                    nrow(df), EXPECT_ROWS, length(age_final)),
            "An age group may be missing.")
  if (nrow(df) == 0) stop("Panel has no data")

  df %>% dplyr::select(age_name, sex_name, cases, cases_lower, cases_upper,
                       rate_val, rate_lower, rate_upper) %>%
    dplyr::arrange(age_name, sex_name)
}


nice_sf <- function(df) {
  top_c <- max(df$cases_upper, na.rm = TRUE)
  top_r <- max(c(df$rate_upper, df$rate_val), na.rm = TRUE)
  if (!is.finite(top_r) || top_r <= 0 || !is.finite(top_c) || top_c <= 0)
    return(1000)
  raw <- SF_ADJUST * top_c / top_r
  p10 <- 10^floor(log10(raw))
  cand <- c(1, 2, 2.5, 5, 10) * p10
  cand[which.min(abs(log(cand / raw)))]
}

plot_gbd <- function(df, ylab_right) {

  sf <- nice_sf(df)
  message("  scale_factor = ", sf)

  df <- df %>% dplyr::mutate(
    rate_scaled       = rate_val   * sf,
    rate_lower_scaled = rate_lower * sf,
    rate_upper_scaled = rate_upper * sf
  )

  ggplot(df, aes(x = age_name, group = sex_name)) +

    
    geom_col(aes(y = cases, fill = sex_name),
             position = position_dodge(0.8), width = 0.7, alpha = 0.85) +
    geom_errorbar(aes(ymin = cases_lower, ymax = cases_upper),
                  position = position_dodge(0.8), width = 0.2,
                  linewidth = 0.6, colour = "black", show.legend = FALSE) +

    
    (if (UI_STYLE == "ribbon")
      geom_ribbon(aes(ymin = rate_lower_scaled, ymax = rate_upper_scaled,
                      fill = sex_name),
                  alpha = 0.15, position = position_dodge(0.8),
                  show.legend = FALSE)) +
    (if (UI_STYLE == "dashed")
      geom_line(aes(y = rate_lower_scaled, colour = sex_name),
                linetype = "dashed", linewidth = 0.5, alpha = 0.8,
                position = position_dodge(0.8))) +
    (if (UI_STYLE == "dashed")
      geom_line(aes(y = rate_upper_scaled, colour = sex_name),
                linetype = "dashed", linewidth = 0.5, alpha = 0.8,
                position = position_dodge(0.8))) +

    geom_line(aes(y = rate_scaled, colour = sex_name),
              linewidth = 1.2, position = position_dodge(0.8)) +
    geom_point(aes(y = rate_scaled, colour = sex_name, shape = sex_name),
               size = 2.6, position = position_dodge(0.8)) +

    scale_y_continuous(
      name = "Number of cases",
      sec.axis = sec_axis(~ . / sf, name = ylab_right),
      labels = scales::comma,
      expand = expansion(mult = c(0, 0.05))) +

    scale_fill_manual(
      values = c("Female" = colors$female_bar, "Male" = colors$male_bar),
      labels = c("Female (Number and 95% UI)", "Male (Number and 95% UI)"),
      name = "") +
    scale_colour_manual(
      values = c("Female" = colors$female_line, "Male" = colors$male_line),
      labels = c("Female (Rate and 95% UI)", "Male (Rate and 95% UI)"),
      name = "") +
    scale_shape_manual(values = c("Female" = 16, "Male" = 17)) +

    guides(shape = "none",
           fill   = guide_legend(order = 1),
           colour = guide_legend(order = 2)) +

    labs(x = "") +
    theme_bw(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid       = element_blank(),
      panel.border     = element_rect(colour = "black", fill = NA),
      axis.line        = element_line(colour = "black"),
      legend.position  = "top",
      legend.title     = element_blank(),
      legend.margin    = margin(t = 0, b = 5),
      plot.title       = element_blank()
    )
}


plots <- list(); tables <- list()
for (i in seq_along(PANELS)) {
  pn <- PANELS[[i]]
  message("== Panel ", i, ": ", pn$num, " ==")
  d <- make_panel_data(pn$num, pn$rate)
  print(as.data.frame(d))
  if (!nrow(d)) { warning("Panel ", i, " has no data, skipped"); next }
  tables[[length(tables) + 1]] <- d %>% dplyr::mutate(panel = pn$num)
  plots[[length(plots) + 1]]   <- plot_gbd(d, pn$ylab)
}

if (!length(plots))
  stop("No panel was produced. Check the measure keywords and file contents.")

H <- PANEL_H * length(plots)
combined <- patchwork::wrap_plots(plots, ncol = 1) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "top",
        legend.box      = "horizontal",
        legend.box.just = "center",
        plot.tag        = element_text(face = "bold", size = 16))


all_tab_out <- dplyr::bind_rows(tables)
write.csv(all_tab_out, paste0(OUT_PREFIX, "_data.csv"), row.names = FALSE)
message("Wrote summary table: ", paste0(OUT_PREFIX, "_data.csv"))


ggsave(paste0(OUT_PREFIX, ".jpg"),  combined,
       width = FIG_W, height = H, dpi = DPI, bg = "white")
ggsave(paste0(OUT_PREFIX, ".tiff"), combined, device = "tiff",
       compression = "lzw", width = FIG_W, height = H, dpi = DPI, bg = "white")
ggsave(paste0(OUT_PREFIX, ".pdf"), combined,
       width = FIG_W, height = H)
message("Wrote figures: ",
        paste(paste0(OUT_PREFIX, c(".jpg", ".tiff", ".pdf")), collapse = " / "))

print(combined)
