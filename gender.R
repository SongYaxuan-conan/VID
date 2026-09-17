library(ggplot2)
library(dplyr)
library(patchwork)
library(scales)


df_num <- read.csv("E:\\gbd\\vid\\gender\\1990+2023+num.csv", stringsAsFactors=FALSE)
df_rate <- read.csv("E:\\gbd\\vid\\gender\\1990+2023+rate.csv", stringsAsFactors=FALSE)

age_order <- c("65-69 years", "70-74 years", "75-79 years", "80+ years")
scale_factor <- 1000


colors <- list(
  female_bar  = "#F8B1B9",
  male_bar    = "#91C4E1",
  female_line = "#E64B65",
  male_line   = "#2A7BBB"
)


plot_gbd <- function(measure_num, measure_rate, ylab_right) {
  

  df_n <- df_num %>%
    filter(location_name == "Global", year == 2023,
           measure_name == measure_num,
           sex_name %in% c("Male","Female"), age_name %in% age_order) %>%
    mutate(age_name = factor(age_name, levels = age_order)) %>%
    group_by(age_name, sex_name) %>%
    summarise(cases = mean(val, na.rm=TRUE),
              cases_lower = mean(lower, na.rm=TRUE),
              cases_upper = mean(upper, na.rm=TRUE), .groups="drop")
  
  
  df_r <- df_rate %>%
    filter(location_name == "Global", year == 2023,
           measure_name == measure_rate,
           sex_name %in% c("Male","Female"), age_name %in% age_order) %>%
    mutate(age_name = factor(age_name, levels = age_order)) %>%
    group_by(age_name, sex_name) %>%
    summarise(rate_val = mean(val, na.rm=TRUE),
              rate_lower = mean(lower, na.rm=TRUE),
              rate_upper = mean(upper, na.rm=TRUE), .groups="drop")
  
  df <- inner_join(df_n, df_r, by=c("age_name","sex_name")) %>%
    mutate(rate_scaled = rate_val * scale_factor,
           rate_lower_scaled = rate_lower * scale_factor,
           rate_upper_scaled = rate_upper * scale_factor)
  
  ggplot(df, aes(x=age_name, group=sex_name)) +
    
    geom_ribbon(aes(ymin=rate_lower_scaled, ymax=rate_upper_scaled, fill=sex_name),
                alpha=0.2, position=position_dodge(0.8), show.legend = FALSE) +
  
    geom_line(aes(y=rate_scaled, color=sex_name),
              linewidth=1.2, position=position_dodge(0.8)) +
    
    geom_col(aes(y=cases, fill=sex_name),
             position=position_dodge(0.8), width=0.7, alpha=0.8) +
    
    geom_errorbar(aes(ymin=cases_lower, ymax=cases_upper, group=sex_name),
                  position=position_dodge(0.8), width=0.2, linewidth=0.6, color="black",
                  show.legend = FALSE) +
   
    scale_y_continuous(
      name = "Number of cases",
      sec.axis = sec_axis(~./scale_factor, name=ylab_right),
      labels = comma
    ) +
    
    scale_fill_manual(
      values=c("Female"=colors$female_bar, "Male"=colors$male_bar),
      labels=c("Female (Number and 95% UI)", "Male (Number and 95% UI)"),
      name = ""
    ) +
   
    scale_color_manual(
      values=c("Female"=colors$female_line, "Male"=colors$male_line),
      labels=c("Female (Rate and 95% UI)", "Male (Rate and 95% UI)"),
      name = ""
    ) +
    labs(x="") +
    
    theme_bw(base_size=14) +
    theme(
      panel.background = element_rect(fill="white"),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA),
      axis.line = element_line(color = "black"),
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_blank(),
      legend.box = "vertical",
      legend.margin = margin(t=0, b=5)
    )
}


p1 <- plot_gbd("Incidence", "Incidence", "Incidence per 100,000")
p2 <- plot_gbd("Deaths", "Deaths", "Death per 100,000")
p3 <- plot_gbd("DALYs (Disability-Adjusted Life Years)",
               "DALYs (Disability-Adjusted Life Years)",
               "DALYs per 100,000")


combined <- p1 / p2 / p3 +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "top",
    legend.box = "horizontal",
    legend.box.just = "center",
    plot.tag = element_text(face = "bold", size = 16)
  )


ggsave(
  filename = "E:\\gbd\\vid\\gender\\gender.jpg",
  plot = combined,
  width = 10, height = 18, dpi = 300
)
ggsave(
  filename = "E:\\gbd\\vid\\gender\\gender.tif",
  plot = combined,
  width = 10, height = 18, dpi = 300
)


print(combined)