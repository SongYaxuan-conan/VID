library(dplyr)
library(ggplot2)
library(scales)
library(ggsci)
library(patchwork)

df <- read.csv("E:\\gbd\\vid\\asir\\65+rate.csv", stringsAsFactors = FALSE)

common_theme <- theme_classic() +
  theme(
    legend.position = "right",          
    legend.box = "vertical",            
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.title = element_blank()      
  )


p_asir <- df %>%
  filter(
    measure_name == "Incidence",        
    metric_name == "Rate"              
  ) %>%
  group_by(year, sex_name) %>%
  summarise(val = sum(val, na.rm=TRUE),
            lower = sum(lower, na.rm=TRUE),
            upper = sum(upper, na.rm=TRUE), .groups="drop") %>%
  ggplot(aes(x = year, group = sex_name)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = sex_name), alpha = 0.15) +
  geom_line(aes(y = val, color = sex_name, linetype = sex_name), linewidth = 1) +
  labs(title = "A", x = "Year", y = "ASIR per 100,000") +  
  scale_x_continuous(breaks = seq(1990, 2023, 5)) +
  scale_color_lancet() + scale_fill_lancet() +
  common_theme

p_asmr <- df %>%
  filter(
    measure_name == "Deaths",           
    metric_name == "Rate"
  ) %>%
  group_by(year, sex_name) %>%
  summarise(val = sum(val, na.rm=TRUE),
            lower = sum(lower, na.rm=TRUE),
            upper = sum(upper, na.rm=TRUE), .groups="drop") %>%
  ggplot(aes(x = year, group = sex_name)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = sex_name), alpha = 0.15) +
  geom_line(aes(y = val, color = sex_name, linetype = sex_name), linewidth = 1) +
  labs(title = "B", x = "Year", y = "ASMR per 100,000") +  
  scale_x_continuous(breaks = seq(1990, 2023, 5)) +
  scale_color_lancet() + scale_fill_lancet() +
  common_theme


p_asdr <- df %>%
  filter(
    measure_name == "DALYs (Disability-Adjusted Life Years)",
    metric_name == "Rate"
  ) %>%
  group_by(year, sex_name) %>%
  summarise(val = sum(val, na.rm=TRUE),
            lower = sum(lower, na.rm=TRUE),
            upper = sum(upper, na.rm=TRUE), .groups="drop") %>%
  ggplot(aes(x = year, group = sex_name)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = sex_name), alpha = 0.15) +
  geom_line(aes(y = val, color = sex_name, linetype = sex_name), linewidth = 1) +
  labs(title = "C", x = "Year", y = "ASDR per 100,000") +  
  scale_x_continuous(breaks = seq(1990, 2023, 5)) +
  scale_color_lancet() + scale_fill_lancet() +
  common_theme

final_plot <- p_asir + p_asmr + p_asdr +
  plot_layout(ncol = 3, guides = "collect") &
  guides(
    color = guide_legend(ncol = 1),
    fill = guide_legend(ncol = 1),
    linetype = guide_legend(ncol = 1)
  )

print(final_plot)

ggsave("E:\\gbd\\vid\\asir\\65+asr.jpg", final_plot, width=18, height=6, dpi=300)
ggsave("E:\\gbd\\vid\\asir\\65+asr.tif", final_plot, width=18, height=6, dpi=300)

