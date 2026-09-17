setwd("E:\\gbd\\vid\\map")


if (!dir.exists("output")) dir.create("output")


packages <- c("ggplot2", "maps", "dplyr", "openxlsx")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}


fix_country_names <- function(df){
  
  df$raw_location_name <- as.character(df$location_name)
 
  df$location_name <- as.character(df$location_name)
  
 
  df$location_name[df$location_name == 'United States of America'] <- 'USA'
  df$location_name[df$location_name == 'Russian Federation'] <- 'Russia'
  df$location_name[df$location_name == 'United Kingdom'] <- 'UK'
  df$location_name[df$location_name == 'Congo'] <- 'Republic of Congo'
  df$location_name[df$location_name == "Iran (Islamic Republic of)"] <- 'Iran'
  df$location_name[df$location_name == "Democratic People's Republic of Korea"] <- 'North Korea'
  df$location_name[df$location_name == "Taiwan (Province of China)"] <- 'Taiwan'
  df$location_name[df$location_name == "Republic of Korea"] <- 'South Korea'
  df$location_name[df$location_name == "United Republic of Tanzania"] <- 'Tanzania'
  df$location_name[df$location_name == "C?te d'Ivoire" | df$location_name == "C么te d'Ivoire" | df$location_name == "Côte d'Ivoire"] <- 'Ivory Coast'
  df$location_name[df$location_name == "Bolivia (Plurinational State of)"] <- 'Bolivia'
  df$location_name[df$location_name == "Venezuela (Bolivarian Republic of)"] <- 'Venezuela'
  df$location_name[df$location_name == "Czechia"] <- 'Czech Republic'
  df$location_name[df$location_name == "Republic of Moldova"] <- 'Moldova'
  df$location_name[df$location_name == "Viet Nam"] <- 'Vietnam'
  df$location_name[df$location_name == "Lao People's Democratic Republic"] <- 'Laos'
  df$location_name[df$location_name == "Syrian Arab Republic"] <- 'Syria'
  df$location_name[df$location_name == "North Macedonia"] <- 'Macedonia'
  df$location_name[df$location_name == "Micronesia (Federated States of)"] <- 'Micronesia'
  df$location_name[df$location_name == "Eswatini"] <- 'Swaziland'
  df$location_name[df$location_name == "Brunei Darussalam"] <- 'Brunei'
  df$location_name[df$location_name == "Cabo Verde"] <- 'Cape Verde'
  df$location_name[df$location_name == "United States Virgin Islands"] <- 'Virgin Islands'
  df$location_name[df$location_name == "Türkiye" | df$location_name == "T眉rkiye" | df$location_name == "Turkiye"] <- "Turkey"
  
  return(df)
}


VI <- read.csv('map2023.csv', header = TRUE, fileEncoding = "UTF-8")


VI_65plus <- VI %>%
  filter(age_name %in% c("65-69 years","70-74 years","75-79 years","80+ years")) %>%
  group_by(location_name, measure_name) %>%
  summarise(
    val = sum(val, na.rm=TRUE),
    lower = sum(lower, na.rm=TRUE),
    upper = sum(upper, na.rm=TRUE),
    .groups = 'drop'
  )


worldData <- map_data('world')


inc <- subset(VI_65plus, measure_name == "Incidence")
inc <- fix_country_names(inc)

total_inc <- full_join(worldData, inc, by=c("region"="location_name"))


total_inc <- total_inc %>% mutate(
  val2 = cut(val, 
             breaks=c(0,100,500,1000,5000,10000,50000,200000),
             labels=c("0-100","100-500","500-1000","1000-5000","5000-10000","10000-50000","50000+"),
             include.lowest = TRUE
  )
)


p_inc <- ggplot() +
  geom_polygon(data=total_inc, aes(long,lat,group=group,fill=val2), color="black", linewidth=0.2) +
  scale_fill_brewer(palette="Reds", na.value = "white") +
  theme_void() +
  labs(fill="Incidence count")


print(p_inc)
ggsave("output/2023_65plus_Incidence.jpg", p_inc, width=12, height=8, dpi=300)
ggsave("output/2023_65plus_Incidence.tif", p_inc, width=12, height=8, dpi=300)

write.xlsx(inc, "output/2023_65plus_Incidence.xlsx", row.Names=FALSE, fileEncoding = "UTF-8")


dea <- subset(VI_65plus, measure_name == "Deaths")
dea <- fix_country_names(dea)

total_dea <- full_join(worldData, dea, by=c("region"="location_name"))


total_dea <- total_dea %>% mutate(
  val2 = cut(val, 
             breaks=c(0,50,200,500,2000,5000,20000,100000),
             labels=c("0-50","50-200","200-500","500-2000","2000-5000","5000-20000","20000+"),
             include.lowest = TRUE
  )
)


p_dea <- ggplot() +
  geom_polygon(data=total_dea, aes(long,lat,group=group,fill=val2), color="black", linewidth=0.2) +
  scale_fill_brewer(palette="Greens", na.value = "white") +
  theme_void() +
  labs(fill="Deaths count")


print(p_dea)
ggsave("output/2023_65plus_Deaths.jpg", p_dea, width=12, height=8, dpi=300)
ggsave("output/2023_65plus_Deaths.tif", p_dea, width=12, height=8, dpi=300)

write.xlsx(dea, "output/2023_65plus_Deaths.xlsx", row.Names=FALSE, fileEncoding = "UTF-8")


dalys <- subset(VI_65plus, measure_name == "DALYs (Disability-Adjusted Life Years)")
dalys <- fix_country_names(dalys)

total_dalys <- full_join(worldData, dalys, by=c("region"="location_name"))


total_dalys <- total_dalys %>% mutate(
  val2 = cut(val, 
             breaks=c(0,1000,5000,10000,50000,100000,500000,2000000),
             labels=c("0-1k","1k-5k","5k-10k","10k-50k","50k-100k","100k-500k","500k+"),
             include.lowest = TRUE
  )
)


p_dalys <- ggplot() +
  geom_polygon(data=total_dalys, aes(long,lat,group=group,fill=val2), color="black", linewidth=0.2) +
  scale_fill_brewer(palette="Purples", na.value = "white") +
  theme_void() +
  labs(fill="DALYs")


print(p_dalys)
ggsave("output/2023_65plus_DALYs.jpg", p_dalys, width=12, height=8, dpi=300)
ggsave("output/2023_65plus_DALYs.tif", p_dalys, width=12, height=8, dpi=300)

write.xlsx(dalys, "output/2023_65plus_DALYs.xlsx", row.Names=FALSE, fileEncoding = "UTF-8")


cat("Done！All saved to the output folder。\n")
cat("list：\n")
cat("1. 2023_65plus_Incidence.jpg & 2023_65plus_Incidence.xlsx\n")
cat("2. 2023_65plus_Deaths.jpg & 2023_65plus_Deaths.xlsx \n")
cat("3. 2023_65plus_DALYs.jpg & 2023_65plus_DALYs.xlsx \n")
