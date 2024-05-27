
library(tidyverse)

data <- read_csv("output/intensity.csv")

# get data for china and its top trading partners
# US, Hong Kong, Japan, Korea, Vietnam, India, Netherlands, Germany, Malaysia, Taiwan
# Australia, Russia, Brazil, Saudi Arabia, Indonesia

top_partners <- c("USA", "HKG", "JPN", "KOR", "VNM", "IND", "NLD", "DEU", 
                  "MYS", "TWN", "S19", "AUS", "RUS", "BRA", "SAU", "IDN")

china <- data %>% filter(i_iso3 == "CHN" | j_iso3 == "CHN") %>%
  filter(i_iso3 %in% c("CHN", top_partners), 
         j_iso3 %in% c("CHN", top_partners))

china_imports <- china %>% filter(j_iso3 == "CHN") %>% 
  select(-c(i, i_iso3, j, j_iso3, j_name)) %>%
  rename("exporter" = i_name) %>% 
  pivot_longer(c(c_ij, b_ij, i_ij), names_to = "index") %>%
  pivot_wider(names_from = "year", values_from = "value")

china_exports <- china %>% filter(i_iso3 == "CHN") %>% 
  select(-c(i, i_iso3, j, j_iso3, i_name)) %>%
  rename("importer" = j_name) %>% 
  pivot_longer(c(c_ij, b_ij, i_ij), names_to = "index") %>%
  pivot_wider(names_from = "year", values_from = "value")

# save tables
write_csv(china_imports, "output/china_imports.csv")
write_csv(china_exports, "output/china_exports.csv")
