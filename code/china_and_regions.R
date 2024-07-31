
library(arrow) # to save results as parquet file if desired
library(data.table)
library(dtplyr)
library(tidyverse)

years <- c(2015:2021) # input years of interest

# get list of countries
countries <- read_csv("data/BACI_HS96_V202401b/country_codes_V202401b.csv")

countries_i <- countries %>% 
  select(country_code, country_name, country_iso3) %>% 
  rename("i" = country_code, "i_name" = country_name, "i_iso3" = country_iso3)

countries_j <- countries %>% 
  select(country_code, country_name, country_iso3) %>% 
  rename("j" = country_code, "j_name" = country_name, "j_iso3" = country_iso3)

# get list of regions
regions <- read_csv("data/countries_cepii_sh.csv") %>% 
  mutate(region = if_else(country_name == "China", 
                          "China", 
                          region)) %>% 
  mutate(region = if_else(region == "Southeast Asia & the Pacific", 
                          "Southeast Asia & Oceania", 
                          region)) %>% 
  mutate(region = if_else(region == "Northeast Asia", 
                          "Northeast Asia (excl. China)", 
                          region))

regions_i <- regions %>% 
  select(country_code, region) %>% 
  rename("i" = country_code, "i_region" = region)

regions_j <- regions %>% 
  select(country_code, region) %>% 
  rename("j" = country_code, "j_region" = region)

# settings for saving the index in loop below
results <- data.frame()

for (y in years) {
  
  # import data
  fresh_data <-
    fread(paste0("data/BACI_HS96_V202401b/BACI_HS96_Y", y, "_V202401b.csv")) %>%
    as_tibble()
  
  # group trade by HS4 (heading)
  hs4_data <- lazy_dt(fresh_data) %>% # begin dtplyr
    # get HS4 from HS6 (k)
    mutate(hs4 = substr(as.character(k), 1, nchar(as.character(k)) - 2), 
           hs4 = as.numeric(hs4)) %>% 
    # add region
    left_join(regions_i, by = "i") %>% 
    left_join(regions_j, by = "j") %>%
    group_by(i_region, j_region, hs4) %>% # group by HS4
    summarise(x_ijk = sum(v)) %>% # get x_ijk (trade value by HS4)
    # note: unit of value is 1000s USD
    arrange(i_region, j_region, hs4) %>% # sort
    as_tibble() %>% # conclude dtplyr
    na.omit() 
  
  remove(fresh_data) # remove redundant data
  
  swapped_data <- lazy_dt(hs4_data) %>%
    # swap so that i = importer
    rename("i_region" = j_region, "j_region" = i_region) %>% 
    group_by(i_region, hs4) %>% 
    summarise(m_ik = sum(x_ijk)) %>% # get m_ik and m_i
    group_by(i_region) %>% 
    mutate(m_i = sum(m_ik)) %>% 
    ungroup() %>% 
    as_tibble()
  
  components <- lazy_dt(hs4_data) %>% 
    left_join(swapped_data, by = c("i_region", "hs4")) %>% 
    group_by(i_region, hs4) %>% # get x_ik
    mutate(x_ik = sum(x_ijk)) %>% 
    ungroup() %>% 
    group_by(i_region) %>% 
    mutate(x_i = sum(x_ijk)) %>% 
    ungroup() %>% 
    mutate(m_w = sum(x_ijk)) %>% 
    group_by(hs4) %>% 
    mutate(m_wk = sum(x_ijk)) %>% 
    ungroup() %>%
    group_by(j_region, hs4) %>% 
    mutate(m_jk = sum(x_ijk)) %>% 
    ungroup() %>% 
    group_by(j_region) %>% 
    mutate(m_j = sum(x_ijk)) %>% 
    ungroup() %>%
    as_tibble()
  
  comp_bias <- components %>% 
    replace(is.na(.), 0) %>% 
    # calculate complementarity and bias index components 
    mutate(c_ijk = (x_ik / x_i) * ((m_w - m_i) / (m_wk - m_ik)) * (m_jk / m_j), 
           b_ijk = (x_ijk / x_ik) / (m_jk / (m_wk - m_ik))) %>% 
    # calculate indexes
    group_by(i_region, j_region) %>% 
    mutate(c_ij = sum(c_ijk)) %>% 
    ungroup() %>% 
    # ratio for bias index (see Drysdale & Garnaut 1982)
    mutate(c_ratio = c_ijk / c_ij) %>% 
    group_by(i_region, j_region) %>% 
    summarise(b_ij = sum(b_ijk * c_ratio), 
              c_ij = first(c_ij)) %>% 
    mutate(i_ij = b_ij * c_ij, 
           year = y) %>% 
    select(year, i_region, j_region, c_ij, b_ij, i_ij)

  results <- bind_rows(results, comp_bias) 
  
}

# write indexes to csv
# write_csv(results, file = "output/intensity_china_regions.csv") 
# write indexes to parquet
# write_parquet(results, "output/intensity_china_regions.parquet") 
