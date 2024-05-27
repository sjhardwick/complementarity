
library(data.table)
library(dtplyr)
library(tidyverse)

years <- c(2000, 2005, 2010, 2015, 2022) # input years of interest

# get list of countries
countries <- read_csv("data/BACI_HS96_V202401b/country_codes_V202401b.csv")

countries_i <- countries %>% 
  select(country_code, country_name, country_iso3) %>% 
  rename("i" = country_code, "i_name" = country_name, "i_iso3" = country_iso3)

countries_j <- countries %>% 
  select(country_code, country_name, country_iso3) %>% 
  rename("j" = country_code, "j_name" = country_name, "j_iso3" = country_iso3)

# settings for saving the index in loop below
results <- data.frame()
count <- 1

for (y in years) {
  
  # import data
  fresh_data <-
    fread(paste0("Data/BACI_HS96_V202401b/BACI_HS96_Y", y, "_V202401b.csv")) %>%
    as_tibble()
  
  # group trade by HS4 (heading)
  hs4_data <- lazy_dt(fresh_data) %>% # begin dtplyr
    # get HS4 from HS6 (k)
    mutate(hs4 = substr(as.character(k), 1, nchar(as.character(k)) - 2), 
           hs4 = as.numeric(hs4)) %>%
    group_by(i, j, hs4) %>% # group by HS4
    summarise(x_ijk = sum(v)) %>% # get x_ijk (trade value by HS4)
    # note: unit of value is 1000s USD
    arrange(i, j, hs4) %>% # sort
    as_tibble() # conclude dtplyr

  remove(fresh_data) # remove redundant data
  
  swapped_data <- lazy_dt(hs4_data) %>%
    rename("i" = j, "j" = i) %>% # swap so that i = importer
    group_by(i, hs4) %>% 
    summarise(m_ik = sum(x_ijk)) %>% # get m_ik and m_i
    group_by(i) %>% 
    mutate(m_i = sum(m_ik)) %>% 
    as_tibble()
  
  components <- lazy_dt(hs4_data) %>% 
    left_join(swapped_data, by = c("i", "hs4")) %>% 
    group_by(i, hs4) %>% # get x_ik
    mutate(x_ik = sum(x_ijk)) %>% ungroup() %>% 
    group_by(i) %>% mutate(x_i = sum(x_ijk)) %>% ungroup() %>% 
    mutate(m_w = sum(x_ijk)) %>% 
    group_by(hs4) %>% mutate(m_wk = sum(x_ijk)) %>% ungroup() %>%
    group_by(j, hs4) %>% mutate(m_jk = sum(x_ijk)) %>% ungroup() %>% 
    group_by(j) %>% mutate(m_j = sum(x_ijk)) %>% ungroup() %>%
    as_tibble()
  
  comp_bias <- components %>% 
    replace(is.na(.), 0) %>% 
    # calculate complementarity and bias index components 
    mutate(c_ijk = (x_ik / x_i) * ( (m_w - m_i) / (m_wk - m_ik) ) * (m_jk / m_j), 
           b_ijk = (x_ijk / x_ik) / (m_jk / (m_wk - m_ik))) %>% 
    # calculate indexes
    group_by(i, j) %>% mutate(c_ij = sum(c_ijk)) %>% ungroup() %>% 
    # ratio for bias index (see Drysdale & Garnaut 1982)
    mutate(c_ratio = c_ijk / c_ij) %>% 
    group_by(i, j) %>% 
    summarise(b_ij = round(sum(b_ijk * c_ratio), 2), 
              c_ij = round(first(c_ij), 2)) %>% 
    mutate(i_ij = round((b_ij * c_ij), 2), 
           year = y) %>% 
    left_join(countries_i, by = "i") %>% 
    left_join(countries_j, by = "j") %>% 
    select(year, i, i_name, i_iso3, j, j_name, j_iso3, c_ij, b_ij, i_ij)
  
  # calculate intensity index by alternative method and check equal
  # intensity <- hs4_data %>% 
  #   mutate(m_w = sum(x_ijk)) %>% 
  #   group_by(i) %>% mutate(x_i = sum(x_ijk)) %>% ungroup() %>% 
  #   group_by(j) %>% mutate(m_j = sum(x_ijk)) %>% ungroup() %>% 
  #   left_join(swapped_data, by = c("i", "hs4")) %>% 
  #   replace(is.na(.), 0) %>% 
  #   group_by(i, j) %>% mutate(x_ij = sum(x_ijk)) %>% ungroup() %>%
  #   mutate(i_ij = (x_ij / x_i) * (m_w - m_i) / m_j) %>% 
  #   group_by(i, j) %>% 
  #   summarise(i_ij2 = first(i_ij))
  
  # check both methods are same 
  # some may be 'false' but differences should be very small
  # test <- comp_bias %>% left_join(intensity, by = c("i", "j"))
  # round(test$i_ij, 1) == round(test$i_ij2, 1)
  
  results <-  bind_rows(results, comp_bias) 
  
}

write_csv(results, file = "output/intensity.csv") # write indexes to csv
