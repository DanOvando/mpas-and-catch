library(tidyverse)
library(sf)
library(countrycode)
library(here)
library(countrycode)
library(wdpar)
renv::install("prioritizr/wdpar")
# http://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_AS_shp.zip
# 
# http://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_AF_shp.zip
# 
# test <- xml2::read_html("http://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_AF_shp.zip")

# crap this is broken again
# if (!file.exists("protectedplanet.zip")) {
#   download.file(
#     "http://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_search_feadb1b4f30799a6dc3ad0b16116d3530ec4a477898f4e10e097e2030e167128_shp.zip",
#     destfile = "protectedplanet.zip"
#   )
#   
#   unzip("protectedplanet.zip")
# }

mpa_shapefile <- list.files(path = here("data-raw"))[str_detect(list.files(path = here("data-raw")), "polygons\\.shp")]

mpas <- sf::st_read(here("data-raw",mpa_shapefile)) %>% 
  janitor::clean_names()

mpa_data <- mpas %>% 
  select(-geometry) %>% 
  as_tibble()



mpa_country_lookup <-
  tibble(
    iso3 = unique(mpas$iso3),
    country = countrycode::countrycode(unique(mpas$iso3), origin = "iso3c",
                                       destination = "un.name.en")
  )

mpa_data <- mpa_data %>% 
  left_join(mpa_country_lookup, by = "iso3")


mpa_data %>% 
  filter(status_yr > 0) %>% 
  ggplot(aes(status_yr)) + 
  geom_histogram() + 
  facet_wrap(~status)

mpa_data %>% 
  filter(status_yr > 0, status_yr > 1950,
         !is.na(country)) %>% 
  group_by(country, status_yr) %>% 
  summarise(no_take_area = sum(no_tk_area)) %>% 
  group_by(country) %>% 
  arrange(status_yr) %>% 
  mutate(cumulative_nta = cumsum(no_take_area),
         total_nta = sum(no_take_area)) %>% 
  mutate(scaled_cumulative_nta = cumulative_nta / max(cumulative_nta)) %>% 
  ungroup() %>%
  filter(total_nta > 0) %>% 
  ggplot(aes(status_yr, scaled_cumulative_nta)) + 
  geom_line() + 
  facet_wrap(~country, scales = "free_y") + 
  scale_y_continuous(name = "Cumulative No Take Area") + 
  scale_x_continuous(name = "year", guide = guide_axis(n.dodge = 2)) + 
  theme_minimal() + 
  theme(axis.text.y = element_blank())
