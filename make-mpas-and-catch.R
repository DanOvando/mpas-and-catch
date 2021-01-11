####### Make MPAs and Catch #######
# Assess global and national changes in marine fish captures
# as a function of increases in MPA coverage

# setup -------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(countrycode)
library(here)
library(countrycode)
library(wdpar)
library(rnaturalearth)


get_wdpa <- TRUE

get_ram_data <- TRUE

get_fao_data <- TRUE

get_saup_data <- TRUE

min_years_catch <- 25

crazy_b <- 20

crazy_u <- 20

catchability <- 1e-6

# load data ---------------------------------------------------------------

countries <- countrycode::codelist$iso3c

# get protected area data -------------------------------------------------


if (get_wdpa) {
  global_pa <-
    tibble(
      country_iso3c = countrycode::codelist$iso3c,
      country = countrycode::codelist$country.name.en
    ) %>% 
    filter(!is.na(country_iso3c))
  swf <- safely(wdpa_fetch)
  
  global_pa <- global_pa %>%
    mutate(pas = map(country_iso3c,
                     ~ swf(.x, wait = TRUE)))
  
  worked <- map_lgl(map(global_pa$pas, "error"), is.null)
  
  swc <- safely(wdpar::wdpa_clean)
  
  
  global_mpas <- global_pa %>%
    filter(worked) %>% 
    mutate(pas = map(pas, "result")) %>% 
    mutate(has_marine = map_lgl(pas, ~ any(.x$MARINE == "2"))) %>% # check whether country has any marine pas
    filter(has_marine) %>% # keep only countries with some marine pa
    mutate(pas = map(
      pas,
      ~ .x %>% filter(MARINE == "2") %>% swc()) # clean up MPAs
    )
  
  
  worked <- map_lgl(map(global_mpas$pas, "error"), is.null)
  
  global_mpas <- global_mpas %>% 
    filter(worked) %>% 
    mutate(pas = map(pas, "result")) 
    
  
  write_rds(global_mpas, here("data", "global_mpas.rds"))
  
} else {
  # close get proteced areas
  
  global_mpas <- read_rds(here("data", "global_mpas.rds"))
  
  
}


global_mpas <- global_mpas  %>%
  select(-has_marine) %>%
  mutate(year_area = map(
    pas,
    ~ .x %>% as_tibble %>%  select(WDPAID, NAME, STATUS_YR, AREA_KM2) %>% janitor::clean_names()
  )) %>%
  unnest(cols = year_area)

# get EEZs ----------------------------------------------------------------



eezs <- sf::read_sf(here("data","World_EEZ_v11_20191118","eez_v11.shp")) %>% 
  mutate(country_iso3c = countrycode(GEONAME, "country.name.en",  "iso3c")) %>% filter(!is.na(country_iso3c))

eez_area <- eezs %>% 
  as_tibble() %>% 
  group_by(country_iso3c) %>% 
  summarise(eez_area_km2 = sum(AREA_KM2, na.rm = TRUE)) %>% 
  ungroup()
  


# eezs %>% 
#   ggplot(aes(GEONAME, AREA_KM2)) + 
#   geom_col() + 
#   coord_flip()

# a = eezs %>% 
#   group_by(GEONAME) %>% 
#   count()


# get ram data ------------------------------------------------------------



if (get_ram_data) {
  if (file.exists(here("data", "ram.zip")) == FALSE) {
    download.file(
      "https://www.dropbox.com/s/jpgz0a5s5of3qev/RAM%20v4.491%20Files%20(1-14-20).zip?dl=1",
      destfile = here("data", "ram.zip"),
      mode = "wb"
    )
    
    unzip(here("data", "ram.zip"), exdir = here("data", "ram")) # unzip = 'unzip' needed for windows
  }
  
  
  ram_dirs <- list.files(here("data", "ram"))
  
  ram_dirs <- ram_dirs[str_detect(ram_dirs, "RAM")]
  
  ram_files <-
    list.files(here("data", "ram", ram_dirs), recursive = TRUE)
  
  ram_files <- ram_files[str_detect(ram_files, ".RData")]
  
  ram_files <- ram_files[str_detect(ram_files, "Model Fit")]
  
  load(here("data", "ram", ram_dirs, ram_files[1]))
  
  # process ram data ------------------------------------------------------------
  
  stock <- stock %>%
    left_join(area, by = "areaid")
  # catches
  ram_catches <- tcbest.data %>%
    mutate(year = rownames(.) %>% as.integer()) %>%
    as_tibble() %>%
    gather(stockid, catch,-year)
  
  # B/Bmsy
  ram_b_v_bmsy <- divbpref.data %>%
    mutate(year = rownames(.) %>% as.integer()) %>%
    tibble() %>%
    gather(stockid, b_v_bmsy,-year)
  
  
  # U/Umsy
  ram_u_v_umsy <- divupref.data %>%
    mutate(year = rownames(.) %>% as.integer()) %>%
    as_tibble() %>%
    gather(stockid, u_v_umsy,-year)
  
  # Effort
  ram_effort <- effort.data %>%
    mutate(year = rownames(.) %>% as.integer()) %>%
    as_tibble() %>%
    gather(stockid, effort,-year)
  
  # biomass
  
  
  ram_total_biomass <- tbbest.data %>%
    mutate(year = rownames(.) %>% as.integer()) %>%
    as_tibble() %>%
    gather(stockid, total_biomass,-year)
  
  # ssb
  
  ram_ss_biomass <- ssb.data %>%
    mutate(year = rownames(.) %>% as.integer()) %>%
    as_tibble() %>%
    gather(stockid, ss_biomass,-year)
  
  
  ram_exp_rate <- ram_catches %>%
    left_join(ram_total_biomass, by = c("stockid", "year")) %>%
    mutate(exploitation_rate = catch / total_biomass) %>%
    select(-catch, -total_biomass)
  
  # ram_exp_rate <- erbest.data %>%
  #   mutate(year = rownames(.) %>% as.integer()) %>%
  #   as_tibble() %>%
  #   gather(stockid, exploitation_rate, -year)
  
  # put it together
  
  ram_data <- ram_catches %>%
    left_join(bioparams_values_views, by = "stockid") %>%
    left_join(ram_b_v_bmsy, by = c("stockid", "year")) %>%
    left_join(ram_u_v_umsy, by = c("stockid", "year")) %>%
    left_join(ram_exp_rate, by = c("stockid", "year")) %>%
    left_join(ram_effort, by = c("stockid", "year")) %>%
    left_join(ram_total_biomass, by = c("stockid", "year")) %>%
    left_join(ram_ss_biomass, by = c("stockid", "year")) %>%
    left_join(stock, by = "stockid") %>%
    select(stockid, scientificname, commonname, everything())
  
  
  # create new variables
  
  ram_data <- ram_data %>%
    mutate(tb_v_tb0 = total_biomass / TB0,
           ssb_v_ssb0 = ss_biomass / SSB0)
  
  # filter data -------------------------------------------------------------
  
  # for now, only include continuous catch series
  
  ram_data <- ram_data %>%
    filter(is.na(catch) == FALSE) %>%
    # filter(stockid == "ATBTUNAEATL") %>%
    group_by(stockid) %>%
    mutate(delta_year = year - lag(year)) %>%
    mutate(delta_year = case_when(year == min(year) ~ as.integer(1),
                                  TRUE ~ delta_year)) %>%
    mutate(missing_gaps = any(delta_year > 1)) %>%
    filter(missing_gaps == FALSE) %>%
    mutate(n_years = n_distinct(year)) %>%
    filter(n_years >= min_years_catch) %>%
    filter(all(b_v_bmsy < crazy_b, na.rm = TRUE),
           all(u_v_umsy < crazy_u, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(stockid) %>%
    mutate(
      has_tb0 = !all(is.na(TB0)),
      has_tb = all(!is.na(total_biomass)),
      first_catch_year = year[which(catch > 0)[1]]
    ) %>%
    filter(year >= first_catch_year) %>%
    mutate(
      pchange_effort = lead(u_v_umsy) / (u_v_umsy + 1e-6),
      cs_effort = (u_v_umsy - mean(u_v_umsy)) / sd(u_v_umsy),
      index = total_biomass * catchability,
      approx_cpue = catch / (u_v_umsy / catchability + 1e-3),
      b_rel = dplyr::case_when(
        has_tb0 ~ total_biomass / max(TB0),
        has_tb ~ total_biomass / max(total_biomass),
        TRUE ~ b_v_bmsy / 2.5
      )
    ) %>%
    mutate(approx_cpue = pmin(quantile(approx_cpue, 0.9, na.rm = TRUE), approx_cpue)) %>%
    ungroup()
  
  ram_b_plot <- ram_data %>%
    ggplot(aes(x = year, y = b_v_bmsy)) +
    geom_bin2d() +
    scale_fill_viridis_c()
  
  kobe_panels <- ram_data %>%
    filter(year >= 1950) %>%
    mutate(year_block = plyr::round_any(year, 10, floor)) %>%
    ggplot(aes(x = b_v_bmsy, y = u_v_umsy)) +
    geom_bin2d(binwidth = c(0.5, 0.5)) +
    facet_wrap(~ year_block) +
    scale_fill_viridis_c()
  
  # kobe_animation <- ram_data %>%
  #   ggplot(aes(x = b_v_bmsy, y = u_v_umsy)) +
  #   geom_bin2d() +
  #   transition_states(factor(year),
  #                     transition_length = 2,
  #                     state_length = 1) +
  #   scale_fill_viridis_c() +
  #   labs(title = "Year: {closest_state}")
  
  write_rds(ram_data, path = here("data", "ram", "ram-data.rds"))
  
} else {
  ram_data <- read_rds(path = here("data", "ram", "ram-data.rds"))
  
  
}

# get fao data ---------------------------------------------------------

if (get_fao_data) {
  if (!dir.exists(here("data", "fao"))) {
    dir.create(here("data", "fao"))
    
    download.file(
      "http://www.fao.org/fishery/static/Data/Capture_2019.1.0.zip",
      destfile = here("data", "fao.zip"),
      mode = "wb"
    )
    
    unzip(here("data", "fao.zip"), exdir = here("data", "fao"))
    
    file.remove(here("data", "fao.zip"))
    
    download.file(
      "http://www.fao.org/fishery/static/ASFIS/ASFIS_sp.zip",
      destfile = here("data", "asfis.zip"),
      mode = "wb"
    )
    
    unzip(here("data", "asfis.zip"), exdir = here("data", "fao"))
    
    file.remove(here("data", "asfis.zip"))
    
    
  }
  
  
  asfis <-
    read_delim(here("data", "fao", "ASFIS_sp_2020.txt"), delim = ",") %>%
    janitor::clean_names() %>%
    rename(isscaap_code = isscaap) %>%
    select(isscaap_code, scientific_name, taxocode) %>%
    unique()
  
  # major issue with NEIs here. There is no database that has both isscaap group and isscaap code, so you need
  # to do a complicated merge based on scientific name.
  
  fao_capture <-
    read_csv(here("data", "fao", "TS_FI_CAPTURE.csv")) %>%
    janitor::clean_names()
  
  sp_groups <-
    read_csv(here("data", "fao", "CL_FI_SPECIES_GROUPS.csv")) %>%
    janitor::clean_names() %>%
    select(x3alpha_code:identifier, contains("_en"), author:cpc_group) %>%
    rename(species_name_en = name_en) %>%
    left_join(asfis, by = c("taxonomic_code" = "taxocode"))
  
  # sp_groups %>%
  #   group_by(x3alpha_code) %>%
  #   summarise(ni = n_distinct(isscaap_group)) %>%
  #   arrange(desc(ni))
  
  country_groups <-
    read_csv(here("data", "fao", "CL_FI_COUNTRY_GROUPS.csv")) %>%
    janitor::clean_names() %>%
    mutate(un_code = as.numeric(un_code)) %>%
    select(un_code:iso3_code, contains("_en")) %>%
    rename(country_name_en = name_en,
           country_official_name_en = official_name_en)
  
  fao_areas <-
    read_csv(here("data", "fao", "CL_FI_WATERAREA_GROUPS.csv")) %>%
    janitor::clean_names() %>%
    mutate(fishing_area = as.numeric(code)) %>%
    select(fishing_area, contains("_en"), contains("group"))
  
  fao_capture <- fao_capture %>%
    left_join(sp_groups, by = c("species" = "x3alpha_code"))
  
  fao_capture <- fao_capture %>%
    left_join(country_groups, by = c("country" = "un_code")) %>%
    left_join(fao_areas, by = "fishing_area")
  
  fao_capture$fao_country_name <-
    countrycode::countrycode(fao_capture$country_name_en, "country.name", "un.name.en")
  
  fao_capture <- fao_capture %>%
    mutate(country = case_when(
      is.na(fao_country_name) ~ country_name_en,
      TRUE ~ fao_country_name
    )) %>%
    mutate(continent = countrycode::countrycode(country, "country.name", "continent"))
  
  fao_capture <- fao_capture %>%
    rename(
      isscaap_number = isscaap_code,
      common_name = species_name_en,
      capture = quantity,
      capture_units = unit,
      fao_area_code = fishing_area,
      fao_area = name_en
    ) %>%
    mutate(fao_stock = paste(common_name, country, fao_area, sep = '_'))
  
  fao_capture <- fao_capture %>%
    group_by(fao_stock) %>%
    nest() %>%
    ungroup() %>%
    mutate(id = 1:nrow(.)) %>%
    unnest(cols = data)
  
  fao_capture <- fao_capture %>%
    select(id, fao_stock, everything())
  
  fao <- fao_capture %>%
    filter(capture_units == "t",
           isscaap_number < 67)
  
  assign("fao", fao, envir = .GlobalEnv)
  
  
  fao_stock_lookup <- fao %>%
    select(scientific_name,
           common_name,
           country,
           fao_area,
           fao_area_code) %>%
    unique()
  
  assign("fao_stock_lookup", fao_stock_lookup, envir = .GlobalEnv)
  
  
  fao_species <- fao %>%
    select(scientific_name, common_name, isscaap_group, isscaap_number) %>%
    unique()
  
  assign("fao_species", fao_species, envir = .GlobalEnv)
  
  fao_genus <-
    str_split(fao_species$scientific_name, ' ', simplify = TRUE)[, 1]
  
  fao_genus <-  fao_species %>%
    mutate(genus = fao_genus) %>%
    group_by(genus, isscaap_group) %>%
    count() %>%
    group_by(genus) %>%
    filter(n == max(n)) %>%
    select(-n) %>%
    ungroup()
  
  write_rds(fao_capture, path = here("data", "fao", "fao-capture.rds"))
  
  
} else {
  fao_capture <-
    read_rds(file = here("data", "fao", "fao-capture.rds"))
  
  
}
# get saup data ------------------------------------------------------------

if (get_saup_data) {
  if (!dir.exists(here("data", "saup"))) {
    dir.create(here("data", "saup"), recursive = TRUE)
  }
  
  fao_area_codes <- sort(unique(fao_capture$fao_area_code))
  
  sdf <- safely(download.file)
  
  dfoo <- function(x, rest = 10) {
    tmp <- sdf(
      glue(
        "http://api.seaaroundus.org/api/v1/fao/tonnage/eez/?format=csv&limit=20000&sciname=true&region_id={x}"
      ),
      destfile = here("data", "saup", glue("saup-catch-fao-area-{x}.zip")),
      mode = "wb"
    )
    
    # in case you get denied, try a few more times with a breather between each attempt
    c <- 0
    while (!is.null(tmp$error) & c < 6) {
      Sys.sleep(rest)
      
      
      tmp <- sdf(
        glue(
          "http://api.seaaroundus.org/api/v1/fao/tonnage/eez/?format=csv&limit=20000&sciname=true&region_id={x}"
        ),
        destfile = here("data", "saup", glue("saup-catch-fao-area-{x}.zip")),
        mode = "wb"
      )
      
      c <- c + 1
      
      warning(glue("timed out trying to download area {x}"))
      
    }
    
  }
  
  
  purrr::walk(fao_area_codes, dfoo, rest = 10)
  
  zips <- list.files(here("data", "saup"))
  
  zips <- zips[str_detect(zips, "\\.zip")]
  
  
  unzipper <- function(z) {
    zdir <-  str_remove_all(z, "\\.zip")
    
    dir.create(here("data", "saup", zdir), recursive = TRUE)
    
    unzip(here("data", "saup", z),
          exdir = here("data", "saup", zdir))
    
    csvs <- list.files(here("data", "saup", zdir))
    
    csvs <- csvs[str_detect(csvs, ".csv")]
    
    fao_area_code <-
      as.integer(paste(str_extract_all(z, "\\d", simplify = TRUE), collapse = ""))
    
    tmp <-
      map_df(csvs, ~ read_csv(here("data", "saup", zdir, .x))) %>%
      mutate(fao_area_code = fao_area_code)
    
  }
  
  
  saup_capture <- purrr::map_df(zips, unzipper)
  
  
  write_rds(saup_capture, path = here("data", "saup", "saup-capture.rds"))
  
  #   test <- download.file("http://api.seaaroundus.org/api/v1/fao/tonnage/eez/?format=csv&limit=20000&sciname=false&region_id=48&region_id=67", destfile = "test.zip")
  
  
  
} else {
  saup_capture <- read_rds(here("data", "saup", "saup-capture.rds"))
  
}




# merge catch and MPA data ------------------------------------------------


fao_capture <- fao_capture %>%
  mutate(country_iso3c = countrycode(country, "country.name.en",  "iso3c")) %>% filter(!is.na(country_iso3c))

dim1 <- nrow(fao_capture)

tmp <- global_mpas %>% select(country_iso3c,status_yr,area_km2) %>% 
  group_by(country_iso3c, status_yr) %>% 
  summarise(area_km2_mpa = sum(area_km2, na.rm = TRUE)) %>% 
  filter(!is.na(status_yr)) %>% 
  arrange(country_iso3c, status_yr) %>% 
  group_by(country_iso3c) %>% 
  rename(year = status_yr)

country_year_mpa <- expand_grid(country_iso3c = unique(tmp$country_iso3c), year = 1950:2018) %>% 
  left_join(tmp, by = c("country_iso3c","year")) %>% 
  replace_na(list(area_km2_mpa = 0)) %>% 
  group_by(country_iso3c) %>% 
  mutate(cumu_area_km2_mpa = cumsum(area_km2_mpa)) %>% 
  ungroup() %>% 
  left_join(eez_area, by = "country_iso3c") %>% 
  mutate(p_eez_mpa = cumu_area_km2_mpa / eez_area_km2)

country_year_mpa %>% 
  filter(p_eez_mpa < 1) %>% 
  ggplot(aes(year, p_eez_mpa, group = country_iso3c)) +
  geom_line()

fao_capture <- fao_capture %>% 
  left_join(country_year_mpa, by = c("country_iso3c","year")) 

dim2 <- nrow(fao_capture)

if (dim1 != dim2){
  stop("something bad has happaned with MPA join")
}


# explore some data -------------------------------------------------------


country_capture <- fao_capture %>% 
  filter(capture_units == "t") %>% 
  group_by(country, year) %>% 
  summarise(capture = sum(capture, na.rm = TRUE), 
            p_eez_mpa = mean(p_eez_mpa, na.rm = TRUE)) %>% 
  group_by(country) %>% 
  mutate(sc_capture = scale(capture),
         max_capture = capture / max(capture)) %>% 
  ungroup()


country_capture %>% 
  filter(p_eez_mpa < 1) %>% 
  ggplot(aes(year, p_eez_mpa, group = country)) +
  geom_line()


country_capture %>% 
  filter(p_eez_mpa < 1) %>% 
  ggplot(aes(p_eez_mpa, sc_capture, group = country)) + 
  geom_line()

country_capture %>% 
  filter(p_eez_mpa < 1) %>% 
  select(country, year, max_capture, p_eez_mpa) %>% 
  pivot_longer(c(max_capture,p_eez_mpa), names_to = "variable", values_to = "value") %>% 
  ggplot(aes(year, value, color = variable, group = interaction(country, variable))) + 
  geom_line()


country_capture %>% 
  group_by(country) %>% 
  filter(all(p_eez_mpa < 1) & any(p_eez_mpa > 0.1)) %>% 
  ungroup() %>% 
  select(country, year, max_capture, p_eez_mpa) %>% 
  pivot_longer(c(max_capture,p_eez_mpa), names_to = "variable", values_to = "value") %>% 
  ggplot(aes(year, value, color = variable, group = interaction(country, variable))) + 
  geom_line() + 
  facet_wrap(~country) + 
  theme_minimal()

country_capture %>% 
  group_by(country) %>% 
  filter(all(p_eez_mpa < 1) & any(p_eez_mpa > 0.1)) %>% 
  ungroup() %>% 
  select(country, year, max_capture, p_eez_mpa) %>% 
  ggplot(aes(p_eez_mpa, value, color = variable, group = interaction(country, variable))) + 
  geom_line()

  