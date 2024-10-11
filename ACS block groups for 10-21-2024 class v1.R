
#===============================================================================================================
# ACS block group data sample code
#
# 10/9/2024 WAS for SCUIRL
#===============================================================================================================

# clear environment
  rm(list = ls())
  
# Load packages 
  if (!require(pacman)) install.packages('pacman', repos = 'https://cran.rstudio.com')
  pacman::p_load(broom, dtplyr,  cowplot,estimatr, knitr) 
  p_load(ggpubr, gridExtra, ggrepel, ggmap, googleAnalyticsR) 
  p_load(leaflet, sf, tigris, tmap, tmaptools, tm) 
  p_load(data.table, tidyverse, tidyfast,gridExtra,ggpubr, grid, tidycensus, lubridate) 
  
# Set working directory
  main_folder="xxxxx/Econ 192 students"
  #setwd(main_folder)
  getwd()
  
# commands to merge with tracking: full_join_track, left_join_track, right_join_track
  merge_url <- "https://github.com/mkevane/econ192/raw/scripts/merge%20full%20join%20with%20tracking.R"
  source(merge_url)
  
# turn off scientific notation except for big numbers
  options(scipen = 9)
  options(digits=3) 

# ACS data for block groups in SC county
  # get tidycensus ACS data -  by county or tract or block group
  # First, get your own API key at http://api.census.gov/data/key_signup.html 
  # census_api_key("xxx", install=T)  

# This downloads a list of ACS variables/ tables for the 2018-2022 5-year ACS
  v20 <- load_variables(2022, "acs5")   

# How to download a table from ACS and reshape
  acs <- get_acs(geography = "block group", table = "B03002", geometry = F, year=2022,
               output = "tidy", state = "CA", county = "Santa Clara") 
  
  # reshape (pivot_wider): please compare acs and acs2 to see what has happened...
  acs2 <- acs %>% 
    filter(variable %in% c("B03002_001","B03002_004")) %>%  # select specific variables
    select(-NAME, -moe) %>% 
    pivot_wider(id_cols = GEOID, names_from = variable, values_from = estimate) %>% 
    rename(total = B03002_001, black_nonhisp = B03002_004) %>% 
    mutate(p_black_nonhisp = black_nonhisp/total) 

# geometry (shapes) for SC block groups
  scc <- get_acs(geography = "block group", variables = c("B03002_001"), geometry = T, year=2022,
                 output = "tidy", state = "CA", county = "Santa Clara") %>% 
    select(GEOID)
  
# merge acs2 to the data with geometry
  scc2 <- scc %>% left_join(acs2) 
# change the coordinate system
  scc2 <- st_transform(scc2, crs = 2227) 
# calculate areas and pop density
  scc2$area <- st_area(scc2)
  scc2 <- scc2 %>% 
   mutate(area = as.numeric(area),
          pop_density=1000000*total/area) 

# get city boundaries
  options(tigris_use_cache = TRUE)
  places_SCC <- places(state = 06)
  places_SCC <- st_transform(places_SCC, crs = 2227)
  places_SCC <- places_SCC %>%
   select(Place = NAMELSAD) 

# Join scc2 (block groups) to city boundary and select San Jose
  scc3 <- st_join(scc2, places_SCC)
  scc3 <- filter(scc3, Place == "San Jose city") 

# make a block group map
  tm_shape(scc3) +
    tm_polygons("p_black_nonhisp", title = "Proportion Black")
  #scc3 <- filter(scc3, area<16699254)

# join ADU centroids and block groups
  # note that ADU_centroids is made in another script 
  ADU <- st_join(ADU_centroids, scc3)
