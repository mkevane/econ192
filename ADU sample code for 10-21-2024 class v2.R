
#===============================================================================================================
# Sample code for 10/21/2024 assignment  
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

### Read raw data on ADU permits, including issued but not completed
  ADU <- "https://github.com/mkevane/econ192/raw/data/ADU_building_permits_sj_city.Rdata"
  load(url(ADU))   # ADU_sj

# ADUs with missing APNs 
  APN_miss <- ADU_sj %>% filter(is.na(apn_num))

# ADUs with duplicate APNs
  APN_dup <- ADU_sj %>% 
    group_by(apn_num) %>% 
    mutate(count_apn = n()) %>% 
    ungroup() %>% 
    filter(count_apn>1)

### Read SJ parcel shape files in 2 batches:
  p1 <- "https://github.com/mkevane/econ192/raw/data/SJ_parcels_2024_1.Rdata"
  load(url(p1))
  p2 <- "https://github.com/mkevane/econ192/raw/data/SJ_parcels_2024_2.Rdata"
  load(url(p2))
  
# Bind them into one file
  SJ_parcels_2024 <- SJ_parcels_2024_1 %>% 
    bind_rows(SJ_parcels_2024_2) %>% 
    rename(APN = apn) %>%  # consistent naming of APNs
    mutate(apn_num = as.numeric(APN))
  
### Merge the ADU permits to the parcel shapes 
  dfj <- ADU_sj %>% 
    left_join_track(SJ_parcels_2024, by="apn_num")
  
# APNs that don't have a match in the shape file
  shape_miss <- dfj %>% filter(is.na(APN.y))
  
### The following is just a couple of tips for simple mapping:
  
# Note that to map anything the data needs to be an "sf" object
  dfjj <- dfj %>% filter(!is.na(APN.y))
  ADU_SJ <- st_as_sf(dfjj)   
  tmap_mode("view") 
  tm_shape(ADU_SJ) + tm_borders() 
  
# Locate centroids of the ADU parcels
  # you may want to save this data set for later use
  ADU_centroids <- st_centroid(ADU_SJ)
  # map 'em
  tm_shape(ADU_centroids) + tm_dots(col = "red", size = 0.01)
  
