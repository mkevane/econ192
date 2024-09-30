
#===============================================================================================================
# Dealing with dates, shape files, etc. 
#
# 9/27/2024 WAS for SCUIRL
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

### Read raw data on permits
  perm <- "https://github.com/mkevane/econ192/raw/data/building_permits_sj_city.Rdata"
  load(url(perm))
  
# Filter on ADUs and select a few vars to look at
  df1 <- bp_sj %>% 
    filter(SUBDESC == "2nd Unit Added") %>% 
    select(APN, JOBLOCATION, ISSUEDATE, FINALDATE) 

### Dealing with dates: Look at the date columns... note formats
  
# To clean dates we can use lubridate functions, but need to reconcile formats
  df2 <- df1 %>% 
    select(ISSUEDATE) %>% 
    mutate(date_1 = mdy(ISSUEDATE), 
           date_2 = dmy(ISSUEDATE),
           year_1 = year(as_date(date_1)),
           year_2 = year(as_date(date_2)))

### Cleaning up the APNs for merging with shape file
  
# Could force APN to be numeric... why might we want to?
  df3 <- df1 %>% 
    mutate(apn_num = as.numeric(APN))

# How to remove duplicate rows (exactly identical for all variables)
  # You don't always want to do this!
  df3 <- distinct(df3)

# Look at the APNs
# Some APNs are missing: What to do with them?
  APN_miss <- df3 %>% 
    filter(is.na(APN)) %>% 
    select(JOBLOCATION, FINALDATE)
# Duplicate APNs: What to do about them?
  APN_dup <- df3 %>% 
    group_by(APN) %>% 
    mutate(count_APN = n()) %>% 
    ungroup() %>% 
    filter(count_APN>1)
# You can (cautiously!) remove rows that are duplicates just for specific variables 
  df4 <- distinct(df3, APN, JOBLOCATION, .keep_all= TRUE)
  df5 <- distinct(df3, APN, .keep_all= TRUE)
  
### Read SJ parcel shape files in 2 batches:
  p1 <- "https://github.com/mkevane/econ192/raw/data/SJ_parcels_2024_1.Rdata"
  load(url(p1))
  p2 <- "https://github.com/mkevane/econ192/raw/data/SJ_parcels_2024_2.Rdata"
  load(url(p2))
  
# Bind them into one file
  SJ_parcels_2024 <- SJ_parcels_2024_1 %>% 
    bind_rows(SJ_parcels_2024_2) %>% 
    rename(APN = apn) %>% 
    mutate(apn_num = as.numeric(APN))
  
# Let's take a look at what is in this file

### Now let's merge the permits to the shapes 
  dfj <- df3 %>% 
    left_join_track(SJ_parcels_2024, by="apn_num")
  
# Examine the APNs that don't have a match in the shape file:
  dfj1 <- dfj %>% filter(is.na(APN.y))
  
# Note that to map anything the data needs to be an "sf" object
  dfjj <- dfj %>% filter(!is.na(APN.y))
  ADU_SJ <- st_as_sf(dfjj)   
  tmap_mode("view") 
  tm_shape(ADU_SJ) + tm_borders() 
  