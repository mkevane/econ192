
#===============================================================================================================
# Clean SJ ADU permit data from CA Housing element annual progress report  
# Permit data - https://data.ca.gov/dataset/housing-element-annual-progress-report-apr-data-by-jurisdiction-and-year 
#
# Revised 9/20/2024 WAS for SCUIRL
#===============================================================================================================

# clear environment
  rm(list = ls())
  
# Load packages 
  if (!require(pacman)) install.packages('pacman', repos = 'https://cran.rstudio.com')
  pacman::p_load(broom, dtplyr,  cowplot,estimatr, knitr) 
  p_load(ggpubr, gridExtra, ggrepel, ggmap, googleAnalyticsR) 
  p_load(leaflet, sf, tigris, tmap, tmaptools, tm) 
  p_load(data.table, tidyverse, tidyfast,gridExtra,ggpubr, grid, tidycensus) 
  
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

#===============================================================================
#   Load and clean permit data
#===============================================================================

# Read raw data on permits
  perm <- "https://github.com/mkevane/econ192/raw/data/adu_permits_sj_element.Rdata"
  load(url(perm))

# Clean data
  adu2 <- adu_permits_sj %>% 
    mutate(apn_num = as.numeric(gsub("[^0-9]","",APN))) %>% 
    distinct(apn_num, STREET_ADDRESS, .keep_all= TRUE)

# read in the addresses obtained from SCC parcel shape files:
  add <- "https://github.com/mkevane/econ192/raw/data/SJ_parcels_address_24.Rdata"
  load(url(add))
  
# Fix addresses and merge with ADU permits on APN and/or street address
  
