
#===============================================================================================================
# Map geolocated ADU permit addresses in SJ... preliminary set of ADU permits
#
# Revised 9/16/2024 WAS for SCUIRL
#===============================================================================================================

  # clear environment
    rm(list = ls())
    
  # Load packages using pacman package
    if (!require(pacman)) install.packages('pacman', repos = 'https://cran.rstudio.com')
    pacman::p_load(broom, dtplyr, cowplot,estimatr, knitr) 
    p_load(ggpubr, gridExtra, ggrepel, ggmap, googleAnalyticsR) 
    p_load(leaflet, sf, tigris, tmap, tmaptools, tm) 
    p_load(data.table, tidyverse, tidyfast, gridExtra, ggpubr, grid, tidycensus) 
    
  # Set working directory on your computer
    # wd_folder <- "path to your folder"
    # setwd(wd_folder)
  
  # turn off scientific notation except for big numbers
    options(scipen = 9)
    options(digits=3) 
  
  # Read in data on permits
    adu <- "https://github.com/mkevane/econ192/raw/data/geocoded_ADU_prelim_09_09_2024.Rdata"
    load(url(adu)) 
    
### Map the permits two ways
    
  # 1 Using tmap dots
    
    ADU_SJ <- st_as_sf(s_u, coords = c("longitude", "latitude"), crs = 4326)   # CRS is Coordinate Reference System
    tmap_mode("view") 
    tm_shape(ADU_SJ) + tm_dots() 

  # 2 Leaflet interactive map
    
  # Make popup labels for map
    s_u$popup <- paste("<b>Address: </b>", s_u$address, 
                              "<br>", "<b>Date: </b>", s_u$ISSUEDATE)
  # create the map
    leaflet(s_u, width="100%") %>% addTiles() %>%
      setView(lat = 37.3, lng = -121.88, zoom = 10) %>% 
      addTiles(group = "OSM (default)") %>% 
      addProviderTiles(provider = "Esri.WorldStreetMap", group = "World StreetMap") %>%
      addProviderTiles(provider = "Esri.WorldImagery", group = "World Imagery") %>%
      addMarkers(~ location$lon, ~location$lat, popup = s_u$popup, clusterOptions = markerClusterOptions()) %>% 
      addLayersControl(baseGroups = c("OSM (default)", "World StreetMap", "World Imagery"),
        options = layersControlOptions(collapsed = FALSE))

  # things can go wrong...
    kevane <- filter(s_u, APN==46727044)  
    leaflet(kevane, width="100%") %>% addTiles() %>%
      setView(lat = 37.3, lng = -121.88, zoom = 10) %>% 
      addTiles(group = "OSM (default)") %>% 
      addProviderTiles(provider = "Esri.WorldStreetMap", group = "World StreetMap") %>%
      addProviderTiles(provider = "Esri.WorldImagery", group = "World Imagery") %>%
      addMarkers(~ location$lon, ~location$lat, popup = kevane$popup, clusterOptions = markerClusterOptions()) %>% 
      addLayersControl(baseGroups = c("OSM (default)", "World StreetMap", "World Imagery"),
                       options = layersControlOptions(collapsed = FALSE))
    
    