# Script to replicate The Economist bar graph of population of Iran
# affected by missile strikes, with "affected" meaning within 1 km
# of a strike (strikes here are simulated randomly)
# Version: Michael Kevane, April 4 2026
# Revised: 

# Install packages 
# install.packages(c("terra", "geodata",  "tidyterra"))
# https://rspatial.github.io/geodata/authors.html#citation
  library(terra)
  library(geodata)
  library(tidyverse)
  library(tidyterra)
  library(sf)
  library(tmap)

# Clear environment
  rm(list=ls())
  t1 = Sys.time()
  
# Read in Iran shapefiles that have downloaded and unzipped from GADM (gadm.org)
  iran_border <- st_read("G:/My Drive/data/gadm41_IRN_shp/gadm41_IRN_0.shp")
  # some other GADM commands
  # iran_boundary <- geodata::gadm(country="IRN", level=0, path=tempdir())
  # iran_sf <- st_as_sf(geodata::gadm("IRN", level=1, path=tempdir()))
  # geodata::country_codes()

# Map the border using tmap
  tm_shape(iran_border) + tm_borders()
  
# Download population data for Iran, save tif file
# res is the resolution, in terms of arc-degrees of longitude
  # iran_res_point5_pop <- geodata::population(year = 2020, res=.5,
  #         ISO3 = "IRN",  path = "C:/Users/mkevane/Downloads/")
  # iran_res25_pop <- geodata::population(year = 2020, res=2.5,
  #        ISO3 = "IRN", path = "C:/Users/mkevane/Downloads/")
# save the raster file as a .tif file
  # writeRaster(iran_res25_pop, filename = file.path("C:/Users/mkevane/Downloads/", "iran_res25_pop.tif"), overwrite=TRUE)
  # writeRaster(iran_res_point5_pop, filename = file.path("C:/Users/mkevane/Downloads/", "iran_res_point5_pop.tif"), overwrite=TRUE)
# Read the raster file that is a .tif file
  iran_res25_pop <- rast("G:/My Drive/data/iran_res25_pop.tif")

# Crop to the polygon border
  iran_pop_cropped <- crop(iran_res25_pop, iran_border, mask = TRUE) 

# Turn into a simple features (sf) set of pixels (not a raster anymore)
  df <- as.polygons(iran_pop_cropped, dissolve=FALSE) 
  df <- st_as_sf(df, crs = 4326)
  class(df)
  tm_shape(df)+tm_borders()
  
# Turn into a dataframe to calculate the population from the density
# by using the area, determined by the arc-degree
  df1 <- as.data.frame(iran_pop_cropped, xy = TRUE)
  R <- 6371                  # Mean Earth radius in km
  res_arc_min <- 2.5         # Resolution in minutes
  res_rad <- (res_arc_min / 60) * (pi / 180) # Resolution in radians

  names(df1)
  df1 <- df1 %>% rename(pop_density=gpw_v4_population_density_rev11_2020_30_sec) %>% 
    mutate(long=x, lat=y,
    lat_rad = lat * (pi / 180),
    # Define northern and southern boundaries of the cell
    phi1 = lat_rad - (res_rad / 2),
    phi2 = lat_rad + (res_rad / 2),
    # Calculate area of the spherical trapezoid (km2)
    cell_area_km2 = (R^2) * res_rad * (sin(phi2) - sin(phi1)),
    pop_count = pop_density * cell_area_km2) %>% 
    dplyr::select(-x,-y,-lat_rad, -phi1, -phi2, -cell_area_km2, -pop_density)

  rm(res_arc_min,res_rad,R)

# Check that resulting population close to standard
  sum(df1$pop_count)
  
# Bind column (check that in same order?) to df and turn into sf
  pop <- cbind(df, df1)
  class(pop)
  
# Map pixels with population, rescale population to make more visible
  tm_shape(pop %>% mutate(pop_count=ifelse(pop_count>5000,5000,pop_count))) +
    tm_fill(fill = "pop_count", 
      fill.scale = tm_scale_intervals(values = "brewer.yl_or_rd"), 
      fill.legend = tm_legend("Pop. per pixel")) +
    tm_shape(iran_border) + 
    tm_borders(col = "black", lwd = 0.5) +
    # tm_title("Population distribution Iran 2020 (population per pixel capped at 5,000)") + 
    tm_layout(frame = FALSE, legend.position = c("left", "bottom"))
  
  set.seed(123) # Set seed for reproducibility
  strikes <- st_sample(iran_border, 100, type = "random")
  class(strikes)
  strikes <- st_sf(geometry = strikes)
  days_range <- seq(as.Date("2026-02-28"), as.Date("2026-03-31"), by = "day")
  strikes$date <- sample(days_range, 100, replace = TRUE)
  
  tm_shape(iran_border)+tm_borders()+tm_shape(strikes)+tm_dots()

# transform projection system to one in meters
# and calculate circles on 1km and crop to iran border
  points_proj <- st_transform(strikes, 3857)
  border_proj <- st_transform(iran_border, 3857)
  points_buffered <- st_buffer(points_proj, dist = 1000)
  # Crop (Intersect) the buffers with the border
  strike_circles <- st_intersection(points_buffered, border_proj)  %>% st_transform(4326)
  tm_shape(iran_border)+tm_borders()+tm_shape(strike_circles)+tm_borders()
  
# Zoom map to just be area around Tehran - define bbox to set map limits
  tehran_coords <- st_sfc(st_point(c(51.3890, 35.6892)), crs = 4326)
  tehran_coords <- st_transform(tehran_coords, 3857)
  tehran_buffer <- st_buffer(tehran_coords, dist = 250000) %>% 
    st_bbox() %>% st_as_sfc() %>% st_transform(4326) %>% st_bbox()

  tm_shape(iran_border, bbox = tehran_buffer) + 
    tm_borders(col = "black") + 
    tm_shape(strike_circles) + 
    tm_borders(col = "red") +
    tm_layout(main.title = "250km around Tehran", frame = TRUE)
  
# Transform to metric CRS and calculate intersection pop and strike circles
  strike_circles <- st_transform(strike_circles, 3857)
  pop <- st_transform(pop, 3857)
  pop$original_area <- units::set_units(st_area(pop), km^2)
  intersections <- st_intersection(pop, strike_circles)
  
# Calculate area of intersection polygons and calculate share of original area of pixel
  intersections <- intersections %>% mutate(fragment_area = st_area(.),
      pop_count_adj = as.numeric(pop_count * (fragment_area / original_area)))
  
# Sum the population of the cropped population areas (that intersect strike circles), by date
  pop__strike_date <- intersections %>%
    st_drop_geometry() %>%  group_by(date) %>%
    summarize(pop__strike_date = sum(pop_count_adj, na.rm = TRUE)) %>% 
    mutate(cum_pop = cumsum(pop__strike_date))
  
# Display using bar plot
  pop__strike_date %>% group_by(date) %>% ggplot(aes(x=date, y=cum_pop))+
    geom_bar(stat="identity")+theme_bw()
  
  t2 = Sys.time()
  t2-t1
  