# Script for creating folders per HUC

# Adds in indicator tif (derived from shapefile)
#  and topographic data only

# See other scripts for fuels and weather data

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  stars,
  terra)

#install.packages("rgdal", repos = "http://R-Forge.R-project.org")

### User settings ---------------------------------------------

base_folder <- "hucs_val"


### Data import ------------------------------------------------

(time_start <- Sys.time())

(hucs_shp <- st_read("data/data_huc/TxPrctRankRrkWipRffc.shp"))
#st_crs(hucs_shp)
crs_ca <- "EPSG:3310"

# Ran first to get intensive project done. 
#  Note: perhaps was not necessary and could have been done in place of resample here
# (aspect <- terra::rast("data_topo/LF2020_Asp_220_CONUS/LC20_Asp_220.tif"))
# aspect_trans <- terra::project(aspect, crs_ca)
# terra::writeRaster(aspect_trans, file.path("data_topo", "aspect.tif"))
# 
# (elev <- terra::rast("data_topo/LF2020_Elev_220_CONUS/LC20_Elev_220.tif")) 
# elev_trans <- terra::project(elev, crs_ca)
# terra::writeRaster(elev_trans, file.path("data_topo", "elevation.tif"))
# 
# (slope <- terra::rast("data_topo/LF2020_SlpD_220_CONUS/LC20_SlpD_220.tif"))
# slope_trans <- terra::project(slope, crs_ca)
# terra::writeRaster(slope_trans, file.path("data_topo", "slope.tif"))


(aspect <- terra::rast(file.path("data", "data_topo", "aspect.tif")))
(elev <- terra::rast(file.path("data", "data_topo", "elevation.tif")))
(slope <- terra::rast(file.path("data", "data_topo", "slope.tif")))


### Loop on ids ----------------------------------------------

#pull ids only
hucs_ids <- hucs_shp %>% pull(huc12)

for (i in seq_along(hucs_ids)){
  
  #this huc
  this_huc <- hucs_ids[i]
  this_huc_str <- paste0("huc_", this_huc)
  
  this_huc_shp <- hucs_shp %>% filter(huc12 == this_huc)
  #plot(this_huc_shp)

  #create 1200m buffer
  # the buffered polygon is ONLY used to create the extent needed for ALL input rasters
  this_huc_buffer <- st_buffer(this_huc_shp, 1200)
  this_huc_buffer_raster <- !is.na(st_rasterize(this_huc_buffer[,"huc12"], dx=30, dy=30)) %>% rast()

  #using the id field just to create binary 1/0 with is.na
  #https://gis.stackexchange.com/questions/450902/rasterize-sf-object-in-r-desired-cell-values-1-and-0
  #https://r-spatial.org/book/07-Introsf.html#sec-raster-to-vector
  this_huc_indicator_raw <- !is.na(st_rasterize(sf = this_huc_shp[,"huc12"], 
                                            dx = 30, dy = 30)) 
  thir <- rast(this_huc_indicator_raw) #make a raster in order to use extend()
  #using the buffered raster to make sure the huc indicator will be the same needed extent
  this_huc_indicator <- extend(thir, this_huc_buffer_raster, fill=0)
  
  #this topo
  #for each, resampling to huc indicator (so that all will have same extent, resolution)
  this_aspect <- resample(aspect, this_huc_indicator, threads=TRUE)
  this_elev <- resample(elev, this_huc_indicator, threads=TRUE)
  this_slope <- resample(slope, this_huc_indicator, threads=TRUE)
  
  
  #folders
  base_huc_dir <- file.path(base_folder, this_huc_str)
  
  inputs_dir <- file.path(base_huc_dir, "inputs")
  topo_dir <- file.path(inputs_dir, "topography") #under inputs
  
  weather_dir <- file.path(base_huc_dir, "weather")
  fuels_dir <- file.path(base_huc_dir, "fuels")

  #creating folders
  dir.create(topo_dir, recursive = TRUE) #won't overwrite #recursive
  dir.create(weather_dir, recursive = TRUE) #won't overwrite #recursive
  dir.create(fuels_dir, recursive = TRUE) #won't overwrite #recursive
  
  
  #write out indicator, topo
  terra::writeRaster(this_huc_indicator, 
                     file.path(inputs_dir, 
                               "huc_indicator.tif"))
  
  terra::writeRaster(this_aspect, 
                     file.path(topo_dir, 
                               "aspect.tif"))
  terra::writeRaster(this_elev, 
                     file.path(topo_dir, 
                               "elevation.tif"))
  terra::writeRaster(this_slope, 
                     file.path(topo_dir, 
                               "slope.tif"))
  
}

(time_end <- Sys.time())
(time_elapsed <- time_end - time_start)
