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
  terra,
  future.apply)

options(future.globals.onReference = "error")

#install.packages("rgdal", repos = "http://R-Forge.R-project.org")

### User settings ---------------------------------------------

base_folder <- "hucs_gf_noweather"

regions <- c("SC", "NC", "CC", "SN")

### Data import ------------------------------------------------

(time_start <- Sys.time())

hucs_shp <- st_read("data/data_huc/TxPrctRankRrkWipRffc.shp")

hr <- hucs_shp %>% 
  #get just the region names
  select(RRK_Rgn) %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  arrange(RRK_Rgn) %>% 
  #manually add code that is used in fuel file names
  #alpha sorted Central Coast, North Coast, Sierra Nevada, South Coast
  add_column(reg_code = c("CC", "NC", "SN", "SC")) %>% 
  #join back to get all fields
  left_join(hucs_shp, by = "RRK_Rgn") %>% 
  #create crosswalk from reg_code to huc12s
  select(RRK_Rgn, reg_code, huc12)


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

#https://cran.r-project.org/web/packages/future/vignettes/future-4-non-exportable-objects.html
#Functions wrap() and vect() of the terra package can be used as workaround 
# to marshal and unmarshal non-exportable terra objects
# UPDATE: wrap and unwrap is verrrry slow here. Read inside function instead. 

# (aspect <- terra::rast(file.path("data", "data_topo", "aspect.tif")) %>% 
#     wrap())
# (elev <- terra::rast(file.path("data", "data_topo", "elevation.tif")) %>% 
#     wrap())
# (slope <- terra::rast(file.path("data", "data_topo", "slope.tif")) %>% 
#     wrap())


### per huc ----------------------------------------------

# region, hucs
# pull folder making out of lapply
# add in region folder
# lapply
# future_lapply

plan(multisession, workers = availableCores(omit=1))


hucs_to_build <- hr %>%
  #only hucs in the specified regions
  filter(reg_code %in% regions) %>% 
  #drop extra field
  select(-RRK_Rgn) %>% 
  #create strings and folder paths
  mutate(huc_str = paste0('huc_', huc12),
         #base huc folder
         folder_base_huc = file.path(base_folder, reg_code, huc_str),
         #various subfolders
         folder_inputs = file.path(folder_base_huc, "inputs"),
         folder_topo = file.path(folder_inputs, "topography"), #under inputs
         folder_weather = file.path(folder_base_huc, "weather"),
         folder_fuels = file.path(folder_base_huc, "fuels"))


#Need to make all these folders first
# (probably can't in parallel, race conditions)
#function to create folder
create_all_folders <- function(r){
  this_row <- hucs_to_build[r,]
  dir.create(this_row[["folder_topo"]], recursive = TRUE) #won't overwrite #recursive
  dir.create(this_row[["folder_weather"]], recursive = TRUE) #won't overwrite #recursive
  dir.create(this_row[["folder_fuels"]], recursive = TRUE) #won't overwrite #recursive
}

#create the folders
folder_created <- lapply(1:nrow(hucs_to_build), create_all_folders)



add_topo <- function(r){
  this_row <- hucs_to_build[r,]
  
  #get shp for this huc
  this_huc_shp <- hucs_shp %>% 
    filter(huc12 == this_row[["huc12"]])
  
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
  
  #topo
  aspect <- terra::rast(file.path("data", "data_topo", "aspect.tif"))
  elev <- terra::rast(file.path("data", "data_topo", "elevation.tif"))
  slope <- terra::rast(file.path("data", "data_topo", "slope.tif"))
  
  #for each, resampling to huc indicator (so that all will have same extent, resolution)
  this_aspect <- resample(aspect, this_huc_indicator, threads=TRUE)
  this_elev <- resample(elev, this_huc_indicator, threads=TRUE)
  this_slope <- resample(slope, this_huc_indicator, threads=TRUE)
  
  # this_aspect <- resample(terra::unwrap(aspect), this_huc_indicator, threads=TRUE)
  # this_elev <- resample(terra::unwrap(elev), this_huc_indicator, threads=TRUE)
  # this_slope <- resample(terra::unwrap(slope), this_huc_indicator, threads=TRUE)
  
  #write out indicator, topo
  terra::writeRaster(this_huc_indicator, 
                     file.path(this_row[["folder_inputs"]], 
                               "huc_indicator.tif"))
  
  terra::writeRaster(this_aspect, 
                     file.path(this_row[["folder_topo"]], 
                               "aspect.tif"))
  terra::writeRaster(this_elev, 
                     file.path(this_row[["folder_topo"]], 
                               "elevation.tif"))
  terra::writeRaster(this_slope, 
                     file.path(this_row[["folder_topo"]], 
                               "slope.tif"))
  
  Sys.time()
  
}


system.time(added_topo <- future_lapply(1:nrow(hucs_to_build), add_topo))



(time_end <- Sys.time())
(time_elapsed <- time_end - time_start)

# timing notes - -----------

# SC
# > system.time(added_topo <- lapply(1:nrow(hucs_to_build), add_topo))
# user  system elapsed 
# 1323.81  576.91 1664.58 
# 
# 7 workers
# system.time(added_topo <- future_lapply(1:nrow(hucs_to_build), add_topo))
# user  system elapsed 
# 46.67   72.54  175.95
# 
# 15 workers> system.time(added_topo <- future_lapply(1:nrow(hucs_to_build), add_topo))
# user  system elapsed 
# 44.79   72.26  157.06 
# 
# SC, NC parallel 15
# system.time(added_topo <- future_lapply(1:nrow(hucs_to_build), add_topo))
# user  system elapsed 
# 87.21  140.24  327.58 

#regions <- c("SC", "NC", "CC", "SN")
# > system.time(added_topo <- future_lapply(1:nrow(hucs_to_build), add_topo))
# user  system elapsed 
# 96.04  168.42  450.53 
# > (time_elapsed <- time_end - time_start)
# Time difference of 7.612103 mins