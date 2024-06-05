# HUC stats of weather
# comparing MACAV2 and gridMET
# relative humidity (2nd percentile)
# wind speed (98th percentile)


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  terra,
  exactextractr,
  future.apply)

options(future.globals.onReference = "warning")


### Data ---------------------------------------------------

#relative humidity
rh_gm_file <- "qa/weather_macav2_gridmet/GridMetvsMaca/GridMetRhMin2percentile.tif"
rh_mv2_file <- "qa/weather_macav2_gridmet/GridMetvsMaca/Macav2RhMin2percentile.tif"

#wind speed
ws_gm_file <- "qa/weather_macav2_gridmet/GridMetvsMaca/GridMetVS98percentile.tif"
ws_mv2_file <- "qa/weather_macav2_gridmet/GridMetvsMaca/Macav2WS2percentile.tif"

#huc shapefile
hucs_sn <- st_read("data/data_huc/TxHucsTimingGroups.shp") %>% 
  filter(region == "SN") 

### Prep ----------------------------------------------------

#build target data set
list_rasters <- tibble(
  target_file = c(rh_gm_file, rh_mv2_file, ws_gm_file, ws_mv2_file))

targets <- hucs_sn %>% 
  st_drop_geometry() %>% 
  dplyr::select(huc12) %>% 
  cross_join(list_rasters) %>% 
  as_tibble()

# function for getting HUC extracts 
get_huc_extract <- function(r){
  
  this_row <- targets[r,]
  
  this_raster <- terra::rast(this_row[["target_file"]])
  
  this_huc <- hucs_sn %>% 
    filter(huc12 == this_row[["huc12"]]) %>% 
    #transform to same crs as raster
    st_transform(st_crs(this_raster))
  
  #extract HUC data
  this_stats <- exact_extract(this_raster, 
                              this_huc,
                              fun = c("mean", "median", "stdev", "min", "max"))
  
  #package up results
  this_results <- this_stats %>% 
    mutate(HUC12 = this_row[["huc12"]],
           source = tools::file_path_sans_ext(basename(this_row[["target_file"]]))) %>% 
    dplyr::select(HUC12, source, everything())
    
  #return list  
  list(this_results)
}

#CHANGE WORKERS AS NEEDED
plan(multisession, workers = availableCores(logical=FALSE, omit=1))

(start_time <- Sys.time())
all_stats <- future_sapply(1:nrow(targets), get_huc_extract)
(end_time <- Sys.time())
(end_time - start_time)

#combine
all_tbl <- do.call(rbind, all_stats) %>% 
  as_tibble() %>% 
  #split out name better
  mutate(weather_var = case_when(
    source=="GridMetRhMin2percentile" ~ "relative_humidity",
    source=="Macav2RhMin2percentile" ~ "relative_humidity",
    source=="GridMetVS98percentile" ~ "wind_speed",
    source=="Macav2WS2percentile" ~ "wind_speed"
  )) %>% 
  mutate(data_source = case_when(
    source=="GridMetRhMin2percentile" ~ "gridmet",
    source=="Macav2RhMin2percentile" ~ "macav2",
    source=="GridMetVS98percentile" ~ "gridmet",
    source=="Macav2WS2percentile" ~ "macav2"
  )) %>% 
  dplyr::select(HUC12, data_source, weather_var, 
                mean, median, stdev, min, max, 
                source) 
  
#save out
saveRDS(all_tbl, file.path("qa", 
                           "weather_macav2_gridmet",
                           "SN_huc_stats_rhws.RDS"))


