# Getting fbfm40 changes over 4 yrs (pixels/area)
# per HUC


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  terra,
  exactextractr,
  future.apply)

options(future.globals.onReference = "warning")

#Hitting issues with large numbers
# ABSOLUTELY CRITICAL SETTING, DO NOT CHANGE
# (maybe not needed on read in, but not finding out)
terraOptions(datatype="FLT8S") #FLT8S #"INT8S"


### user settings -------------------------------------------

#do all HUCs, scenarios (just using results file to get list)
res <- read_csv(file.path("results",
                               "absolute", #"datacube", 
                               "SN_SNbl_SNbw_absolute_20240423.csv")) %>% 
  mutate(HUC12 = as.character(HUC12)) 


### Data ---------------------------------------------------

raster_folder <- file.path("E:", "MAS", "blended_rasters",
                           "SN_yrcollapsed")

raster_files <- list.files(raster_folder,
                           full.names = TRUE,
                           pattern = "*fml.+tif$")



#huc shapefile
hucs_all <- st_read("data/data_huc/TxHucsTimingGroups.shp") 


### set up targets  -----------------------------------------------------

huc_scen <- res %>% 
  dplyr::select(HUC12, Region, Priority, TxIntensity, TxType) %>% 
  filter(!Priority == "baseline") %>% 
  filter(!Priority == "baseweather") %>% 
  distinct()


scenario_targets <- huc_scen %>% 
  #back to RFFC for Hybrid
  mutate(Priority = if_else(Priority == "Hybrid", "RFFC", Priority)) %>% 
  unite(col = search_pattern, sep = "_", remove = FALSE,
        Priority, TxIntensity, TxType) %>% 
  rowwise() %>% 
  mutate(raster_target = grep(raster_files, 
                              pattern = search_pattern, 
                              value = TRUE)) %>% 
  #stop rowwise
  ungroup() %>% 
  dplyr::select(-search_pattern)

#SN 1148 *27 = 30996


### function to apply -------------------------------------- 

#function for apply
get_scenario_fbfm4yr_counts <- function(r){
  
  this_row <- scenario_targets[r,]
  
  this_raster <- terra::rast(this_row[["raster_target"]])
  
  this_huc <- hucs_all %>% 
    filter(huc12 == this_row[["HUC12"]]) %>% 
    #transform to same crs as raster
    st_transform(st_crs(this_raster))
  
  
  this_fbfm4yr_counts <- exact_extract(this_raster, 
                                    this_huc, 
                                    function(value, coverage_fraction ) {table(value)})
  

  colnames(this_fbfm4yr_counts) <- "pixel_counts"
  
  this_fbfm4yr_counts <- rbind(this_fbfm4yr_counts, total = colSums(this_fbfm4yr_counts))
  
  this_result <- cbind(HUC12 = this_row[["HUC12"]],
                       Priority = this_row[["Priority"]],
                       TxType = this_row[["TxType"]],
                       TxIntensity = this_row[["TxIntensity"]],
                       fbfm4yrs = dimnames(this_fbfm4yr_counts)[[1]],
                       this_fbfm4yr_counts)
  #will complain otherwise when transforming later into a tibble
  rownames(this_result) <- NULL
  
  this_list <- list(this_result)
  
}



plan(multisession, workers = availableCores(omit=1))

system.time(zonal_raw <- future_sapply(1:nrow(scenario_targets),
                                    get_scenario_fbfm4yr_counts))

zonal <- do.call(rbind, zonal_raw) %>% 
  as_tibble() %>% 
  mutate(pixel_counts = as.numeric(pixel_counts))

zonal_wide <- zonal %>% 
  pivot_wider(names_from = fbfm4yrs, 
              names_prefix = "f", #R doesn't like field names starting with a number
              values_from = pixel_counts) %>% 
  #counts would be NA is that fbfm combo value was missing from the huc, replace with 0
  replace(is.na(.), 0) %>% 
  # sort better
  dplyr::select(HUC12, Priority, TxType, TxIntensity, sort(names(.)))
  

saveRDS(zonal_wide, file.path("qa", "SN_FBFM_4yrcollapsed_zonal_counts.RDS"))

#write_csv(zonal_wide, file.path("qa", "SN_FBFM_zonal_counts.csv"))
