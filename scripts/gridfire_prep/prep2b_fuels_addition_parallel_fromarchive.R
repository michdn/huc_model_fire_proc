# Script for adding in fuel data into existing structure
#  Designed to add fuels for a region (grep patterns below)

# As such, there is no checking on runids or total fuels processed. 
# User must keep track of what fuels have been processed.

# This is old 202401 prep2 REWRITTEN to be parallelized and run on bluejay. (for 202403)

# Pulling fuels got VERY complicated from the shared drive with 
#  the new NC trt7, the trt6 reruns, the new 2039, etc. 
# 202405: Now that they were previously pulled to local archive
#  (see 'blended_raster_fuels_archive.R')
# this script will use those already-gathered files
#  since we don't need to update them for the 202405 reruns. 

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  terra,
  stars,
  future.apply)

options(future.globals.onReference = "warning")

### User settings ---------------------------------------------

region_to_run <- "CC" #"SN" "SC", CC", "NC"

base_folder <- file.path("E:", "MAS", 
                         "gridfire_prep", 
                         "hucs_gf_noweather")

archive <- file.path("E:", "MAS", 
                     "blended_rasters", 
                     region_to_run)


### Data finding: fuels ---------------------------------------

#working with files that look like:
# E:/MAS/blended_rasters/SN/RunID1_SN_WUI_500k_trt1_2024_cbd_32611_FF_FVS.tif


fuel_files <- list.files(archive,
                         full.names = TRUE,
                         #the $ is important to not match the qgis xmls
                         pattern="RunID.+tif$")

length(fuel_files)
if (!length(fuel_files) == 540){
  stop("Wrong number of fuel files")
}



### HUC data import ------------------------------------------------

# for making list of HUCs to do

hucs_all <- st_read("data/data_huc/TxHucsTimingGroups.shp")

hr <- hucs_all %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  dplyr::select(huc12, region) %>% 
  rename(HUC12 = huc12,
         #matching join field name below
         reg_code = region)

rm(hucs_all)

### Create input dataframe & go -----------------------------------------------

#create input dataframe
# column fuel file
# pull out region (column)
# add rows for each huc in region 


input_df <- tibble(fuel_file = fuel_files) %>% 
  mutate(file_name = tools::file_path_sans_ext(basename(fuel_file))) %>% 
  separate_wider_delim(cols = file_name, delim = "_",
                       names = c("runid", "reg_code",
                                 "priority", "intensity", "trt",
                                 "yr", "layer", "crs", "ff", "fvs"),
                       cols_remove = TRUE) %>%
  #remove unwanted (could have used NA in names, but for troubleshooting doing it step by step)
  select(-c(crs, ff, fvs)) %>%
  #run id str / full scenario
  unite('runid_str', runid, reg_code, priority, intensity, trt, sep="_", remove=FALSE) %>% 
  #join with hucs to get row for each huc for each fuel file
  left_join(hr, 
            by = join_by(reg_code), 
            relationship = "many-to-many") %>% 
  #create runid and folder names
  mutate(
    folder_huc = file.path(base_folder, reg_code, paste0("huc_", HUC12)),
    folder_h_fuels = file.path(folder_huc, "fuels"),
    folder_h_f_scenario = file.path(folder_h_fuels, runid_str),
    folder_h_f_s_yr = file.path(folder_h_f_scenario, yr)) %>% 
  #remove intermediates
  dplyr::select(-c(
    runid, priority, intensity, trt,
    reg_code, runid_str, yr, 
    folder_h_fuels, folder_h_f_scenario))



#Need to make all these folders first
# (can't in parallel, race conditions)
#function to create folder
create_all_folders <- function(r){
  this_row <- input_df[r,]
  this_folder_h_f_s_yr <- this_row[["folder_h_f_s_yr"]]
  dir.create(this_folder_h_f_s_yr, recursive = TRUE) #won't overwrite #recursive
}

#create the folders
(time_start <- Sys.time())
folder_created <- lapply(1:nrow(input_df), create_all_folders)
(time_end <- Sys.time())
(time_elapsed <- time_end - time_start)


#function for adding the huc-fuels to the correct folder
add_fuels_to_huc <- function(r){
  
  this_row <- input_df[r,]
  #this_file_name <- this_row[["file_name"]]
  this_huc_folder <- this_row[["folder_huc"]]
  this_folder_h_f_s_yr <- this_row[["folder_h_f_s_yr"]]
  this_layer <- this_row[["layer"]]
  
  #load fuel raster
  this_fuel <- terra::rast(this_row[["fuel_file"]])
  
  #most will be bilinear, but fbfm40 needs nearest neighbor (categorical)
  this_method <- if_else(this_layer == "fml", "near", "bilinear")
  
  #get huc indicator to exactly align extents
  this_indicator <- terra::rast(file.path(this_huc_folder, 
                                          "inputs",
                                          "huc_indicator.tif"))
  
  #project fuel, using indicator for extent and resolution
  this_fuel_huc <- project(this_fuel, 
                           this_indicator,
                           method=this_method,
                           threads=T)
  
  #saving into existing folder
  # default GeoTiff written with LZW compression
  terra::writeRaster(this_fuel_huc, 
                     file.path(this_folder_h_f_s_yr, #this_fuels_yr,
                               paste0(this_layer, ".tif")), 
                     overwrite=T) #change to T if need to 
  
  
  log <- c(this_row[["fuel_file"]], 
           this_row[["HUC12"]], 
           format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  
}

(time_start <- Sys.time())

plan(multisession, workers = availableCores(omit=1))

system.time(added_fuels <- future_lapply(1:nrow(input_df), add_fuels_to_huc))

(time_end <- Sys.time())
(time_elapsed <- time_end - time_start)

fuel_log <- do.call(rbind, added_fuels)
colnames(fuel_log) <- c("fuel_file", "HUC12", "datetime")

fuel_log <- as_tibble(fuel_log) 

write_csv(fuel_log, 
          file = paste0("log_fuels_", region_to_run, ".csv"),
          append = TRUE)


# 202405 Timing notes from archive -----------------------
# SN Time difference of 10.30523 hours
# SC Time difference of 4.362524 hours
# CC Time difference of 3.808765 hours

# Timing notes from reading from share ----------------------------------
# 2 hours extra for reading from share
# On Bluejay
#SC
# > plan(multisession, workers = availableCores(omit=1))
#   > system.time(added_fuels <- future_lapply(1:nrow(input_df), add_fuels_to_huc))
# user   system  elapsed 
# 23.78     6.92 22258.89 
#   > (time_end <- Sys.time())
# [1] "2024-03-11 18:00:49 PDT"
# > (time_elapsed <- time_end - time_start)
# Time difference of 6.224809 hours
# Original loop version on laptop ran (segmented) for ~17 hours

# CC
# > plan(multisession, workers = availableCores(omit=1))
# > system.time(added_fuels <- future_lapply(1:nrow(input_df), add_fuels_to_huc))
# user   system  elapsed 
# 22.92     6.94 21314.01 
# > (time_end <- Sys.time())
# [1] "2024-03-12 20:35:13 PDT"
# > (time_elapsed <- time_end - time_start)
# Time difference of 5.958782 hours

#SN
# system.time(added_fuels <- future_lapply(1:nrow(input_df), add_fuels_to_huc))
# user   system  elapsed 
# 52.78    16.07 48132.18 
# [1] "2024-03-25 05:33:33 PDT"
# > (time_elapsed <- time_end - time_start)
# Time difference of 13.46338 hours


