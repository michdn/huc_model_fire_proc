# Script for adding in fuel data into existing structure
#  Designed to add fuels for a region (grep patterns below)

# As such, there is no checking on runids or total fuels processed. 
# User must keep track of what fuels have been processed.

# This is old prep2 REWRITTEN to be parallelized and run on bluejay. 

# ALTERNATIVE VERSION FOR BASELINE RUNS
# Run for each region 

# Use TWO different baseline folders!!
# One for treatment baseline 'baseline'
#  and one for treatment and weather baseline 'baseweather'

# creates RunID like setup so doesn't break anything downstream

# For GF input folders, rename region as region_BL or region_BW?

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  terra,
  stars,
  future.apply)

options(future.globals.onReference = "error")

### User settings ---------------------------------------------

region_to_run <- "NC" #"SC", "CC", "SN", "NC"

base_type <- "baseweather" #baseline #baseweather

base_folder <- file.path("E:", "MAS", "gridfire_prep", "hucs_gf_baseweather")



shared <- file.path("R:", "rem")

archive_folder <- file.path("E:", "MAS", "blended_rasters", "baseline")
dir.create(archive_folder, recursive = TRUE) 


### Data finding: fuels ---------------------------------------

#{SC,CC,SN,NC}_Baseline/FVS_fuels/blended_outputs/

bl_folder <- file.path(shared, 
                       paste0(region_to_run, "_Baseline"),
                       "FVS_fuels",
                       "blended_outputs")

fuel_files <- list.files(bl_folder,
                         full.names = TRUE,
                         pattern = "tif$")
# 20. 4 years, 5 fuel layers per year. 


## Archive ----------------------------------------------------

#temporary location to zipped and upload to google drive for archiving

file.copy(from=fuel_files, to=archive_folder)


### Data import ------------------------------------------------

(time_start <- Sys.time())

#for reference & crosswalk 
hucs_shp <- st_read("data/data_huc/TxHucsTimingGroups.shp")

hr2 <- hucs_shp %>% 
  st_drop_geometry() %>% 
  select(huc12, region) %>% 
  rename(reg_code = region) %>% 
  distinct() 


### Create input dataframe & go -----------------------------------------------

#create input dataframe
# column fuel file
# pull out region (column)
# add rows for each huc in region 


input_df <- tibble(fuel_file = fuel_files) %>% 
  mutate(file_name = tools::file_path_sans_ext(basename(fuel_file))) %>% 
  separate_wider_delim(cols = file_name, delim = "_",
                       names = c("reg_code",
                                 "baseline", 
                                 "yr", "layer", "crs", "ff", "fvs"),
                       cols_remove = FALSE) %>%
  #create runid like structure
  # names = c("runid", "reg_code",
  #           "priority", "intensity", "trt",
  #           "yr", "layer", "crs", "ff", "fvs"),
  mutate(runid = "RunIDx",
         priority = base_type,
         intensity = base_type,
         trt = base_type) %>% 
  #remove unwanted
  select(-c(crs, ff, fvs)) %>%
  #run id str / full scenario
  unite('runid_str', runid, reg_code, priority, intensity, trt, sep="_", remove=FALSE) %>% 
  #join with hucs to get row for each huc for each fuel file
  left_join(hr2, by = join_by(reg_code), relationship = "many-to-many") %>% 
  #create runid and folder names
  mutate(
    folder_huc = file.path(base_folder, reg_code, paste0("huc_", huc12)),
    folder_h_fuels = file.path(folder_huc, "fuels"),
    folder_h_f_scenario = file.path(folder_h_fuels, runid_str),
    folder_h_f_s_yr = file.path(folder_h_f_scenario, yr))
  
  

#Need to make all these folders first
# (can't in parallel, race conditions)
#function to create folder
create_all_folders <- function(r){
  this_row <- input_df[r,]
  this_folder_h_f_s_yr <- this_row[["folder_h_f_s_yr"]]
  dir.create(this_folder_h_f_s_yr, recursive = TRUE) #won't overwrite #recursive
}

#create the folders
folder_created <- lapply(1:nrow(input_df), create_all_folders)


#function for adding the huc-fuels to the correct folder
add_fuels_to_huc <- function(r){
  
  this_row <- input_df[r,]
  this_file_name <- this_row[["file_name"]]
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
           this_row[["huc12"]], 
           format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  
}


plan(multisession, workers = availableCores(omit=1))

system.time(added_fuels <- future_lapply(1:nrow(input_df), add_fuels_to_huc))

(time_end <- Sys.time())
(time_elapsed <- time_end - time_start)

fuel_log <- do.call(rbind, added_fuels)
colnames(fuel_log) <- c("fuel_file", "huc12", "datetime")

fuel_log <- as_tibble(fuel_log) 

write_csv(fuel_log, 
          file = "log_fuel_files_baseline.csv",
          append = TRUE)

# Timing notes ----------------------------------
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
