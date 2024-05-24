# Script for adding in fuel data into existing structure
#  Designed to add whatever fuels are in the fuels_in folder
#   so not necessarily all at the same time. 

# As such, there is NO CHECKING. 
# User must keep track of what fuels have been processed.

# This is prep2 REWRITTEN to be parallelized. 

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

base_folder <- "hucs_gf_test_f2rf"

input_folder <- file.path("data", "data_fuels_in")


### Data import ------------------------------------------------

(time_start <- Sys.time())

#for reference & crosswalk 
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



#Read in list of all files in input folder
# note: no checking occurs, up to the user to have added all by the end of all runs
fuel_files <- list.files(input_folder, 
                       full.names = TRUE,
                       pattern = "*.tif$")


#create input dataframe
# column fuel file
# pull out region (column)
# add rows for each huc in region 


input_df <- tibble(fuel_file = fuel_files) %>% 
  mutate(file_name = tools::file_path_sans_ext(basename(fuel_file))) %>% 
         #reg_code = str_split(file_name, "_") %>% purrr::map_chr(., 2)) %>% 
  separate_wider_delim(cols = file_name, delim = "_",
                       names = c("runid", "reg_code",
                                 "priority", "intensity", "trt",
                                 "yr", "layer", "crs", "ff", "fvs"),
                       cols_remove = FALSE) %>%
  #remove unwanted
  select(-c(crs, ff, fvs)) %>%
  #run id str / full scenario
  unite('runid_str', runid, reg_code, priority, intensity, trt, sep="_", remove=FALSE) %>% 
  #join with hucs to get row for each huc for each fuel file
  left_join(hr, by = join_by(reg_code), relationship = "many-to-many") %>% 
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
          file = "log_fuel_files.csv",
          append = TRUE)

# Timing notes ----------------------------------

# 10 fuel files (5 SC, 5 NC)
# > system.time(added_fuels <- lapply(1:nrow(input_df), add_fuels_to_huc))
# user  system elapsed 
# 2406.16   50.61 4033.72 
# > (time_elapsed <- time_end - time_start)
# Time difference of 1.121138 hours

# > system.time(added_fuels <- future_lapply(1:nrow(input_df), add_fuels_to_huc))
# user  system elapsed 
# 76.54  134.61  323.25 
# > (time_elapsed <- time_end - time_start)
# Time difference of 5.441077 mins

# > system.time(added_fuels <- future_lapply(1:nrow(input_df), add_fuels_to_huc))
# user  system elapsed 
# 774.42   23.79 2432.49 
# > (time_elapsed <- time_end - time_start)
# Time difference of 40.66287 mins