# Script for adding in fuel data into existing structure
#  Designed to add fuels for a region (grep patterns below)

# As such, there is no checking on runids or total fuels processed. 
# User must keep track of what fuels have been processed.

# This is old prep2 REWRITTEN to be parallelized and run on bluejay. 

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

region_to_run <- "CC" #"CC", "SN", "NC"

base_folder <- file.path("E:", "MAS", "gridfire_prep", "hucs_gf")

shared <- file.path("R:", "rem")

fvs_names <- read_csv(file.path("data", "FvsNames.csv"))

### Data finding: fuels ---------------------------------------

#create folders that look like R:/rem/RunID36_CC_Fire_2m_trt4/FVS_fuels/blended_outputs/

fvs_names <- fvs_names %>% 
  mutate(runid_folders = file.path('R:',
                                   'rem',
                                   name,
                                   'FVS_fuels',
                                   'blended_outputs'))


selected <- grep(pattern=region_to_run, 
                 fvs_names %>% pull(runid_folders),
                 value=TRUE)
#should be 27 for a region


# #this will take a few minutes to scan all folders - TOO MANY FOLDERS TO USE NOW
# all_dirs <- list.dirs(shared) 
# #Run per region   
# region_dirs <- grep(pattern=region_to_run, all_dirs, value=TRUE)
# #just FVS_fuels and blended
# fuels_dirs <- grep(pattern="/FVS_fuels/blended_outputs$", region_dirs, value=TRUE)
# # CHECK TO SEE IF ANY FOLDERS NEED REMOVING
# fuels_dirs
# #need to get rid of various folders (may not need, no harm to include)
# do_not_find <- '_orig|_wrong|_re-run|MAS_delete|MAS_orig|_moved'
# dnf_removed <- grep(pattern=do_not_find, fuels_dirs, invert=TRUE, value=TRUE)
# # CONFIRM GOOD FOLDERS. 27 for a region
# dnf_removed

fuel_files <- list.files(selected,
                        full.names = TRUE,
                        #probably only needed "tif$" but extra pattern shouldn't hurt
                        pattern = "RunID.*tif$")

fuel_files

# Need to REMOVE 2044
fuel_files <- grep(fuel_files, pattern="_2044_", invert=TRUE, value=TRUE)


#should be 540 for a region
fuel_files


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
