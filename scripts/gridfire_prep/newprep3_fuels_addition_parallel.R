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

region_to_run <- "NC" #"CC", "SN", "NC"

base_folder <- file.path("E:", "MAS", "gridfire_prep", "hucs_gf")

shared <- file.path("R:", "rem")

#original fvs
fvs_names_orig <- read_csv(file.path("data", "FvsNames.csv"))

### Data finding: fuels ---------------------------------------

#create folders that look like R:/rem/RunID36_CC_Fire_2m_trt4/FVS_fuels/blended_outputs/

#create NC trt7 runids. 
fvs_nc7 <- fvs_names_orig %>% 
  filter(grepl('NC.+trt4', name)) %>% 
  mutate(name = str_replace(name, 'trt4', 'trt7'))
#remove NC trt4, add in trt7
fvs_names_all <- fvs_names_orig %>% 
  #! is not
  filter(!grepl('NC.+trt4', name)) %>% 
  bind_rows(fvs_nc7) 

#for SN, all 2039 and new trt6 are under FVS_Fuels2
# and all new reblended under FVS_fuels (but some old as well that we will need to filter out)
# and as rest of SN is still archived, we will pull the remaining from an archive source below
fvs_names_all <- fvs_names_all %>% 
  #add normal fuel folder
  mutate(fuel = "FVS_fuels") 

# Need to REMOVE affected trt6 from FVS_fuels (NOT FVS_fuels2)
sn6 <- c("RunID2_SN_RFFC_500k_trt6", "RunID3_SN_Fire_500k_trt6", "RunID5_SN_RFFC_1m_trt6",
         "RunID6_SN_Fire_1m_trt6", "RunID8_SN_RFFC_2m_trt6", "RunID9_SN_Fire_2m_trt6") 
sn6_pattern <- paste(sn6, collapse="|")

fvs_names_snreduced <- fvs_names_all %>% 
  filter(!grepl(sn6_pattern, name))

#new data until FVS_Fuels2
fvs_names <- fvs_names_snreduced %>% 
  #add in special (extra rows for SN)
  #NOTE pulling from fvs_names_all here
  bind_rows(fvs_names_all %>% 
              filter(grepl('_SN_', name)) %>% 
              mutate(fuel="FVS_Fuels2"))

#108 - 6 + 27 = 129 for SN, 108 for all others

fvs_names <- fvs_names %>% 
  #temporary just for nice sorting to check
  mutate(run = str_split(name, "_") %>% map_chr(.,1),
         id = str_remove(run, "\\D+") %>% as.numeric()) %>%
  arrange(id) %>% 
  select(-run, -id)


#create folders
fvs_names <- fvs_names %>% 
  mutate(runid_folders = file.path('R:',
                                   'rem',
                                   name,
                                   fuel,
                                   'blended_outputs'))


selected_rem <- grep(pattern=region_to_run, 
                     fvs_names %>% pull(runid_folders),
                     value=TRUE)
#should be 27 for a region, except for SN (27 + (27-6) = 48)


fuel_files_rem <- list.files(selected_rem,
                        full.names = TRUE,
                        #probably only needed "tif$" but extra pattern shouldn't hurt
                        pattern = "RunID.*tif$")

# Need to REMOVE 2044 if present
fuel_files_rem <- grep(fuel_files_rem, pattern="_2044_", invert=TRUE, value=TRUE)

# Need to REMOVE any unarchived canopy layers from FVS_fuels (NOT FVS_Fuels2)
#  as pulling those from bluejay source
# Run9 trt1 and trt4
# (Technically won't hurt if overwriting, but no.)
# cbd, cbh, cc, cht
# Flipped around, only fml layers from FVS_fuels
# This is a mess, sorry. Would have been better to split if else entirely
if (region_to_run == "SN"){
  fuel_files_temp <- c(grep(fuel_files_rem, pattern="FVS_Fuels2", value=TRUE), 
                       (grep(fuel_files_rem, pattern="FVS_fuels.+fml", value=TRUE)))
  fuel_files_rem <- fuel_files_temp
}

#should be 540 for a region (SN different, just the new ones)
length(fuel_files_rem)
# SN. 
# FVS_Fuels2: 
# 27 scenarios with 5 fuels for 2039 [135] plus
#  6 scenarios with 3 years of 5 fuels [90]. 135 + 90 = 225
# FVS_fuels:
#     21 scenarios with 3 years of reblended fml [63]
# Total 225 + 63 = 288


if (region_to_run == "SN"){
  # SN correct old non-fml rasters
  #special SN bluejay local folder
  all_orig_sn <- list.files(file.path('data', 'data_fuels_sn_badblend'),
                            full.names = TRUE,
                            pattern="RunID.+tif$")
  #get region (none if not SN)
  selected_orig_sn <- grep(pattern=region_to_run, 
                           all_orig_sn,
                           value=TRUE)
  
  #We need to remove all fml files (new will be in R:/rem/)
  selected_sn <- grep(selected_orig_sn,
                      pattern="fml_.+tif$",
                      invert=TRUE,
                      value=TRUE)
  #27 * 4 years * 4 fuel files = 432
  #we also need to remove the old trt6 FVS_fuels, but only for affected
  fuel_files_blue <- grep(selected_sn,
                          pattern=sn6_pattern, #set above 
                          invert=TRUE,
                          value=TRUE)
  # 432 - (6 * 4 fuels * 4 years) = 336
  # Also need to REMOVE 2044
  fuel_files_blue <- grep(fuel_files_blue, 
                          pattern="_2044_", 
                          invert=TRUE, 
                          value=TRUE)
  # 336 - (21 remaining scenarios * 4 fuels) = 252
  
  #combine
  fuel_files <- c(fuel_files_rem, fuel_files_blue)
  
} else if (region_to_run == "NC"){
  
  nc6 <- c("RunID11_NC_RFFC_500k_trt6", "RunID12_NC_Fire_500k_trt6", "RunID14_NC_RFFC_1m_trt6",
           "RunID15_NC_Fire_1m_trt6", "RunID17_NC_RFFC_2m_trt6", "RunID18_NC_Fire_2m_trt6") 
  nc6_pattern <- paste(nc6, collapse="|")
  
  # SN correct old non-fml rasters
  #special SN bluejay local folder
  all_orig_nc <- list.files(file.path('data', 'data_fuels_nc_badblend'),
                            full.names = TRUE,
                            pattern="RunID.+tif$")
  #remove trt4
  selected_orig_nc <- grep(all_orig_nc,
                           pattern="trt4",
                           invert=TRUE,
                           value=TRUE)
  
  #We need to remove all fml files (new will be in R:/rem/)
  selected_nc <- grep(selected_orig_nc,
                      pattern="fml_.+tif$",
                      invert=TRUE,
                      value=TRUE)
  
  #we also need to remove the old trt6 FVS_fuels, but only for affected
  fuel_files_blue <- grep(selected_nc,
                          pattern=nc6_pattern, #set above 
                          invert=TRUE,
                          value=TRUE)
  
  # Also need to REMOVE 2044
  fuel_files_blue <- grep(fuel_files_blue, 
                          pattern="_2044_", 
                          invert=TRUE, 
                          value=TRUE)
  # 144
  
  
  #combine
  fuel_files <- c(fuel_files_rem, fuel_files_blue)
  #540
  
} else {
  fuel_files <- fuel_files_rem
}

nrow(fuel_files)
#should be 540 now for any region 

#write_csv(tibble(filelocation = fuel_files), 'sn_fuel_files_log.csv')


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
