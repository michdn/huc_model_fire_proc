# Region pull script
# Script to pull out a region to its own folder for passing off to Val

# Entire dataset too large and only ~1/2 way through. 
# Running out of disk space. 

# **********  WARNING  ************
#
# Do NOT move until ALL data for that region is processed 
#   (huc, topo, weather, fuels)
#  All other scripts are built around 'hucs_val' base directory 
#    not the region directories

### Libraries -------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf)

### User settings ---------------------------------------------

region <- "NC" #"CC", "NC", "SN", "SC"

from_folder <- "hucs_val"
to_folder <- paste0("hucs_val_", region)
dir.create(to_folder)

# from_folder <- "test_from" 
# to_folder <- "test_to"

### Move the folders of the HUCs of the region ----------------

(time_start <- Sys.time())

#for reference & crosswalk 
hucs_shp <- st_read("data/data_huc/TxPrctRankRrkWipRffc.shp")

#borrowing from fuels code
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

reg_hucs <- hr %>% filter(reg_code == region) %>% pull(huc12)

# FOR TEST #reg_hucs <- c("180701060203", "180701020103")

for (i in seq_along(reg_hucs)){
 
  this_huc <- reg_hucs[i]
  this_huc_folder <- file.path(from_folder, paste0("huc_", this_huc))
  
  #don't have space to copy, must move. 
  fs::file_move(this_huc_folder,
                to_folder)
  
}

(time_end <- Sys.time())
(time_elapsed <- time_end - time_start)


