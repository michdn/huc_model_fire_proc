# Script to correct huc_indicator.tif 
#  To nonburnable area for selected HUCs

# Note: script updated 2024-06-27 post Jun MAS runs to fix
#   NoData issue on very western coastal HUCs (extends into ocean)
#   NC 180101070302 and 180101070401

### Libraries -------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  terra)


### User settings -------------------------------------------

#FOR SC, CC, SN reruns ONLY
# region_to_run <- "" # "SC", CC", "SN"
# region_folder <- file.path("E:", "MAS", "gridfire_prep", 
#                            "reruns_nonburn", region_to_run)

# region_to_run is actually folder name

region_to_run <- "NC"
region_folder <- file.path("E:", "MAS", "gridfire_prep",
                           "hucs_gf_noweather_nofuel", region_to_run)

# region_to_run <- "NC"
# region_folder <- file.path("qajun", "nc_copies", "nb_test")

## FML ------------------------------------------------------

# # fuel model layer to identify nonburnable values
# fml <- terra::rast("other_datasets/LC22_F40_220_CA.tif")
# 
# nonburn <- c(91, 1, 
#              92, 1, 
#              93, 1, 
#              98, 1,
#              99, 1,
#              -9999, 1)
# nonburn <- matrix(nonburn, ncol=2, byrow=TRUE)
# 
# #binary 1 = nonburnable, 0 = everything else
# fml_nb <- terra::classify(fml, 
#                           nonburn,
#                           others=0)
# 
# #save out to read in without needing to reclass everytime
# terra::writeRaster(fml_nb, 
#                    "other_datasets/LC22_F40_220_CA_NBbinary.tif", 
#                    overwrite=TRUE)


## Base data in ----------------------------------------------

fml_nb <- terra::rast("other_datasets/LC22_F40_220_CA_NBbinary.tif")

#plan: subtract this layer from huc_indicator. 
# huc_indicator has a 1 for the huC, 
# so nonburnable becomes 1 - 1 = 0. 
# Good HUC area 1 - 0 = 1
# and outside of HUC 0 - 0 = 0 or 0 - 1 = -1. 
# Then classify -1 to 0. 
# Update 2024-06-27: recode NoData arising from LC22 calc above to 0

#hucs to modify, from scripts/qa_misc/hucs_nonburnable.R
nb_hucs <- readRDS("data/nonburnable_rerun_list.RDS") %>% 
  filter(region == region_to_run)
  #filter(huc12 %in% c("180101070302", "180101070401"))

## Collect huc indicators to modify ------------------------

#nb_hucs_pattern <- nb_hucs %>% pull(huc12) %>% paste(., collapse="|")

target_folders <- nb_hucs %>% 
  dplyr::select(huc12) %>% 
  mutate(huc_folder_name = paste0("huc_", huc12),
         huc_folder = file.path(region_folder, huc_folder_name),
         target = file.path(huc_folder, "inputs", "huc_indicator.tif"))



# Few enough, just loop

for (i in 1:nrow(target_folders)){
  
  this_target <- target_folders[i,]
  
  this_indicator <- rast(this_target[["target"]])
  
  #project fuel, using indicator for extent and resolution
  this_fmlnb <- project(fml_nb, 
                        this_indicator,
                        method="near", 
                        threads=T)
  
  #subtract and reclassify -1 back to 0; ensure no NoData
  idiff <- this_indicator - this_fmlnb
  
  updated <- terra::classify(idiff, 
                             matrix(c(-1, NA, 0, 0), ncol=2))
  
  #save out updated indicator, overwriting
  writeRaster(updated, this_target[["target"]],
              overwrite=TRUE)
  
  target_folders[i,"update_time"] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  
}


write_csv(target_folders %>%
            dplyr::select(huc12, target, update_time),
          file = file.path("logs",
                           paste0("log_indicator_update_",
                                  region_to_run,
                                  ".csv")),
          append = TRUE)

# write_csv(target_folders %>% 
#             dplyr::select(huc12, target, update_time), 
#           file = file.path("qajun",
#                            "nc_copies",
#                            paste0("log_indicator_update_", 
#                                   region_to_run, 
#                                   ".csv")),
#           append = TRUE)
