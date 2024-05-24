# Nonburnable issue

# This pulls out the affected HUCs in SC, CC, SN (previously run)

# Puts them into a new folder

# Then can use the more general nonburnable_huc_indicator.R on these folders

### Libraries -------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf, #can remove now, nb hucs no longer a sf object
  fs) #for easier directory copy


### User settings -------------------------------------------

region_to_run <- "SN" # "SC", CC", "SN"

original_folder <- file.path("E:", "MAS", "gridfire_prep", 
                           "hucs_gf", 
                           region_to_run)

new_folder <- file.path("E:", "MAS", "gridfire_prep",
                            "reruns_nonburn", 
                            region_to_run)

### Data ----------------------------------------------------

#hucs to collect, from scripts/qa_misc/hucs_nonburnable.R
nb_hucs <- readRDS("data/nonburnable_rerun_list.RDS") %>% 
  filter(region == region_to_run)


## Collect huc indicators to modify ------------------------

#nb_hucs_pattern <- nb_hucs %>% pull(huc12) %>% paste(., collapse="|")

target_folders <- nb_hucs %>% 
  dplyr::select(huc12) %>% 
  mutate(huc_folder_name = paste0("huc_", huc12),
         huc_folder = file.path(original_folder, huc_folder_name),
         output_folder = file.path(new_folder, huc_folder_name))


for (i in 1:nrow(target_folders)){
  
  this_target <- target_folders[i,]

  fs::dir_copy(this_target[["huc_folder"]],
               this_target[["output_folder"]])
  
  
  #    file.copy(this_file, file.path(this_year_folder, "weather.json"))
  # # create empty folder Z inside folder X
  # dir.create('(...)/folder_X/folder_Z')
  # # copy the folder
  # file.copy('(...)/path_to_folder_Z', '(...)/folder_X')
  #https://stackoverflow.com/questions/32453455/copy-folder-recursive-in-r
}




