#script for adding pre-generated weather data from Anna
#copy files is fast, can stay as loop. 
# Note! Can be done before or later fuels addition

### **********

# THIS IS AN ALTERNATE SCRIPT TO newprep2_weather_addition.R

# This script will pull just the 2024 weather files, and 
#  then duplicate them for 2029, 2034, and 2039. 

# This is for the baseline-no-climate-change scenario (baseweather). 

### **********


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf) 

### User settings ---------------------------------------------

base_folder <- file.path("E:", "MAS", "gridfire_prep", "baseweather")

input_folder <- file.path("data", "data_weather")

yr_list <- c("2024", "2029", "2034", "2039")

(time_start <- Sys.time())

### Data -----------------------------------------------------

weather_all <- list.files(input_folder, 
                         full.names = TRUE,
                         recursive = TRUE,
                         pattern = "*.json$") 

# Filter to ONLY 2024 weather files
#  (this way, we can use the same input folder as normal)

weather_2024 <- grep(pattern="_2024", 
                     weather_all,
                     value=TRUE)
#2837, one for each HUC
  

#for reference & crosswalk 
hucs_shp <- st_read("data/data_huc/TxHucsTimingGroups.shp")

hr2 <- hucs_shp %>% 
  st_drop_geometry() %>% 
  select(huc12, region) %>% 
  rename(reg_code = region) %>% 
  distinct() 


# #for reference & crosswalk 
# hucs_shp <- st_read("data/data_huc/TxPrctRankRrkWipRffc.shp")
# 
# hr <- hucs_shp %>% 
#   #get just the region names
#   select(RRK_Rgn) %>% 
#   st_drop_geometry() %>% 
#   distinct() %>% 
#   arrange(RRK_Rgn) %>% 
#   #manually add code that is used in fuel file names
#   #alpha sorted Central Coast, North Coast, Sierra Nevada, South Coast
#   add_column(reg_code = c("CC", "NC", "SN", "SC")) %>% 
#   #join back to get all fields
#   left_join(hucs_shp, by = "RRK_Rgn") %>% 
#   #create crosswalk from reg_code to huc12s
#   select(RRK_Rgn, reg_code, huc12)


### Loop for each file -------------------------------------

#for each 2024 weather file, duplicate out into each year

for (i in seq_along(weather_2024)){
  
  this_file <- weather_2024[i]

  #file name string manipulation 
  this_file_name <- tools::file_path_sans_ext(basename(this_file))
  #sep name at dash, huc and year
  this_huc <- str_split(this_file_name, "_")[[1]][1] %>% 
    #removing extra spaces (leading and trailing)
    str_trim() 
  #this_yr <- str_split(this_file_name, "_")[[1]][2] 
  
  
  # get region
  this_region <- hr2 %>% 
    filter(huc12 == this_huc) %>% 
    pull(reg_code)
  
  
  #folder names
  this_huc_folder <- file.path(base_folder, this_region, paste0("huc_", this_huc))
  this_weather_folder <- file.path(this_huc_folder, "weather")
  
  
  #INNER loop for years
  for (y in seq_along(yr_list)){
    
    this_yr <- yr_list[[y]]
    
    this_year_folder <- file.path(this_weather_folder, this_yr)
    dir.create(this_year_folder, recursive = TRUE) #won't overwrite #recursive
    
    
    #copy file into folder 
    file.copy(this_file, file.path(this_year_folder, "weather.json"))
    
  } # end year 
  
  
} #end weather file loop
  
(time_end <- Sys.time())
(time_elapsed <- time_end - time_start)
