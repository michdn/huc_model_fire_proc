#script for adding pre-generated weather data from Anna

#copy files is fast, can stay as loop. 

# NOTE! Can be done before or later fuels addition


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf) 

### User settings ---------------------------------------------

#base_folder <- "hucs_gf_test_f4rf"
base_folder <- file.path("E:", "MAS", "gridfire_prep", "hucs_gf")

input_folder <- file.path("data", "data_weather")

(time_start <- Sys.time())

### Data -----------------------------------------------------

weather_files <- list.files(input_folder, 
                         full.names = TRUE,
                         recursive = TRUE,
                         pattern = "*.json$") 

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


### Loop for each file -------------------------------------

for (i in seq_along(weather_files)){
  
  this_file <- weather_files[i]

  #file name string manipulation 
  this_file_name <- tools::file_path_sans_ext(basename(this_file))
  #sep name at dash, huc and year
  this_huc <- str_split(this_file_name, "_")[[1]][1] %>% 
    #removing extra spaces (leading and trailing)
    str_trim() 
  this_yr <- str_split(this_file_name, "_")[[1]][2] 
  
  
  # get region
  this_region <- hr %>% 
    filter(huc12 == this_huc) %>% 
    pull(reg_code)
  
  
  #folder names
  this_huc_folder <- file.path(base_folder, this_region, paste0("huc_", this_huc))
  this_weather_folder <- file.path(this_huc_folder, "weather")
  this_year_folder <- file.path(this_weather_folder, this_yr)
  dir.create(this_year_folder, recursive = TRUE) #won't overwrite #recursive
  
  
  #copy file into folder 
  file.copy(this_file, file.path(this_year_folder, "weather.json"))
  
} #end weather file loop
  
(time_end <- Sys.time())
(time_elapsed <- time_end - time_start)
