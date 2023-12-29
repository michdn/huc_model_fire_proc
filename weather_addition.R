#script for adding pre-generated weather data from Anna

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse) 

### User settings ---------------------------------------------

base_folder <- "hucs_val"
input_folder <- file.path("data", "data_weather_in")

(time_start <- Sys.time())

### Data -----------------------------------------------------

weather_files <- list.files(input_folder, 
                         full.names = TRUE,
                         recursive = TRUE,
                         pattern = "*.json$") 

### Loop for each file -------------------------------------

for (i in seq_along(weather_files)){
  
  this_file <- weather_files[i]

  #file name string manipulation 
  this_file_name <- tools::file_path_sans_ext(basename(this_file))
  #sep name at dash, huc and year
  this_huc <- str_split(this_file_name, "_")[[1]][1] %>% 
    #removing extra space next to underscore
    str_trim() 
  this_yr <- str_split(this_file_name, "_")[[1]][2] 
  

  #folder names
  this_huc_folder <- file.path(base_folder, paste0("huc_", this_huc))
  this_weather_folder <- file.path(this_huc_folder, "weather")
  this_year_folder <- file.path(this_weather_folder, this_yr)
  dir.create(this_year_folder, recursive = TRUE) #won't overwrite #recursive
  
  
  #copy file into folder 
  file.copy(this_file, file.path(this_year_folder, "weather.json"))
  
} #end weather file loop
  
(time_end <- Sys.time())
(time_elapsed <- time_end - time_start)