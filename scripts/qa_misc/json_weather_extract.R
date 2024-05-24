# pull indexed weather data for a particular fire


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  tidyjson)


### User settings --------------------------------------------

reg_group <- "NC" 

data_path <- file.path("data",
                       "data_weather_20240520",
                       reg_group)

### Identify path and weather files --------------------------

#Read in list of all sql files in results folder
json_files <- list.files(data_path, 
                         full.names=TRUE,
                         recursive=TRUE,
                         pattern=".json$")
num_json <- length(json_files)


### Loop, read each weather and pull info -------------

weather_collect <- list()


for (i in seq_along(json_files)){
  
  this_file <- json_files[[i]]
  
  #read in appropriate json
  this_json <- tidyjson::read_json(this_file)
  
  #gather weather
  this_weather <- this_json %>% 
    gather_array %>% 
    spread_all %>% 
    dplyr::select(array.index, `relative-humidity`, temperature, `wind-from-direction`, `wind-speed-20ft`, `foliar-moisture`) %>% 
    #drop json
    as_tibble() %>% 
    rename(array_index = array.index,
           relative_humidity = `relative-humidity`,
           wind_from_direction = `wind-from-direction`,
           wind_speed_20ft = `wind-speed-20ft`,
           foliar_moisture = `foliar-moisture`)
  
  
  # add file name (to get huc and year)
  this_weather <- this_weather %>% 
    mutate(file = this_file) %>% 
    tidyr::separate_wider_delim(file, 
                                delim = "/", 
                                names = c(NA, NA, 
                                          "Region", "Year", "huc_year_json")) %>%
    tidyr::separate_wider_delim(huc_year_json, 
                                delim = "_",
                                names = c("HUC12", NA)) %>% 
    mutate(Year = as.numeric(Year)) %>% 
    dplyr::select(HUC12, Year, Region, everything()) 
  
  
  #collect combined ignition weather data
  weather_collect[[i]] <- this_weather
  
  
  #print progress every n files
  if (i%%500 == 0){
    print(paste0(i, " of ", num_json, " at ", Sys.time()))
  }
}


## SAVE
#Ignition info, only RDS for reference
all_weather <- do.call(bind_rows, weather_collect)
#save out collected data
saveRDS(all_weather,
        file.path("qa",
                  "qa_weather0520",
                  paste0(reg_group, "_weather0520.RDS")))


