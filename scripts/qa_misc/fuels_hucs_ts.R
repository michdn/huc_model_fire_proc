# fuels timeseries 
# based on treatment blocks (assigned)

#plan
# import assigned treatment blocks
# import fuel layers (canopy and fm40)
# focus on SC Fire trt 4 and trt 6 for now
#  but set up for all-run if possible

# look at what was SUPPOSED to have been treated at that time step
# calculate average canopies
# see when decrease 
# make time series similar to weighting8 but at timinggrp block level

# scale: treatment blocks in a trt timing group

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf, 
  terra,
  exactextractr)

#function for mode
# https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
my_mode <- function(x, method = "one", na.rm = FALSE) {
  x <- unlist(x)
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  
  # Get unique values
  ux <- unique(x)
  n <- length(ux)
  
  # Get frequencies of all unique values
  frequencies <- tabulate(match(x, ux))
  modes <- frequencies == max(frequencies)
  
  # Determine number of modes
  nmodes <- sum(modes)
  nmodes <- ifelse(nmodes==n, 0L, nmodes)
  
  if (method %in% c("one", "mode", "") | is.na(method)) {
    # Return NA if not exactly one mode, else return the mode
    if (nmodes != 1) {
      return(NA)
    } else {
      return(ux[which(modes)])
    }
  } else if (method %in% c("n", "nmodes")) {
    # Return the number of modes
    return(nmodes)
  } else if (method %in% c("all", "modes")) {
    # Return NA if no modes exist, else return all modes
    if (nmodes > 0) {
      return(ux[which(modes)])
    } else {
      return(NA)
    }
  }
  warning("Warning: method not recognised.  Valid methods are 'one'/'mode' [default], 'n'/'nmodes' and 'all'/'modes'")
}


### Import data ----------------------------------------------

hucs <- st_read("data/data_huc/TxPrctRankRrkWipRffc.shp")

#fuels
# files
sc_fuels <- list.files(path = file.path("data", "data_fuels_sc"),
                       full.names = TRUE,
                       pattern = "*.tif$")
nc_fuels <- list.files(path = file.path("data", "data_fuels_nc"),
                       full.names = TRUE,
                       pattern = "*.tif$")
cc_fuels <- list.files(path = file.path("data", "data_fuels_cc"),
                       full.names = TRUE,
                       pattern = "*.tif$")
sn_fuels <- list.files(path = file.path("data", "data_fuels_sn"),
                       full.names = TRUE,
                       pattern = "*.tif$")

fuel_loop_name <- 'sn'
fuels_to_run <- sn_fuels

# fuels_to_run <- grep(pattern = "Fire", x = sc_fuels, value=TRUE) %>% 
#   grep(pattern = "trt6", x = ., value=TRUE)

(start_time <- Sys.time())

#loop
collector <- list()

for (i in seq_along(fuels_to_run)){
  
  this_file <- fuels_to_run[i]
  
  #file name string manipulation 
  this_file_name <- tools::file_path_sans_ext(basename(this_file))
  #sep name at dash
  this_runid <- str_split(this_file_name, "_")[[1]][1]
  this_reg_code <- str_split(this_file_name, "_")[[1]][2]
  this_priority <- str_split(this_file_name, "_")[[1]][3]
  this_intensity <- str_split(this_file_name, "_")[[1]][4]
  this_trt <- str_split(this_file_name, "_")[[1]][5]
  this_runid_str <- paste0(str_split(this_file_name, "_")[[1]][1:5], collapse="_")
  this_year <- str_split(this_file_name, "_")[[1]][6]
  this_layer <- str_split(this_file_name, "_")[[1]][7]
  
  #read file
  this_fuel <- terra::rast(this_file)
  

  #extract : sum for all fuel layers, except mode for fml
  # different crs, it auto transforms
  if (this_layer == "fml"){
    this_extract <- exact_extract(this_fuel, 
                                  hucs,
                                  append_cols = c('huc12'),
                                  fun = 'mode') %>% 
      rename(summary = mode)
  } else {
    this_extract <- exact_extract(this_fuel, 
                                  hucs,
                                  append_cols = c('huc12'),
                                  fun = 'sum') %>% 
      rename(summary = sum)
  }
  
  #create huc dataset
  this_ds <- bind_cols(
    this_extract,
    scenario = this_runid_str,
    runID = this_runid,
    region = this_reg_code,
    priority = this_priority,
    intensity = this_intensity,
    trt = this_trt,
    year = this_year,
    layer = this_layer,
    fuel_file = this_file_name
    )
  
  #add to collector
  collector[[i]] <- this_ds
  
} #end i fuel layer

#bind rows
all_collected <- do.call(bind_rows, collector)

saveRDS(all_collected, file = file.path('qa', 
                                        'fuel_timing', 
                                        paste0(fuel_loop_name, "_hucs.RDS")))

(end_time <- Sys.time())
(end_time - start_time)
