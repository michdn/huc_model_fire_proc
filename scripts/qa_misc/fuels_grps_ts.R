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

### lookup table ---------------------------------------------

# tx_code        tx_type
#        1   year_int_1-5 #for fire, hybrid
#        2  year_int_6-10
#        3 year_int_11-15
#        4 year_int_16-20 #for fire, hybrid
#        4 year_int_1-5_16-20 #for WUI

tx_yr <- tibble(
  tx_code = c(1:4, 4),
  tx_type = c("year_int_1-5", "year_int_6-10", "year_int_11-15", "year_int_16-20",
              "year_int_1-5_16-20"),
  tx_yr_lyr = c('2024', '2029', '2034', '2044', '2024_2044')
)


### Import data ----------------------------------------------

(start_time <- Sys.time())

#treatment blocks
# tx type for timing
run_blocks <- list.files(path = file.path("qa", "MAS_FCATtreatments"),
                        full.names = TRUE,
                        pattern = "*.shp$") 

# scf_500_r21 <- st_read("qa/MAS_FCATtreatments/RunID21_TreatmentsSHP_SC_Fire_500k_20230926.shp")

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

# CHANGE HERE! *********************

fuel_loop_name <- 'blocks_cc'
fuels_to_run <- cc_fuels

# ***********************************

 # fuels_to_run <- grep(pattern = "Fire", x = sc_fuels, value=TRUE) %>% 
 #   grep(pattern = "trt6", x = ., value=TRUE)

# scf_500_trt4_2024_fml <- raster("data/data_fuels_sc/RunID21_SC_Fire_500k_trt4_2024_fml_32611_FF_FVS.tif")


#loop
canopy_collector <- list()
fml_collector <- list()

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
  
  #Now find matching treatment block shp file
  # Match on runid 
  #  will get region, priority, intensity match. Same RunID for diff trt types
  this_block_file <- grep(pattern = paste0(this_runid, "_"),
                          x = run_blocks, 
                          value = TRUE)
  
  #read in treatment shp
  this_blocks <- st_read(this_block_file, quiet = TRUE)
  
  
  #extract : sum for all fuel layers, except mode for fml
  # same crs, no need to transform
  if (this_layer == "fml"){
    this_extract <- exact_extract(this_fuel, 
                                  this_blocks,
                                  append_cols = c('tx_code', 'tx_type'),
                                  fun = 'mode')
    this_extract_summary <- this_extract %>%
      left_join(tx_yr, by = join_by(tx_code, tx_type)) %>%
      group_by(tx_code, tx_type, tx_yr_lyr, mode) %>%
      summarize(val_count = n()) %>%
      arrange(tx_code, desc(val_count))
  } else {
    this_extract <- exact_extract(this_fuel, 
                                  this_blocks,
                                  append_cols = c('tx_code', 'tx_type'),
                                  fun = 'sum')
    this_extract_summary <- this_extract %>%
      left_join(tx_yr, by = join_by(tx_code, tx_type)) %>%
      group_by(tx_code, tx_type, tx_yr_lyr) %>%
      summarize(summary = sum(sum))
    
  }
  
  #create dataset
  this_ds <- bind_cols(
    this_extract_summary,
    scenario = this_runid_str,
    runID = this_runid,
    region = this_reg_code,
    priority = this_priority,
    intensity = this_intensity,
    trt = this_trt,
    year = this_year,
    layer = this_layer,
    fuel_file = this_file_name,
    block_file = tools::file_path_sans_ext(basename(this_block_file))
  ) 
  
  #add df (4 rows) to collector
  if (this_layer == "fml"){
    fml_collector[[i]] <- this_ds
  } else {
    canopy_collector[[i]] <- this_ds
  }
  
} #end i fuel layer

#bind rows
canopy_collected <- do.call(bind_rows, canopy_collector)
fml_collected <- do.call(bind_rows, fml_collector)

saveRDS(canopy_collected, file = file.path('qa', 
                                        'fuel_timing', 
                                        paste0(fuel_loop_name, 
                                               "_canopy.RDS")))
saveRDS(fml_collected, file = file.path('qa', 
                                           'fuel_timing', 
                                           paste0(fuel_loop_name, 
                                                  "_fml.RDS")))

(end_time <- Sys.time())
(end_time - start_time)

## parking lot ---------------
# # Need ALL year groups. Inner loop
# trtgrp_collector <- list()
# for (j in seq_along(tx_yr[['tx_code']])){
#   
#   this_tx_code <- tx_yr[['tx_code']][[j]]
#   this_tx_yr_desc <- tx_yr[['tx_type']][[j]]
#   this_tx_yr_lyr <- tx_yr[['tx_yr_lyr']][[j]]
#   
#   #filter for year
#   this_blocks_yr <- this_blocks %>%
#     filter(tx_code == this_tx_code)
#   
#   
#   #extract : sum for all fuel layers, except mode for fml
#   # same crs, no need to transform
#   if (this_layer == "fml"){
#     this_extract <- exact_extract(this_fuel, 
#                                   this_blocks_yr,
#                                   fun = 'mode')
#     this_extract_summary <- my_mode(this_extract)
#   } else {
#     this_extract <- exact_extract(this_fuel, 
#                                   this_blocks_yr,
#                                   fun = 'sum')
#     this_extract_summary <- sum(this_extract)
#   }
#   
#   #create row
#   this_row <- tibble_row(
#     scenario = this_runid_str,
#     runID = this_runid,
#     region = this_reg_code,
#     priority = this_priority,
#     intensity = this_intensity,
#     trt = this_trt,
#     year = this_year,
#     tx_yr_code = this_tx_code,
#     tx_yr_desc = this_tx_yr_desc,
#     tx_yr_lyr = this_tx_yr_lyr,
#     layer = this_layer,
#     summary_blks = this_extract_summary,
#     fuel_file = this_file_name,
#     block_file = tools::file_path_sans_ext(basename(this_block_file))
#   ) 
#   
#   #add row to collector
#   #collector[[(i-1)*nrow(tx_yr)+j]] <- this_row
#   trtgrp_collector[[j]] <- this_row
#   
# } #end j year of treatment blocks
