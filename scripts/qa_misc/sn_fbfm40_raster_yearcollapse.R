# bitwise combine all four years of FBFM40 into one
# later to do zonal summary and then sankey diagrams of fuel changes

# WARNING!!! 
#
# This is slow because you CANNOT use future.lapply
# If you do, even though INT8S is set, it loses accuracy
#  and you end up with nonsense fbfm40 values.
# Unknown exact trigger/cause. But works singly, or with lapply

### Library ---------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  terra)
  #future.apply,
  #progressr)

#options(future.globals.onReference = "warning")

#handlers(global=TRUE)
#handlers("progress")

#stop scientific notation
options(scipen = 999)

#Hitting issues with large numbers
# ABSOLUTELY CRITICAL SETTING, DO NOT CHANGE
terraOptions(datatype="FLT8S") #FLT8S INT8S

### User settings --------------------------------------

folder_to_run <- "SN" # #SN"

#raster locations
raster_folder <- file.path("E:", "MAS", "blended_rasters",
                           folder_to_run)

#raster output
outpath <- file.path("E:", "MAS", "blended_rasters",
                     paste0(folder_to_run, "_yrcollapsed"))
dir.create(outpath, recursive = TRUE)

### Data ------------------------------------------------

#original fvs
fvs_names_orig <- read_csv(file.path("data", "FvsNames.csv"))

#create NC trt7 runids. 
fvs_nc7 <- fvs_names_orig %>% 
  filter(grepl('NC.+trt4', name)) %>% 
  mutate(name = str_replace(name, 'trt4', 'trt7'))
#remove NC trt4, add in trt7
fvs_names_all <- fvs_names_orig %>% 
  #! is not
  filter(!grepl('NC.+trt4', name)) %>% 
  bind_rows(fvs_nc7) 


### Year Fuel list by scenario --------------------------

#create year fml file names
input_tbl <- fvs_names_all %>% 
  #filter to this region
  filter(grepl(folder_to_run, name)) %>% 
  #create file paths to each year in that scenario
  mutate(yr1 = file.path(raster_folder,
                         paste0(name, "_2024_fml_32611_FF_FVS.tif")),
         yr2 = file.path(raster_folder,
                         paste0(name, "_2029_fml_32611_FF_FVS.tif")),
         yr3 = file.path(raster_folder,
                         paste0(name, "_2034_fml_32611_FF_FVS.tif")),
         yr4 = file.path(raster_folder,
                         paste0(name, "_2039_fml_32611_FF_FVS.tif")),
         #file path and name to save out
         outfile = file.path(outpath, 
                             paste0(name, "_fml_yearcollapsed.tif")))



### Function for combining ----------------------------

collapse_years <- function(r){
  
  this_row <- input_tbl[r,]
  
  this_yr1 <- terra::rast(this_row[["yr1"]])
  this_yr2 <- terra::rast(this_row[["yr2"]])
  this_yr3 <- terra::rast(this_row[["yr3"]])
  this_yr4 <- terra::rast(this_row[["yr4"]])
  
  # 'stack' using c()
  # and putting the year fbfm40 value in the right bit position
  yrs1234 <- c(this_yr1*1000*1000*1000, 
               this_yr2*1000*1000, 
               this_yr3*1000, 
               this_yr4)
  
  # add it all up
  all_yrs <- terra::app(yrs1234, fun = "sum")
  #all_yrs
  #freq(all_yrs)
  
  #report progress
  #p(sprintf("r=%g", r))
  
  #save
  terra::writeRaster(all_yrs,
                     this_row[["outfile"]],
                     datatype = "FLT8S") #INT8S
  
  #read_test <- terra::rast(this_row[["outfile"]])
  #freq(read_test)
}


### set up for parallel runs

#remove everything not needed to pass along
rm(fvs_names_orig)
rm(fvs_nc7)
rm(fvs_names_all)
rm(folder_to_run)
rm(outpath)
rm(raster_folder)
#keep only input_tbl


#plan(multisession, workers = availableCores(omit=1))

(start_time <- Sys.time())
# with_progress({
#   p <- progressor(along = 1:nrow(input_tbl))
#   future_lapply(1:nrow(input_tbl), collapse_years)
# })
#future_lapply(1:nrow(input_tbl), collapse_years)

lapply(1:nrow(input_tbl), collapse_years)

(end_time <- Sys.time())
(elapsed_time <- end_time - start_time)



### warnings to eventually hunt down --------------------

#Error: Detected a non-exportable reference (‘externalptr’ of class ‘RegisteredNativeSymbol’) in the value (of class ‘list’) of the resolved future
#In addition: Warning message:
#  UNRELIABLE VALUE: One of the ‘future.apply’ iterations (‘future_lapply-1’) unexpectedly generated random numbers without declaring so. There is a risk that those random numbers are not statistically sound and the overall results might be invalid. To fix this, specify 'future.seed=TRUE'. This ensures that proper, parallel-safe random numbers are produced via the L'Ecuyer-CMRG method. To disable this check, use 'future.seed = NULL', or set option 'future.rng.onMisuse' to "ignore". 



### Attempts -----------------------------------------
# this_yr1 <- terra::rast(this_row[["yr1"]])
# this_yr2 <- terra::rast(this_row[["yr2"]])
# this_yr3 <- terra::rast(this_row[["yr3"]])
# this_yr4 <- terra::rast(this_row[["yr4"]])
# new_yr4 <- this_yr4*1 #things don't work right with stack otherwise. 
# new_yr3 <- this_yr3*1000
# new_yr2 <- this_yr2*1000*1000
# new_yr1 <- this_yr1*1000*1000*1000
# all_yrs <- new_yr1 + new_yr2 + new_yr3 + this_yr4
# all_yrs <- sum(new_yr1, new_yr2, new_yr3, new_yr4)
# 
# #compareGeom on four orig rasters is TRUE yet when add/sum, get nonsense values (interpolated)
# # (and diff values using + vs sum())
# test1 <- c(this_yr1, this_yr2, this_yr3, this_yr4)
# test2 <- c(new_yr1, new_yr2, new_yr3, this_yr4)
# test3 <- c(new_yr1, new_yr2, new_yr3, new_yr4)
# 
# all_t2 <- terra::app(test2, fun = "sum")
# all_t3 <- terra::app(test3, fun = "sum")
# test4 <- terra::rast(list(terra::rast(this_row[["yr1"]]),
#                           terra::rast(this_row[["yr2"]]),
#                           terra::rast(this_row[["yr3"]]),
#                           terra::rast(this_row[["yr4"]])))
# test4[[1]] <- test4[[1]]*1000*1000*1000
# test4[[2]] <- test4[[2]]*1000*1000
# test4[[3]] <- test4[[3]]*1000
# test4[[4]] <- test4[[4]]*1
# all_t4 <- terra::app(test4, fun = "sum")

#in any combination of separately, stacked, + of app sum(), 
# doing all four at a time yields nonsense in 3rd and 4th positions
# max value 203203215360. (plus gives different weird, oddly)
# summing two at a time does not work either at the last step
#  2.03203e+11, 203203  = 203203198976 
# 2024 & 2029 appear slightly different than 2034 & 2039. 

# yr1_2 <- terra::rast(list(terra::rast(this_row[["yr1"]]),
#                           terra::rast(this_row[["yr2"]])))
# yr1_2[[1]] <- yr1_2[[1]]*1000*1000*1000
# yr1_2[[2]] <- yr1_2[[2]]*1000*1000
# 
# yr3_4 <- terra::rast(list(terra::rast(this_row[["yr3"]]),
#                           terra::rast(this_row[["yr4"]])))
# yr3_4[[1]] <- yr3_4[[1]]*1000
# 
# new_yr12 <- terra::app(yr1_2, fun = "sum")
# new_yr12
# new_yr34 <- terra::app(yr3_4, fun = "sum")
# new_yr34
# 
# new_yr1234 <- c(new_yr12, new_yr34)
# all_yrs <- terra::app(new_yr1234, fun = "sum")
# all_yrs
# 
# 
# yr2_3 <- terra::rast(list(terra::rast(this_row[["yr2"]]),
#                           terra::rast(this_row[["yr3"]])))
# yr2_3[[1]] <- yr2_3[[1]]*1000*1000
# yr2_3[[2]] <- yr2_3[[2]]*1000
# new_yr23 <- terra::app(yr2_3, fun = "sum")
# new_yr23

# # Resampling using 2024, specifically using nearest neighbor
# this_yr2 <- terra::resample(this_yr2_raw, this_yr1, method = "near")
# this_yr3 <- terra::resample(this_yr3_raw, this_yr1, method = "near")
# this_yr4 <- terra::resample(this_yr4_raw, this_yr1, method = "near")
# 
# test1 <- c(this_yr1*1000*1000*1000, this_yr3*1000)
# test1a <- terra::app(test1, fun = "sum")
# test1a # 203000203000 # passes by freq
# 
# test2 <- c(this_yr1*1000*1000*1000, this_yr4)
# test2a <- terra::app(test2, fun = "sum")
# test2a # 2.03e+11? # freq total failure
# 
# test3 <- c(this_yr2*1000*1000, this_yr4)
# test3a <- terra::app(test3, fun = "sum")
# test3a #  203000208 FAIL
# 
# test4 <- c(this_yr2*1000*1000, this_yr3*1000)
# test4a <- terra::app(test4, fun = "sum")
# test4a #203203008 FAIL
# 
# test5 <- c(this_yr1*1000*1000*1000, this_yr2*1000*1000)
# test5a <- terra::app(test5, fun = "sum")
# test5a # 2.03203e+11 # freq total failure

# # Read in
# this_yr1 <- terra::rast(this_row[["yr1"]])
# this_yr2_raw <- terra::rast(this_row[["yr2"]])
# this_yr3_raw <- terra::rast(this_row[["yr3"]])
# this_yr4_raw <- terra::rast(this_row[["yr4"]])
# 
# # Resampling using 2024, specifically using nearest neighbor
# this_yr2 <- terra::project(this_yr2_raw, this_yr1, method = "near")
# this_yr3 <- terra::project(this_yr3_raw, this_yr1, method = "near")
# this_yr4 <- terra::project(this_yr4_raw, this_yr1, method = "near")
# 
# test1 <- c(this_yr1*1000*1000*1000, this_yr2*1000*1000)
# test1a <- terra::app(test1, fun = "sum")
# test1a #203203002368 FAIL
# freq(test1a) # FAIL
# 
# freq(this_yr1*1000*1000*1000) # FAILURE HERE! #PASSES with terraOptions(datatype="FLT8S")

#Despite all having the same extent, dimensions, resolution, and crs
# it seems that 2024 & 2029 vs 2034 & 2039 are not perfectly aligned
# When adding I would get nonsense fbfm40 values in those positions, 
# indicating it was interpolating during the summation. 
# Therefore, resample all years using 2024 as base, then sum. 
