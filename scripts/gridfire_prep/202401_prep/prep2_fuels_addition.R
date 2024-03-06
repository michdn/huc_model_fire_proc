# Script for adding in fuel data into existing structure
#  Designed to add whatever fuels are in the fuels_in folder
#   so not necessarily all at the same time. 


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  terra,
  stars)

### User settings ---------------------------------------------

base_folder <- "hucs_val"

input_folder <- file.path("data", "data_fuels_in")

### Data import ------------------------------------------------

(time_start <- Sys.time())

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
  add_column(reg_code = list("CC", "NC", "SN", "SC")) %>% 
  #join back to get all fields
  left_join(hucs_shp, by = "RRK_Rgn") %>% 
  #create crosswalk from reg_code to huc12s
  select(RRK_Rgn, reg_code, huc12)

#crs_ca <- "EPSG:3310"


#Read in list of all files in input folder
# note: no checking occurs, up to the user to do it right
fuel_files <- list.files(input_folder, 
                       full.names = TRUE,
                       pattern = "*.tif$")
num_files <- length(fuel_files)

### Loop - per file, then per (filtered set) huc of relevant region ------
#fuels are run per region, so need to include region information

for (i in seq_along(fuel_files)){

  this_file <- fuel_files[i]
  
  this_fuel <- terra::rast(this_file) #%>% 
  #   terra::project(crs_ca) #now happens inner loop part of project/clip/extent
  
  #file name string manipulation to get region
  this_file_name <- tools::file_path_sans_ext(basename(this_file))
  #sep name at dash, get where region code is in name
  this_reg_code <- str_split(this_file_name, "_")[[1]][2]
  #match xwalk
  this_huc_set <- hr %>% filter(reg_code == this_reg_code) %>% pull(huc12)
  
  #other name pieces, important for saving later
  this_runid_str <- paste0(str_split(this_file_name, "_")[[1]][1:5], collapse="_")
  this_year <- str_split(this_file_name, "_")[[1]][6]
  this_layer <- str_split(this_file_name, "_")[[1]][7]
  
  #most will be bilinear, but fbfm40 needs nearest neighbor (categorical)
  this_method <- if_else(this_layer == "fml", "near", "bilinear")
  
  for (j in seq_along(this_huc_set)){
    
    this_huc <- this_huc_set[j]

    this_huc_folder <- file.path(base_folder, paste0("huc_", this_huc))
    this_fuels_dir <- file.path(this_huc_folder, "fuels")
    this_fuels_trt <- file.path(this_fuels_dir, this_runid_str) 
    this_fuels_yr <- file.path(this_fuels_trt, this_year)
    dir.create(this_fuels_yr, recursive = TRUE) #won't overwrite #recursive
    
    
    #get huc indicator to exactly align extents
    inputs_folder <- file.path(this_huc_folder, "inputs")
    this_indicator <- terra::rast(file.path(inputs_folder,
                                            "huc_indicator.tif"))
    
    #project fuel, using indicator for extent and resolution
    this_fuel_huc <- project(this_fuel, 
                             this_indicator,
                             method=this_method,
                             threads=T)
    
    #saving into existing folder
    terra::writeRaster(this_fuel_huc, 
                       file.path(this_fuels_yr,
                                 paste0(this_layer, ".tif")), 
                       overwrite=T) #change to T if need to 
    
  }#end inner loop per filtered huc
  
  log <- data.frame(file = this_file, time_done = Sys.time())
  write.table(log, 
              file = 'log.csv', 
              append=TRUE,
              quote=TRUE, sep=",", 
              row.names=FALSE, col.names=FALSE)
  
  print(paste(i, "/", num_files, "at", Sys.time()))
  
} # end outer loop, per file

(time_end <- Sys.time())
(time_elapsed <- time_end - time_start)

