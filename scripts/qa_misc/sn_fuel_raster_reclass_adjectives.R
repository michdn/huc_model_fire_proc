# Reclass SN fuel rasters with adjectives


### Library ---------------------------------------------
if (!require("pacman")) install.packages("pacman")
if (!require("RSQLite")) install.packages("RSQLite") #needs to be installed, but not library

pacman::p_load(
  tidyverse,
  sf,
  terra,
  exactextractr,
  DBI,
  future.apply,
  progressr)

options(future.globals.onReference = "error")

#handlers(global=TRUE)
handlers("progress")

### User settings --------------------------------------

adj_to_use <- "ROS" #"ROS" "FL" (rate of spread, or flame length)


folder_to_run <- "SN" #"baseline" #SN"

#raster locations
raster_folder <- file.path("E:", "MAS", "blended_rasters",
                                folder_to_run)

#adjective raster output
outpath <- file.path("E:", "MAS", "blended_rasters",
                     paste0(folder_to_run, "_adjective_", adj_to_use))
dir.create(outpath, recursive = TRUE)

### Data ------------------------------------------------

#adjective crosswalk from Dave Schmidt
adj_file <- file.path("qa", "fbfm_adjectives", "FBFM_behavior_adjectives.db")

adj_db <- dbConnect(RSQLite::SQLite(), adj_file)
#dbListTables(adj_db)

adjectives <- dbReadTable(adj_db, "FBFM_behavior_adjectives")


nonburn_codes <- tibble(FBFM = c(91, 92, 93, 98, 99),
                        adj_code = rep(1, 5))
#0 is no data in raster

if (adj_to_use == "FL"){
  
  #nb VL L M H VH X
  # 1 2  3 4 5  6 7
  adj_codes <- tibble(FL = c("VL", "L", "M", "H", "VH", "X"),
                      adj_code = 2:7)
  
  
  adj_xwalk <- adjectives %>% 
    select(FBFM, FL) %>% 
    #matrices must be of same type, raster is numeric
    #turn adjectives into factor, basically
    mutate(FBFM = as.numeric(FBFM)) %>% 
    left_join(adj_codes, by = join_by("FL")) %>% 
    dplyr::select(-FL) %>% 
    #add nonburnables as a 0 category
    bind_rows(nonburn_codes) %>%
    #matrix for terra::classify()
    as.matrix
  
} else if (adj_to_use == "ROS"){
  
  #nb VL L M H VH X
  # 1 2  3 4 5  6 7
  adj_codes <- tibble(ROS = c("VL", "L", "M", "H", "VH", "X"),
                      adj_code = 2:7)
  
  
  adj_xwalk <- adjectives %>% 
    select(FBFM, ROS) %>% 
    #matrices must be of same type, raster is numeric
    #turn adjectives into factor, basically
    mutate(FBFM = as.numeric(FBFM)) %>% 
    left_join(adj_codes, by = join_by("ROS")) %>% 
    dplyr::select(-ROS) %>% 
    #add nonburnables as a 0 category
    bind_rows(nonburn_codes) %>%
    #matrix for terra::classify()
    as.matrix
  
}


#adjectives
#FBFM - the fuel model number 
#FL - flame length adjective
#ROS - the rate of spread adjective

dbDisconnect(adj_db)

#remove to prevent future apply from complaining about non-exportable references?
rm(adj_db)

# #For saving out
# adjectives %>% 
#   select(FBFM, FL) %>% 
#   #matrices must be of same type, raster is numeric
#   #turn adjectives into factor, basically
#   mutate(FBFM = as.numeric(FBFM)) %>% 
#   left_join(adj_codes, by = join_by("FL")) %>% 
#   bind_rows(tibble(FBFM = c(91, 92, 93, 98, 99),
#                    adj_code = rep(1, 5),
#                    FL = rep("nonburn", 5))) %>% 
#   arrange(FBFM) %>% 
#   write_csv(file.path("qa", "fbfm_adjectives_crosswalk.csv"))
# 


### Fuels reclass set up --------------------------------

#list of fuel files to reclass (all fml)
fuel_files <- list.files(raster_folder,
                         full.names = TRUE,
                         pattern = "fml.+tif$")
#108: 27 scenarios * 4 years

#create tibble
fuel_tbl <- tibble(fuel_file = fuel_files) %>% 
  mutate(file_name = basename(fuel_file),
         out_file = file.path(outpath, 
                              paste0(tools::file_path_sans_ext(file_name),
                                     "_adjectivized.tif")))


#function to reclass
reclass_fuel <- function(r){
  
  this_row <- fuel_tbl[r,]
  
  this_fml <- terra::rast(this_row[["fuel_file"]])
  
  #reclass
  this_fml_adj <- terra::classify(this_fml,
                                    adj_xwalk)
  
  #save
  terra::writeRaster(this_fml_adj,
                     this_row[["out_file"]])

  #report progress
  p(sprintf("r=%g", r))
  
}


plan(multisession, workers = availableCores(omit=1))

#system.time(future_lapply(1:nrow(fuel_tbl), reclass_fuel))

(start_time <- Sys.time())
with_progress({
  p <- progressor(along = 1:nrow(fuel_tbl))
  future_lapply(1:nrow(fuel_tbl), reclass_fuel)
})
(end_time <- Sys.time())
(elapsed_time <- end_time - start_time)


#NOT WORKING. could not find function p
# # function with future and progress
# progress_future <- function(xs){
#   p <- progressor(along = xs)
#   future_lapply(xs, reclass_fuel)
# }
#system.time(progress_future(1:nrow(fuel_tbl)))

