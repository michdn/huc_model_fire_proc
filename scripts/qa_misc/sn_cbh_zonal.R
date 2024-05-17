# 'zonal summary' of ALL SN HUC-scenario-years
# of CBH

# mean, median, min, max, etc


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  terra,
  exactextractr,
  future.apply)
  #progressr)

options(future.globals.onReference = "warning")

#handlers("progress")


## res data  --------------------------------------------------

res <- read_csv(file.path("results",
                          "absolute", #"datacube", 
                          "SN_SNbl_SNbw_absolute_20240423.csv")) %>% 
  mutate(HUC12 = as.character(HUC12)) 

#do all scenarios
scen <- res %>% 
  filter(!Priority == "baseline") %>% 
  filter(!Priority == "baseweather")


#to add treatment year later
huc_trt_yr <- res %>% 
  filter(!Priority == "baseline") %>% 
  filter(!Priority == "baseweather") %>% 
  dplyr::select(HUC12, Priority, timeFire, timeHybrid, timeWui) %>% 
  distinct() %>% 
  #treatment years
  separate_wider_delim(timeFire, "_", names=c("fire_trt_yr", NA)) %>% 
  separate_wider_delim(timeHybrid, "_", names=c("hybrid_trt_yr", NA)) %>% 
  mutate(fire_trt_yr = as.numeric(fire_trt_yr),
         hybrid_trt_yr = as.numeric(hybrid_trt_yr),
         wui_trt_yr = case_when(
           timeWui == "2024_2039_yr1to5_16to20" ~ 2024,
           timeWui == "2029_yr6to10" ~ 2029,
           timeWui == "2034_yr11to15" ~ 2034,
           timeWui == "Not treated" ~ 9999)) %>% 
  #treat year for priority
  mutate(trt_yr = case_when(
    Priority == "Fire" ~ fire_trt_yr,
    Priority == "Hybrid" ~ hybrid_trt_yr,
    Priority == "WUI" ~ wui_trt_yr)) %>%
  #final columns
  dplyr::select(HUC12, Priority, trt_yr)

#skip baselines
# huc_bl <- res %>% 
#   dplyr::select(HUC12, Region, Priority, TxIntensity, TxType, Year) %>% 
#   filter(Priority == "baseline")


#for coastal/nonburn issue flag
nb_hucs <- readRDS("data/nonburnable_rerun_list.RDS") %>%
  filter(region == "SN")


# ### setup -------------------------------------------

fuel_raster_folder <- file.path("E:", "MAS", "blended_rasters",
                                "SN")


cbh_raster_files <- list.files(fuel_raster_folder,
                               full.names = TRUE,
                               pattern = "cbh.+tif$")

#huc shapefile
hucs_all <- st_read("data/data_huc/TxHucsTimingGroups.shp") 


### Target files setup ---------------------------------------

#create/get file paths to the needed rasters

scen_targets <- scen %>% 
  #back to RFFC for Hybrid
  mutate(Priority = if_else(Priority == "Hybrid", "RFFC", Priority)) %>% 
  unite(col = search_pattern, sep = "_", remove = FALSE,
        Priority, TxIntensity, TxType, Year) %>% 
  rowwise() %>% 
  mutate(raster_target = grep(cbh_raster_files, 
                              pattern = search_pattern, 
                              value = TRUE)) %>% 
  #stop rowwise
  ungroup() %>% 
  dplyr::select(-search_pattern)





get_cbh_zonal <- function(r){
  
  this_row <- scen_targets[r,] 
  
  #this adjective fuel raster 
  this_raster <- terra::rast(this_row[["raster_target"]]) 
  
  #this huc
  this_huc <- hucs_all %>% 
    filter(huc12 == this_row[["HUC12"]]) %>% 
    #transform to same crs as raster
    st_transform(st_crs(this_raster))
  
  #extracts
  count_pixels <- exact_extract(this_raster,
                                this_huc,
                                fun = "count")
  
  cbh_ave <- exact_extract(this_raster, 
                       this_huc,
                       fun = "mean")
  
  cbh_median <- exact_extract(this_raster, 
                       this_huc,
                       fun = "median")
  
  cbh_min <- exact_extract(this_raster, 
                       this_huc,
                       fun = "min")
  
  cbh_max <- exact_extract(this_raster, 
                           this_huc,
                           fun = "max")
  
  cbh_stdev <- exact_extract(this_raster, 
                           this_huc,
                           fun = "stdev")
  
  
  this_res <- c(HUC12 = this_row[["HUC12"]], 
                Priority = this_row[["Priority"]],
                TxType = this_row[["TxType"]],
                TxIntensity = this_row[["TxIntensity"]],
                Year = this_row[["Year"]],
                cbh_ave = cbh_ave,
                cbh_median = cbh_median,
                cbh_min = cbh_min,
                cbh_max = cbh_max,
                cbh_stdev = cbh_stdev,
                tot_pixels = count_pixels)
  
}


### parallel --------------------------------------------
#remove unneeded

rm(res, scen, fuel_raster_folder, cbh_raster_files)

plan(multisession, workers = availableCores(omit=1))

(start_time <- Sys.time())
scenario_zonal_raw <- future_sapply(1:nrow(scen_targets), 
                                    get_cbh_zonal)
(end_time <- Sys.time())
(elapsed_time <- end_time - start_time)


#results processing ------------------------------------

scenario_zonal <- scenario_zonal_raw %>% 
  #transpose and convert to tibble, make numeric columns numeric
  t() %>% 
  as_tibble() %>% 
  mutate_at(c("Year",  
              "cbh_ave", "cbh_median", "cbh_min", "cbh_max", "cbh_stdev",
              "tot_pixels"), 
            as.numeric) %>% 
  #switch to Hybrid
  mutate(Priority = if_else(Priority == "RFFC", "Hybrid", Priority)) %>% 
  #add treatment year
  left_join(huc_trt_yr, by = join_by(HUC12, Priority)) %>% 
  #add nonburn issue flag
  mutate(nonburn_coastal = if_else(HUC12 %in% nb_hucs$huc12, TRUE, FALSE)) 

  

saveRDS(scenario_zonal, file.path("qa", 
                                  "SN_scenario_zonal_cbh.RDS"))


#2.8 hours

