# 'zonal summary' of ALL SN HUC-scenario-years
# of the adjective version of the fuel rasters


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  terra,
  exactextractr,
  future.apply)
  #progressr)

options(future.globals.onReference = "error")

#handlers("progress")


## Data input --------------------------------------------------

adj_to_use <- "ROS" #"ROS" or "FL"


#do all 
res <- read_csv(file.path("results",
                          "absolute", #"datacube", 
                          "SN_SNbl_SNbw_absolute_20240423.csv")) %>% 
  mutate(HUC12 = as.character(HUC12)) 

huc_scen <- res %>% 
  dplyr::select(HUC12, Region, Priority, TxIntensity, TxType, Year) %>% 
  filter(!Priority == "baseline") %>% 
  filter(!Priority == "baseweather")

huc_bl <- res %>% 
  dplyr::select(HUC12, Region, Priority, TxIntensity, TxType, Year) %>% 
  filter(Priority == "baseline")


#add treat year in later with crosswalk
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


#for coastal/nonburn issue flag
nb_hucs <- readRDS("data/nonburnable_rerun_list.RDS") %>% 
  filter(region == "SN")


# ### user settings -------------------------------------------
# 
# #previously flagged HUC-scenario-years
# flagged_raw <- readRDS(file.path("qa",
#                                  "SN_intensity_inverted_flagged_1.01_20240416.RDS"))
# # flagged_raw <- readRDS(file.path("qa",
# #                                  "SN_intensity_inverted_flagged_1.5_20240416.RDS"))
# #MATCH with IMPORT! 
# threshold <-  "1.01" #"1.5"
# 
# ### setup -------------------------------------------
# 
# flagged <- flagged_raw %>% 
#   filter(tany > 0) %>% 
#   dplyr::select(HUC12, Priority, TxType, Year, t2m, t1m, t500k)
# 
# #long form (1 row for each intensity, only flagged intensities, make intensity value)
# flag_long <- flagged %>% 
#   pivot_longer(cols = c(t2m, t1m, t500k)) %>% 
#   filter(value == TRUE) %>% 
#   mutate(intensity = case_when(
#     name == "t2m" ~ "2m",
#     name == "t1m" ~ "1m",
#     name == "t500k" ~ "500k"
#   ))
# 
# # t2m - get 2m and baseline
# # t1m - get 1m and baseline
# # t500k - get 500k and baseline
# # if multiple, get baseline only once. 

if (adj_to_use == "FL"){
  
  adj_raster_folder <- file.path("E:", "MAS", "blended_rasters",
                                 "SN_adjective")
  
  bl_raster_folder <- file.path("E:", "MAS", "blended_rasters",
                                "baseline_adjective")
  
} else if (adj_to_use == "ROS"){
  
  adj_raster_folder <- file.path("E:", "MAS", "blended_rasters",
                                 "SN_adjective_ROS")
  
  bl_raster_folder <- file.path("E:", "MAS", "blended_rasters",
                                "baseline_adjective_ROS")
  
}


adj_raster_files <- list.files(adj_raster_folder,
                               full.names = TRUE,
                               pattern = "*adjectivized.tif$")


bl_raster_files <- list.files(bl_raster_folder,
                              full.names = TRUE,
                              pattern = "SN.+adjectivized.tif$")


#huc shapefile
hucs_all <- st_read("data/data_huc/TxHucsTimingGroups.shp") 
#immediately transform to raster crs EPSG:5070
#st_transform(st_crs(fm40ca))


### Target files setup ---------------------------------------

#create/get file paths to the needed rasters

#RunID1_SN_WUI_500k_trt1_2024_fml_32611_FF_FVS_adjectivized.tif
#SN_Baseline_2024_fml_32611_FF_FVS_adjectivized

# flag_long <- flag_long %>% 
#   #back to RFFC for Hybrid
#   mutate(Priority = if_else(Priority == "Hybrid", "RFFC", Priority)) %>% 
#   unite(col = search_pattern, sep = "_", remove = FALSE,
#         Priority, intensity, TxType, Year) %>% 
#   rowwise() %>% 
#   mutate(raster_target = grep(adj_raster_files, 
#                               pattern = search_pattern, 
#                               value = TRUE)) %>% 
#   #stop rowwise
#   ungroup() 
# 
# #baseline tibble 
# bl_long <- flag_long %>% 
#   dplyr::select(HUC12, Year) %>% 
#   rowwise() %>% 
#   mutate(raster_target = grep(bl_raster_files, 
#                               pattern = Year,
#                               value = TRUE)) %>% 
#   ungroup() %>% 
#   distinct()

scenario_targets <- huc_scen %>% 
  #back to RFFC for Hybrid
  mutate(Priority = if_else(Priority == "Hybrid", "RFFC", Priority)) %>% 
  unite(col = search_pattern, sep = "_", remove = FALSE,
        Priority, TxIntensity, TxType, Year) %>% 
  rowwise() %>% 
  mutate(raster_target = grep(adj_raster_files, 
                              pattern = search_pattern, 
                              value = TRUE)) %>% 
  #stop rowwise
  ungroup() %>% 
  dplyr::select(-search_pattern)


bl_targets <- huc_bl %>% 
  rowwise() %>% 
  mutate(raster_target = grep(bl_raster_files, 
                              pattern = Year,
                              value = TRUE)) %>% 
  ungroup() 



count_value_pixels <- function(my_raster, my_sf, my_value){
  exact_extract(my_raster, 
                my_sf,
                fun = function(value, coverage_fraction){
                  sum(coverage_fraction[value == my_value], na.rm = TRUE)})
}

get_adj_zonal_scenario <- function(r){
  this_row <- scenario_targets[r,] #flag_long[r,]
  
  #this adjective fuel raster 
  this_raster <- terra::rast(this_row[["raster_target"]]) 
  
  #this huc
  this_huc <- hucs_all %>% 
    filter(huc12 == this_row[["HUC12"]]) %>% 
    #transform to same crs as raster
    st_transform(st_crs(this_raster))
  
  #extract pixel counts of different classes
  count_pixels <- exact_extract(this_raster,
                                this_huc,
                                fun = "count")
  
  count1 <- count_value_pixels(this_raster, this_huc, 1)
  count2 <- count_value_pixels(this_raster, this_huc, 2)
  count3 <- count_value_pixels(this_raster, this_huc, 3)
  count4 <- count_value_pixels(this_raster, this_huc, 4)
  count5 <- count_value_pixels(this_raster, this_huc, 5)
  count6 <- count_value_pixels(this_raster, this_huc, 6)
  count7 <- count_value_pixels(this_raster, this_huc, 7)
  
  ave <- exact_extract(this_raster, 
                       this_huc,
                       fun = "mean")
  
  # #report progress
  # p1(sprintf("r=%g", r))
  
  
  #nb VL L M H VH X
  # 1 2  3 4 5  6 7
  
  this_res <- c(HUC12 = this_row[["HUC12"]], 
                Priority = this_row[["Priority"]],
                TxType = this_row[["TxType"]],
                TxIntensity = this_row[["TxIntensity"]],
                Year = this_row[["Year"]],
                NB = count1,
                VL = count2,
                L = count3,
                M = count4,
                H = count5,
                VH = count6,
                X = count7,
                mean = ave,
                tot_pixels = count_pixels)
  
  
}

get_adj_zonal_baseline <- function(r){
  this_row <- bl_targets[r,]
  
  #this adjective fuel raster 
  this_raster <- terra::rast(this_row[["raster_target"]]) 
  
  #this huc
  this_huc <- hucs_all %>% 
    filter(huc12 == this_row[["HUC12"]]) %>% 
    #transform to same crs as raster
    st_transform(st_crs(this_raster))
  
  #extract pixel counts of different classes
  count_pixels <- exact_extract(this_raster,
                                this_huc,
                                fun = "count")
  
  count1 <- count_value_pixels(this_raster, this_huc, 1)
  count2 <- count_value_pixels(this_raster, this_huc, 2)
  count3 <- count_value_pixels(this_raster, this_huc, 3)
  count4 <- count_value_pixels(this_raster, this_huc, 4)
  count5 <- count_value_pixels(this_raster, this_huc, 5)
  count6 <- count_value_pixels(this_raster, this_huc, 6)
  count7 <- count_value_pixels(this_raster, this_huc, 7)
  
  
  #nb VL L M H VH X
  # 1 2  3 4 5  6 7
  
  ave <- exact_extract(this_raster, 
                       this_huc,
                       fun = "mean")
  
  # #report progress
  # p2(sprintf("r=%g", r))
  
  
  this_res <- c(HUC12 = this_row[["HUC12"]], 
                Year = this_row[["Year"]],
                NB_bl = count1,
                VL_bl = count2,
                L_bl = count3,
                M_bl = count4,
                H_bl = count5,
                VH_bl = count6,
                X_bl = count7,
                mean_bl = ave,
                tot_pixels_bl = count_pixels)
  
}



#stest <- sapply(1:2, get_adj_zonal_baseline)
#t(stest) %>% as_tibble()
#stest2 <- sapply(1:2, get_adj_zonal_scenario)
#t(stest2) %>% as_tibble()

plan(multisession, workers = availableCores(omit=1))

(start_time <- Sys.time())
# with_progress({
#   p1 <- progressor(along = 1:nrow(scenario_targets))
#   scenario_zonal_raw <- future_sapply(1:nrow(scenario_targets), 
#                                       get_adj_zonal_scenario)
# })
scenario_zonal_raw <- future_sapply(1:nrow(scenario_targets), 
                                    get_adj_zonal_scenario)
(end_time <- Sys.time())
(elapsed_time <- end_time - start_time)


# system.time(scenario_zonal_raw <- future_sapply(1:nrow(scenario_targets), #nrow(flag_long), 
#                                             get_adj_zonal_scenario))
#275 4.2 minutes
#user  system elapsed 
#221.17   30.55  252.10 

#all scenarios
# user   system  elapsed 
#   9.24     2.33 15997.84  
# 

(start_time <- Sys.time())
# with_progress({
#   p2 <- progressor(along = 1:nrow(bl_targets))
#   baseline_zonal_raw <- future_sapply(1:nrow(bl_targets),  
#                                       get_adj_zonal_baseline)
# })
baseline_zonal_raw <- future_sapply(1:nrow(bl_targets), 
                                    get_adj_zonal_baseline)
(end_time <- Sys.time())
(elapsed_time <- end_time - start_time)

 
# system.time(baseline_zonal_raw <- future_sapply(1:nrow(bl_targets), #nrow(bl_long), 
#                                                 get_adj_zonal_baseline))
#97 1.5 minutes
#user  system elapsed 
#78.42   10.11   88.20

#all baselines
# user  system elapsed 
#  2.20    1.14  610.41 

scenario_zonal <- scenario_zonal_raw %>% 
  t() %>% as_tibble() %>% 
  mutate_at(c("Year", "NB", "VL", "L", "M", "H", "VH", "X", "mean", "tot_pixels"), as.numeric)

baseline_zonal <- baseline_zonal_raw %>% 
  t() %>% as_tibble() %>% 
  mutate_at(c("Year", "NB_bl", "VL_bl", "L_bl", "M_bl", "H_bl", "VH_bl", "X_bl", 
              "mean_bl", "tot_pixels_bl"), as.numeric)

zonal_comp <- scenario_zonal %>% 
  left_join(baseline_zonal,
            by = join_by(HUC12, Year)) %>% 
  dplyr::select(-tot_pixels_bl) %>% 
  #positive when scenario has more pixels of that category than baseline 
  mutate(VL_diff = VL - VL_bl,
         L_diff = L - L_bl,
         M_diff = M - M_bl,
         H_diff = H - H_bl,
         VH_diff = VH - VH_bl,
         X_diff = X - X_bl,
         mean_diff = mean - mean_bl) %>% 
  dplyr::select(HUC12, Priority, TxType, TxIntensity, Year, 
                mean_diff, VL_diff, L_diff, M_diff, H_diff, VH_diff, X_diff,
                tot_pixels,
                everything())

# zonal_comp <- zonal_comp %>% 
#   left_join(flagged_raw %>% 
#               dplyr::select(HUC12, Priority, TxType, Year,
#                             trt_yr, 
#                             expFlame_base, expFlame_500k,
#                             expFlame_1m, expFlame_2m) %>% 
#               mutate(Priority = if_else(Priority == "Hybrid", "RFFC", Priority)), 
#             by = join_by(HUC12, Priority, TxType, Year)) %>% 
#   dplyr::select(HUC12, Priority, TxType, TxIntensity, Year, trt_yr,
#                 mean_diff, VL_diff, L_diff, M_diff, H_diff, VH_diff, X_diff, 
#                 tot_pixels,
#                 expFlame_base, expFlame_500k,
#                 expFlame_1m, expFlame_2m,
#                 everything()) 

# so that baseline has its own rows here too, create baseline entry and join with itself
zonal_comp_bl <- baseline_zonal %>% 
  mutate(Priority = "baseline",
         TxIntensity = "baseline",
         TxType = "baseline") %>% 
  rename(NB = NB_bl,
         VL = VL_bl,
         L = L_bl,
         M = M_bl,
         H = H_bl,
         VH = VH_bl,
         X = X_bl,
         mean = mean_bl,
         tot_pixels = tot_pixels_bl) %>% 
  left_join(baseline_zonal,
            by = join_by(HUC12, Year)) %>% 
  dplyr::select(-tot_pixels_bl) %>% 
  mutate(VL_diff = VL - VL_bl,
         L_diff = L - L_bl,
         M_diff = M - M_bl,
         H_diff = H - H_bl,
         VH_diff = VH - VH_bl,
         X_diff = X - X_bl,
         mean_diff = mean - mean_bl) %>% 
  dplyr::select(HUC12, Priority, TxType, TxIntensity, Year, 
                mean_diff, VL_diff, L_diff, M_diff, H_diff, VH_diff, X_diff,
                tot_pixels,
                everything())

#scenarios and baseline 'comparison' results
zonal_comp_all <- bind_rows(zonal_comp, zonal_comp_bl) %>% 
  #add in treatment year for later filtering
  mutate(Priority = if_else(Priority == "RFFC", "Hybrid", Priority)) %>% 
  left_join(huc_trt_yr, by = join_by(HUC12, Priority)) %>% 
  #add nonburn/coastal flag
  mutate(nonburn_coastal = if_else(HUC12 %in% nb_hucs$huc12, TRUE, FALSE)) 


if (adj_to_use == "FL"){
  
  write_csv(zonal_comp_all, file.path("qa", 
                                      "SN_fuel_FL_adjective_comparison_baseline.csv"))
  saveRDS(zonal_comp_all, file.path("qa", 
                                    "SN_fuel_FL_adjective_comparison_baseline.RDS"))
  
} else if (adj_to_use == "ROS"){
  
  write_csv(zonal_comp_all, file.path("qa", 
                                      "SN_fuel_ROS_adjective_comparison_baseline.csv"))
  saveRDS(zonal_comp_all, file.path("qa", 
                                    "SN_fuel_ROS_adjective_comparison_baseline.RDS"))
  
}




# fix treatmentyear

# zc <- readRDS(file.path("qa","SN_fuel_adjective_comparison_baseline.RDS")) %>% 
#   dplyr::select(-trt_yr) %>% 
#   left_join(huc_trt_yr, by = join_by(HUC12, Priority))
# write_csv(zc, file.path("qa", 
#                                     "SN_fuel_adjective_comparison_baseline.csv"))
# saveRDS(zc, file.path("qa", 
#                                   "SN_fuel_adjective_comparison_baseline.RDS"))


