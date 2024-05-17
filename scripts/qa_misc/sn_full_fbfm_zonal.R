# hail mary full of grace


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  terra,
  exactextractr,
  future.apply)

options(future.globals.onReference = "error")



### user settings -------------------------------------------

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


# #previously flagged HUC-scenario-years
# flagged_raw <- readRDS(file.path("qa",
#                                  "SN_intensity_inverted_flagged_1.01_20240416.RDS"))
# # flagged_raw <- readRDS(file.path("qa",
# #                                  "SN_intensity_inverted_flagged_1.5_20240416.RDS"))
# #MATCH with IMPORT! 
# threshold <-  "1.01" #"1.5"

### setup -------------------------------------------

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


### Data ---------------------------------------------------

raster_folder <- file.path("E:", "MAS", "blended_rasters",
                           "SN")

raster_files <- list.files(raster_folder,
                           full.names = TRUE,
                           pattern = "*fml.+tif$")


bl_raster_folder <- file.path("E:", "MAS", "blended_rasters",
                              "baseline")

bl_raster_files <- list.files(bl_raster_folder,
                              full.names = TRUE,
                              pattern = "SN.+fml.+tif$")


#huc shapefile
hucs_all <- st_read("data/data_huc/TxHucsTimingGroups.shp") 


### testing ----------------------------------------------

# huc_id1 <- "180201250901"
# 
# priority <- "Fire"
# trt <- "trt1"
# 
# #target
# scenario_targets <- tibble(HUC12 = huc_id1, 
#                            Priority = priority, 
#                            TxType = trt,
#                            TxIntensity = c("2m", "1m", "500k")) %>% 
#   cross_join(tibble(Year = c("2024", "2029", "2034", "2039"))) %>% 
#   unite(col = search_pattern, sep = "_", remove=FALSE,
#         Priority, TxIntensity, TxType, Year) %>% 
#   #scenario rasters
#   rowwise() %>% 
#   mutate(raster_target = grep(raster_files, 
#                               pattern = search_pattern, 
#                               value = TRUE)) %>% 
#   #stop rowwise
#   ungroup()
# 
# bl_targets <- tibble(HUC12 = huc_id1,
#                      Priority = "baseline", 
#                      TxType = "baseline",
#                      TxIntensity = "baseline",
#                      Year =  c("2024", "2029", "2034", "2039")) %>% 
#   #baseline rasters
#   rowwise() %>% 
#   mutate(raster_target = grep(bl_raster_files, 
#                               pattern = Year,
#                               value = TRUE)) %>% 
#   ungroup() %>% 
#   distinct()
# 
# #scenario and baseline
# all_targets <- bind_rows(scenario_targets, bl_targets)


### apply -------------------------------------------------------------


scenario_targets <- huc_scen %>% 
  #back to RFFC for Hybrid
  mutate(Priority = if_else(Priority == "Hybrid", "RFFC", Priority)) %>% 
  unite(col = search_pattern, sep = "_", remove = FALSE,
        Priority, TxIntensity, TxType, Year) %>% 
  rowwise() %>% 
  mutate(raster_target = grep(raster_files, 
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


#all
all_targets <- bind_rows(scenario_targets, bl_targets)



#function for apply
get_scenario_fbfm_counts <- function(r){
  
  this_row <- all_targets[r,]
  
  this_raster <- terra::rast(this_row[["raster_target"]])
  
  this_huc <- hucs_all %>% 
    filter(huc12 == this_row[["HUC12"]]) %>% 
    #transform to same crs as raster
    st_transform(st_crs(this_raster))
  
  
  this_fbfm_counts <- exact_extract(this_raster, 
                                    this_huc, 
                                    function(value, coverage_fraction ) {table(value)})
  
  #rename_with(~paste0("fbfm",.), -c(HUC12, etc.))
  
  ##tbl would have been useful if looping, but we are using apply function
  # this_result <- this_target_row %>% 
  #   bind_cols(as_tibble(this_fbfm_counts, rownames = "fbfm")) %>% 
  #   rename(pixel_counts = V1) %>% 
  #   dplyr::select(HUC12, Priority, TxType, TxIntensity, Year, fbfm, pixel_counts)
  
  colnames(this_fbfm_counts) <- "pixel_counts"
  
  this_fbfm_counts <- rbind(this_fbfm_counts, total = colSums(this_fbfm_counts))
  
  this_result <- cbind(HUC12 = this_row[["HUC12"]],
                       Priority = this_row[["Priority"]],
                       TxType = this_row[["TxType"]],
                       TxIntensity = this_row[["TxIntensity"]],
                       Year = this_row[["Year"]],
                       fbfm = dimnames(this_fbfm_counts)[[1]],
                       this_fbfm_counts)
  #will complain otherwise when transforming later into a tibble
  rownames(this_result) <- NULL
  
  this_list <- list(this_result)
  
}



plan(multisession, workers = availableCores(omit=1))

system.time(zonal_raw <- future_sapply(1:nrow(all_targets),
                                    get_scenario_fbfm_counts))

zonal <- do.call(rbind, zonal_raw) %>% 
  as_tibble() %>% 
  mutate(pixel_counts = as.numeric(pixel_counts))

zonal_wide <- zonal %>% 
  pivot_wider(names_from = fbfm, 
              names_prefix = "f", 
              values_from = pixel_counts) %>% 
  #counts would be NA is that fbfm value was missing from the huc, replace with 0
  replace(is.na(.), 0) %>% 
  # sort better
  dplyr::select(HUC12, Priority, TxType, TxIntensity, Year, sort(names(.))) %>% 
  mutate(Year = as.numeric(Year))
  

saveRDS(zonal_wide, file.path("qa", "SN_FBFM_zonal_counts.RDS"))

#write_csv(zonal_wide, file.path("qa", "SN_FBFM_zonal_counts.csv"))
