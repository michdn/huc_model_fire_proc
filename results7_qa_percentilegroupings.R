# comparison of treatment percentile groups

# QA only as we are looking at absolute values
#  not for analysis unless switch to relative measurement

# Uses output of results2_datacube plus orig shapefile

#"For QAQC, we should subset the data by priority and treatment percentile groups, 
# then summarize across those HUCs for each year (i.e., 2024, 2029, 2034, 2044), 
# treatment intensity, and treatment type."

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf)

### User settings ---------------------------------------------

input_folder <- 'results_csv' # file.path('results_csv', 'testing_qaqc') 

### Base Data import -------------------------------------------

res <- read_csv(file.path(input_folder, 
                          'datacube_expanded_20240119.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

#to get treatment percentile groups
#hucs_shp <- st_read("data/data_huc/TxPrctRankRrkWipRffc.shp")


### data prep ------------------------------

#now in expanded datacube
# # Want priority 'RFFC' to show as 'Hybrid'
# 
# res <- res %>% 
#   #change name to Hybrid
#   # name change will already be done in later versions of datacube,
#   # but won't matter if here as well, it just won't do anything
#   mutate(Priority = ifelse(Priority == 'RFFC', 'Hybrid', Priority)) 
# 
# #TxBpPrc == for fire priority
# #TxRffcP == for RFFC (aka hybrid) priority
# #TxWPrct == for WUI priority
# res_hucs <- res %>%
#   left_join(hucs_shp %>% 
#               select(huc12, TxBpPrc, TxRffcP, TxWPrct), 
#             by = join_by("HUC12" == "huc12"))


### Summarizing -------------------------------------------------

#Fire
fire <- res %>% 
  filter(Priority == "Fire") %>% 
  group_by(RRK, TxBpPrc, Priority, TxIntensity, TxType, Year) %>% 
  summarize(count = n(),
            median_cbp = median(HaCBP, na.rm = TRUE),
            mean_cbp = mean(HaCBP, na.rm = TRUE),
            min_cbp = min(HaCBP, na.rm = TRUE),
            max_cbp = max(HaCBP, na.rm = TRUE),
            #flame length
            median_cfl = median(HaCFL, na.rm = TRUE),
            mean_cfl = mean(HaCFL, na.rm = TRUE),
            min_cf = min(HaCFL, na.rm = TRUE),
            max_cfl = max(HaCFL, na.rm = TRUE),
            #act crown fire
            median_acf = median(active_crown_fire_perc, na.rm = TRUE),
            mean_acf = mean(active_crown_fire_perc, na.rm = TRUE),
            min_acf = min(active_crown_fire_perc, na.rm = TRUE),
            max_acf = max(active_crown_fire_perc, na.rm = TRUE))

#Hybrid
hybrid <- res %>% 
  filter(Priority == "Hybrid") %>% 
  group_by(RRK, TxRffcP, Priority, TxIntensity, TxType, Year) %>% 
  summarize(count = n(),
            median_cbp = median(HaCBP, na.rm = TRUE),
            mean_cbp = mean(HaCBP, na.rm = TRUE),
            min_cbp = min(HaCBP, na.rm = TRUE),
            max_cbp = max(HaCBP, na.rm = TRUE),
            #flame length
            median_cfl = median(HaCFL, na.rm = TRUE),
            mean_cfl = mean(HaCFL, na.rm = TRUE),
            min_cf = min(HaCFL, na.rm = TRUE),
            max_cfl = max(HaCFL, na.rm = TRUE),
            #act crown fire
            median_acf = median(active_crown_fire_perc, na.rm = TRUE),
            mean_acf = mean(active_crown_fire_perc, na.rm = TRUE),
            min_acf = min(active_crown_fire_perc, na.rm = TRUE),
            max_acf = max(active_crown_fire_perc, na.rm = TRUE))

#WUI
wui <- res %>% 
  filter(Priority == "WUI") %>% 
  group_by(RRK, TxWPrct, Priority, TxIntensity, TxType, Year) %>% 
  summarize(count = n(),
            median_cbp = median(HaCBP, na.rm = TRUE),
            mean_cbp = mean(HaCBP, na.rm = TRUE),
            min_cbp = min(HaCBP, na.rm = TRUE),
            max_cbp = max(HaCBP, na.rm = TRUE),
            #flame length
            median_cfl = median(HaCFL, na.rm = TRUE),
            mean_cfl = mean(HaCFL, na.rm = TRUE),
            min_cf = min(HaCFL, na.rm = TRUE),
            max_cfl = max(HaCFL, na.rm = TRUE),
            #act crown fire
            median_acf = median(active_crown_fire_perc, na.rm = TRUE),
            mean_acf = mean(active_crown_fire_perc, na.rm = TRUE),
            min_acf = min(active_crown_fire_perc, na.rm = TRUE),
            max_acf = max(active_crown_fire_perc, na.rm = TRUE))




write_csv(fire, 
          file = file.path(input_folder, "percentilegroup_raw_summaries_fire_20240119.csv"))

write_csv(hybrid, 
          file = file.path(input_folder, "percentilegroup_raw_summaries_hybrid_20240119.csv"))

write_csv(wui, 
          file = file.path(input_folder, "percentilegroup_raw_summaries_wui_20240119.csv"))














