# Compare results with other datasets

#uncertain if it makes sense to compare numerically
# however, if we are doing so: 
#  scatter plot per HUC12 value, looking for a diagonal 1:1 cloud
#  need to take rasters and summarize to HUC12 (mean) first
#   -done in QGIS because terra::zonal does not work as expected



# does not make sense to compare numerically until 
# weighted by fire frequency 






### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  ggplot2,
  sf) 

### User settings ---------------------------------------------

input_folder <- 'results_csv'
plots_folder <- 'plots'

### Base Data import -------------------------------------------

res <- read_csv(file.path(input_folder, 
                          'datacube_expanded_20240119.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

res <- res %>% 
  #change name to Hybrid
  # name change will already be done in later versions of datacube,
  # but won't matter if here as well, it just won't do anything
  mutate(Priority = ifelse(Priority == 'RFFC', 'Hybrid', Priority)) 


#QGIS zonal summaries attached to shp
hucs <- st_read("other_datasets/rrk_whp_zonal_huc.shp")

### Scatter ------------------------------------------------------

#create data set
# Using 2024, 500k, Hybrid, trt1 
combined <- res %>% 
  filter(Priority == "Hybrid", TxIntensity == "500k", TxType == "trt1", Year == 2024) %>% 
  rename(huc12 = HUC12) %>% 
  left_join(hucs, by = join_by(huc12))

#rrk_mean, whp_mean
# HaCBP

ggplot() + 
  geom_point(data = combined,
             aes(x = HaCBP, 
                 y = rrk_mean))

ggplot() + 
  geom_point(data = combined,
             aes(x = HaCBP, 
                 y = whp_mean))


ggplot() + 
  geom_point(data = combined,
             aes(x = whp_mean, 
                 y = rrk_mean))


### parking lot ----------------------------------------------------

# #abandoned. rrk takes a very long time, and both return just a vector of data, not as.polygons 
# # also returns a lot of NaN 

# #reading in with terra so is a SpatVector for processing steps
# hucs <- terra::vect("data/data_huc/TxPrctRankRrkWipRffc.shp")
# 
# #other datasets
# whp <- terra::rast("other_datasets/RDS-2015-0047-4_Data/Data/whp2023_GeoTIF/whp2023_cnt_conus.tif")
# rrk <- terra::rast("other_datasets/fireDynamics/fireDynamics/AnnualBurnProbability2022.tif")
# 
# rrk_zoned <- zonal(rrk, hucs, fun=mean, na.rm=TRUE, as.polygons=TRUE)
# rrk_zoned
# whp_zoned <- terra::zonal(whp, hucs, fun=mean, na.rm=TRUE, as.polygons=TRUE)
# whp_zoned
