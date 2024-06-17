#testing zonal means with Dave's files per request


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  terra,
  exactextractr,
  future.apply)

options(future.globals.onReference = "warning")

### Data -----------------------------------------------------

cbh_fvs <- terra::rast("qa/cbh_fraction_qa/SN_Baseline_2024_cbh_32611_FF_FVS.tif")

cbh_lf <- terra::rast("qa/cbh_fraction_qa/LF2022_CBH_230_CONUS/Tif/LC22_CBH_230.tif")

shp <- sf::st_read("qa/cbh_fraction_qa/WBDHUC12_CA_SnHucExtBound_3310.shp")
shp_sv <- terra::vect("qa/cbh_fraction_qa/WBDHUC12_CA_SnHucExtBound_3310.shp")

shp_at <- sf::st_read("data/data_huc/TxHucsTimingGroups.shp") %>% 
  filter(region == "SN")

### plan ------------------------------------------------------

# zonal extract from fvs and lf 
#  with extract_extract (partial pixel)
#  and terra::extract (centroid only)

# also maybe do same with my shp file and see if different. 

### zonals, Dave's shp, and exact ------------------------------------------

#FVS, Dave's shp, and exact
fvs_mean <- exact_extract(cbh_fvs, 
                         shp,
                         fun = "mean",
                         append_cols = c("huc12"))


#LF, Daven's shp, and exact
lf_mean <- exact_extract(cbh_lf, 
                          shp,
                          fun = "mean",
                          append_cols = c("huc12"))

#Dave's, exact
exactd <- fvs_mean %>% 
  rename(fvs = mean) %>% 
  left_join(lf_mean %>% 
              rename(lf = mean), 
            by = join_by("huc12")) %>% 
  mutate(fx_ed = fvs/lf) %>% 
  as_tibble() 
exactd

mean(exactd$fx_ed, na.rm=TRUE)


### zonals, Dave's shp and terra extract ----------------------------------
#FVS

fvs_terra <- terra::extract(cbh_fvs, 
                            shp_sv,
                            fun="mean",
                            bind=TRUE)

lf_terra <- terra::extract(cbh_lf, 
                           shp_sv,
                           fun="mean",
                           bind=TRUE)

terrad <- fvs_terra %>% 
  sf::st_as_sf() %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(huc12, cbh) %>% 
  rename(fvs = cbh) %>% 
  left_join(lf_terra %>% 
              sf::st_as_sf() %>% 
              sf::st_drop_geometry() %>% 
              dplyr::select(huc12, METERSX10) %>% 
              rename(lf = METERSX10),
            by = join_by("huc12")) %>% 
  mutate(fx_td = fvs/lf) %>% 
  as_tibble()

terrad 

mean(terrad$fx_td, na.rm=TRUE)
