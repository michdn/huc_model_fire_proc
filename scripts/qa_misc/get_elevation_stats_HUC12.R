# get min, max, mean elevation for all HUCs

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  terra,
  exactextractr)



### data ------------------------------------------------------

#huc shapefile
hucs_all <- st_read("data/data_huc/TxHucsTimingGroups.shp") 


#elevation
elev <- terra::rast("data/data_topo/elevation.tif")

m_to_ft <- 3.280839895

### extract ----------------------------------------------------

elev_na <- terra::subst(elev, from=-9999, to=NA)

elev_stats <- exact_extract(elev_na, 
                            hucs_all,
                            append_cols = c("huc12", "region"),
                            fun = c("mean", "min", "max"))

#elev_stats_orig <- elev_stats

elev_out <- elev_stats %>% 
  as_tibble() %>% 
  rename(HUC12 = huc12,
         Region = region,
         mean_m = mean,
         min_m = min,
         max_m = max) %>% 
  mutate(mean_ft = mean_m * m_to_ft,
         min_ft = min_m * m_to_ft,
         max_ft = max_m * m_to_ft) %>% 
  mutate(midmontane25007000 = if_else(min_ft >= 2500 & max_ft <= 7000, 
                                      TRUE, 
                                      FALSE))

elev_out %>% filter(Region == "SN") %>% filter(midmontane25007000)

#write_csv(elev_out, "qa/elev/hucs_elevation_stats.csv")
saveRDS(elev_out, "qa/elev/hucs_elevation_stats.RDS")
