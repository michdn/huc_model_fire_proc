

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf)
  



### HUC data ----------------------------------------------------

#to get area for priority groups
hucs_shp <- st_read("data/data_huc/TxPrctRankRrkWipRffc.shp")


# this_row_raw <- dbGetQuery(this_db, 'SELECT t1.huc_id, t1.time_horizon, t1.mas_scenario, AVG(t2.huc_burned_frac) AS HaCBP, AVG(t3.huc_avg_fl) AS HaCFL FROM mas_metadata AS t1, ha_cbp_series AS t2, ha_cfl_series AS t3')
# 
# this_cft_raw <- dbGetQuery(this_db, 'SELECT t1.huc_id, t1.time_horizon, t1.mas_scenario, t4.fire_type, AVG(t4.huc_burned_frac_in_ftype_bin) AS burn_frac_ave FROM mas_metadata AS t1, ha_cft_hist_series AS t4 GROUP BY fire_type')

#rename(Region = RRK)
