# Maps of best scenario per HUC
# For 2024-03-26 meeting
# 4 maps: 1) best priority, 2) best intensity, 3) best trt, 4) best combo

# best scenario per HUC12
# averaged over all years
# based on flame index (expFlame)

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  ggspatial)

### Base Data import -------------------------------------------

res_sc <- read_csv(file.path('results',
                          'absolute',
                          'SC_absolute_expanded_NOFVS_20240319.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

# res_cc <- read_csv(file.path('results',
#                              'absolute',
#                              'CC_absolute_expanded_NOFVS_20240319.csv')) %>% 
#   mutate(HUC12 = as.character(HUC12))

res <- res_sc #bind_rows(res_sc, res_cc)

#spatial
hucs_shp <- st_read("data/data_huc/TxHucsTimingGroups.shp")
#Simplified for faster drawing 
#hucs_simpl <- sf::st_simplify(hucs_shp, preserveTopology = TRUE, dTolerance = 200)

#If CA state boundary wanted, download from 
# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
#states <- st_read("data/tl_2023_us_state/tl_2023_us_state.shp")
#ca <- states %>% filter(STUSPS == "CA")


### relabeling trt --------------------------------

res <- res %>% 
  mutate(trt_desc = case_when(
    TxType == "trt1" ~ "Mod thin removal",
    TxType == "trt4" ~ "Heavy thinning",
    TxType == "trt6" ~ "Mastication", 
    TxType == "trt7" ~ "Prescribed fire",
    .default = "TRT DESC ERROR"
  ))


### Lowest flame index -------------------------

res_ave <- res %>% 
  # note, NOT by year, averaging over year
  group_by(HUC12, Priority, TxIntensity, TxType, trt_desc) %>% 
  summarize(mean_expFlame = mean(expFlame),
            count_check = n(),
            .groups = 'drop')


lowest_flame <- res_ave %>% 
  #by HUC
  group_by(HUC12) %>% 
  #lowest value first
  arrange(mean_expFlame) %>% 
  #take first
  slice(1)


lowest_flame %>% 
  group_by(Priority) %>% 
  summarize(count = n())

lowest_flame %>% 
  group_by(TxIntensity) %>% 
  summarize(count = n())

lowest_flame %>% 
  group_by(trt_desc) %>% 
  summarize(count = n())

lowest_flame %>% 
  group_by(Priority, TxIntensity, trt_desc) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% View()


#add scenario name
lowest_flame <- lowest_flame %>% 
  mutate(scenario_itp = paste0(TxIntensity, "-", trt_desc, "-", Priority))



#add spatial data 

#INNER join to only get hucs in this cube
hucs_lowest <- hucs_shp %>% 
  inner_join(lowest_flame,
            by = join_by('huc12' == 'HUC12'))


#save out to do nice plot in QGIS with easier sorting
# otherwise would need to do n-level manual factor order
# and nicer background maps. 

st_write(hucs_lowest,
         dsn="plots/data_spatial_maps/scenario_lowest_flame_20240325.gpkg",
         layer="scenario_lowest_flame",
         append=FALSE)
