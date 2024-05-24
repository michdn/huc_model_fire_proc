# experiment
# map
# Magnitude of difference between 2.3m and ... 500k intensities

# Pick single priority, trt. 


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  ggspatial,
  viridis,
  scales)

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
hucs_simpl <- sf::st_simplify(hucs_shp, preserveTopology = TRUE, dTolerance = 200)

#If CA state boundary wanted, download from 
# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
states <- st_read("data/tl_2023_us_state/tl_2023_us_state.shp")
ca <- states %>% filter(STUSPS == "CA")


### 2m & 500k --------------------------------------------

#Val suggests to normalize by baseline expected burned acres for expBurn


i2m <- res %>% 
  filter(TxIntensity == "2m") %>% 
  mutate(expFlame_hucAc_2m = expFlame / hucAc) %>% 
  rename(expBurn_2m = expBurn,
         expFlame_2m = expFlame,
         expPcActive_2m = expPcActive)

i500k <- res %>% 
  filter(TxIntensity == "500k") %>% 
  mutate(expFlame_hucAc_500k = expFlame / hucAc) %>% 
  rename(expBurn_500k = expBurn,
         expFlame_500k = expFlame,
         expPcActive_500k = expPcActive)

res_2m500k <- i2m %>% 
  left_join(i500k, 
            by = join_by(HUC12, Region, Priority, TxType, Year)) %>%
  #negative when 2m 'better'
  mutate(expBurn_2m500k = expBurn_2m - expBurn_500k,
         expFlame_2m500k = expFlame_2m - expFlame_500k,
         expFlame_hucAc_2m500k = expFlame_hucAc_2m - expFlame_hucAc_500k, 
         expPcActive_2m500k = expPcActive_2m - expPcActive_500k) %>% 
  #too many columns
  dplyr::select(HUC12, Region, Priority, TxType, Year, 
                expBurn_2m, expBurn_500k, #example
                expBurn_2m500k, expFlame_2m500k, expFlame_hucAc_2m500k, expPcActive_2m500k)

#only 1 year, priority, trt (1 row per HUC now)
res_2m500k_sel <- res_2m500k %>% 
  filter(Year == 2039,
         Priority == "Fire",
         TxType == "trt4")


#INNER join to only get hucs in this cube
hucs_2m500k <- hucs_shp %>% 
  inner_join(res_2m500k_sel,
             by = join_by('huc12' == 'HUC12'))



#plot in R to see if worth
# NOTE!
#Val suggests to normalize by baseline expected burned acres for expBurn
ggplot() +
  #California
  #geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
  #HUCs
  geom_sf(data = hucs_2m500k, 
          aes(fill = expBurn_2m500k),
          color = "grey90", size = 0.01) +
  #scale_fill_viridis("Burned acres", option='mako') + 
  scale_fill_gradient2("Difference in expected burned acres\n2m - 500k intensity",
                       low = "blue",
                       mid = "white",
                       high = "red",
                       midpoint = 0,
                       na.value = "grey80") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        strip.background = element_blank(),
        strip.text = element_text(size = 13),
        axis.text = element_text(family = 'sans'),
        legend.position = "right",
        legend.direction = "vertical") +
  ggspatial::annotation_scale(
    location = "bl", bar_cols = c("grey50", "white")) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    height = unit(0.6, "cm"), width = unit(0.6, "cm"),
    pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
    style = ggspatial::north_arrow_minimal()) +
  labs(title = "Benefit of 2.3m intensity over 500k",
       subtitle = "2039: Fire trt4")



ggplot() +
  #California
  #geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
  #HUCs
  geom_sf(data = hucs_2m500k, 
          aes(fill = expFlame_hucAc_2m500k),
          color = "grey90", size = 0.01) +
  scale_fill_gradient2("Difference in expected flame length\n2m - 500k intensity",
                       low = "blue",
                       mid = "white",
                       high = "red",
                       midpoint = 0,
                       na.value = "grey80") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        strip.background = element_blank(),
        strip.text = element_text(size = 13),
        axis.text = element_text(family = 'sans'),
        legend.position = "right",
        legend.direction = "vertical") +
  ggspatial::annotation_scale(
    location = "bl", bar_cols = c("grey50", "white")) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    height = unit(0.6, "cm"), width = unit(0.6, "cm"),
    pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
    style = ggspatial::north_arrow_minimal()) +
  labs(title = "Benefit of 2.3m intensity over 500k",
       subtitle = "2039: Fire trt4")


#write out
# st_write(hucs_2m500k,
#          dsn="plots/data_spatial_maps/hucs2m500k.gpkg",
#          layer="benefit_2m")

