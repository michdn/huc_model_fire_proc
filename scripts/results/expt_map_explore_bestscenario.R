# Map of best scenario per HUC
# Colorful

# best scenario per HUC12
# Year 20

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

res_cc <- read_csv(file.path('results',
                             'absolute',
                             'CC_absolute_expanded_NOFVS_20240319.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

res <- bind_rows(res_sc, res_cc)

#spatial
hucs_shp <- st_read("data/data_huc/TxHucsTimingGroups.shp")
#Simplified for faster drawing 
hucs_simpl <- sf::st_simplify(hucs_shp, preserveTopology = TRUE, dTolerance = 200)

#If CA state boundary wanted, download from 
# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
states <- st_read("data/tl_2023_us_state/tl_2023_us_state.shp")
ca <- states %>% filter(STUSPS == "CA")


### Lowest expected total burned acres -------------------------

#Year 2039
res2039 <- res %>% 
  filter(Year == 2039)

lowest_burn <- res2039 %>% 
  dplyr::select(HUC12, Region, Priority, TxIntensity, TxType, expBurn) %>% 
  group_by(HUC12, Region) %>% 
  arrange(expBurn) %>% 
  slice(1)

lowest_burn %>% 
  group_by(Priority) %>% 
  summarize(count = n())

lowest_burn %>% 
  group_by(TxIntensity) %>% 
  summarize(count = n())

lowest_burn %>% 
  group_by(TxType) %>% 
  summarize(count = n())

lowest_burn %>% 
  group_by(Priority, TxIntensity, TxType) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% View()

lowest_burn %>% 
  group_by(Region, Priority, TxIntensity, TxType) %>% 
  summarize(count = n()) %>% 
  arrange(Region, desc(count)) %>% 
  ungroup() %>% 
  group_by(Region) %>% 
  slice(1)


#add scenarios, and factor order (27)
lowest_burn <- lowest_burn %>% 
  mutate(scenario_tpi = paste0(TxType, "-", Priority, "-", TxIntensity),
         scenario_itp = paste0(TxIntensity, "-", TxType, "-", Priority),
         scenario_pti = paste0(Priority, "-", TxType, "-", TxIntensity),
         scenario_pit = paste0(Priority, "-", TxIntensity, "-", TxType),
         scenario_ipt = paste0(TxIntensity, "-", TxType, "-", Priority))


#add spatial data 

#INNER join to only get hucs in this cube
hucs_lowest <- hucs_shp %>% 
  inner_join(lowest_burn,
            by = join_by('huc12' == 'HUC12'))



#Plot a few in R to explore

plot_best_tpi <- ggplot() +
  #California
  #geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
  #HUCs
  geom_sf(data = hucs_lowest, 
          aes(fill = scenario_tpi),
          color = "grey90", size = 0.01) +
  scale_fill_viridis("Scenario", discrete=TRUE, option='viridis') + 
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
  labs(title = "2039: Best scenarios")

#plot_best_tpi


plot_best_pti <- ggplot() +
  #California
  #geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
  #HUCs
  geom_sf(data = hucs_lowest, 
          aes(fill = scenario_pti),
          color = "grey90", size = 0.01) +
  scale_fill_viridis("Scenario", discrete=TRUE, option='viridis') + 
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
  labs(title = "2039: Best scenarios")

#plot_best_pti

#save out to do nice plot in QGIS with easier sorting
# otherwise would need to do 27-level manual factor order

hucs_lowest <- hucs_lowest %>% 
  #remove unwanted to avoid confusion. Want pit and ipt
  select(-c(scenario_tpi, scenario_itp, scenario_pti))

st_write(hucs_lowest,
         dsn="plots/data_spatial_maps/scenario_lowest_burn_acres_20240321.gpkg",
         layer="scenario_lowest_burn_acres")

