# Just FF fuel moisture graphs/maps? 


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  arrow,
  viridis, 
  gridExtra) 

### Data -----------------------------------------------------

#wind speed and fuel moistures
# but wind speeds going to be altered.... 
ff <- arrow::read_parquet(file.path("qa",
                                    "weather_ff", 
                                    "mas_wx_ff_worsthour_2_vars.parquet")) 

hucs_all <- st_read("data/data_huc/TxHucsTimingGroups.shp") 

# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
states <- st_read("data/tl_2023_us_state/tl_2023_us_state.shp")
ca <- states %>% filter(STUSPS == "CA")


# #temperature, rel hum, wind speed
# mg_stats <- readRDS(file.path("qa", 
#                            "weather_macav2_gridmet",
#                            "SN_huc_stats_rhws.RDS"))
# mg_stats_wide <- stats %>% 
#   pivot_wider(id_cols = c(HUC12, weather_var),
#               names_from = data_source,
#               values_from = c(mean, median, stdev, min, max))


### Data processing ------------------------------

ff_mean <- ff %>% 
  dplyr::select(mas_regcode, huc_id,
                ws, wd, m1, m10, m100) %>% 
  group_by(mas_regcode, huc_id) %>% 
  summarize(ws = mean(ws),
            m1 = mean(m1),
            m10 = mean(m10),
            m100 = mean(m100),
            .groups = "drop")

ff_mean

ff_mean %>% 
  group_by(mas_regcode) %>% 
  summarize(count_hucs = n())
# mas_regcode count_hucs
# 1 CC                 434
# 2 NC                 774
# 3 SC                 480
# 4 SN                1148
# Missing one in CC. 
# (Likely the ABP=0 HUC, I'll bet. HUC 180500040804. 
#zero_huc_id <- "180500040804"
#ff %>% filter(huc_id == zero_huc)
# ... 982 rows. Not this HUC then. But this HUC had fires in it in FOD? or does it just look like that with FF data, and these are simulated fires perhaps. 

#find missing
hucs_all %>% 
  st_drop_geometry() %>% 
  dplyr::select(region, huc12) %>% 
  arrange(region, huc12) %>% 
  left_join(ff_mean, by = join_by(huc12==huc_id)) %>% 
  filter(is.na(ws))

missing_huc_id <- "180500021001"

#make long, drop ws
ffm_long <- ff_mean %>% 
  dplyr::select(-ws) %>% 
  pivot_longer(cols=c(m1, m10, m100), names_to="variable")

### plots/maps -------------------------------------------

ff_hucs <- hucs_all %>% 
  #dropping the lone NA HUC 
  inner_join(ffm_long, 
            by = join_by(huc12==huc_id))

p_ff <- ggplot() +
  #California
  geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
  #HUCs
  geom_sf(data = ff_hucs, 
          #color the HUC by stat
          aes(fill = value),
          color = "grey90", size = 0.01) +
  #use viridis color scheme
  scale_fill_viridis("", 
                     option="viridis",
                     #dealing with NAs 
                     na.value="grey95") + 
  #plot settings for a nicer map
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        strip.background = element_blank(),
        strip.text = element_text(size = 13),
        axis.text = element_text(family = "sans"),
        legend.position = "right",
        legend.direction = "vertical") +
  #add a scale bar
  ggspatial::annotation_scale(
    location = "bl", bar_cols = c("grey50", "white")) +
  #add a north arrow
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    height = unit(0.6, "cm"), width = unit(0.6, "cm"),
    pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
    style = ggspatial::north_arrow_minimal()) +
  #add a plot title
  labs(title = "FF weather") + 
  facet_wrap(~variable)

p_ff

ggsave(plot = p_ff,
       filename=file.path("qa",
                          "weather_ff",
                          "map_m1m10m100.jpg"),
       height=6, width=12, units=c("in"))


# #force axis to be the same for data source (but respective of variable)
# facet_limits <- stats %>% 
#   group_by(weather_var) %>% 
#   summarize(min=min(mean), max=max(mean)) %>% 
#   gather(range, mean, -weather_var) %>% 
#   dplyr::select(-range)
# 
# p_scatter <- ggplot() + 
#   geom_point(data = stats_wide,
#              mapping = aes(x=mean_gridmet,
#                            y=mean_macav2),
#              shape=1, color='darkblue') +
#   geom_abline(intercept=0, slope=1, color='black') + 
#   #fake to force limits
#   geom_blank(data=facet_limits,
#              mapping=aes(x=mean, y=mean)) + 
#   facet_wrap(~weather_var, scales = "free") + 
#   theme_bw() + 
#   theme(aspect.ratio = 1) + 
#   labs(title="Comparison of weather data sources",
#        subtitle="SN HUC means",
#        caption="RH: Min 2nd percentile\nWind: 98th percentile",
#        y="HUC Mean from MACAV2",
#        x = "HUC Mean from gridMET")
# 
# p_scatter
# 
# ggsave(plot = p_scatter,
#        filename=file.path("qa",
#                           "weather_macav2_gridmet",
#                           "scatter_huc_means_rh_ws_mv2gm.jpg"),
#        height=6, width=8, units=c("in"))
# 
