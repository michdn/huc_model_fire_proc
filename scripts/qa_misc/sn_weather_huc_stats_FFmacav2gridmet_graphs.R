# graphs of data
# maps 


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  arrow,
  viridis, 
  gridExtra) 

### Data -----------------------------------------------------

mg_stats <- readRDS(file.path("qa", 
                           "weather_macav2_gridmet",
                           "SN_huc_stats_rhws.RDS"))

ff <- arrow::read_parquet(file.path("data",
                                    "data_weather_20240612_FF", 
                                    "mas_wx_ff_worsthour_2_vars.parquet")) 

### Data processing and joining ------------------------------

mg_stats_wide <- stats %>% 
  pivot_wider(id_cols = c(HUC12, weather_var),
              names_from = data_source,
              values_from = c(mean, median, stdev, min, max))



### Scatter plots -------------------------------------------

#force axis to be the same for data source (but respective of variable)
facet_limits <- stats %>% 
  group_by(weather_var) %>% 
  summarize(min=min(mean), max=max(mean)) %>% 
  gather(range, mean, -weather_var) %>% 
  dplyr::select(-range)

p_scatter <- ggplot() + 
  geom_point(data = stats_wide,
             mapping = aes(x=mean_gridmet,
                           y=mean_macav2),
             shape=1, color='darkblue') +
  geom_abline(intercept=0, slope=1, color='black') + 
  #fake to force limits
  geom_blank(data=facet_limits,
             mapping=aes(x=mean, y=mean)) + 
  facet_wrap(~weather_var, scales = "free") + 
  theme_bw() + 
  theme(aspect.ratio = 1) + 
  labs(title="Comparison of weather data sources",
       subtitle="SN HUC means",
       caption="RH: Min 2nd percentile\nWind: 98th percentile",
       y="HUC Mean from MACAV2",
       x = "HUC Mean from gridMET")

p_scatter

ggsave(plot = p_scatter,
       filename=file.path("qa",
                          "weather_macav2_gridmet",
                          "scatter_huc_means_rh_ws_mv2gm.jpg"),
       height=6, width=8, units=c("in"))

