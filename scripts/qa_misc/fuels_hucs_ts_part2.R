# takes result for fuels_grps_ts.R and makes timeseries charts


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, 
  sf)
  

## Data ------------------------------------------------------  

scf4_hucs <- readRDS(file = file.path('qa', 'fuel_timing',
                                      'sc_fire_trt4_hucs.RDS'))
scf6_hucs <- readRDS(file = file.path('qa', 'fuel_timing',
                                      'sc_fire_trt6_hucs.RDS'))


hucs <- st_read("data/data_huc/TxPrctRankRrkWipRffc.shp")


year_breaks <- c(2024, 2029, 2034, 2044)

## TS plots -------------------------------------------------

scf4h <- scf4_hucs %>% 
  left_join(hucs %>% 
              st_drop_geometry(), 
            by = join_by(huc12)) %>% 
  group_by(TxBpPrc, year, intensity, layer) %>% 
  summarize(sum = sum(summary, na.rm = TRUE)) %>% 
  mutate(year = as.numeric(year))

ggplot(data = scf4h %>% 
         filter(layer == 'cc'),
       mapping = aes(x = year, y = sum,
                     color = intensity)) + 
  geom_point() +
  geom_line() + 
  scale_x_continuous(breaks = year_breaks) +
  facet_wrap(~TxBpPrc) + 
  labs(title = "SC Fire trt 4: Sum CC",
       subtitle = "By HUC treatment timing group",
       y = "Sum of CC",
       x = "Year")

scf6h <- scf6_hucs %>% 
  left_join(hucs %>% 
              st_drop_geometry(), 
            by = join_by(huc12)) %>% 
  group_by(TxBpPrc, year, intensity, layer) %>% 
  summarize(sum = sum(summary, na.rm = TRUE)) %>% 
  mutate(year = as.numeric(year))

ggplot(data = scf6h %>% 
         filter(layer == 'cc'),
       mapping = aes(x = year, y = sum,
                     color = intensity)) + 
  geom_point() +
  geom_line() + 
  scale_x_continuous(breaks = year_breaks) +
  facet_wrap(~TxBpPrc) + 
  labs(title = "SC Fire trt 6: Sum CC",
       subtitle = "By HUC treatment timing group",
       y = "Sum of CC",
       x = "Year")
