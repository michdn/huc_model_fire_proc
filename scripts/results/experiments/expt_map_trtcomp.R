# map? 

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  ggspatial,
  viridis,
  scales)


### Base Data import -------------------------------------------

res_orig <- read_csv(file.path('results',
                          'absolute',
                          'SC_absolute_expanded_NOFVS_20240319.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

#For mapping
hucs_shp <- st_read("data/data_huc/TxPrctRankRrkWipRffc.shp")
#Simplified for faster drawing 
hucs_simpl <- sf::st_simplify(hucs_shp, preserveTopology = TRUE, dTolerance = 200)

#If CA state boundary wanted, download from 
# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
states <- st_read("data/tl_2023_us_state/tl_2023_us_state.shp")
ca <- states %>% filter(STUSPS == "CA")


### Data filter ------------------------------------------------

#to be replaced by loops, if that's the direction we end up going

res_yr_p_t <- res_orig %>% 
  filter(Year == 2039,
         Priority == "Fire",
         TxType == "trt6")

### Comp 500k to 2m ----------------------------------------

res_yr_p_t_500k <- res_yr_p_t %>%
  filter(TxIntensity == "500k") %>%
  rename(expBurn_500k = expBurn)

res_yr_p_t_2m <- res_yr_p_t %>%
  filter(TxIntensity == "2m") %>%
  rename(expBurn_2m = expBurn)

res_yr_p_t_2m500k <- res_yr_p_t_2m %>%
  left_join(res_yr_p_t_500k,
            by = join_by(HUC12, Region, Priority, TxType, Year)) %>%
  mutate(expBurn_2m500k = expBurn_2m - expBurn_500k)


# res_yr_p_i_trt6 <- res_yr_p_i %>% 
#   filter(TxType == "trt6") %>% 
#   rename(expBurn_trt6 = expBurn)
# 
# res_yr_p_i_trt1 <- res_yr_p_i %>% 
#   filter(TxType == "trt1") %>% 
#   rename(expBurn_trt1 = expBurn)
# 
# res_yr_p_i_trt61 <- res_yr_p_i_trt6 %>% 
#   left_join(res_yr_p_i_trt1, 
#             by = join_by(HUC12, Region, Priority, TxIntensity, Year)) %>% 
#   mutate(expBurn61 = expBurn_trt6 - expBurn_trt1)


### Map ----------------------------------------------------------

this_hucs_2m500k <- hucs_shp %>% 
  left_join(res_yr_p_t_2m500k %>% 
              dplyr::select(HUC12, Region, Priority, TxType, Year, 
                            expBurn_2m500k), 
            by = join_by("huc12" == "HUC12"))

plot_2m500k <- ggplot() +
  #California
  geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
  #HUCs
  geom_sf(data = this_hucs_2m500k, 
          aes(fill = expBurn_2m500k),
          color = "grey90", size = 0.01) +
  scale_fill_gradient2("Difference in expected burned acres\n2m - 500k intensity",
                       low = muted("blue"),
                       mid = "white",
                       high = muted("red"),
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
  labs(title = "2039: Fire trt6 scenarios",
       subtitle = "Comparing 2m to 500k intensity") + 
  coord_sf(xlim = c(-121, -115.5), ylim = c(32, 35.5), expand = FALSE)

plot_2m500k


# this_hucs_trt61 <- hucs_shp %>% 
#   left_join(res_yr_p_i_trt61 %>% 
#               dplyr::select(HUC12, Region, Priority, TxIntensity, Year, 
#                             expBurn61), 
#             by = join_by("huc12" == "HUC12"))
# 
# plot_61 <- ggplot() +
#   #California
#   geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
#   #HUCs
#   geom_sf(data = this_hucs_trt61, 
#           aes(fill = expBurn61),
#           color = "grey90", size = 0.01) +
#   scale_fill_gradient2("Difference in expected burned acres\ntrt6 - trt1",
#                        low = muted("blue"),
#                        mid = "white",
#                        high = muted("red"),
#                        midpoint = 0,
#                        na.value = "grey80") +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         panel.background = element_blank(),
#         legend.title = element_text(size = 11),
#         legend.text = element_text(size = 10),
#         strip.background = element_blank(),
#         strip.text = element_text(size = 13),
#         axis.text = element_text(family = 'sans'),
#         legend.position = "right",
#         legend.direction = "vertical") +
#   ggspatial::annotation_scale(
#     location = "bl", bar_cols = c("grey50", "white")) +
#   ggspatial::annotation_north_arrow(
#     location = "tr", which_north = "true",
#     height = unit(0.6, "cm"), width = unit(0.6, "cm"),
#     pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
#     style = ggspatial::north_arrow_minimal()) +
#   labs(title = "2034: Fire 500k scenarios",
#        subtitle = "Comparing trt6 to trt1")
# 
# plot_61
