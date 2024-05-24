# Year 20
# Map experiments


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  ggspatial,
  viridis,
  scales)

### User settings ---------------------------------------------

input_folder <- 'results_csv'

### Base Data import -------------------------------------------

res <- read_csv(file.path(input_folder, 
                          'datacube_expanded_20240119.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))


#Year 20
res2044 <- res %>% 
  filter(Year == 2044)

#For mapping
hucs_shp <- st_read("data/data_huc/TxPrctRankRrkWipRffc.shp")
#Simplified for faster drawing 
hucs_simpl <- sf::st_simplify(hucs_shp, preserveTopology = TRUE, dTolerance = 200)

#If CA state boundary wanted, download from 
# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
# https://www2.census.gov/geo/tiger/TIGER2023/STATE/tl_2023_us_state.zip
states <- st_read("data/tl_2023_us_state/tl_2023_us_state.shp")
ca <- states %>% filter(STUSPS == "CA")


### Compare 500k, 1m, 2m ---------------------------------------

# Create a relative metric: 1m/500k, 2m/500k
# Divide by Priority, e.g. do WUI, Fire, Hybrid separately
# Facet by treatment type (3 facets)
# 3 facet maps for 3 priorities. 

#pull the different intensities to join into single row
res2044_500k <- res2044 %>% 
  filter(TxIntensity == "500k") %>% 
  rename(hacbp_500k = HaCBP, 
         hacfl_500k = HaCFL)

res2044_1m <- res2044 %>% 
  filter(TxIntensity == "1m") %>% 
  rename(hacbp_1m = HaCBP,
         hacfl_1m = HaCFL)

res2044_2m <- res2044 %>% 
  filter(TxIntensity == "2m") %>% 
  rename(hacbp_2m = HaCBP,
         hacfl_2m = HaCFL)

res2044_jt <- res2044_500k %>% 
  left_join(res2044_1m, by = join_by(HUC12, RRK, Priority, TxType)) %>% 
  left_join(res2044_2m, by = join_by(HUC12, RRK, Priority, TxType)) 



ggplot() + 
  geom_point(data=res2044_jt, 
             aes(x=hacbp_500k, y=hacbp_2m, color=RRK),
             shape=1) +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2044_jt, 
              aes(x=hacbp_500k, y=hacbp_2m), 
              method=lm, 
              geom="smooth")


ggplot() + 
  geom_point(data=res2044_jt, 
             aes(x=hacbp_500k, y=hacbp_2m),
             shape=1) +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2044_jt, 
              aes(x=hacbp_500k, y=hacbp_2m), 
              method=lm, 
              geom="smooth") + 
  xlab("HaCBP at 500k Intensity") + 
  ylab("HaCBP at 2.3m Intensity") + 
  facet_wrap(~RRK)
#all regression lines below diagonal


## attempted ratio, abandoned ----------------------------------------------

# #calculate relative metric
# res2044_jt <- res2044_jt %>% 
#   mutate(hacbp_1m_500k = hacbp_1m / hacbp_500k,
#          hacbp_2m_500k = hacbp_2m / hacbp_500k,
#          hacfl_1m_500k = hacfl_1m / hacfl_500k,
#          hacfl_2m_500k = hacfl_2m / hacfl_500k)
# 
# res2044_jt %>% select(HUC12, RRK, Priority, TxType, hacbp_500k, hacbp_2m, hacbp_2m_500k) %>% View()
# res2044_jt %>% pull(hacbp_1m_500k) %>% summary()
# res2044_jt %>% pull(hacbp_2m_500k) %>% summary()
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# # 0.03323 0.99490 1.02139 1.04311 1.15861 3.20183 
# 
# #mean/median is above 1??
# # does that align with scatter plot below? 
# #a lot of tiny values...
# 
# ggplot() + 
#   geom_point(data=res2044_jt, 
#              aes(x=hacbp_500k, y=hacbp_2m, color=RRK),
#              shape=1) +
#   theme(aspect.ratio = 1) + 
#   geom_abline(intercept=0, slope=1) + 
#   stat_smooth(data=res2044_jt, 
#               aes(x=hacbp_500k, y=hacbp_2m), 
#               method=lm, 
#               geom="smooth")
# 
# ggplot() + 
#   geom_violin(data = res2044_jt,
#               aes(x=RRK, y=hacbp_2m_500k),
#               trim=TRUE) 
# 
# res2044_jt %>% group_by(RRK) %>% 
#   summarize(mean_hacbp_2m_500k = mean(hacbp_2m_500k),
#             median_hacbp_2m_500k = median(hacbp_2m_500k))
# 
# # RRK   mean_hacbp_2m_500k median_hacbp_2m_500k
# # <chr>              <dbl>                <dbl>
# # 1 CC                 0.908                 1.00
# # 2 NC                 1.11                  1.08
# # 3 SC                 0.910                 1   
# # 4 SN                 1.10                  1.07


## percent change ----------------------------------------

#calculate relative metric
res2044_pc <- res2044_jt %>%
  mutate(hacbp_1m_500k = (hacbp_1m - hacbp_500k) / hacbp_500k * 100,
         hacbp_2m_500k = (hacbp_2m - hacbp_500k) / hacbp_500k * 100,
         hacfl_1m_500k = (hacfl_1m - hacfl_500k) / hacfl_500k * 100,
         hacfl_2m_500k = (hacfl_2m - hacfl_500k) / hacfl_500k * 100)


res2044_pc %>% group_by(RRK) %>%
  summarize(
    min_hacbp_2m_500k = min(hacbp_2m_500k), 
    mean_hacbp_2m_500k = mean(hacbp_2m_500k),
    median_hacbp_2m_500k = median(hacbp_2m_500k),
    max_hacbp_2m_500k = max(hacbp_2m_500k))

# RRK   min_hacbp_2m_500k mean_hacbp_2m_500k median_hacbp_2m_500k max_hacbp_2m_500k
# <chr>             <dbl>              <dbl>                <dbl>             <dbl>
# 1 CC                -96.7              -9.23             -0.00726              164.
# 2 NC                -72.0              11.1               8.17                 198.
# 3 SC                -89.5              -8.97              0                    220.
# 4 SN                -66.2              10.4               6.76                 124.


res2044_pc %>% 
  select(HUC12, RRK, Priority, TxType, hacbp_500k, hacbp_2m, hacbp_2m_500k) %>% 
  arrange(desc(hacbp_2m_500k))
# HUC12        RRK   Priority TxType  hacbp_500k   hacbp_2m hacbp_2m_500k
# 1 180701040300 SC    WUI      trt1   0.00000444  0.0000142           2.20
# 2 180701040300 SC    WUI      trt6   0.00000446  0.0000142           2.18
# 3 180101070401 NC    Fire     trt4   0.000000898 0.00000268          1.98


# res2044_logdiff <- res2044_jt %>%
#   mutate(hacbp_1m_500k = log(hacbp_1m) - log(hacbp_500k), 
#          hacbp_2m_500k = log(hacbp_2m) - log(hacbp_500k),
#          hacfl_1m_500k = log(hacfl_1m) - log(hacfl_500k),
#          hacfl_2m_500k = log(hacfl_2m) - log(hacfl_500k))
# 
# res2044_logdiff %>% group_by(RRK) %>%
#   summarize(
#     min_hacbp_2m_500k = min(hacbp_2m_500k), 
#     mean_hacbp_2m_500k = mean(hacbp_2m_500k),
#     median_hacbp_2m_500k = median(hacbp_2m_500k),
#     max_hacbp_2m_500k = max(hacbp_2m_500k))
# RRK   min_hacbp_2m_500k mean_hacbp_2m_500k median_hacbp_2m_500k max_hacbp_2m_500k
# <chr>             <dbl>              <dbl>                <dbl>             <dbl>
#   1 CC                -3.40            -0.137            -0.0000726             0.970
# 2 NC                -1.27             0.0873            0.0785                1.09 
# 3 SC                -2.25            -0.131             0                     1.16 
# 4 SN                -1.09             0.0811            0.0654                0.808



## Map WUI percent change ---------------------------------------------------

res2044_pc %>% 
  filter(Priority == "WUI") %>% 
  group_by(RRK) %>% 
  summarize(
    min_hacbp_2m_500k = min(hacbp_2m_500k), 
    mean_hacbp_2m_500k = mean(hacbp_2m_500k),
    median_hacbp_2m_500k = median(hacbp_2m_500k),
    max_hacbp_2m_500k = max(hacbp_2m_500k))

ggplot() + 
  geom_point(data=res2044_pc %>% 
               filter(Priority == "WUI"), 
             aes(x=hacbp_500k, y=hacbp_2m),
             shape=1) +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2044_jt, 
              aes(x=hacbp_500k, y=hacbp_2m), 
              method=lm, 
              geom="smooth") + 
  facet_wrap(~RRK)



hucs_wui_jt <- hucs_simpl %>% 
  left_join(res2044_pc %>% 
              filter(Priority == "WUI"), 
            by = join_by("huc12" == "HUC12"))



wui_plot <- ggplot() +
  #California
  geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
  #HUCs
  geom_sf(data = hucs_wui_jt, 
          aes(fill = hacbp_2m_500k),
          color = "grey90", size = 0.01) +
  scale_fill_gradient2("Priority: WUI\nHaCBP Percent Change from 500k to 2m",
                       low = muted("blue"),
                       mid = "white",
                       high = muted("red"),
                       midpoint = 0,
                       na.value = "grey80") +
  facet_wrap(~TxType) + 
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
    style = ggspatial::north_arrow_minimal())

wui_plot
