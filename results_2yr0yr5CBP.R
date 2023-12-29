#script to compare Year 0 to Year 5 results
# by priority
# by intensity

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  ggspatial,
  viridis)

### User settings ---------------------------------------------

input_folder <- 'results_csv'

### Base Data import -------------------------------------------

res_sc <- read_csv(file.path(input_folder, 
                          'main_results_from_sql_SC.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

# res_cc_sn <- read_csv(file.path(input_folder, 
#                                 'main_results_from_sql_CC_SN.csv')) %>% 
#   mutate(HUC12 = as.character(HUC12))
# res_nc <- read_csv(file.path(input_folder, 
#                                 'main_results_from_sql_NC.csv')) %>% 
#   mutate(HUC12 = as.character(HUC12))

res <- res_sc #for now, until others available
#res <- bind_rows(res_sc, res_cc_sn, res_ns)

# #For mapping
hucs_shp <- st_read("data/data_huc/TxPrctRankRrkWipRffc.shp")
#simplified for faster drawing 
hucs_simpl <- sf::st_simplify(hucs_shp, preserveTopology = TRUE, dTolerance = 200)

#if CA state boundary wanted, download from 
# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
states <- st_read("data/tl_2023_us_state/tl_2023_us_state.shp")
ca <- states %>% filter(STUSPS == "CA")

### Data Prep -------------------------------------------

# 1. Make two datasets
# Split on Y2024 (year 0) and Y2029 (year 5)
# 2. Join on HUC12, Priority, TxIntensity, TxType, run
# Have single line with yr 0 and yr 5 HaCBP value for that
#  HUC, priority, TxIntensity, and TxType (and run)
# 3. Calculate percent change (from Year 0 to Year 5)
#  (yr5-yr0) / yr0

res2024 <- res %>% 
  filter(Year == 2024)

res2029 <- res %>% 
  filter(Year == 2029)

res05 <- res2024 %>% 
  left_join(res2029, by = c("HUC12", "RRK", "Priority", "TxIntensity", "TxType", "run")) %>% 
  #Note: .x is 2024, .y is 2029
  mutate(hacbp_diff = HaCBP.y - HaCBP.x, #2029-2024 value
         hacbp_pchange = hacbp_diff / HaCBP.x) %>% #percent CHANGE
  select(HUC12, RRK, Priority, TxIntensity, TxType, run, hacbp_diff, hacbp_pchange, everything())



### by PRIORITY -------------------------------------------

#group by HUC12, priority

res_priority <- res05 %>% 
  group_by(HUC12, Priority) %>% 
  summarize(hacpb_pc_ave = mean(hacbp_pchange))

priority_cut_breaks <- quantile(res_priority$hacpb_pc_ave, 
                                seq(0, 1, 0.25), 
                                na.rm = TRUE) %>% 
  #adjust rounding as needed
  round(4) %>% 
  #to pull out values
  unique()
#labels reversed here, because adding after reversing levels below
priority_cut_labels <- c("higher", "moderate-high", "low-moderate", "lower")

res_priority <- res_priority %>% 
  mutate(hacbp_pc_ave_cut = cut(hacpb_pc_ave, 
                                priority_cut_breaks, 
                                ordered_results=TRUE, 
                                include.lowest=TRUE),
         hacbp_pc_ave_labeled = factor(hacbp_pc_ave_cut, 
                                       levels = rev(levels(hacbp_pc_ave_cut)),
                                       labels = priority_cut_labels, 
                                       ordered = TRUE))

res_priority

### by PRIORITY MAP -------------------------------------------

#NOTE: once all regions are available, No data sections are not needed,
#     however do no harm by remaining

# join with shapefile and map
hucs_priority_raw <- hucs_simpl %>% 
  left_join(res_priority, by = join_by("huc12" == "HUC12")) %>% 
  mutate(hacbp_pc_ave_labeled = fct_na_value_to_level(hacbp_pc_ave_labeled,
                                                      level = "Not yet available"))

#add NA hucs to each priority
na_hucs <- hucs_priority_raw %>% 
  filter(is.na(hacbp_pc_ave_cut))
na_hucs_fire <- na_hucs %>% 
  mutate(Priority = "Fire")
na_hucs_rffc <- na_hucs %>% 
  mutate(Priority = "RFFC")
na_hucs_wui <- na_hucs %>% 
  mutate(Priority = "WUI")

hucs_priority <- hucs_priority_raw %>% 
  filter(!is.na(Priority)) %>% 
  rbind(na_hucs_fire, na_hucs_rffc, na_hucs_wui)
  

#edit scale fill to have grey NA appended to viridis
priority_colors <- c(viridis(4, option="D", direction=-1), "grey70")

priority_plot <- ggplot() +
  ##california
  geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
  #hucs
  geom_sf(data = hucs_priority, 
          aes(fill = hacbp_pc_ave_labeled),
          #no boundaries
          color = "grey90", size = 0.01) +
  scale_fill_manual(values = priority_colors,
                    "Conditional Burn Probability\nAverage Percent Change\nBetween Year 0 and Year 5") +
  facet_wrap(~Priority) + 
  #plot adjustments
  #scale_x_continuous(expand = c(0,0)) +
  #scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        strip.background = element_blank(),
        strip.text = element_text(size = 13),
        axis.text = element_text(family = 'sans'),
        legend.position = "bottom") +
  ggspatial::annotation_scale(
    location = "bl", bar_cols = c("grey50", "white")) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    height = unit(0.6, "cm"), width = unit(0.6, "cm"),
    pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
    style = ggspatial::north_arrow_minimal())
  
priority_plot

#Uncomment when you want to save. 
# ggsave(plot = priority_plot, 
#        filename = "hacbp_yr5_yr0_priority.jpg",
#        width = 9,
#        height = 5,
#        units = "in")

### by INTENSITY -------------------------------------------

#group by HUC12, intensity

res_intensity <- res05 %>% 
  group_by(HUC12, TxIntensity) %>% 
  summarize(hacpb_pc_ave = mean(hacbp_pchange))

intensity_cut_breaks <- quantile(res_intensity$hacpb_pc_ave, 
                                seq(0, 1, 0.25), 
                                na.rm = TRUE) %>% 
  #adjust rounding as needed
  round(4) %>% 
  #to pull out values
  unique()
#labels reversed here, because adding after reversing levels below
intensity_cut_labels <- c("4th quartile", "3rd quartile", "2nd quartile", "1st quartile")

res_intensity <- res_intensity %>% 
  mutate(hacbp_pc_ave_cut = cut(hacpb_pc_ave, 
                                intensity_cut_breaks, 
                                ordered_results=TRUE, 
                                include.lowest=TRUE),
         hacbp_pc_ave_labeled = factor(hacbp_pc_ave_cut, 
                                       levels = rev(levels(hacbp_pc_ave_cut)),
                                       labels = intensity_cut_labels, 
                                       ordered = TRUE))

res_priority


### by INTENSITY MAP -------------------------------------------

# join with shapefile and map
hucs_intensity_raw <- hucs_simpl %>% 
  left_join(res_intensity, by = join_by("huc12" == "HUC12")) %>% 
  mutate(hacbp_pc_ave_labeled = fct_na_value_to_level(hacbp_pc_ave_labeled,
                                                      level = "Not yet available"))

#add NA hucs to each priority
# so that it shows properly per level
na_hucs <- hucs_intensity_raw %>% 
  filter(is.na(hacbp_pc_ave_cut))
na_hucs_1m <- na_hucs %>% 
  mutate(TxIntensity = "1m")
na_hucs_2m <- na_hucs %>% 
  mutate(TxIntensity = "2m")
na_hucs_500k <- na_hucs %>% 
  mutate(TxIntensity = "500k")

hucs_intensity <- hucs_intensity_raw %>% 
  filter(!is.na(TxIntensity)) %>% 
  rbind(na_hucs_1m, na_hucs_2m, na_hucs_500k)


#edit scale fill to have grey NA appended to viridis
intensity_colors <- c(viridis(4, option="D", direction=-1), "grey70")

intensity_plot <- ggplot() +
  ##california
  geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
  #hucs
  geom_sf(data = hucs_intensity, 
          aes(fill = hacbp_pc_ave_labeled),
          #no boundaries
          color = "grey90", size = 0.01) +
  scale_fill_manual(values = intensity_colors,
                    "Conditional Burn Probability\nAverage Percent Change\nBetween Year 0 and Year 5") +
  facet_wrap(~TxIntensity) + 
  #plot adjustments
  #scale_x_continuous(expand = c(0,0)) +
  #scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        strip.background = element_blank(),
        strip.text = element_text(size = 13),
        axis.text = element_text(family = 'sans'),
        legend.position = "bottom") +
  ggspatial::annotation_scale(
    location = "bl", bar_cols = c("grey50", "white")) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    height = unit(0.6, "cm"), width = unit(0.6, "cm"),
    pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
    style = ggspatial::north_arrow_minimal())

intensity_plot

# #uncomment when you want to save
# ggsave(plot = intensity_plot, 
#        filename = "hacbp_yr5_yr0_intensity.jpg",
#        width = 9,
#        height = 5,
#        units = "in")

