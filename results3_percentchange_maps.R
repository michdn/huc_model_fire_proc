# Script to create graphs of the percent change of conditional burn probability, 
#  and active crown fire between Year 0 and Years 5, 10, 20, respectively. 
#  Panels will by priority and intensity, separately. 


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

res_orig <- read_csv(file.path(input_folder, 
                          'datacube_expanded_20240119.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

#For mapping
hucs_shp <- st_read("data/data_huc/TxPrctRankRrkWipRffc.shp")
#Simplified for faster drawing 
hucs_simpl <- sf::st_simplify(hucs_shp, preserveTopology = TRUE, dTolerance = 200)

#If CA state boundary wanted, download from 
# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
states <- st_read("data/tl_2023_us_state/tl_2023_us_state.shp")
ca <- states %>% filter(STUSPS == "CA")

### Reformatting and factoring ------------------------------

# Want priority order: 'Fire', 'WUI', 'Hybrid'
# Want intensity order: '500k', '1m', '2m'

res <- res_orig %>% 
  #For graphing in the correct order
  # make factor with set order (priority)
  mutate(Priority = as.factor(Priority),
         Priority = forcats::fct_relevel(Priority,
                                            "Fire", "WUI", "Hybrid"),
         #Make factor with set order (intensity))
         TxIntensity = as.factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                             "500k", "1m", "2m"))


### Year Data Prep -------------------------------------------

# 1. Make year datasets
# e.g. Split on Y2024 (year 0) and Y2029 (year 5)
# 2. Join on HUC12, Priority, TxIntensity, TxType, run
# Have single line with yr 0 and yr 5 HaCBP value for that
#  HUC, priority, TxIntensity, and TxType (and run)
# 3. Calculate percent change (from Year 0 to Year 5)
#  (yr5-yr0) / yr0
# 4. Repeat for other years. 
# Note: active crown fire has a lot of 0/0 for percent change
#  which is NaN, but want to interpret as 0 here. 

#Baseline Year 0
res2024 <- res %>% 
  filter(Year == 2024)

#Year 5
res2029 <- res %>% 
  filter(Year == 2029)

res05 <- res2024 %>% 
  left_join(res2029, 
            by = c("HUC12", "RRK", "Priority", "TxIntensity", "TxType")) %>% 
  #Note: .x is 2024, .y is 2029
  mutate(hacbp_diff = HaCBP.y - HaCBP.x, #2029-2024 value
         #percent CHANGE
         hacbp_pchange = hacbp_diff / HaCBP.x,
         #active crown fire
         act_crown_diff = active_crown_fire_perc.y - active_crown_fire_perc.x,
         act_crown_pchange = act_crown_diff / active_crown_fire_perc.x,
         ac_pchange = ifelse(is.nan(act_crown_pchange) & act_crown_diff == 0, 
                             0, 
                             act_crown_pchange)) %>% 
  select(HUC12, RRK, Priority, TxIntensity, TxType,  
         hacbp_pchange, ac_pchange)

#Year 10
res2034 <- res %>% 
  filter(Year == 2034)

res010 <- res2024 %>% 
  left_join(res2034, 
            by = c("HUC12", "RRK", "Priority", "TxIntensity", "TxType")) %>% 
  #Note: .x is 2024, .y is 2034
  mutate(hacbp_diff = HaCBP.y - HaCBP.x, #later year - 2024 value
         #percent CHANGE
         hacbp_pchange = hacbp_diff / HaCBP.x,
         #active crown fire
         act_crown_diff = active_crown_fire_perc.y - active_crown_fire_perc.x,
         act_crown_pchange = act_crown_diff / active_crown_fire_perc.x,
         ac_pchange = ifelse(is.nan(act_crown_pchange) & act_crown_diff == 0, 
                             0, 
                             act_crown_pchange)) %>% 
  select(HUC12, RRK, Priority, TxIntensity, TxType, 
         hacbp_pchange, ac_pchange)

#Year 20
res2044 <- res %>% 
  filter(Year == 2044)

res020 <- res2024 %>% 
  left_join(res2044, 
            by = c("HUC12", "RRK", "Priority", "TxIntensity", "TxType")) %>% 
  #Note: .x is 2024, .y is 2044
  mutate(hacbp_diff = HaCBP.y - HaCBP.x, #later year - 2024 value
         #percent CHANGE
         hacbp_pchange = hacbp_diff / HaCBP.x,
         #active crown fire
         act_crown_diff = active_crown_fire_perc.y - active_crown_fire_perc.x,
         act_crown_pchange = act_crown_diff / active_crown_fire_perc.x,
         ac_pchange = ifelse(is.nan(act_crown_pchange) & act_crown_diff == 0, 
                             0, 
                             act_crown_pchange)) %>% 
  select(HUC12, RRK, Priority, TxIntensity, TxType, 
         hacbp_pchange, ac_pchange)



### CBP by PRIORITY -------------------------------------------

#Look at max and mins at all time points
res05 %>% pull(hacbp_pchange) %>% summary()
res010 %>% pull(hacbp_pchange) %>% summary()
res020 %>% pull(hacbp_pchange) %>% summary()

#Create a symmetrical range for graphing a diverging scale
cut_pts_cbp <- c(-65, seq(-3, 3, 1), 65)

#Function for graphing CBP by priority
graph_cbp_priority_cut <- function(df, legend_title, cuts){
  
  #Group by HUC12, priority
  priority <- df %>% 
    group_by(HUC12, Priority) %>% 
    summarize(hacpb_pc_ave = mean(hacbp_pchange, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(hacbp_pc_ave_cut = cut(hacpb_pc_ave, 
                                  cuts, 
                                  ordered_results=TRUE, 
                                  include.lowest=TRUE))

  cut_values <- levels(priority$hacbp_pc_ave_cut)
  
  #Join with shapefile
  hucs_priority_raw <- hucs_simpl %>% 
    left_join(priority, by = join_by("huc12" == "HUC12"))
  #Add NA hucs to each priority
  na_hucs <- hucs_priority_raw %>% 
    filter(is.na(hacpb_pc_ave))
  na_hucs_fire <- na_hucs %>% 
    mutate(Priority = "Fire")
  na_hucs_rffc <- na_hucs %>% 
    mutate(Priority = "RFFC")
  na_hucs_wui <- na_hucs %>% 
    mutate(Priority = "WUI")
  
  hucs_priority <- hucs_priority_raw %>% 
    filter(!is.na(Priority)) %>% 
    rbind(na_hucs_fire, na_hucs_rffc, na_hucs_wui)
  
  #Map
  priority_plot <- ggplot() +
    #California
    geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
    #HUCs
    geom_sf(data = hucs_priority, 
            aes(fill = hacbp_pc_ave_cut),
            color = "grey90", size = 0.01) +
    scale_fill_brewer(legend_title,
                      palette = "RdYlBu",
                      direction = -1,
                      na.value = "grey80",
                      #To center at zero
                      limits = cut_values) +
    facet_wrap(~Priority) + 
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
}


#Create maps
cbp_05_priority_plot <- graph_cbp_priority_cut(
  res05, 
  "Conditional Burn Probability\nAverage Percent Change\nBetween Year 0 and Year 5", 
  cut_pts_cbp)

cbp_010_priority_plot <- graph_cbp_priority_cut(
  res010, 
  "Conditional Burn Probability\nAverage Percent Change\nBetween Year 0 and Year 10", 
  cut_pts_cbp)

cbp_020_priority_plot <- graph_cbp_priority_cut(
  res020, 
  "Conditional Burn Probability\nAverage Percent Change\nBetween Year 0 and Year 20", 
  cut_pts_cbp)


#Save out  
ggsave(plot = cbp_05_priority_plot,
       filename = "hacbp_yr5_yr0_priority.jpg",
       path = "plots",
       width = 10, height = 4, units = "in")

ggsave(plot = cbp_010_priority_plot,
       filename = "hacbp_yr10_yr0_priority.jpg",
       path = "plots",
       width = 10, height = 4, units = "in")

ggsave(plot = cbp_020_priority_plot,
       filename = "hacbp_yr20_yr0_priority.jpg",
       path = "plots",
       width = 10, height = 4, units = "in")


### Active crown fire by PRIORITY -------------------------------------------

#Review max and min values
res05 %>% pull(ac_pchange) %>% summary()
res010 %>% pull(ac_pchange) %>% summary()
res020 %>% pull(ac_pchange) %>% summary()

#Create symmetrical range for graphing diverging scale
cut_pts_acf <- c(-Inf, seq(-1, 1, 0.5), Inf)


#Function for graphing by priority
graph_acf_priority_cut <- function(df, legend_title, cuts){
  
  #Group by HUC12, priority
  priority <- df %>% 
    group_by(HUC12, Priority) %>% 
    summarize(act_crown_pc_ave = mean(ac_pchange, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(act_crown_pc_ave_cut = cut(act_crown_pc_ave, 
                                  cuts, 
                                  ordered_results=TRUE, 
                                  include.lowest=TRUE))
  
  cut_values <- levels(priority$act_crown_pc_ave_cut)
  
  
  #Join with shapefile
  hucs_priority_raw <- hucs_simpl %>% 
    left_join(priority, by = join_by("huc12" == "HUC12"))
  #Add NA hucs to each priority
  na_hucs <- hucs_priority_raw %>% 
    filter(is.na(act_crown_pc_ave))
  na_hucs_fire <- na_hucs %>% 
    mutate(Priority = "Fire")
  na_hucs_rffc <- na_hucs %>% 
    mutate(Priority = "RFFC")
  na_hucs_wui <- na_hucs %>% 
    mutate(Priority = "WUI")
  
  hucs_priority <- hucs_priority_raw %>% 
    filter(!is.na(Priority)) %>% 
    rbind(na_hucs_fire, na_hucs_rffc, na_hucs_wui)
  
  #Map
  priority_plot <- ggplot() +
    #California
    geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
    #HUC12s
    geom_sf(data = hucs_priority, 
            aes(fill = act_crown_pc_ave_cut),
            #no boundaries
            color = "grey90", size = 0.01) +
    scale_fill_brewer(legend_title,
                      palette = "PuOr",
                      direction = -1,
                      na.value = "grey80",
                      #to center at zero
                      limits = cut_values) +
    facet_wrap(~Priority) + 
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
}

#Create maps
acf_05_priority_plot <- graph_acf_priority_cut(
  res05, 
  "Active Crown Fire\nAverage Percent Change\nBetween Year 0 and Year 5",
  cut_pts_acf)

acf_010_priority_plot <- graph_acf_priority_cut(
  res010, 
  "Active Crown Fire\nAverage Percent Change\nBetween Year 0 and Year 10",
  cut_pts_acf)

acf_020_priority_plot <- graph_acf_priority_cut(
  res020, 
  "Active Crown Fire\nAverage Percent Change\nBetween Year 0 and Year 20",
  cut_pts_acf)

#Save out plots
ggsave(plot = acf_05_priority_plot,
       filename = "actcrown_yr5_yr0_priority.jpg",
       path = "plots",
       width = 10, height = 4, units = "in")

ggsave(plot = acf_010_priority_plot,
       filename = "actcrown_yr10_yr0_priority.jpg",
       path = "plots",
       width = 10, height = 4, units = "in")

ggsave(plot = acf_020_priority_plot,
       filename = "actcrown_yr20_yr0_priority.jpg",
       path = "plots",
       width = 10, height = 4, units = "in")


### CBP by INTENSITY -------------------------------------------

#Function for graphing CBP by intensity
graph_cbp_intensity_cut <- function(df, legend_title, cuts){
  
  #Group by HUC12, Intensity
  intensity <- df %>% 
    group_by(HUC12, TxIntensity) %>% 
    summarize(hacpb_pc_ave = mean(hacbp_pchange, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(hacbp_pc_ave_cut = cut(hacpb_pc_ave, 
                                  cuts, 
                                  ordered_results=TRUE, 
                                  include.lowest=TRUE))
  
  cut_values <- levels(intensity$hacbp_pc_ave_cut)
  
  
  #Join with shapefile
  hucs_intensity_raw <- hucs_simpl %>% 
    left_join(intensity, by = join_by("huc12" == "HUC12"))
  #Add NA hucs to each priority
  na_hucs <- hucs_intensity_raw %>% 
    filter(is.na(hacpb_pc_ave))
  na_hucs_1m <- na_hucs %>% 
    mutate(TxIntensity = "1m")
  na_hucs_2m <- na_hucs %>% 
    mutate(TxIntensity = "2m")
  na_hucs_500k <- na_hucs %>% 
    mutate(TxIntensity = "500k")
  
  hucs_intensity <- hucs_intensity_raw %>% 
    filter(!is.na(TxIntensity)) %>% 
    rbind(na_hucs_1m, na_hucs_2m, na_hucs_500k)
  
  #Map
  intensity_plot <- ggplot() +
    #California
    geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
    #HUC12s
    geom_sf(data = hucs_intensity, 
            aes(fill = hacbp_pc_ave_cut),
            #no boundaries
            color = "grey90", size = 0.01) +
    scale_fill_brewer(legend_title,
                      palette = "RdYlBu",
                      direction = -1,
                      na.value = "grey80",
                      #To center at zero
                      limits = cut_values) +
    facet_wrap(~TxIntensity) + 
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
}

#Create maps
cbp_05_intensity_plot <- graph_cbp_intensity_cut(
  res05, 
  "Conditional Burn Probability\nAverage Percent Change\nBetween Year 0 and Year 5",
  cut_pts_cbp)

cbp_010_intensity_plot <- graph_cbp_intensity_cut(
  res010, 
  "Conditional Burn Probability\nAverage Percent Change\nBetween Year 0 and Year 10",
  cut_pts_cbp)

cbp_020_intensity_plot <- graph_cbp_intensity_cut(
  res020, 
  "Conditional Burn Probability\nAverage Percent Change\nBetween Year 0 and Year 20",
  cut_pts_cbp)

#Save out plots
ggsave(plot = cbp_05_intensity_plot,
       filename = "hacbp_yr5_yr0_intensity.jpg",
       path = "plots",
       width = 10, height = 4, units = "in")

ggsave(plot = cbp_010_intensity_plot,
       filename = "hacbp_yr10_yr0_intensity.jpg",
       path = "plots",
       width = 10, height = 4, units = "in")

ggsave(plot = cbp_020_intensity_plot,
       filename = "hacbp_yr20_yr0_intensity.jpg",
       path = "plots",
       width = 10, height = 4, units = "in")



### Active crown fire by INTENSITY -------------------------------------------

#Function for graphing by priority
graph_acf_intensity_cut <- function(df, legend_title, cuts){
  
  #Group by HUC12, priority
  intensity <- df %>% 
    group_by(HUC12, TxIntensity) %>% 
    summarize(act_crown_pc_ave = mean(ac_pchange, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(act_crown_pc_ave_cut = cut(act_crown_pc_ave, 
                                      cuts, 
                                      ordered_results=TRUE, 
                                      include.lowest=TRUE))
  
  cut_values <- levels(intensity$act_crown_pc_ave_cut)
  
  #Join with shapefile
  hucs_intensity_raw <- hucs_simpl %>% 
    left_join(intensity, by = join_by("huc12" == "HUC12"))
  #Add NA hucs to each priority
  na_hucs <- hucs_intensity_raw %>% 
    filter(is.na(act_crown_pc_ave))
  na_hucs_1m <- na_hucs %>% 
    mutate(TxIntensity = "1m")
  na_hucs_2m <- na_hucs %>% 
    mutate(TxIntensity = "2m")
  na_hucs_500k <- na_hucs %>% 
    mutate(TxIntensity = "500k")
  
  hucs_intensity <- hucs_intensity_raw %>% 
    filter(!is.na(TxIntensity)) %>% 
    rbind(na_hucs_1m, na_hucs_2m, na_hucs_500k)
  
  #Map
  priority_plot <- ggplot() +
    #California
    geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
    #HUC12s
    geom_sf(data = hucs_intensity, 
            aes(fill = act_crown_pc_ave_cut),
            #no boundaries
            color = "grey90", size = 0.01) +
    scale_fill_brewer(legend_title,
                      palette = "PuOr",
                      direction = -1,
                      na.value = "grey80",
                      #To center at zero
                      limits = cut_values) +
    facet_wrap(~TxIntensity) + 
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
}

#Create maps
acf_05_intensity_plot <- graph_acf_intensity_cut(
  res05, 
  "Active Crown Fire\nAverage Percent Change\nBetween Year 0 and Year 5",
  cut_pts_acf)

acf_010_intensity_plot <- graph_acf_intensity_cut(
  res010, 
  "Active Crown Fire\nAverage Percent Change\nBetween Year 0 and Year 10",
  cut_pts_acf)

acf_020_intensity_plot <- graph_acf_intensity_cut(
  res020, 
  "Active Crown Fire\nAverage Percent Change\nBetween Year 0 and Year 20",
   cut_pts_acf)

#Save out plots
ggsave(plot = acf_05_intensity_plot,
       filename = "actcrown_yr5_yr0_intensity.jpg",
       path = "plots",
       width = 10, height = 4, units = "in")

ggsave(plot = acf_010_intensity_plot,
       filename = "actcrown_yr10_yr0_intensity.jpg",
       path = "plots",
       width = 10, height = 4, units = "in")

ggsave(plot = acf_020_intensity_plot,
       filename = "actcrown_yr20_yr0_intensity.jpg",
       path = "plots",
       width = 10, height = 4, units = "in")


### Violin, scatter plots -----------------------------------------------------

res020

ggplot() + 
  geom_violin(data = res020,
              aes(x=RRK, y=hacbp_pchange),
                  trim=TRUE) 
  # geom_dotplot(data = res020,
  #              aes(x=RRK, y=hacbp_pchange),
  #              binaxis='y', stackdir='center', dotsize=1)

ggplot() +
  # geom_point(data = res020,
  #            aes(x=RRK, y=hacbp_pchange)) + 
  geom_jitter(data = res020,
              aes(x=RRK, y=hacbp_pchange),
              shape=16, position=position_jitter(0.4))

res020v2 <- res2024 %>% 
  left_join(res2044, 
            by = c("HUC12", "RRK", "Priority", "TxIntensity", "TxType")) %>% 
  #Note: .x is 2024, .y is 2044
  mutate(hacbp_diff = HaCBP.y - HaCBP.x, #later year - 2024 value
         #percent CHANGE
         hacbp_pchange = hacbp_diff / HaCBP.x,
         #active crown fire
         act_crown_diff = active_crown_fire_perc.y - active_crown_fire_perc.x,
         act_crown_pchange = act_crown_diff / active_crown_fire_perc.x,
         ac_pchange = ifelse(is.nan(act_crown_pchange) & act_crown_diff == 0, 
                             0, 
                             act_crown_pchange))

ggplot() + 
  geom_point(data=res020v2, 
             aes(x=log10(HaCBP.x), y=log10(HaCBP.y), color=RRK),
             shape=1) +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1)


res05v2 <- res2024 %>% 
  left_join(res2029, 
            by = c("HUC12", "RRK", "Priority", "TxIntensity", "TxType")) %>% 
  #Note: .x is 2024, .y is 2029
  mutate(hacbp_diff = HaCBP.y - HaCBP.x, #2029-2024 value
         #percent CHANGE
         hacbp_pchange = hacbp_diff / HaCBP.x,
         #active crown fire
         act_crown_diff = active_crown_fire_perc.y - active_crown_fire_perc.x,
         act_crown_pchange = act_crown_diff / active_crown_fire_perc.x,
         ac_pchange = ifelse(is.nan(act_crown_pchange) & act_crown_diff == 0, 
                             0, 
                             act_crown_pchange))

ggplot() + 
  geom_point(data=res05v2, 
             aes(x=log10(HaCBP.x), y=log10(HaCBP.y), color=RRK),
             shape=1) +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1)


# #example intensity oddness
# #Baseline Year 0
# res2024v3 <- res_orig %>% 
#   filter(Year == 2024)
# #Year 20
# res2044v3 <- res %>% 
#   filter(Year == 2044)
# 
# res020v3 <- res2024v3 %>% 
#   left_join(res2044v3, 
#             by = c("HUC12", "RRK", "Priority", "TxIntensity", "TxType")) %>% 
#   #Note: .x is 2024, .y is 2044
#   mutate(hacbp_diff = HaCBP.y - HaCBP.x, #later year - 2024 value
#          #percent CHANGE
#          hacbp_pchange = hacbp_diff / HaCBP.x,
#          #active crown fire
#          act_crown_diff = active_crown_fire_perc.y - active_crown_fire_perc.x,
#          act_crown_pchange = act_crown_diff / active_crown_fire_perc.x,
#          ac_pchange = ifelse(is.nan(act_crown_pchange) & act_crown_diff == 0, 
#                              0, 
#                              act_crown_pchange)) %>% 
#   select(HUC12, RRK, Priority, TxIntensity, TxType, 
#          hacbp_pchange, ac_pchange)
# 
# res020v3 %>% 
#   group_by(RRK, TxIntensity) %>% 
#   summarize(ave_cbp_pc = median(hacbp_pchange, na.rm = TRUE),
#             .groups = "drop") %>% 
#   group_by(RRK) %>% 
#   slice_min(order_by = ave_cbp_pc, n = 3) #only 3
# 
# # # A tibble: 12 × 3
# # # Groups:   RRK [4]
# # RRK   TxIntensity ave_cbp_pc
# # <chr> <chr>            <dbl>
# # 1 CC    500k            0.105 
# # 2 CC    1m              0.116 
# # 3 CC    2m              0.131 
# # 4 NC    500k           -0.0294
# # 5 NC    1m              0.0159
# # 6 NC    2m              0.0774
# # 7 SC    500k            0.0515
# # 8 SC    1m              0.0624
# # 9 SC    2m              0.0836
# # 10 SN    500k            0.104 
# # 11 SN    1m              0.156 
# # 12 SN    2m              0.222 
# 
