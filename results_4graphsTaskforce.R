# script for 2024-01-05 task force results

#"for burn prob, what about a red to white to blue continuous gradient, with the HUC12 colors by % change from Year 0 (eg. -5% from Yr0 to Yr5 is blue, 0% change is white, +5% is red, or whatever the min/max % change is)."

#"Panels could either be by intensity, trt type, or year (0-5, 0-10, and 0-20yr change)."
# UPDATE: Would have FOR EACH time range, panels for intensity or trt type
#  So Yr0-5: 3-panel graph by intensity, 3-panel graph by trt type
#  Then Yr0-10: same, Yr0-20: same. 

#"For flame length, could really be the same. For % crown fire, it would be the change in proportion of category 3 (active crown fire)"

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
                          'datacube_20230102a.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

# #For mapping
hucs_shp <- st_read("data/data_huc/TxPrctRankRrkWipRffc.shp")
#simplified for faster drawing 
hucs_simpl <- sf::st_simplify(hucs_shp, preserveTopology = TRUE, dTolerance = 200)

#if CA state boundary wanted, download from 
# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
states <- st_read("data/tl_2023_us_state/tl_2023_us_state.shp")
ca <- states %>% filter(STUSPS == "CA")

### Year Data Prep -------------------------------------------

# 1. Make year datasets
# e.g. Split on Y2024 (year 0) and Y2029 (year 5)
# 2. Join on HUC12, Priority, TxIntensity, TxType, run
# Have single line with yr 0 and yr 5 HaCBP value for that
#  HUC, priority, TxIntensity, and TxType (and run)
# 3. Calculate percent change (from Year 0 to Year 5)
#  (yr5-yr0) / yr0

# 4. active crown fire has a lot of 0/0 for percent change
#  which is NaN, but want to interpret as 0 here. 

#baseline
res2024 <- res %>% 
  filter(Year == 2024)

#Year 5
res2029 <- res %>% 
  filter(Year == 2029)

res05 <- res2024 %>% 
  left_join(res2029, by = c("HUC12", "RRK", "Priority", "TxIntensity", "TxType")) %>% 
  #Note: .x is 2024, .y is 2029
  mutate(hacbp_diff = HaCBP.y - HaCBP.x, #2029-2024 value
         #percent CHANGE
         hacbp_pchange = hacbp_diff / HaCBP.x,
         #active crown fire
         act_crown_diff = active_crown_fire_perc.y - active_crown_fire_perc.x,
         act_crown_pchange = act_crown_diff / active_crown_fire_perc.x,
         ac_pchange = ifelse(is.nan(act_crown_pchange) & act_crown_diff == 0, 0, act_crown_pchange)) %>% 
  select(HUC12, RRK, Priority, TxIntensity, TxType,  
         hacbp_pchange, ac_pchange)

#Year 10
res2034 <- res %>% 
  filter(Year == 2034)

res010 <- res2024 %>% 
  left_join(res2034, by = c("HUC12", "RRK", "Priority", "TxIntensity", "TxType")) %>% 
  #Note: .x is 2024, .y is 2034
  mutate(hacbp_diff = HaCBP.y - HaCBP.x, #later year - 2024 value
         #percent CHANGE
         hacbp_pchange = hacbp_diff / HaCBP.x,
         #active crown fire
         act_crown_diff = active_crown_fire_perc.y - active_crown_fire_perc.x,
         act_crown_pchange = act_crown_diff / active_crown_fire_perc.x,
         ac_pchange = ifelse(is.nan(act_crown_pchange) & act_crown_diff == 0, 0, act_crown_pchange)) %>% 
  select(HUC12, RRK, Priority, TxIntensity, TxType, 
         hacbp_pchange, ac_pchange)

#Year 20
res2044 <- res %>% 
  filter(Year == 2044)

res020 <- res2024 %>% 
  left_join(res2044, by = c("HUC12", "RRK", "Priority", "TxIntensity", "TxType")) %>% 
  #Note: .x is 2024, .y is 2044
  mutate(hacbp_diff = HaCBP.y - HaCBP.x, #later year - 2024 value
         #percent CHANGE
         hacbp_pchange = hacbp_diff / HaCBP.x,
         #active crown fire
         act_crown_diff = active_crown_fire_perc.y - active_crown_fire_perc.x,
         act_crown_pchange = act_crown_diff / active_crown_fire_perc.x,
         ac_pchange = ifelse(is.nan(act_crown_pchange) & act_crown_diff == 0, 0, act_crown_pchange)) %>% 
  select(HUC12, RRK, Priority, TxIntensity, TxType, 
         hacbp_pchange, ac_pchange)


# write_csv(res020, 
#           file.path("results_csv", 
#                     paste0('res020_example_20230102.csv')))


### CBP by PRIORITY -------------------------------------------

#max and mins across time points
#res05 %>% pull(hacbp_pchange) %>% min(na.rm = TRUE) #-0.78
#res020 %>% pull(hacbp_pchange) %>% max(na.rm = TRUE) #64
#res020 %>% pull(hacbp_pchange) %>% summary()
# but predominantly closer to 1

cut_pts_cbp <- c(-65, seq(-3, 3, 1), 65)

#function for graphing CBP by priority

graph_cbp_priority_cut <- function(df, legend_title, cuts){
  
  #group by HUC12, priority
  priority <- df %>% 
    group_by(HUC12, Priority) %>% 
    summarize(hacpb_pc_ave = mean(hacbp_pchange, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(hacbp_pc_ave_cut = cut(hacpb_pc_ave, 
                                  cuts, 
                                  ordered_results=TRUE, 
                                  include.lowest=TRUE))

  cut_values <- levels(priority$hacbp_pc_ave_cut)
  
  #join with shapefile
  hucs_priority_raw <- hucs_simpl %>% 
    left_join(priority, by = join_by("huc12" == "HUC12"))
  #add NA hucs to each priority
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
  
  #map
  priority_plot <- ggplot() +
    ##california
    geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
    #hucs
    geom_sf(data = hucs_priority, 
            aes(fill = hacbp_pc_ave_cut),
            color = "grey90", size = 0.01) +
    scale_fill_brewer(legend_title,
                      palette = "RdYlBu",
                      direction = -1,
                      na.value = "grey80",
                      #to center at zero
                      limits = cut_values) +
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
          legend.position = "bottom",
          legend.direction = "vertical") +
    ggspatial::annotation_scale(
      location = "bl", bar_cols = c("grey50", "white")) +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      height = unit(0.6, "cm"), width = unit(0.6, "cm"),
      pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
      style = ggspatial::north_arrow_minimal())
}


cbp_05_priority_plot <- graph_cbp_priority_cut(res05, 
  "Conditional Burn Probability\nAverage Percent Change\nBetween Year 0 and Year 5", 
  cut_pts_cbp)

cbp_010_priority_plot <- graph_cbp_priority_cut(res010, 
  "Conditional Burn Probability\nAverage Percent Change\nBetween Year 0 and Year 10", 
  cut_pts_cbp)

cbp_020_priority_plot <- graph_cbp_priority_cut(res020, 
  "Conditional Burn Probability\nAverage Percent Change\nBetween Year 0 and Year 20", 
  cut_pts_cbp)


#Uncomment when you want to save. 
ggsave(plot = cbp_05_priority_plot,
       filename = "hacbp_yr5_yr0_priority.jpg",
       path = "plots",
       width = 9, height = 6, units = "in")

ggsave(plot = cbp_020_priority_plot,
       filename = "hacbp_yr20_yr0_priority.jpg",
       path = "plots",
       width = 9, height = 6, units = "in")


### Active crown fire by PRIORITY -------------------------------------------

#res05 %>% pull(ac_pchange) %>% summary()
#res020 %>% pull(ac_pchange) %>% summary()

cut_pts_acf <- c(-Inf, seq(-1, 1, 0.5), Inf)


#function for graphing by priority
graph_acf_priority_cut <- function(df, legend_title, cuts){
  
  #group by HUC12, priority
  priority <- df %>% 
    group_by(HUC12, Priority) %>% 
    summarize(act_crown_pc_ave = mean(ac_pchange, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(act_crown_pc_ave_cut = cut(act_crown_pc_ave, 
                                  cuts, 
                                  ordered_results=TRUE, 
                                  include.lowest=TRUE))
  
  cut_values <- levels(priority$act_crown_pc_ave_cut)
  
  
  #join with shapefile
  hucs_priority_raw <- hucs_simpl %>% 
    left_join(priority, by = join_by("huc12" == "HUC12"))
  #add NA hucs to each priority
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
  
  #map
  priority_plot <- ggplot() +
    ##california
    geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
    #hucs
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
    # scale_fill_gradient2(legend_title,
    #                      low = muted("purple"),
    #                      mid = "white",
    #                      high = muted("orange"),
    #                      midpoint = 0,
    #                      na.value = "grey85") +
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
          legend.position = "bottom",
          legend.direction = "vertical") +
    ggspatial::annotation_scale(
      location = "bl", bar_cols = c("grey50", "white")) +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      height = unit(0.6, "cm"), width = unit(0.6, "cm"),
      pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
      style = ggspatial::north_arrow_minimal())
}

acf_05_priority_plot <- graph_acf_priority_cut(res05, 
                                               "Active Crown Fire\nAverage Percent Change\nBetween Year 0 and Year 5",
                                               cut_pts_acf)

acf_010_priority_plot <- graph_acf_priority_cut(res010, 
                                                "Active Crown Fire\nAverage Percent Change\nBetween Year 0 and Year 10",
                                                cut_pts_acf)

acf_020_priority_plot <- graph_acf_priority_cut(res020, 
                                                "Active Crown Fire\nAverage Percent Change\nBetween Year 0 and Year 20",
                                                cut_pts_acf)

ggsave(plot = acf_05_priority_plot,
       filename = "actcrown_yr5_yr0_priority.jpg",
       path = "plots",
       width = 9, height = 6, units = "in")

ggsave(plot = acf_020_priority_plot,
       filename = "actcrown_yr20_yr0_priority.jpg",
       path = "plots",
       width = 9, height = 6, units = "in")


### CBP by INTENSITY -------------------------------------------

#function for graphing CBP by intensity
graph_cbp_intensity_cut <- function(df, legend_title, cuts){
  
  #group by HUC12, Intensity
  intensity <- df %>% 
    group_by(HUC12, TxIntensity) %>% 
    summarize(hacpb_pc_ave = mean(hacbp_pchange, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(hacbp_pc_ave_cut = cut(hacpb_pc_ave, 
                                  cuts, 
                                  ordered_results=TRUE, 
                                  include.lowest=TRUE))
  
  cut_values <- levels(intensity$hacbp_pc_ave_cut)
  
  
  #join with shapefile
  hucs_intensity_raw <- hucs_simpl %>% 
    left_join(intensity, by = join_by("huc12" == "HUC12"))
  #add NA hucs to each priority
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
  
  
  
  #map
  intensity_plot <- ggplot() +
    ##california
    geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
    #hucs
    geom_sf(data = hucs_intensity, 
            aes(fill = hacbp_pc_ave_cut),
            #no boundaries
            color = "grey90", size = 0.01) +
    scale_fill_brewer(legend_title,
                      palette = "RdYlBu",
                      direction = -1,
                      na.value = "grey80",
                      #to center at zero
                      limits = cut_values) +
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
          legend.position = "bottom",
          legend.direction = "vertical") +
    ggspatial::annotation_scale(
      location = "bl", bar_cols = c("grey50", "white")) +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      height = unit(0.6, "cm"), width = unit(0.6, "cm"),
      pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
      style = ggspatial::north_arrow_minimal())
}

cbp_05_intensity_plot <- graph_cbp_intensity_cut(res05, 
  "Conditional Burn Probability\nAverage Percent Change\nBetween Year 0 and Year 5",
  cut_pts_cbp)

cbp_010_intensity_plot <- graph_cbp_intensity_cut(res010, 
  "Conditional Burn Probability\nAverage Percent Change\nBetween Year 0 and Year 10",
  cut_pts_cbp)

cbp_020_intensity_plot <- graph_cbp_intensity_cut(res020, 
  "Conditional Burn Probability\nAverage Percent Change\nBetween Year 0 and Year 20",
  cut_pts_cbp)

ggsave(plot = cbp_05_intensity_plot,
       filename = "hacbp_yr5_yr0_intensity.jpg",
       path = "plots",
       width = 9, height = 6, units = "in")

ggsave(plot = cbp_020_intensity_plot,
       filename = "hacbp_yr20_yr0_intensity.jpg",
       path = "plots",
       width = 9, height = 6, units = "in")



### Active crown fire by INTENSITY -------------------------------------------

#function for graphing by priority
graph_acf_intensity_cut <- function(df, legend_title, cuts){
  
  #group by HUC12, priority
  intensity <- df %>% 
    group_by(HUC12, TxIntensity) %>% 
    summarize(act_crown_pc_ave = mean(ac_pchange, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(act_crown_pc_ave_cut = cut(act_crown_pc_ave, 
                                      cuts, 
                                      ordered_results=TRUE, 
                                      include.lowest=TRUE))
  
  cut_values <- levels(intensity$act_crown_pc_ave_cut)
  
  
  #join with shapefile
  hucs_intensity_raw <- hucs_simpl %>% 
    left_join(intensity, by = join_by("huc12" == "HUC12"))
  #add NA hucs to each priority
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
  
  #map
  priority_plot <- ggplot() +
    ##california
    geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
    #hucs
    geom_sf(data = hucs_intensity, 
            aes(fill = act_crown_pc_ave_cut),
            #no boundaries
            color = "grey90", size = 0.01) +
    scale_fill_brewer(legend_title,
                      palette = "PuOr",
                      direction = -1,
                      na.value = "grey80",
                      #to center at zero
                      limits = cut_values) +
    # scale_fill_gradient2(legend_title,
    #                      low = muted("purple"),
    #                      mid = "white",
    #                      high = muted("orange"),
    #                      midpoint = 0,
    #                      na.value = "grey85") +
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
          legend.position = "bottom",
          legend.direction = "vertical") +
    ggspatial::annotation_scale(
      location = "bl", bar_cols = c("grey50", "white")) +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      height = unit(0.6, "cm"), width = unit(0.6, "cm"),
      pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
      style = ggspatial::north_arrow_minimal())
}

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

ggsave(plot = acf_05_intensity_plot,
       filename = "actcrown_yr5_yr0_intensity.jpg",
       path = "plots",
       width = 9, height = 6, units = "in")

ggsave(plot = acf_020_intensity_plot,
       filename = "actcrown_yr20_yr0_intensity.jpg",
       path = "plots",
       width = 9, height = 6, units = "in")





### PARKING LOT ---------------------------------------------------------------

# graph_acf_priority <- function(df, legend_title){
#   
#   #group by HUC12, priority
#   priority <- df %>% 
#     group_by(HUC12, Priority) %>% 
#     summarize(act_crown_pc_ave = mean(act_crown_pchange, na.rm = TRUE),
#               .groups = "drop")
#   
#   #join with shapefile
#   hucs_priority_raw <- hucs_simpl %>% 
#     left_join(priority, by = join_by("huc12" == "HUC12"))
#   #add NA hucs to each priority
#   na_hucs <- hucs_priority_raw %>% 
#     filter(is.na(act_crown_pc_ave))
#   na_hucs_fire <- na_hucs %>% 
#     mutate(Priority = "Fire")
#   na_hucs_rffc <- na_hucs %>% 
#     mutate(Priority = "RFFC")
#   na_hucs_wui <- na_hucs %>% 
#     mutate(Priority = "WUI")
#   
#   hucs_priority <- hucs_priority_raw %>% 
#     filter(!is.na(Priority)) %>% 
#     rbind(na_hucs_fire, na_hucs_rffc, na_hucs_wui)
#   
#   #map
#   priority_plot <- ggplot() +
#     ##california
#     geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
#     #hucs
#     geom_sf(data = hucs_priority, 
#             aes(fill = act_crown_pc_ave),
#             #no boundaries
#             color = "grey90", size = 0.01) +
#     scale_fill_gradient2(legend_title,
#                          low = muted("purple"),
#                          mid = "white",
#                          high = muted("orange"),
#                          midpoint = 0,
#                          na.value = "grey85") +
#     facet_wrap(~Priority) + 
#     #plot adjustments
#     #scale_x_continuous(expand = c(0,0)) +
#     #scale_y_continuous(expand = c(0,0)) +
#     theme_bw() +
#     theme(panel.grid = element_blank(),
#           panel.background = element_blank(),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10),
#           strip.background = element_blank(),
#           strip.text = element_text(size = 13),
#           axis.text = element_text(family = 'sans'),
#           legend.position = "bottom") +
#     ggspatial::annotation_scale(
#       location = "bl", bar_cols = c("grey50", "white")) +
#     ggspatial::annotation_north_arrow(
#       location = "tr", which_north = "true",
#       height = unit(0.6, "cm"), width = unit(0.6, "cm"),
#       pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
#       style = ggspatial::north_arrow_minimal())
# }


# graph_cbp_priority <- function(df, legend_title){
#   
#   #group by HUC12, priority
#   priority <- df %>% 
#     group_by(HUC12, Priority) %>% 
#     summarize(hacpb_pc_ave = mean(hacbp_pchange, na.rm = TRUE),
#               .groups = "drop") 
#   
#   #join with shapefile
#   hucs_priority_raw <- hucs_simpl %>% 
#     left_join(priority, by = join_by("huc12" == "HUC12"))
#   #add NA hucs to each priority
#   na_hucs <- hucs_priority_raw %>% 
#     filter(is.na(hacpb_pc_ave))
#   na_hucs_fire <- na_hucs %>% 
#     mutate(Priority = "Fire")
#   na_hucs_rffc <- na_hucs %>% 
#     mutate(Priority = "RFFC")
#   na_hucs_wui <- na_hucs %>% 
#     mutate(Priority = "WUI")
#   
#   hucs_priority <- hucs_priority_raw %>% 
#     filter(!is.na(Priority)) %>% 
#     rbind(na_hucs_fire, na_hucs_rffc, na_hucs_wui)
#   
#   #map
#   priority_plot <- ggplot() +
#     ##california
#     geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
#     #hucs
#     geom_sf(data = hucs_priority, 
#             aes(fill = hacpb_pc_ave),
#             #no boundaries
#             color = "grey90", size = 0.01) +
#     scale_fill_gradient2(legend_title,
#                          low = muted("blue"),
#                          mid = "white",
#                          high = muted("red"),
#                          midpoint = 0,
#                          na.value = "grey85") +
#     facet_wrap(~Priority) + 
#     #plot adjustments
#     #scale_x_continuous(expand = c(0,0)) +
#     #scale_y_continuous(expand = c(0,0)) +
#     theme_bw() +
#     theme(panel.grid = element_blank(),
#           panel.background = element_blank(),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10),
#           strip.background = element_blank(),
#           strip.text = element_text(size = 13),
#           axis.text = element_text(family = 'sans'),
#           legend.position = "bottom") +
#     ggspatial::annotation_scale(
#       location = "bl", bar_cols = c("grey50", "white")) +
#     ggspatial::annotation_north_arrow(
#       location = "tr", which_north = "true",
#       height = unit(0.6, "cm"), width = unit(0.6, "cm"),
#       pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
#       style = ggspatial::north_arrow_minimal())
# }



# #group by HUC12, priority
# 
# priority05 <- res05 %>% 
#   group_by(HUC12, Priority) %>% 
#   summarize(hacpb_pc_ave = mean(hacbp_pchange))
# 
# priority05
# 
# #MAP
# 
# # join with shapefile and map
# hucs_priority05_raw <- hucs_simpl %>% 
#   left_join(priority05, by = join_by("huc12" == "HUC12"))
# 
# #add NA hucs to each priority
# na_hucs_05 <- hucs_priority05_raw %>% 
#   filter(is.na(hacpb_pc_ave))
# na_hucs_fire05 <- na_hucs_05 %>% 
#   mutate(Priority = "Fire")
# na_hucs_rffc05 <- na_hucs_05 %>% 
#   mutate(Priority = "RFFC")
# na_hucs_wui05 <- na_hucs_05 %>% 
#   mutate(Priority = "WUI")
# 
# hucs_priority05 <- hucs_priority05_raw %>% 
#   filter(!is.na(Priority)) %>% 
#   rbind(na_hucs_fire05, na_hucs_rffc05, na_hucs_wui05)
# 
# 
# 
# priority05_plot <- ggplot() +
#   ##california
#   geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
#   #hucs
#   geom_sf(data = hucs_priority05, 
#           aes(fill = hacpb_pc_ave),
#           #no boundaries
#           color = "grey90", size = 0.01) +
#   scale_fill_gradient2("Conditional Burn Probability\nAverage Percent Change\nBetween Year 0 and Year 5",
#                        low = scales::muted("blue"),
#                        mid = "white",
#                        high = scales::muted("red"),
#                        midpoint = 0,
#                        na.value = "grey60") +
#   facet_wrap(~Priority) + 
#   #plot adjustments
#   #scale_x_continuous(expand = c(0,0)) +
#   #scale_y_continuous(expand = c(0,0)) +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         panel.background = element_blank(),
#         legend.title = element_text(size = 12),
#         legend.text = element_text(size = 10),
#         strip.background = element_blank(),
#         strip.text = element_text(size = 13),
#         axis.text = element_text(family = 'sans'),
#         legend.position = "bottom") +
#   ggspatial::annotation_scale(
#     location = "bl", bar_cols = c("grey50", "white")) +
#   ggspatial::annotation_north_arrow(
#     location = "tr", which_north = "true",
#     height = unit(0.6, "cm"), width = unit(0.6, "cm"),
#     pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
#     style = ggspatial::north_arrow_minimal())
# 
# priority05_plot
