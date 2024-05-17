# worst Gridfire HaCFL inversions combination report

# inversion with respect to BASELINE
# RAW value, not percent change

# To limit the number of repeat HUCs
# worst of each HUC12

# Calc most extreme. 
# then show:

# GF 
#   HaCFL
#   HaCBP
#   surface, crown

# Fuel Adj
#   ROS
#   FL

# Locator


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  viridis,
  sf,
  ggspatial,
  scales,
  gridExtra)

### Data -----------------------------------------------------

# GRIDFIRE
gf <- read_csv(file.path("results",
                         "absolute", 
                         "SN_SNbl_SNbw_absolute_20240423.csv")) %>% 
  mutate(HUC12 = as.character(HUC12)) 


# # BEHAVEPLUS
# 
# bp <- read_csv(file.path("qa", "behaveplus",
#                          "SN_FBFM_zonal_counts_BP_namesrepaired.csv")) %>%
#   mutate(HUC12 = as.character(HUC12),
#          Priority = if_else(Priority == "RFFC", "Hybrid", Priority)) 


# FUEL ADJ ROS & FL
adj_fl <- readRDS(file.path("qa", 
                            "SN_fuel_adjective_comparison_baseline.RDS")) 
adj_ros <-  readRDS(file.path("qa", 
                              "SN_fuel_ROS_adjective_comparison_baseline.RDS")) 

# OTHER

#spatial
hucs_shp <- st_read("data/data_huc/TxHucsTimingGroups.shp") %>% 
  rename(HUC12 = huc12)
#Simplified for faster drawing 
hucs_simpl <- sf::st_simplify(hucs_shp, preserveTopology = TRUE, dTolerance = 200)


#to remove ones affected by nonburn issue
nb_hucs <- readRDS("data/nonburnable_rerun_list.RDS")


### set up for INVERSION CALC ---------------------

#treat year using already calc from adj
trt_yrs <- adj_fl %>% 
  dplyr::select(HUC12, Priority, trt_yr) %>% 
  distinct()


# # baseline masquerade 
# #baseline to be treated as an 'intensity' level PER ALL priorities, trts
# gfi_bl <- gf %>% 
#   filter(TxIntensity == "baseline") %>% 
#   #remove priorities, trts from base
#   dplyr::select(-Priority, -TxType)
# #get all combination of Priority and TxTypes
# frame <- gf %>% 
#   filter(!TxIntensity == "baseline") %>% 
#   dplyr::select(Priority, TxType) %>% 
#   distinct()
# #duplicate bases values for all priority-txtype combos
# gfi_bl_dup <- frame %>% 
#   cross_join(gfi_bl) 
# 
# #recreate res with baselines masquerade (only bl not bw here for inversion calc)
# gfi <- gf %>% 
#   filter(!TxIntensity == "baseline") %>% 
#   filter(!TxIntensity == "baseweather") %>% 
#   dplyr::bind_rows(gfi_bl_dup)
# 
# #limited
# gfi <- gfi %>% 
#   #filter out nonburnable issue HUCs
#   filter(!HUC12 %in% (nb_hucs %>% pull(huc12))) %>% 
#   #get trt yr (could calc but have)
#   left_join(trt_yrs, by = join_by(HUC12, Priority)) %>% 
#   #filter to year of treatment OR LATER
#   filter(Year >= trt_yr)
# 
# #Calc HaCFL inversions 
# gfi %>%
#   dplyr::select(HUC12, Priority, TxIntensity, TxType,
#                 Year, trt_yr, HaCFL) %>%
#   #pivot to wide, all in one row
#   pivot_wider(names_from = TxIntensity,
#               values_from = HaCFL,
#               names_prefix = "hacfl_") %>%


### Actually, just do inversions wrt baseline for this

gfi_bl <- gf %>% 
  filter(TxIntensity == "baseline") %>% 
  dplyr::select(HUC12, Year, HaCFL) %>% 
  rename(hacfl_base = HaCFL)

gfi <- gf %>% 
  #remove baselines
  filter(!TxIntensity == "baseline") %>%
  filter(!TxIntensity == "baseweather") %>%
  #limited columns
  dplyr::select(HUC12, Priority, TxIntensity, TxType,
                Year, HaCFL) %>% 
  #get baselines in row
  left_join(gfi_bl, by = join_by(HUC12, Year)) %>% 
  #filter out nonburnable issue HUCs
  filter(!HUC12 %in% (nb_hucs %>% pull(huc12))) %>%
  #get trt yr (could calc but have)
  left_join(trt_yrs, by = join_by(HUC12, Priority)) %>%
  #filter to year of treatment OR LATER
  filter(Year >= trt_yr) %>% 
  #calculate inversion to baseline, raw value (not percent change)
  # baseline minus value@intensity, so NEGATIVE when inversion. 
  mutate(hacfl_diff = hacfl_base - HaCFL) 


#top 2 of each HUC12 (to avoid lots of repeats)
# then top n
gfi_sel <- gfi %>% 
  group_by(HUC12) %>% 
  arrange(hacfl_diff) %>% 
  slice(2) %>% 
  #top n
  ungroup() %>% 
  arrange(hacfl_diff) %>% 
  slice(1:10)


### graphing prep ------------------------------------------

#GRIDFIRE 
gfg <- gf %>% 
  #For graphing in the correct order (generic, used in multiple places, with modifications)
  # make factor with set order (priority)
  mutate(Priority = as.factor(Priority),
         Priority = forcats::fct_relevel(Priority,
                                         "Fire", "WUI", "Hybrid", "baseline", "baseweather"),
         #Make factor with set order (intensity))
         TxIntensity = as.factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                            "baseweather", "baseline", "500k", "1m", "2m"),
         #recode so it fits on graphs
         TxIntensity = forcats::fct_recode(TxIntensity, "bw" = "baseweather"),
         TxIntensity = forcats::fct_recode(TxIntensity, "base" = "baseline"))

#baseline/baseweather to be treated as an 'intensity' level PER ALL priorities, trts
gfg_bases <- gfg %>% 
  filter(TxIntensity %in% c("base", "bw")) %>% 
  #remove priorities, trts from bases
  dplyr::select(-Priority, -TxType)

#remove bases from rest of results (temporarily)
gfg_scen <- gfg %>% 
  filter(!TxIntensity %in% c("base", "bw"))

#get all combination of Priority and TxTypes
frame <- gfg_scen %>% 
  dplyr::select(Priority, TxType) %>% 
  distinct()

#duplicate bases values for all priority-txtype combos
gfg_bases <- frame %>% 
  cross_join(gfg_bases) 

#add back in
gfgs <- bind_rows(gfg_scen, gfg_bases)


# FUEL MEANS
adj <- adj_fl %>% 
  dplyr::select(HUC12, Priority, TxType, TxIntensity, Year, trt_yr, mean) %>% 
  rename(fl_mean = mean) %>% 
  left_join(adj_ros %>% 
              dplyr::select(HUC12, Priority, TxType, TxIntensity, 
                            Year, trt_yr, mean) %>% 
              rename(ros_mean = mean))

# FUEL Categories
fl <- adj_fl %>% 
  #convert to percentage of total area (pixels)
  mutate(across(c(VL_diff, L_diff, M_diff, H_diff, VH_diff, X_diff), ~ ./tot_pixels)) %>% 
  #select wanted
  dplyr::select(HUC12, Priority, TxType, TxIntensity, Year,
                VL_diff, L_diff, M_diff, H_diff, VH_diff, X_diff) %>% 
  #pivot long
  pivot_longer(cols= c(VL_diff, L_diff, M_diff, H_diff, VH_diff, X_diff),
               names_to = "adjective",
               values_to = "area_fraction") %>% 
  #factor adjective so correct order
  separate_wider_delim(col = adjective, names=c("adjective", NA), delim="_") %>% 
  mutate(adjective = as.factor(adjective),
         adjective = forcats::fct_relevel(adjective,
                                          "VL", "L", "M", "H", "VH", "X")) 

ros <- adj_ros %>% 
  #convert to percentage of total area (pixels)
  mutate(across(c(VL_diff, L_diff, M_diff, H_diff, VH_diff, X_diff), ~ ./tot_pixels)) %>% 
  #select wanted
  dplyr::select(HUC12, Priority, TxType, TxIntensity, Year,
                VL_diff, L_diff, M_diff, H_diff, VH_diff, X_diff) %>% 
  #pivot long
  pivot_longer(cols= c(VL_diff, L_diff, M_diff, H_diff, VH_diff, X_diff),
               names_to = "adjective",
               values_to = "area_fraction") %>% 
  #factor adjective so correct order
  separate_wider_delim(col = adjective, names=c("adjective", NA), delim="_") %>% 
  mutate(adjective = as.factor(adjective),
         adjective = forcats::fct_relevel(adjective,
                                          "VL", "L", "M", "H", "VH", "X")) 


# Graphing things
year_breaks <- c(2024, 2029, 2034, 2039)

#colors for intensities on line graphs
i_colors <- c("bw" = "grey50",
              "base" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[1],
              "500k" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[2],
              "1m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[3],
              "2m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[4])

highlight_color <- "magenta"

ft_lines <-  c("surface" = "solid", 
               "passive_crown" = "dashed",
               "active_crown" = "dotted")

p_theme <- theme_bw() + 
  theme(plot.title = element_text(size=10),
        axis.title = element_text(size=9),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        legend.margin=margin(c(0,0,0,0)))


### Loop per row of gfi_sel ------------------------------------------

#DATA RECAP
#gfgs:  gridfire
#adj:   fuel means (fl and ros)
#fl:    fuel fl year changes from baseline
#ros:   fuel ros year changes from baseline

# loop row of selected
# that gives specific HUC - scenario - year

# for timeseries -- HUC - scenario all intensities - all years
p_collector <- list()
for (i in 1:nrow(gfi_sel)){
  
  this_row <- gfi_sel[i,]
  
  this_huc <- this_row[["HUC12"]]
  this_priority <- this_row[["Priority"]]
  this_trt <- this_row[["TxType"]]
  this_intensity <- this_row[["TxIntensity"]]
  this_year <- this_row[["Year"]]
  this_trtyr <- this_row[["trt_yr"]] #for this HUC-Priority
  
  all_title <- paste("HUC", this_huc, 
                     this_priority, this_trt, this_intensity, 
                     this_year)
  
  #gridfire data
  this_gfgs <- gfgs %>% 
    filter(HUC12 == this_huc,
           Priority == this_priority,
           TxType == this_trt)
  
  this_gfgs_specific <- this_gfgs %>% 
    filter(TxIntensity == this_intensity,
           Year == this_year)
  
  #gridfire plots
  pg_hacfl <- ggplot() +
    geom_point(data = this_gfgs,
                mapping = aes(x=Year, y=HaCFL, color=TxIntensity),
                shape = 1) +
    geom_line(data = this_gfgs,
              mapping = aes(x=Year, y=HaCFL, color=TxIntensity)) +
    scale_x_continuous(breaks = year_breaks) +
    scale_color_manual("Intensity", values = i_colors) + 
    #highlight specific intensity-year
    geom_point(data = this_gfgs_specific,
               mapping = aes(x=Year, y=HaCFL),
               color = highlight_color,
               shape = 13, size = 4) + 
    labs(title = paste0("Gridfire HaCFL (Treat year: ", this_trtyr, ")")) + 
    p_theme
  
  pg_hacbp <- ggplot() +
    geom_point(data = this_gfgs,
               mapping = aes(x=Year, y=HaCBP, color=TxIntensity),
               shape = 1) +
    geom_line(data = this_gfgs,
              mapping = aes(x=Year, y=HaCBP, color=TxIntensity)) +
    scale_x_continuous(breaks = year_breaks) +
    scale_color_manual("Intensity", values = i_colors) + 
    #highlight specific intensity-year
    geom_point(data = this_gfgs_specific,
               mapping = aes(x=Year, y=HaCBP),
               color = highlight_color,
               shape = 13, size = 4) +
    labs(title = paste0("Gridfire HaCBP (Treat year: ", this_trtyr, ")")) + 
    p_theme
  
  
  #gridfire firetype
  this_ft_gf <- this_gfgs %>% 
    dplyr::select(HUC12, Year, TxIntensity,
                  surface, passive_crown, active_crown) %>%
    pivot_longer(cols = c(surface, passive_crown, active_crown),
                 names_to = "firetype",
                 values_to = "mean_burned_frac")
  
  this_ft_specific <- this_ft_gf %>% 
    filter(Year == this_year, 
           TxIntensity == this_intensity)
  
  pg_ft <- ggplot() +
    geom_line(data = this_ft_gf,
              mapping = aes(x=Year, y=mean_burned_frac, 
                            color=TxIntensity,
                            linetype=firetype)) + 
    scale_x_continuous(breaks = year_breaks) +
    scale_color_manual("Intensity", values = i_colors) + 
    scale_linetype_manual("Fire Type", values = ft_lines) + 
    geom_point(data = this_ft_specific,
               mapping = aes(x=Year, y=mean_burned_frac),
               color = highlight_color,
               shape = 13, size = 4) + 
    labs(title = paste0("Gridfire FireTypes (Treat year: ", this_trtyr, ")")) + 
    p_theme
  
  #fuel data 
  
  this_adj <- adj %>% 
    filter(HUC12 == this_huc,
           Priority == this_priority,
           TxType == this_trt)
  
  this_adj_specific <- this_adj %>% 
    filter(TxIntensity == this_intensity,
           Year == this_year)
  
  #fuel timeseries
  pf_fl <- ggplot() +
    geom_point(data = this_adj,
                mapping = aes(x=Year, y=fl_mean, color=TxIntensity),
                shape = 1) +
    geom_line(data = this_adj,
              mapping = aes(x=Year, y=fl_mean, color=TxIntensity)) +
    scale_x_continuous(breaks = year_breaks) +
    scale_color_manual("Intensity", values = i_colors) + 
    geom_point(data = this_adj_specific,
               mapping = aes(x=Year, y=fl_mean),
               shape=13, size=4,
               color = highlight_color) + 
    labs(title = "Fuel adjective FL mean",
         y = "Ave coded FL fuel adj") + 
    p_theme
  
  pf_ros <- ggplot() +
    geom_point(data = this_adj,
               mapping = aes(x=Year, y=ros_mean, color=TxIntensity),
               shape = 1) +
    geom_line(data = this_adj,
              mapping = aes(x=Year, y=ros_mean, color=TxIntensity)) +
    scale_x_continuous(breaks = year_breaks) +
    scale_color_manual("Intensity", values = i_colors) + 
    geom_point(data = this_adj_specific,
               mapping = aes(x=Year, y=ros_mean),
               shape=13, size=4,
               color = highlight_color) + 
    labs(title = "Fuel adjective ROS mean",
         y = "Ave coded ROS fuel adj") + 
    p_theme
  
  # SKIPPING YR FUEL CHANGES
  
  # Locator
  ploc <- ggplot() +
    #HUCs
    geom_sf(data = hucs_simpl %>% 
              filter(region == "SN"), 
            color = "grey90", size = 0.01) +
    geom_sf(data = hucs_simpl %>% 
              filter(HUC12 == this_huc),
            color = highlight_color, 
            fill=highlight_color) + 
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
  
  
  # Putting the page together
  p_page <- arrangeGrob(pg_hacfl, pf_fl, 
                        pg_hacbp, pf_ros, 
                        pg_ft, ploc,
                        ncol = 2, 
                        top = all_title)
  
  p_collector[[i]] <- p_page
}

plots_to_pdf <- marrangeGrob(p_collector, nrow = 1, ncol = 1)

#dir.create(file.path("plots", 
#          "inversion_combo"))
ggsave(plots_to_pdf,
       filename = file.path("plots", 
                            "inversion_combo", 
                            "top_hacfl_inversions_multi.pdf"))
