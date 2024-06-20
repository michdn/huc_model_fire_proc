# worst Gridfire HaCFL inversions combination report

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

#path_out
folder_out <- file.path("plots", "inversion_combo")
dir.create(folder_out, recursive = TRUE)


# GRIDFIRE
gf <- read_csv(file.path("results",
                               "datacube", 
                               "datacube_interim_SNSCCC_20240617.csv")) %>% 
  mutate(HUC12 = as.character(HUC12)) %>% 
  mutate(Priority = as.factor(Priority),
         Priority = forcats::fct_relevel(Priority,
                                         "Fire", "WUI", "Hybrid"),
         #Make factor with set order (intensity))
         TxIntensity = as.factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                            "500k", "1m", "2m"))
         

#hucs extreme inversions 
hucs_extreme <- read_csv("qajun/extreme_inversion_185.csv") %>% 
  mutate(Priority = as.factor(Priority),
         Priority = forcats::fct_relevel(Priority,
                                         "Fire", "WUI", "Hybrid"))

# # BEHAVEPLUS
# 
# bp <- read_csv(file.path("qa", "behaveplus",
#                          "SN_FBFM_zonal_counts_BP_namesrepaired.csv")) %>%
#   mutate(HUC12 = as.character(HUC12),
#          Priority = if_else(Priority == "RFFC", "Hybrid", Priority)) 


# FUEL ADJ ROS & FL
#  Note, not using comparison to baseline info, since baseline scrapped
#  However, can use that huc-scenario info
# NOTE: only SN, so CC outliers will not have
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


### prep -------------------------------------------

#order hucs by worst inversion (by huc)
huc_order <- hucs_extreme %>% 
  dplyr::select(Region, HUC12, Priority, trt_yr, TxType, hacfl_2m500k_pc) %>% 
  group_by(Region, HUC12) %>% 
  slice_max(order_by = hacfl_2m500k_pc, n=1) %>% 
  ungroup() %>% 
  arrange(Region, desc(hacfl_2m500k_pc)) %>% 
  mutate(order_num = row_number())

# then within a HUC, we will sort by hacfl_2m500k_pc. 

inversion_list <- hucs_extreme %>% 
  left_join(huc_order %>% 
              dplyr::select(HUC12, order_num),
            by = join_by(HUC12)) %>% 
  arrange(order_num, hacfl_2m500k_pc) %>% 
  #just to have if I need later
  mutate(row_order = row_number())


# FUEL MEANS
adj <- adj_fl %>% 
  dplyr::select(HUC12, Priority, TxType, TxIntensity, Year, trt_yr, mean) %>% 
  rename(fl_mean = mean) %>% 
  left_join(adj_ros %>% 
              dplyr::select(HUC12, Priority, TxType, TxIntensity, 
                            Year, trt_yr, mean) %>% 
              rename(ros_mean = mean),
            by = join_by(HUC12, Priority, TxType, TxIntensity, Year, trt_yr)) %>% 
  mutate(TxIntensity = as.factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                            "500k", "1m", "2m"))




# Graphing things
year_breaks <- c(2024, 2029, 2034, 2039)

#colors for intensities on line graphs
i_colors <- c("500k" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[2],
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


### Loop by inversion list tibble -----------------------

#Each row identifies a huc-priority-txtype
# filter gf, adj_fuels to this
# then graph set for a combo page

p_collector <- list()

for (i in seq_along(1:nrow(inversion_list))){
  
  this_row <- inversion_list[i,]
  
  #filter data sets
  this_gf <- gf %>% 
    filter(HUC12 == this_row[["HUC12"]],
           Priority == this_row[["Priority"]],
           TxType == this_row[["TxType"]])
  
  if (this_row[["Region"]] == "SN"){
    
    this_adj <- adj %>% 
      filter(HUC12 == this_row[["HUC12"]],
             Priority == this_row[["Priority"]],
             TxType == this_row[["TxType"]])
    
  }
  

  #graphs  
  all_title <- paste0(this_row[["Region"]], " ", this_row[["HUC12"]], 
                     " ", this_row[["Priority"]], 
                     " (Treated ", this_row[["trt_yr"]], ") ",
                     this_row[["TxType"]],
                     " ", this_row[["Year"]])
  

  #gridfire plots
  pg_hacfl <- ggplot() +
    geom_point(data = this_gf,
               mapping = aes(x=Year, y=HaCFL, color=TxIntensity),
               shape = 1) +
    geom_line(data = this_gf,
              mapping = aes(x=Year, y=HaCFL, color=TxIntensity)) +
    scale_x_continuous(breaks = year_breaks) +
    scale_color_manual("Intensity", values = i_colors) + 
    #highlight specific intensity-year
    geom_point(data = this_row,
               mapping = aes(x=Year, y=hacfl_2m),
               color = highlight_color,
               shape = 13, size = 4) + 
    labs(title = "Gridfire HaCFL") + 
    p_theme
  
  pg_flame <- ggplot() +
    geom_point(data = this_gf,
               mapping = aes(x=Year, y=expFlame, color=TxIntensity),
               shape = 1) +
    geom_line(data = this_gf,
              mapping = aes(x=Year, y=expFlame, color=TxIntensity)) +
    scale_x_continuous(breaks = year_breaks) +
    scale_color_manual("Intensity", values = i_colors) + 
    #highlight specific intensity-year
    geom_point(data = this_row,
               mapping = aes(x=Year, y=expFlame_2m),
               color = highlight_color,
               shape = 13, size = 4) + 
    labs(title = "Gridfire expFlame") + 
    p_theme
  
  
  pg_hacbp <- ggplot() +
    geom_point(data = this_gf,
               mapping = aes(x=Year, y=HaCBP, color=TxIntensity),
               shape = 1) +
    geom_line(data = this_gf,
              mapping = aes(x=Year, y=HaCBP, color=TxIntensity)) +
    scale_x_continuous(breaks = year_breaks) +
    scale_color_manual("Intensity", values = i_colors) + 
    labs(title = "Gridfire HaCBP") + 
    p_theme
  
  pg_burn <- ggplot() +
    geom_point(data = this_gf,
               mapping = aes(x=Year, y=expBurn, color=TxIntensity),
               shape = 1) +
    geom_line(data = this_gf,
              mapping = aes(x=Year, y=expBurn, color=TxIntensity)) +
    scale_x_continuous(breaks = year_breaks) +
    scale_color_manual("Intensity", values = i_colors) + 
    labs(title = "Gridfire expBurn") + 
    p_theme
  
  
  #gridfire firetype
  this_ft_gf <- this_gf %>% 
    dplyr::select(HUC12, Year, TxIntensity,
                  surface, passive_crown, active_crown) %>%
    pivot_longer(cols = c(surface, passive_crown, active_crown),
                 names_to = "firetype",
                 values_to = "mean_burned_frac")
  
  this_ft_specific <- this_ft_gf %>% 
    filter(Year == this_row[["Year"]], 
           TxIntensity == "2m")
  
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
    labs(title = "Gridfire FireTypes") + 
    p_theme
  
  #fuel data 
  
  this_adj <- adj %>% 
    filter(HUC12 == this_row[["HUC12"]],
           Priority == this_row[["Priority"]],
           TxType == this_row[["TxType"]])
  
  this_adj_specific <- this_adj %>% 
    filter(TxIntensity == "2m",
           Year == this_row[["Year"]])
  
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
  

  # Locator
  ploc <- ggplot() +
    #HUCs
    geom_sf(data = hucs_simpl %>% 
              filter(region == this_row[["Region"]]), 
            color = "grey90", size = 0.01) +
    geom_sf(data = hucs_simpl %>% 
              filter(HUC12 == this_row[["HUC12"]]),
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
  p_page <- arrangeGrob(pg_hacfl, pg_flame,
                        pg_hacbp, pg_burn, 
                        pf_fl, pf_ros, 
                        pg_ft, ploc,
                        ncol = 2, 
                        top = all_title)
  
  p_collector[[i]] <- p_page
  
  
}


plots_to_pdf <- marrangeGrob(p_collector, nrow = 1, ncol = 1)

ggsave(plots_to_pdf,
       filename = file.path(folder_out, 
                            "top_hacfl_inversions_multi.pdf"),
       width = 8.5, height = 11, units = c("in"))
