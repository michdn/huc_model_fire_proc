# gf, bp, and fuel adj scatter plots

# BASELINE MINUS 2M
# negative when inversion (positive when no inversion)

# SWITCHING to 500k - 2m
#  to avoid changing too much code, 500k will be 'baseline' terminology in code

# Select relative years to treatment in user choice 'comparison' below

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  gridExtra)

### USER ----------------------------------------------------

#"PRE" Before treatment year
#"POST" After treatment year (not including treatment year)
#"AT" Year of treatment
comparison <- "POST" 

### Data -----------------------------------------------------

#NON BURN
nb_hucs <- readRDS("data/nonburnable_rerun_list.RDS") 


# GRIDFIRE

gf_raw <- read_csv(file.path("results",
                         "absolute", 
                         "SN_SNbl_SNbw_absolute_20240423.csv")) %>% 
  mutate(HUC12 = as.character(HUC12)) 

#filter out nonburn issue HUCs
gf <- gf_raw %>% 
  filter(!HUC12 %in% nb_hucs$huc12)

# BEHAVEPLUS

bp <- read_csv(file.path("qa", "behaveplus",
                         "SN_FBFM_zonal_counts_BP_namesrepaired.csv")) %>%
  mutate(HUC12 = as.character(HUC12),
         Priority = if_else(Priority == "RFFC", "Hybrid", Priority)) 


# FUEL ADJ ROS & FL


adj_fl <- readRDS(file.path("qa", 
                            "SN_fuel_adjective_comparison_baseline.RDS")) 

adj_ros <-  readRDS(file.path("qa", 
                              "SN_fuel_ROS_adjective_comparison_baseline.RDS")) 


# OTHER

#new set, 3 from each of the top three timing groups for each priority for each region
# uses the qualifying HUCs from original sample plus new ones
#hucs_sample_new <- read_csv(file.path('qa', 'sampledhucs_topthreegroups.csv')) 


### treat yr ---------------------

#using already calc from adj
trt_yrs <- adj_fl %>% 
  filter(!Priority=="baseline") %>% 
  dplyr::select(HUC12, Priority, trt_yr) %>% 
  distinct()

### Split baseline and 2m & calculate differences -----------------

# GRIDFIRE

gf_bl <- gf %>% 
  filter(TxIntensity == "500k") %>% 
  dplyr::select(HUC12, Priority, TxType, Year, 
                HaCFL, HaCBP, surface, passive_crown, active_crown) %>% 
  rename(hacfl_base = HaCFL,
         hacbp_base = HaCBP,
         surface_base = surface,
         passive_crown_base = passive_crown,
         active_crown_base = active_crown)

gf_b2 <- gf %>% 
  #get treat yr (could calc but since have, just join)
  left_join(trt_yrs, by = join_by(HUC12, Priority)) %>% 
  #only 2m 
  filter(TxIntensity == "2m") %>% 
  #join with bl for single row
  left_join(gf_bl, by = join_by(HUC12, Priority, TxType, Year)) %>% 
  #calculate differences (base - 2m)
  mutate(hacfl_pc = (hacfl_base - HaCFL) / hacfl_base,
         hacbp_pc = (hacbp_base - HaCBP) / hacbp_base,
         hacflhacbp_base = hacfl_base/hacbp_base,
         hacflhacbp = HaCFL/HaCBP,
         hacflhacbp_pc = (hacflhacbp_base - hacflhacbp) / hacflhacbp_base,
         surface_pc = (surface_base - surface) / surface_base,
         active_pc = (active_crown_base - active_crown) / active_crown_base,
         passive_pc = (passive_crown_base - passive_crown) / passive_crown_base) %>% 
  # select only wanted for convenience
  dplyr::select(HUC12, Priority, TxIntensity, TxType, Year, trt_yr, 
                hacfl_pc, hacbp_pc, hacflhacbp_pc,
                surface_pc, active_pc, passive_pc)
#hacfl_base, hacbp_base, surface_base, active_crown_base, passive_crown_base,
#HaCFL, HaCBP, surface, passive_crown, active_crown) 


# BEHAVEPLUS

bp_bl <- bp %>% 
  filter(TxIntensity == "500k") %>% 
  dplyr::select(HUC12, Priority, TxType, Year,  
                wtd_avg_FL, wtd_avg_ROS, wtd_avg_type) %>% 
  rename(bpfl_bl = wtd_avg_FL,
         bpros_bl = wtd_avg_ROS,
         bptype_bl = wtd_avg_type)


bp_b2 <- bp %>% 
  #get treat yr 
  left_join(trt_yrs, by = join_by(HUC12, Priority)) %>% 
  #only 2m (no baselines)
  filter(TxIntensity == "2m") %>% 
  #join with bl for single row
  left_join(bp_bl, by = join_by(HUC12, Priority, TxType, Year)) %>% 
  #calculate differences (base - 2m)
  mutate(bp_fl_pc = (bpfl_bl - wtd_avg_FL) / bpfl_bl,
         bp_ros_pc = (bpros_bl - wtd_avg_ROS) / bpros_bl,
         bp_type_pc = (bptype_bl - wtd_avg_type) / bptype_bl) %>% 
  #select only wanted for convenience
  dplyr::select(HUC12, Priority, TxIntensity, TxType, Year, trt_yr, 
                bp_fl_pc, bp_ros_pc, bp_type_pc)
#bpfl_bl, bpros_bl, bptype_bl,
#wtd_avg_FL, wtd_avg_ROS, wtd_avg_type)

# FUEL ADJ ROS and FL
# Already have baseline info in rows - but uses baseline baseline. 
# need to switch to using 500k 

adj_fl_500k <- adj_fl %>% 
  filter(TxIntensity == "500k") %>% 
  dplyr::select(HUC12, Priority, TxType, Year, mean) %>% 
  rename(mean_500k = mean)

adj_fl_b2 <- adj_fl %>% 
  filter(TxIntensity == "2m") %>% 
  left_join(adj_fl_500k, by = join_by(HUC12, Priority, TxType, Year)) %>% 
  mutate(adj_fl_pc = (mean_500k - mean) / mean_500k) %>% 
  dplyr::select(HUC12, Priority, TxIntensity, TxType, Year, trt_yr, 
                adj_fl_pc) # ,mean_bl, mean) 


adj_ros_500k <- adj_ros %>% 
  filter(TxIntensity == "500k") %>% 
  dplyr::select(HUC12, Priority, TxType, Year, mean) %>% 
  rename(mean_500k = mean)

adj_ros_b2 <- adj_ros %>% 
  filter(TxIntensity == "2m") %>% 
  left_join(adj_ros_500k, by = join_by(HUC12, Priority, TxType, Year)) %>% 
  mutate(adj_ros_pc = (mean_500k - mean) / mean_500k) %>% 
  dplyr::select(HUC12, Priority, TxIntensity, TxType, Year, trt_yr, 
                adj_ros_pc) # ,mean_bl, mean)


### ALL JOIN

b2 <- gf_b2 %>% 
  left_join(bp_b2, 
            by = join_by(HUC12, Priority, TxIntensity, TxType, Year, trt_yr)) %>% 
  left_join(adj_fl_b2, 
            by = join_by(HUC12, Priority, TxIntensity, TxType, Year, trt_yr)) %>% 
  left_join(adj_ros_b2, 
            by = join_by(HUC12, Priority, TxIntensity, TxType, Year, trt_yr)) %>% 
  #not really needed, just for consistency
  mutate(Priority = as.factor(Priority),
         Priority = forcats::fct_relevel(Priority,
                                         "Fire", "WUI", "Hybrid"))


# Filter years based on user choice
if (comparison == "PRE"){
  
  b2 <- b2 %>% 
    #filter to before treatment year. 
    filter(Year < trt_yr)
  
} else if (comparison == "AT"){
  
  b2 <- b2 %>% 
    #filter to treatment year. 
    filter(Year == trt_yr)
  
} else if (comparison == "POST"){
  
  b2 <- b2 %>% 
    #filter to after treatment year. 
    filter(Year > trt_yr)
  
} else {stop("Unknown time comparison")}


### Loop and graph -------------------------------------------------

#output 
plot_folder <- file.path('plots', 'scatter_gfbpadj', 'SN')
#sampled_folder_v2 <- file.path(plot_folder, 'sampledhucs_v2')

dir.create(plot_folder, recursive = TRUE)


#priorities in this region (all same)
priorities <- b2 %>% pull(Priority) %>% unique() %>% sort()

p_theme <- theme_bw() + 
  theme(aspect.ratio = 1) + 
  theme(plot.title = element_text(size=10),
        axis.title = element_text(size=9),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        legend.margin=margin(c(0,0,0,0)))


#PRIORITY 
for (p in seq_along(priorities)){
  
  this_priority <- priorities[[p]]
  
  b2_p <- b2 %>% 
    filter(Priority == this_priority)
  
  #gf fl vs bp fl
  t1a_prep <- b2_p %>% 
    mutate(x=hacfl_pc,
           y=bp_fl_pc,
           x_neg = if_else(x < 0, 1, 0),
           y_neg = if_else(y < 0, 1, 0)) %>% 
    group_by(TxType, x_neg, y_neg) %>% 
    summarize(.groups="drop",
              count=n(),
              max_x=max(x),
              min_x=min(x),
              max_y=max(y),
              min_y=min(y)) %>% 
    mutate(max_x=max(max_x),
           min_x=min(min_x),
           max_y=max(max_y),
           min_y=min(min_y))

  t_r <- t1a_prep %>% filter(x_neg==0, y_neg==0)
  b_r <- t1a_prep %>% filter(x_neg==0, y_neg==1)
  b_l <- t1a_prep %>% filter(x_neg==1, y_neg==1)
  t_l <- t1a_prep %>% filter(x_neg==1, y_neg==0)
  
  p1a <- ggplot() + 
    geom_point(data=b2_p,
               mapping=aes(x=hacfl_pc,
                           y=bp_fl_pc,
                           color=TxType),
               shape=1) + 
    geom_hline(yintercept=0) + 
    geom_vline(xintercept=0) + 
    scale_color_brewer(palette = "Dark2") + 
    labs(title="Gridfire HaCFL vs BehavePlus FL", 
         x="GF HaCFL 500k-2m %change",
         y="BP FL 500k-2m %change") +
    geom_label(data=t_r,
              mapping=aes(label=count, x=max_x, y=max_y), 
              size=3, hjust="inward", alpha=0.5) +
    geom_label(data=b_r,
               mapping=aes(label=count, x=max_x, y=min_y), 
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=b_l,
               mapping=aes(label=count, x=min_x, y=min_y), 
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=t_l,
               mapping=aes(label=count, x=min_x, y=max_y), 
               size=3, hjust="inward", alpha=0.5) +
    facet_wrap(~TxType) + 
    p_theme
  
  #gf hacfl/hacbp vs bp fl
  t1a2_prep <- b2_p %>% 
    mutate(x=hacflhacbp_pc,
           y=bp_fl_pc,
           x_neg = if_else(x < 0, 1, 0),
           y_neg = if_else(y < 0, 1, 0)) %>% 
    group_by(TxType, x_neg, y_neg) %>% 
    summarize(.groups="drop",
              count=n(),
              max_x=max(x),
              min_x=min(x),
              max_y=max(y),
              min_y=min(y)) %>% 
    mutate(max_x=max(max_x),
           min_x=min(min_x),
           max_y=max(max_y),
           min_y=min(min_y))
  
  t_r <- t1a2_prep %>% filter(x_neg==0, y_neg==0)
  b_r <- t1a2_prep %>% filter(x_neg==0, y_neg==1)
  b_l <- t1a2_prep %>% filter(x_neg==1, y_neg==1)
  t_l <- t1a2_prep %>% filter(x_neg==1, y_neg==0)
  
  p1a2 <- ggplot() + 
    geom_point(data=b2_p,
               mapping=aes(x=hacflhacbp_pc,
                           y=bp_fl_pc,
                           color=TxType),
               shape=1) + 
    geom_hline(yintercept=0) + 
    geom_vline(xintercept=0) + 
    scale_color_brewer(palette = "Dark2") + 
    labs(title="Gridfire HaCFL/HaCBP vs BehavePlus FL", 
         x="GF HaCFL/HaCBP 500k-2m %change",
         y="BP FL 500k-2m %change") +
    geom_label(data=t_r,
               mapping=aes(label=count, x=max_x, y=max_y), 
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=b_r,
               mapping=aes(label=count, x=max_x, y=min_y), 
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=b_l,
               mapping=aes(label=count, x=min_x, y=min_y), 
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=t_l,
               mapping=aes(label=count, x=min_x, y=max_y), 
               size=3, hjust="inward", alpha=0.5) +
    facet_wrap(~TxType) + 
    p_theme
  
  
  #gf fl vs adj fl 
  t2a_prep <- b2_p %>% 
    mutate(x=hacfl_pc,
           y=adj_fl_pc,
           x_neg = if_else(x < 0, 1, 0),
           y_neg = if_else(y < 0, 1, 0)) %>% 
    group_by(TxType, x_neg, y_neg) %>% 
    summarize(.groups="drop",
              count=n(),
              max_x=max(x),
              min_x=min(x),
              max_y=max(y),
              min_y=min(y)) %>% 
    mutate(max_x=max(max_x),
           min_x=min(min_x),
           max_y=max(max_y),
           min_y=min(min_y))
  
  t_r <- t2a_prep %>% filter(x_neg==0, y_neg==0)
  b_r <- t2a_prep %>% filter(x_neg==0, y_neg==1)
  b_l <- t2a_prep %>% filter(x_neg==1, y_neg==1)
  t_l <- t2a_prep %>% filter(x_neg==1, y_neg==0)
  
  p2a <- ggplot() + 
    geom_point(data=b2_p,
               mapping=aes(x=hacfl_pc,
                           y=adj_fl_pc,
                           color=TxType),
               shape=1) + 
    geom_hline(yintercept=0) + 
    geom_vline(xintercept=0) + 
    scale_color_brewer(palette = "Dark2") + 
    labs(title="Gridfire HaCFL vs Fuel Adjective FL", 
         x="GF HaCFL 500k-2m %change",
         y="Fuel Adj FL 500k-2m %change") +
    geom_label(data=t_r,
               mapping=aes(label=count, x=max_x, y=max_y),              
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=b_r,
               mapping=aes(label=count, x=max_x, y=min_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=b_l,
               mapping=aes(label=count, x=min_x, y=min_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=t_l,
               mapping=aes(label=count, x=min_x, y=max_y),
               size=3, hjust="inward", alpha=0.5) +
    facet_wrap(~TxType) +
    p_theme
  
  #gf hacfl/hacbp vs adj fl 
  t2a2_prep <- b2_p %>% 
    mutate(x=hacflhacbp_pc,
           y=adj_fl_pc,
           x_neg = if_else(x < 0, 1, 0),
           y_neg = if_else(y < 0, 1, 0)) %>% 
    group_by(TxType, x_neg, y_neg) %>% 
    summarize(.groups="drop",
              count=n(),
              max_x=max(x),
              min_x=min(x),
              max_y=max(y),
              min_y=min(y)) %>% 
    mutate(max_x=max(max_x),
           min_x=min(min_x),
           max_y=max(max_y),
           min_y=min(min_y))
  
  t_r <- t2a2_prep %>% filter(x_neg==0, y_neg==0)
  b_r <- t2a2_prep %>% filter(x_neg==0, y_neg==1)
  b_l <- t2a2_prep %>% filter(x_neg==1, y_neg==1)
  t_l <- t2a2_prep %>% filter(x_neg==1, y_neg==0)
  
  p2a2 <- ggplot() + 
    geom_point(data=b2_p,
               mapping=aes(x=hacflhacbp_pc,
                           y=adj_fl_pc,
                           color=TxType),
               shape=1) + 
    geom_hline(yintercept=0) + 
    geom_vline(xintercept=0) + 
    scale_color_brewer(palette = "Dark2") + 
    labs(title="Gridfire HaCFL/HaCBP vs Fuel Adjective FL", 
         x="GF HaCFL/HaCBP 500k-2m %change",
         y="Fuel Adj FL 500k-2m %change") +
    geom_label(data=t_r,
               mapping=aes(label=count, x=max_x, y=max_y),              
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=b_r,
               mapping=aes(label=count, x=max_x, y=min_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=b_l,
               mapping=aes(label=count, x=min_x, y=min_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=t_l,
               mapping=aes(label=count, x=min_x, y=max_y),
               size=3, hjust="inward", alpha=0.5) +
    facet_wrap(~TxType) +
    p_theme
  
  
  #bp fl vs adj fl
  t3a_prep <- b2_p %>% 
    mutate(x=bp_fl_pc,
           y=adj_fl_pc,
           x_neg = if_else(x < 0, 1, 0),
           y_neg = if_else(y < 0, 1, 0)) %>% 
    group_by(TxType, x_neg, y_neg) %>% 
    summarize(.groups="drop",
              count=n(),
              max_x=max(x),
              min_x=min(x),
              max_y=max(y),
              min_y=min(y)) %>% 
    mutate(max_x=max(max_x),
           min_x=min(min_x),
           max_y=max(max_y),
           min_y=min(min_y))
  
  t_r <- t3a_prep %>% filter(x_neg==0, y_neg==0)
  b_r <- t3a_prep %>% filter(x_neg==0, y_neg==1)
  b_l <- t3a_prep %>% filter(x_neg==1, y_neg==1)
  t_l <- t3a_prep %>% filter(x_neg==1, y_neg==0)

  p3a <- ggplot() + 
    geom_point(data=b2_p,
               mapping=aes(x=bp_fl_pc,
                           y=adj_fl_pc,
                           color=TxType),
               shape=1) + 
    geom_hline(yintercept=0) + 
    geom_vline(xintercept=0) + 
    scale_color_brewer(palette = "Dark2") + 
    labs(title="BehavePlus FL vs Fuel Adjective FL", 
         x="BP FL 500k-2m %change",
         y="Fuel Adj FL 500k-2m %change") +
    geom_label(data=t_r,
               mapping=aes(label=count, x=max_x, y=max_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=b_r,
               mapping=aes(label=count, x=max_x, y=min_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=b_l,
               mapping=aes(label=count, x=min_x, y=min_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=t_l,
               mapping=aes(label=count, x=min_x, y=max_y),
               size=3, hjust="inward", alpha=0.5) +
    facet_wrap(~TxType) + 
    p_theme
  
  #gf hacfl vs fuel adj ROS
  t4a_prep <- b2_p %>% 
    mutate(x=hacfl_pc,
           y=adj_ros_pc,
           x_neg = if_else(x < 0, 1, 0),
           y_neg = if_else(y < 0, 1, 0)) %>% 
    group_by(TxType, x_neg, y_neg) %>% 
    summarize(.groups="drop",
              count=n(),
              max_x=max(x),
              min_x=min(x),
              max_y=max(y),
              min_y=min(y)) %>% 
    mutate(max_x=max(max_x),
           min_x=min(min_x),
           max_y=max(max_y),
           min_y=min(min_y))
  
  t_r <- t4a_prep %>% filter(x_neg==0, y_neg==0)
  b_r <- t4a_prep %>% filter(x_neg==0, y_neg==1)
  b_l <- t4a_prep %>% filter(x_neg==1, y_neg==1)
  t_l <- t4a_prep %>% filter(x_neg==1, y_neg==0)
  
  p4a <- ggplot() + 
    geom_point(data=b2_p,
               mapping=aes(x=hacfl_pc,
                           y=adj_ros_pc,
                           color=TxType),
               shape=1) + 
    geom_hline(yintercept=0) + 
    geom_vline(xintercept=0) + 
    scale_color_brewer(palette = "Dark2") + 
    labs(title="GF HaCFL vs Fuel Adjective ROS", 
         x="GF HaCFL 500k-2m %change",
         y="Fuel Adj ROS 500k-2m %change") +
    geom_label(data=t_r,
               mapping=aes(label=count, x=max_x, y=max_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=b_r,
               mapping=aes(label=count, x=max_x, y=min_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=b_l,
               mapping=aes(label=count, x=min_x, y=min_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=t_l,
               mapping=aes(label=count, x=min_x, y=max_y),
               size=3, hjust="inward", alpha=0.5) +
    facet_wrap(~TxType) + 
    p_theme
  
  
  #gf hacbp vs bp ros
  t1b_prep <- b2_p %>% 
    mutate(x=hacbp_pc,
           y=bp_ros_pc,
           x_neg = if_else(x < 0, 1, 0),
           y_neg = if_else(y < 0, 1, 0)) %>% 
    group_by(TxType, x_neg, y_neg) %>% 
    summarize(.groups="drop",
              count=n(),
              max_x=max(x),
              min_x=min(x),
              max_y=max(y),
              min_y=min(y)) %>% 
    mutate(max_x=max(max_x),
           min_x=min(min_x),
           max_y=max(max_y),
           min_y=min(min_y))
  
  t_r <- t1b_prep %>% filter(x_neg==0, y_neg==0)
  b_r <- t1b_prep %>% filter(x_neg==0, y_neg==1)
  b_l <- t1b_prep %>% filter(x_neg==1, y_neg==1)
  t_l <- t1b_prep %>% filter(x_neg==1, y_neg==0)
  
  p1b <- ggplot() + 
    geom_point(data=b2_p,
               mapping=aes(x=hacbp_pc,
                           y=bp_ros_pc,
                           color=TxType),
               shape=1) + 
    geom_hline(yintercept=0) + 
    geom_vline(xintercept=0) + 
    scale_color_brewer(palette = "Dark2") + 
    labs(title="Gridfire HaCBP vs BehavePlus ROS", 
         x="GF HaCBP 500k-2m %change",
         y="BP ROS 500k-2m %change") +
    geom_label(data=t_r,
               mapping=aes(label=count, x=max_x, y=max_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=b_r,
               mapping=aes(label=count, x=max_x, y=min_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=b_l,
               mapping=aes(label=count, x=min_x, y=min_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=t_l,
               mapping=aes(label=count, x=min_x, y=max_y),
               size=3, hjust="inward", alpha=0.5) +
    facet_wrap(~TxType) + 
    p_theme
  
  
  #gf hacbp vs adj ros
  t2b_prep <- b2_p %>% 
    mutate(x=hacbp_pc,
           y=bp_ros_pc,
           x_neg = if_else(x < 0, 1, 0),
           y_neg = if_else(y < 0, 1, 0)) %>% 
    group_by(TxType, x_neg, y_neg) %>% 
    summarize(.groups="drop",
              count=n(),
              max_x=max(x),
              min_x=min(x),
              max_y=max(y),
              min_y=min(y)) %>% 
    mutate(max_x=max(max_x),
           min_x=min(min_x),
           max_y=max(max_y),
           min_y=min(min_y))
  
  t_r <- t2b_prep %>% filter(x_neg==0, y_neg==0)
  b_r <- t2b_prep %>% filter(x_neg==0, y_neg==1)
  b_l <- t2b_prep %>% filter(x_neg==1, y_neg==1)
  t_l <- t2b_prep %>% filter(x_neg==1, y_neg==0)
  
  p2b <- ggplot() + 
    geom_point(data=b2_p,
               mapping=aes(x=hacbp_pc,
                           y=adj_ros_pc,
                           color=TxType),
               shape=1) + 
    geom_hline(yintercept=0) + 
    geom_vline(xintercept=0) + 
    scale_color_brewer(palette = "Dark2") + 
    labs(title="Gridfire HaCBP vs Fuel Adjective ROS", 
         x="GF HaCBP 500k-2m %change",
         y="Fuel Adj ROS 500k-2m %change") +
    geom_label(data=t_r,
               mapping=aes(label=count, x=max_x, y=max_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=b_r,
               mapping=aes(label=count, x=max_x, y=min_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=b_l,
               mapping=aes(label=count, x=min_x, y=min_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=t_l,
               mapping=aes(label=count, x=min_x, y=max_y),
               size=3, hjust="inward", alpha=0.5) +
    facet_wrap(~TxType) + 
    p_theme
  
  
  #bp ros vs adj ros
  t3b_prep <- b2_p %>% 
    mutate(x=bp_ros_pc,
           y=adj_ros_pc,
           x_neg = if_else(x < 0, 1, 0),
           y_neg = if_else(y < 0, 1, 0)) %>% 
    group_by(TxType, x_neg, y_neg) %>% 
    summarize(.groups="drop",
              count=n(),
              max_x=max(x),
              min_x=min(x),
              max_y=max(y),
              min_y=min(y)) %>% 
    mutate(max_x=max(max_x),
           min_x=min(min_x),
           max_y=max(max_y),
           min_y=min(min_y))
  
  t_r <- t3b_prep %>% filter(x_neg==0, y_neg==0)
  b_r <- t3b_prep %>% filter(x_neg==0, y_neg==1)
  b_l <- t3b_prep %>% filter(x_neg==1, y_neg==1)
  t_l <- t3b_prep %>% filter(x_neg==1, y_neg==0)
  
  p3b <- ggplot() + 
    geom_point(data=b2_p,
               mapping=aes(x=bp_ros_pc,
                           y=adj_ros_pc,
                           color=TxType),
               shape=1) + 
    geom_hline(yintercept=0) + 
    geom_vline(xintercept=0) + 
    scale_color_brewer(palette = "Dark2") + 
    labs(title="BehavePlus ROS vs Fuel Adjective ROS",  
         x="BP ROS 500k-2m %change",
         y="Fuel Adj ROS 500k-2m %change") +
    geom_label(data=t_r,
               mapping=aes(label=count, x=max_x, y=max_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=b_r,
               mapping=aes(label=count, x=max_x, y=min_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=b_l,
               mapping=aes(label=count, x=min_x, y=min_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=t_l,
               mapping=aes(label=count, x=min_x, y=max_y),
               size=3, hjust="inward", alpha=0.5) +
    facet_wrap(~TxType) + 
    p_theme
  
  #bp fl bs adj ros
  t4b_prep <- b2_p %>% 
    mutate(x=bp_fl_pc,
           y=adj_ros_pc,
           x_neg = if_else(x < 0, 1, 0),
           y_neg = if_else(y < 0, 1, 0)) %>% 
    group_by(TxType, x_neg, y_neg) %>% 
    summarize(.groups="drop",
              count=n(),
              max_x=max(x),
              min_x=min(x),
              max_y=max(y),
              min_y=min(y)) %>% 
    mutate(max_x=max(max_x),
           min_x=min(min_x),
           max_y=max(max_y),
           min_y=min(min_y))
  
  t_r <- t4b_prep %>% filter(x_neg==0, y_neg==0)
  b_r <- t4b_prep %>% filter(x_neg==0, y_neg==1)
  b_l <- t4b_prep %>% filter(x_neg==1, y_neg==1)
  t_l <- t4b_prep %>% filter(x_neg==1, y_neg==0)
  
  p4b <- ggplot() + 
    geom_point(data=b2_p,
               mapping=aes(x=bp_fl_pc,
                           y=adj_ros_pc,
                           color=TxType),
               shape=1) + 
    geom_hline(yintercept=0) + 
    geom_vline(xintercept=0) + 
    scale_color_brewer(palette = "Dark2") + 
    labs(title="BehavePlus FL vs Fuel Adjective ROS", 
         x="BP FL 500k-2m %change",
         y="Fuel Adj ROS 500k-2m %change") +
    geom_label(data=t_r,
               mapping=aes(label=count, x=max_x, y=max_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=b_r,
               mapping=aes(label=count, x=max_x, y=min_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=b_l,
               mapping=aes(label=count, x=min_x, y=min_y),
               size=3, hjust="inward", alpha=0.5) +
    geom_label(data=t_l,
               mapping=aes(label=count, x=min_x, y=max_y),
               size=3, hjust="inward", alpha=0.5) +
    facet_wrap(~TxType) + 
    p_theme
  
  
  #all title
  if (comparison == "PRE"){
    
    all_title <- paste0("Scatter plots BEFORE Treatment: Percent Change (fraction) 2m to 500k (500k-2m): ", this_priority)
    fn_p <- paste0("scatters_PREONLY_gf_bp_fa_", this_priority, "_500k2m.jpg")
    
    
  } else if (comparison == "AT"){
    
    all_title <- paste0("Scatter plots AT Treatment: Percent Change (fraction) 2m to 500k (500k-2m): ", this_priority)
    fn_p <- paste0("scatters_ATTX_gf_bp_fa_", this_priority, "_500k2m.jpg")
    
    
  } else if (comparison == "POST"){
    
    all_title <- paste0("Scatter plots AFTER Treatment: Percent Change (fraction) 2m to 500k (500k-2m): ", this_priority)
    fn_p <- paste0("scatters_POSTONLY_gf_bp_fa_", this_priority, "_500k2m.jpg")
    
  } else {stop("Unknown time comparison")}
  
  
  #collect all
  p_priority <- arrangeGrob(p1a, p1a2, 
                            p2a, p2a2, 
                            p1b, p2b,
                            p3a, p4a,
                            #dropping p3b, p4b to make room for a2
                            ncol = 2, top = all_title)
  
  ggsave(plot=p_priority,
         filename=file.path(plot_folder, fn_p),
         width=13, height=11, units = "in", dpi=200)
  
}


##### TROUBLESHOOTING ---------------------------------------------

# p <- 1
# 
# 
# this_priority <- priorities[[p]]
# 
# b2_p <- b2 %>%
#   filter(Priority == this_priority)
# 
# #gf fl vs bp fl
# t1a_prep <- b2_p %>%
#   mutate(x=hacfl_pc,
#          y=bp_fl_pc,
#          x_neg = if_else(x < 0, 1, 0),
#          y_neg = if_else(y < 0, 1, 0)) %>%
#   group_by(TxType, x_neg, y_neg) %>%
#   summarize(.groups="drop",
#             count=n(),
#             max_x=max(x),
#             min_x=min(x),
#             max_y=max(y),
#             min_y=min(y)) %>%
#   mutate(max_x=max(max_x),
#          min_x=min(min_x),
#          max_y=max(max_y),
#          min_y=min(min_y))
# 
# t_r <- t1a_prep %>% filter(x_neg==0, y_neg==0)
# b_r <- t1a_prep %>% filter(x_neg==0, y_neg==1)
# b_l <- t1a_prep %>% filter(x_neg==1, y_neg==1)
# t_l <- t1a_prep %>% filter(x_neg==1, y_neg==0)
# 
# p1a <- ggplot() +
#   geom_point(data=b2_p,
#              mapping=aes(x=hacfl_pc,
#                          y=bp_fl_pc,
#                          color=TxType),
#              shape=1) +
#   geom_hline(yintercept=0) +
#   geom_vline(xintercept=0) +
#   scale_color_brewer(palette = "Dark2") +
#   labs(title="Gridfire HaCFL vs BehavePlus FL",
#        x="GF HaCFL 500k-2m %change",
#        y="BP FL 500k-2m %change") +
#   geom_label(data=t_r,
#              mapping=aes(label=count, x=max_x, y=max_y),
#              size=3, hjust="inward", alpha=0.5) +
#   geom_label(data=b_r,
#              mapping=aes(label=count, x=max_x, y=min_y),
#              size=3, hjust="inward", alpha=0.5) +
#   geom_label(data=b_l,
#              mapping=aes(label=count, x=min_x, y=min_y),
#              size=3, hjust="inward", alpha=0.5) +
#   geom_label(data=t_l,
#              mapping=aes(label=count, x=min_x, y=max_y),
#              size=3, hjust="inward", alpha=0.5) +
#   facet_wrap(~TxType) +
#   p_theme
# 
# p1a
# 
# b2_p %>% filter(abs(hacfl_pc) > 0.1) %>%
#   left_join(trt_yrs) %>%
#   group_by(HUC12, trt_yr) %>%
#   summarize(count=n()) %>%
#   View()
# 
# b2_p %>% filter(abs(hacfl_pc) > 0.1) %>%
#   left_join(trt_yrs) %>%
#   group_by(HUC12, Year, trt_yr) %>%
#   summarize(count=n()) %>%
#   View()
# 
# b2_e <- b2_p %>% filter(abs(hacfl_pc) > 0.1)
# 
# #write_csv(b2_e, "qa/scatter_noisyHUCs_10_Fire.csv")
# saveRDS(b2_e, "qa/scatter_noisyHUCs_10_Fire.RDS")
# 
# gf_e <- gf %>%
#   filter(HUC12 %in% b2_e$HUC12)
# 
# gf_e1 <- gf_e %>%
#   filter(HUC12 == "180200030803") %>%
#   filter(Priority == "Fire" | Priority == "baseline")
# 
# gf_e1 %>% 
#   dplyr::select(HUC12, TxIntensity, TxType, Year, timeFire, HaCFL) %>% 
#   View()
# 
# gf_e1 %>% 
#   filter(!Priority == "baseline") %>% 
#   dplyr::select(HUC12, TxIntensity, TxType, Year, timeFire, HaCFL) %>% 
#   left_join(gf_e1 %>% 
#               filter(Priority=="baseline") %>% 
#               dplyr::select(HUC12, Year, HaCFL) %>% 
#               rename(hacfl_base = HaCFL),
#               by = join_by(HUC12, Year)) %>% 
#   mutate(hacfl_diff = HaCFL - hacfl_base) %>% 
#   View()


# ## noisy across any priority, top 9 per priority 
# #for graphing in other places as 'sampled hucs'
# 
# top9 <- b2 %>% 
#   filter(abs(hacfl_pc) > 0.1) %>% 
#   arrange(Priority, desc(abs(hacfl_pc))) %>% 
#   # grabbing the 9 worst HUCs in each Priority
#   # first grab worst of each HUC-Priority
#   group_by(HUC12, Priority) %>% 
#   slice_max(order_by=abs(hacfl_pc), n=1, with_ties=FALSE) %>% 
#   #arrange(Priority, desc(abs(hacfl_pc)))
#   #then grab top 9 in each Priority
#   ungroup() %>% 
#   group_by(Priority) %>% 
#   slice_max(order_by=abs(hacfl_pc), n=9, with_ties=FALSE)
# 
# saveRDS(top9, "qa/scatter_noisyHUCs_top9priority.RDS")  
# write_csv(top9, "qa/scatter_noisyHUCs_top9priority.csv")  

