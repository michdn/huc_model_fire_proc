# Time series graphs by treatment timing group

# line graphs: for specified HUCs

# Multiple fire metrics
# e.g. HaCBP, HaCFL, expBurn, expFlame, active


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  viridis)

### User ------------------------------------------------------

#output 
plot_folder <- file.path("plots", "timeseries_extreme")
dir.create(plot_folder, recursive = TRUE)


### Results import -------------------------------------------

# can change between region-only and full datacubes. 
# will overwrite output
res_orig <- read_csv(file.path("results",
                               "datacube", 
                               "datacube_interim_SNSCCC_20240617.csv")) %>% 
  mutate(HUC12 = as.character(HUC12)) 

#new set, 3 from each of the top three timing groups for each priority for each region
# uses the qualifying HUCs from original sample plus new ones
#hucs_sample <- read_csv(file.path("qa", "sampledhucs_topthreegroups.csv")) 

hucs_extreme <- read_csv("qajun/extreme_inversion_185.csv") 

### Data set up ------------------------------------------------

# Want priority order: "Fire", "WUI", "Hybrid"
# Want intensity order: "500k", "1m", "2m"

res <- res_orig %>% 
  #remove any baseline
  filter(Priority %in% c("Fire", "WUI", "Hybrid")) %>% 
  #For graphing in the correct order (generic, used in multiple places, with modifications)
  # make factor with set order (priority)
  mutate(Priority = as_factor(Priority),
         Priority = forcats::fct_relevel(Priority,
                                         "Fire", "WUI", "Hybrid"),
         #Make factor with set order (intensity))
         TxIntensity = as_factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                            "500k", "1m", "2m"))


# get the correct timing group for the different priorities
res <- res %>% 
  mutate(timing_group = case_when(
    Priority == "Fire" ~ timeFire, 
    Priority == "WUI" ~ timeWui,
    Priority == "Hybrid" ~ timeHybrid
  )) %>% 
  #and WUI 4th category is too long for graphs
  mutate(timing_group = if_else(timing_group=="2024_2039_yr1to5_16to20",
                                "2024_2039",
                                timing_group))



#Graphing set up

#Show these years on graph
year_breaks <- c(2024, 2029, 2034, 2039)

#number of treatment time steps, for faceting boxplots correctly
trt_cols <- 4


#colors for intensities on line graphs
i_colors <- c("500k" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[2],
              "1m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[3],
              "2m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[4])


#fire types
ft_lines <-  c("surface" = "solid", 
               "passive_crown" = "dashed",
               "active_crown" = "dotted")


### Filter and graph --------------------------

#use hucs_extreme to id scenario-hucs to graph

hucs_extreme

#huc, region, priority, txtype 

hucs_extreme %>% 
  dplyr::select(Region, Priority, TxType, HUC12) %>% 
  distinct() %>% 
  group_by(Region, Priority, TxType) %>% 
  summarize(count = n())


# for every huc-scenario (30)
# order by HUC first, so that hucs with multiple scenario inversions are next to each other
#page: hacfl, expFlame, hacbp, expBurn, active, exppcactive
#  could long, then facet by stat
# title Region, HUC, year of treatment. Sub: Priority, TxType













res_x <- res %>% 
  filter(HUC12 %in% hucs_sample$HUC12)


#priorities 
priorities <- res_x %>% pull(Priority) %>% unique() %>% sort()


#PRIORITY (first inner loop)
for (p in seq_along(priorities)){
  
  this_priority <- priorities[[p]]
  
  res_r_p <- res_x %>% 
    filter(Priority == this_priority)
  
  #get treatments in this region-priority (CAN VARY)
  trts <- res_r_p %>% pull(TxType) %>% unique() %>% sort()
  
  
  #TREATMENT TYPE (second inner loop)
  for (t in seq_along(trts)){
    
    this_trt <- trts[[t]]
    
    #get datasets for this trt
    res_r_p_t_sample <- res_r_p %>% 
      filter(TxType == this_trt)
    
    #plot 11: sampled line, HaCFL, timeseries
    p11 <- ggplot(data = res_r_p_t_sample,
                  mapping = aes(x=Year, y=HaCFL, color=TxIntensity)) +
      geom_point(shape = 1) +
      geom_line() +
      scale_x_continuous(breaks = year_breaks) +
      scale_color_manual("Intensity", values = i_colors) + 
      theme(legend.position = "bottom") + 
      facet_wrap(~paste0(timing_group, "\n", HUC12, " (", Region, ")"),
                 ncol = 3, scales="free_y") +
      labs(title = paste0("Priority: ", this_priority,
                          ", Treatment: ", this_trt,
                          " for Extreme Inversion HUCs"),
           subtitle = "Facets by HUC showing Year of Treatment",
           y = "HaCFL") + 
      theme_bw()
    
    fn11 <- paste0("extremehucs_", this_priority, "_", this_trt, 
                   "_HaCFL.jpg")
    ggsave(plot = p11,
           filename = file.path(plot_folder, fn11),
           width = 7, height = 6, units = "in")
    
    
    #plot 12: sampled line, expFlame, timeseries
    p12 <- ggplot(data = res_r_p_t_sample,
                  mapping = aes(x=Year, y=expFlame, color=TxIntensity)) +
      geom_point(shape = 1) +
      geom_line() +
      scale_x_continuous(breaks = year_breaks) +
      scale_color_manual("Intensity", values = i_colors) + 
      theme(legend.position = "bottom") + 
      facet_wrap(~paste0(timing_group, "\n", HUC12, " (", Region, ")"),
                 ncol = 3, scales="free_y") +
      labs(title = paste0("Priority: ", this_priority,
                          ", Treatment: ", this_trt,
                          " for Extreme Inversion HUCs"),
           subtitle = "Facets by HUC showing Year of Treatment",
           y = "Flame index (expFlame)") + 
      theme_bw()
    
    fn12 <- paste0("extremehucs_", this_priority, "_", this_trt, 
                   "_expFlame.jpg")
    ggsave(plot = p12,
           filename = file.path(plot_folder, fn12),
           width = 7, height = 6, units = "in")
    
    
    #plot 13: sampled line, HaCBP, timeseries
    p13 <- ggplot(data = res_r_p_t_sample,
                  mapping = aes(x=Year, y=HaCBP, color=TxIntensity)) +
      geom_point(shape = 1) +
      geom_line() +
      scale_x_continuous(breaks = year_breaks) +
      scale_color_manual("Intensity", values = i_colors) + 
      theme(legend.position = "bottom") + 
      facet_wrap(~paste0(timing_group, "\n", HUC12, " (", Region, ")"),
                 ncol = 3, scales="free_y") +
      labs(title = paste0("Priority: ", this_priority,
                          ", Treatment: ", this_trt,
                          " for Extreme Inversion HUCs"),
           subtitle = "Facets by HUC showing Year of Treatment",
           y = "HaCBP") + 
      theme_bw()
    
    fn13 <- paste0("extremehucs_", this_priority, "_", this_trt, 
                   "_HaCBP.jpg")
    
    ggsave(plot = p13,
           filename = file.path(plot_folder, fn13),
           width = 7, height = 6, units = "in")
    
    
    #plot 14: sampled line, expBurn, timeseries
    p14 <- ggplot(data = res_r_p_t_sample,
                  mapping = aes(x=Year, y=expBurn, color=TxIntensity)) +
      geom_point(shape = 1) +
      geom_line() +
      scale_x_continuous(breaks = year_breaks) +
      scale_color_manual("Intensity", values = i_colors) + 
      theme(legend.position = "bottom") + 
      facet_wrap(~paste0(timing_group, "\n", HUC12, " (", Region, ")"),
                 ncol = 3, scales="free_y") +
      labs(title = paste0("Priority: ", this_priority,
                          ", Treatment: ", this_trt,
                          " for Extreme Inversion HUCs"),
           subtitle = "Facets by HUC showing Year of Treatment",
           y = "Expected burned acres (expBurn)") + 
      theme_bw()
    
    fn14 <- paste0("extremehucs_", this_priority, "_", this_trt, 
                   "_expBurn.jpg")
    
    ggsave(plot = p14,
           filename = file.path(plot_folder, fn14),
           width = 7, height = 6, units = "in")
    
    #plot 15: sampled line, expPcActive, timeseries
    p15 <- ggplot(data = res_r_p_t_sample,
                  mapping = aes(x=Year, y=expPcActive, color=TxIntensity)) +
      geom_point(shape = 1) +
      geom_line() +
      scale_x_continuous(breaks = year_breaks) +
      scale_color_manual("Intensity", values = i_colors) + 
      theme(legend.position = "bottom") + 
      facet_wrap(~paste0(timing_group, "\n", HUC12, " (", Region, ")"),
                 ncol = 3, scales="free_y") +
      labs(title = paste0("Priority: ", this_priority,
                          ", Treatment: ", this_trt,
                          " for Extreme Inversion HUCs"),
           subtitle = "Facets by HUC showing Year of Treatment",
           y = "Expected % burned by active crown fire (expPcActive)") + 
      theme_bw()
    
    fn15 <- paste0("extremehucs_", this_priority, "_", this_trt, 
                   "_expPcActive.jpg")
    ggsave(plot = p15,
           filename = file.path(plot_folder, fn15),
           width = 7, height = 6, units = "in")
    
    
    #fire type
    this_ft_res <- res_r_p_t_sample %>% 
      dplyr::select(Region, HUC12, timing_group, Year, TxIntensity,
                    surface, passive_crown, active_crown) %>%
      pivot_longer(cols = c(surface, passive_crown, active_crown),
                   names_to = "firetype",
                   values_to = "mean_burned_frac")
    
    p16 <- ggplot() +
      geom_line(data = this_ft_res,
                mapping = aes(x=Year, y=mean_burned_frac, 
                              color=TxIntensity,
                              linetype=firetype)) + 
      scale_x_continuous(breaks = year_breaks) +
      scale_color_manual("Intensity", values = i_colors) + 
      scale_linetype_manual("Fire Type", values = ft_lines) + 
      facet_wrap(~paste0(timing_group, "\n", HUC12, " (", Region, ")"),
                 ncol = 3, scales="free_y") + 
      labs(title = paste0("Priority: ", this_priority,
                          ", Treatment: ", this_trt,
                          " for Selected HUCs"),
           subtitle = "Facets by HUC showing Year of Treatment",
           y = "Burn fraction by fire type") + 
      theme_bw()
    
    fn16 <- paste0("extremehucs_", this_priority, "_", this_trt, 
                   "_firetype.jpg")
    
    ggsave(plot = p16,
           filename = file.path(plot_folder, fn16),
           width = 7, height = 6, units = "in")
    
    
  }
}


