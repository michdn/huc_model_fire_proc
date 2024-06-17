# Time series graphs by treatment timing group

# Creates: 
# boxplots: for all HUCs
# line graphs: for sampled HUCs

# Multiple fire metrics
# e.g. HaCBP, HaCFL, expBurn, expFlame, active
# Will run for any/all regions, scenarios (loops)

# baselines, optional, set with user flag:  
# baselines masquerade as a treatment intensity inside every priority/trt


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  viridis)

### User settings ---------------------------------------------

# For the boxplots, free or fixed y axis? 
box_scales <- "free_y" #"fixed" #free_y

# Flag for if baseline should be included
use_baseline <- FALSE

# Flags for which graphs to make
graph_boxplots <- TRUE
graph_sample <- TRUE


### Results import -------------------------------------------

# can change between region-only and full datacubes. 
# will overwrite output
res_orig <- read_csv(file.path("results",
                               "datacube", 
                               "datacube_interim_SNSCCC_20240617.csv")) %>% 
  mutate(HUC12 = as.character(HUC12)) 

#new set, 3 from each of the top three timing groups for each priority for each region
# uses the qualifying HUCs from original sample plus new ones
hucs_sample <- read_csv(file.path("qa", "sampledhucs_topthreegroups.csv")) 


### Data set up ------------------------------------------------

# Want priority order: "Fire", "WUI", "Hybrid"
# Want intensity order: "500k", "1m", "2m"

if (use_baseline){
  
  res <- res_orig %>% 
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
  
  
  #baseline/baseweather to be treated as an "intensity" level PER ALL priorities, trts
  res_bases <- res %>% 
    filter(TxIntensity %in% c("base", "bw")) %>% 
    #remove priorities, trts from bases
    dplyr::select(-Priority, -TxType)
  
  #remove bases from rest of results (temporarily)
  res <- res %>% 
    filter(!TxIntensity %in% c("base", "bw"))
  
  #get all combination of Priority and TxTypes
  frame <- res %>% 
    dplyr::select(Priority, TxType) %>% 
    distinct()
  
  #duplicate bases values for all priority-txtype combos
  res_bases <- frame %>% 
    cross_join(res_bases) 
  
  #add back in
  res <- bind_rows(res, res_bases)
  
} else {
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
}


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


if (use_baseline){
  
  #colors for intensities on line graphs
  i_colors <- c("bw" = "grey50",
                "base" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[1],
                "500k" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[2],
                "1m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[3],
                "2m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[4])
  
} else {
  #colors for intensities on line graphs
  i_colors <- c("500k" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[2],
                "1m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[3],
                "2m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[4])
  
}

#fire types
ft_lines <-  c("surface" = "solid", 
               "passive_crown" = "dashed",
               "active_crown" = "dotted")


### Looping version -----------------------------------------------

regions <- res %>% pull(Region) %>% unique() %>% sort()

#REGION (outer loop)
for (r in seq_along(regions)){
  
  this_reg <- regions[[r]]
  
  this_reg_label <- case_when(
    this_reg == "CC" ~ "Central Coast",
    this_reg == "NC" ~ "North Coast",
    this_reg == "SC" ~ "South Coast",
    this_reg == "SN" ~ "Sierra Nevada",
  )
  
  res_r <- res %>% 
    filter(Region == this_reg)
  
  
  #output 
  plot_folder <- file.path("plots", "timeseries", this_reg)
  
  if (graph_sample){
    
    sampled_folder <- file.path(plot_folder, "samplehucs")
    dir.create(sampled_folder, recursive = TRUE)
    
  } else {
    dir.create(plot_folder, recursive = TRUE)
  }
  
  
  #priorities in this region (all same)
  priorities <- res_r %>% pull(Priority) %>% unique() %>% sort()
  
  #PRIORITY (first inner loop)
  for (p in seq_along(priorities)){
    
    this_priority <- priorities[[p]]
    
    res_r_p <- res_r %>% 
      filter(Priority == this_priority)
    
    #get treatments in this region-priority (CAN VARY)
    trts <- res_r_p %>% pull(TxType) %>% unique() %>% sort()
    
    
    #TREATMENT TYPE (second inner loop)
    for (t in seq_along(trts)){
      
      this_trt <- trts[[t]]
      
      #get datasets for this trt
      res_r_p_t <- res_r_p %>% 
        filter(TxType == this_trt)

      if (graph_sample){
        
        #get the  sampled hucs for this region, priority
        this_sample <- hucs_sample %>% 
          filter(Region == this_reg,
                 grouping == this_priority)
        
        res_r_p_t_sample <- res_r_p %>% 
          filter(HUC12 %in% this_sample$HUC12,
                 TxType == this_trt)
        
      } 
  
      
      if (graph_boxplots) {
        #plot 1 : boxplot all HUCs, HACFL, timeseries
        p1 <- ggplot() +
          geom_boxplot(data = res_r_p_t,
                       mapping = aes(x = Year, y = HaCFL, group=Year)) +
          scale_x_continuous(breaks = year_breaks) +
          facet_wrap(~TxIntensity+timing_group, 
                     scales = box_scales, 
                     ncol = trt_cols) +
          labs(title = paste0(this_reg_label, " - ",
                             "Priority: ", this_priority,
                             ", Treatment: ", this_trt),
               subtitle = "Facets by Intensity and Year of Treatment",
               y = "HUC-averaged conditional flame length (HaCFL)") + 
          theme_bw()
        
        fn1 <- paste0(this_reg, "_", this_priority, "_", this_trt, "_",
                      "boxplot_", box_scales, "_HaCFL.jpg")
        ggsave(plot = p1,
               filename = file.path(plot_folder, fn1),
               width = 8, height = 8, units = "in")
        
        
        #plot 2 : boxplot all HUCs, expFlame, timeseries
        p2 <- ggplot() +
          geom_boxplot(data = res_r_p_t,
                       mapping = aes(x = Year, y = expFlame, group=Year)) +
          scale_x_continuous(breaks = year_breaks) +
          facet_wrap(~TxIntensity+timing_group, 
                     scales = box_scales,
                     ncol = trt_cols) + 
          labs(title = paste0(this_reg_label, " - ",
                              "Priority: ", this_priority,
                              ", Treatment: ", this_trt),
               subtitle = "Facets by Intensity and Year of Treatment",
               y = "Flame index (expFlame)") + 
          theme_bw()
        
        fn2 <- paste0(this_reg, "_", this_priority, "_", this_trt, "_",
                      "boxplot_", box_scales, "_expFlame.jpg")
        ggsave(plot = p2,
               filename = file.path(plot_folder, fn2),
               width = 8, height = 8, units = "in")
        
        
        #plot 3 : boxplot all HUCs, HaCBP, timeseries
        p3 <- ggplot() +
          geom_boxplot(data = res_r_p_t,
                       mapping = aes(x = Year, y = HaCBP, group=Year)) +
          scale_x_continuous(breaks = year_breaks) +
          facet_wrap(~TxIntensity+timing_group, 
                     scales = box_scales,
                     ncol = trt_cols) + 
          labs(title = paste0(this_reg_label, " - ",
                              "Priority: ", this_priority,
                              ", Treatment: ", this_trt),
               subtitle = "Facets by Intensity and Year of Treatment",
               y = "HUC-average conditional burn probability (HaCBP)") + 
          theme_bw()
        
        fn3 <- paste0(this_reg, "_", this_priority, "_", this_trt, "_",
                      "boxplot_", box_scales, "_HaCBP.jpg")
        ggsave(plot = p3,
               filename = file.path(plot_folder, fn3),
               width = 8, height = 8, units = "in")
        
        
        #plot 4 : boxplot all HUCs, expBurn, timeseries
        p4 <- ggplot() +
          geom_boxplot(data = res_r_p_t,
                       mapping = aes(x = Year, y = expBurn, group=Year)) +
          scale_x_continuous(breaks = year_breaks) +
          facet_wrap(~TxIntensity+timing_group, 
                     scales = box_scales,
                     ncol = trt_cols) + 
          labs(title = paste0(this_reg_label, " - ",
                              "Priority: ", this_priority,
                              ", Treatment: ", this_trt),
               subtitle = "Facets by Intensity and Year of Treatment",
               y = "Expected Burned Acres (expBurn)") + 
          theme_bw()
        
        fn4 <- paste0(this_reg, "_", this_priority, "_", this_trt, "_",
                      "boxplot_", box_scales, "_expBurn.jpg")
        ggsave(plot = p4,
               filename = file.path(plot_folder, fn4),
               width = 8, height = 8, units = "in")
        
        
        #plot 5 : boxplot all HUCs, active crown, timeseries
        p5 <- ggplot() +
          geom_boxplot(data = res_r_p_t,
                       mapping = aes(x = Year, y = expPcActive, group=Year)) +
          scale_x_continuous(breaks = year_breaks) +
          facet_wrap(~TxIntensity+timing_group, 
                     scales = box_scales,
                     ncol = trt_cols) + 
          labs(title = paste0(this_reg_label, " - ",
                              "Priority: ", this_priority,
                              ", Treatment: ", this_trt),
               subtitle = "Facets by Intensity and Year of Treatment",
               y= "Expected % burned by active crown fire (expPcActive)") + 
          theme_bw()
        
        fn5 <- paste0(this_reg, "_", this_priority, "_", this_trt, "_",
                      "boxplot_", box_scales, "_active.jpg")
        ggsave(plot = p5,
               filename = file.path(plot_folder, fn5),
               width = 8, height = 8, units = "in")
        
        
        #plot 6 : boxplot all HUCs, surface, timeseries
        p6 <- ggplot() +
          geom_boxplot(data = res_r_p_t,
                       mapping = aes(x = Year, y = expPcSurface, group=Year)) +
          scale_x_continuous(breaks = year_breaks) +
          facet_wrap(~TxIntensity+timing_group, 
                     scales = box_scales,
                     ncol = trt_cols) + 
          labs(title = paste0(this_reg_label, " - ",
                              "Priority: ", this_priority,
                              ", Treatment: ", this_trt),
               subtitle = "Facets by Intensity and Year of Treatment",
               y = "Expected % burned by surface fire (expPcSurface)") + 
          theme_bw()
        
        fn6 <- paste0(this_reg, "_", this_priority, "_", this_trt, "_",
                       "boxplot_", box_scales, "_surface.jpg")
        ggsave(plot = p6,
               filename = file.path(plot_folder, fn6),
               width = 8, height = 8, units = "in")
      } # end if(graph_boxplots)
      
    
      
      if(graph_sample){
        
        #numbering skips from removal of graphing blocks, not changing
        
        #plot 11: sampled line, HaCFL, timeseries
        p11 <- ggplot(data = res_r_p_t_sample,
                      mapping = aes(x=Year, y=HaCFL, color=TxIntensity)) +
          geom_point(shape = 1) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          theme(legend.position = "bottom") + 
          facet_wrap(~paste0(timing_group, "\n", HUC12),
                     ncol = 3, scales="free_y") +
          labs(title = paste0(this_reg_label, " - ",
                              "Priority: ", this_priority,
                              ", Treatment: ", this_trt,
                              " for Selected HUCs"),
               subtitle = "Facets by HUC showing Year of Treatment",
               y = "HUC-averaged conditional flame length (HaCFL)") + 
          theme_bw()
        
        fn11 <- paste0("sampledhucs_", this_reg, "_", this_priority, "_", this_trt, 
                       "_HaCFL.jpg")
        ggsave(plot = p11,
               filename = file.path(sampled_folder, fn11),
               width = 7, height = 6, units = "in")
        
        
        #plot 12: sampled line, expFlame, timeseries
        p12 <- ggplot(data = res_r_p_t_sample,
                      mapping = aes(x=Year, y=expFlame, color=TxIntensity)) +
          geom_point(shape = 1) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          theme(legend.position = "bottom") + 
          facet_wrap(~paste0(timing_group, "\n", HUC12),
                     ncol = 3, scales="free_y") +
          labs(title = paste0(this_reg_label, " - ",
                              "Priority: ", this_priority,
                              ", Treatment: ", this_trt,
                              " for Selected HUCs"),
               subtitle = "Facets by HUC showing Year of Treatment",
               y = "Flame index (expFlame)") + 
          theme_bw()
        
        fn12 <- paste0("sampledhucs_", this_reg, "_", this_priority, "_", this_trt, 
                       "_expFlame.jpg")
        ggsave(plot = p12,
               filename = file.path(sampled_folder, fn12),
               width = 7, height = 6, units = "in")
        
        
        #plot 13: sampled line, HaCBP, timeseries
        p13 <- ggplot(data = res_r_p_t_sample,
                      mapping = aes(x=Year, y=HaCBP, color=TxIntensity)) +
          geom_point(shape = 1) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          theme(legend.position = "bottom") + 
          facet_wrap(~paste0(timing_group, "\n", HUC12),
                     ncol = 3, scales="free_y") +
          labs(title = paste0(this_reg_label, " - ",
                              "Priority: ", this_priority,
                              ", Treatment: ", this_trt,
                              " for Selected HUCs"),
               subtitle = "Facets by HUC showing Year of Treatment",
               y = "HUC averaged conditional burn probability (HaCBP)") + 
          theme_bw()
        
        fn13 <- paste0("sampledhucs_", this_reg, "_", this_priority, "_", this_trt, 
                       "_HaCBP.jpg")
        
        ggsave(plot = p13,
               filename = file.path(sampled_folder, fn13),
               width = 7, height = 6, units = "in")
        
        
        #plot 14: sampled line, expBurn, timeseries
        p14 <- ggplot(data = res_r_p_t_sample,
                      mapping = aes(x=Year, y=expBurn, color=TxIntensity)) +
          geom_point(shape = 1) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          theme(legend.position = "bottom") + 
          facet_wrap(~paste0(timing_group, "\n", HUC12),
                     ncol = 3, scales="free_y") +
          labs(title = paste0(this_reg_label, " - ",
                              "Priority: ", this_priority,
                              ", Treatment: ", this_trt,
                              " for Selected HUCs"),
               subtitle = "Facets by HUC showing Year of Treatment",
               y = "Expected burned acres (expBurn)") + 
          theme_bw()
        
        fn14 <- paste0("sampledhucs_", this_reg, "_", this_priority, "_", this_trt, 
                       "_expBurn.jpg")
        
        ggsave(plot = p14,
               filename = file.path(sampled_folder, fn14),
               width = 7, height = 6, units = "in")
        
        #plot 15: sampled line, expPcActive, timeseries
        p15 <- ggplot(data = res_r_p_t_sample,
                      mapping = aes(x=Year, y=expPcActive, color=TxIntensity)) +
          geom_point(shape = 1) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          theme(legend.position = "bottom") + 
          facet_wrap(~paste0(timing_group, "\n", HUC12),
                     ncol = 3, scales="free_y") +
          labs(title = paste0(this_reg_label, " - ",
                              "Priority: ", this_priority,
                              ", Treatment: ", this_trt,
                              " for Selected HUCs"),
               subtitle = "Facets by HUC showing Year of Treatment",
               y = "Expected % burned by active crown fire (expPcActive)") + 
          theme_bw()
        
        fn15 <- paste0("sampledhucs_", this_reg, "_", this_priority, "_", this_trt, 
                       "_expPcActive.jpg")
        ggsave(plot = p15,
               filename = file.path(sampled_folder, fn15),
               width = 7, height = 6, units = "in")
        
        
        #fire type
        this_ft_res <- res_r_p_t_sample %>% 
          dplyr::select(HUC12, timing_group, Year, TxIntensity,
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
          facet_wrap(~paste0(timing_group, "\n", HUC12),
                     ncol = 3, scales="free_y") + 
          labs(title = paste0(this_reg_label, " - ",
                              "Priority: ", this_priority,
                              ", Treatment: ", this_trt,
                              " for Selected HUCs"),
               subtitle = "Facets by HUC showing Year of Treatment",
               y = "Burn fraction by fire type") + 
          theme_bw()
        
        fn16 <- paste0("sampledhucs_", this_reg, "_", this_priority, "_", this_trt, 
                       "_firetype.jpg")
        
        ggsave(plot = p16,
               filename = file.path(sampled_folder, fn16),
               width = 7, height = 6, units = "in")
        
        
        
      } # end if(graph_sampled)
      
    } # end t trt
    
  } # end p priorities
  
} #end r regions



