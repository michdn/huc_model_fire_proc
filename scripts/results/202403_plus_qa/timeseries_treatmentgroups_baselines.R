# time series graphs

# all HUCs: boxplots
# sampled HUCs: line graphs

# all regions, all scenarios
# HaCBP, HaCFL
# expBurn, expFlame, active

# BASELINES: 
#version where baselines masquerade as a treatment intensity inside every priority/trt


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  viridis)

### user ---------------------------------------------------

#for the boxplots, free or fixed y axis? 
box_scales <- "free_y" #"fixed" #free_y


#flags for which graphs to make
graph_boxplots <- TRUE
graph_original_sampled <- TRUE
graph_new_sampled <- TRUE


### Results import -------------------------------------------

# can change between region-only and full datacube. 
# will overwrite
res_orig <- read_csv(file.path("results",
                               "absolute", #"datacube", 
                               #"SN_SNbl_SNbw_absolute_20240423.csv")) %>% 
                               "SC_SCbl_SCbw_absolute_20240510.csv")) %>%
  mutate(HUC12 = as.character(HUC12)) 


nb_hucs <- readRDS("data/nonburnable_rerun_list.RDS") 
  #filter(region == "CC")

res_orig <- res_orig %>% 
  mutate(nonburn_coastal = if_else(HUC12 %in% nb_hucs$huc12, TRUE, FALSE)) %>% 
  filter(nonburn_coastal == FALSE)

### Data set up ------------------------------------------------

# Want priority order: 'Fire', 'WUI', 'Hybrid'
# Want intensity order: '500k', '1m', '2m'

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


#baseline/baseweather to be treated as an 'intensity' level PER ALL priorities, trts
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


#post 2024-03-20, new fields in cube: timeFire, timeHybrid, timeWUI
#relabeling groups
# res <- res %>%
#   mutate(fireGrpLbl = case_when(
#     fireGroup == 25 ~ "2024 (25)",
#     fireGroup == 50 ~ "2029 (50)",
#     fireGroup == 75 ~ "2034 (75)",
#     fireGroup == 100 ~ "2039 (100)",
#     .default = as.character(fireGroup)
#   )) %>%
#   mutate(wuiGrpLbl = case_when(
#     wuiGroup == 25 ~ "2024_2039 (25)",
#     wuiGroup == 50 ~ "2029 (50)",
#     wuiGroup == 75 ~ "2034 (75)",
#     wuiGroup == 100 ~ "not treated",
#     .default = as.character(wuiGroup)
#   )) %>%
#   mutate(hybridGrpLbl = case_when(
#     hybridGroup == 25 ~ "2024 (25)",
#     hybridGroup == 50 ~ "2029 (50)",
#     hybridGroup == 75 ~ "2034 (75)",
#     hybridGroup == 100 ~ "2039 (100)",
#     .default = as.character(hybridGroup)
#   ))
  
year_breaks <- c(2024, 2029, 2034, 2039)

#number of treatment time steps, for facetted boxplots correctly
trt_cols <- 4


#colors for intensities on line graphs
i_colors <- c("bw" = "grey50",
              "base" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[1],
              "500k" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[2],
              "1m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[3],
              "2m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[4])

ft_lines <-  c("surface" = "solid", 
               "passive_crown" = "dashed",
               "active_crown" = "dotted")



## additional sampled hucs ---------------------------------------------

#original sampled hucs, 2 from each timing group for each priority for each region
hucs_sample <- read_csv(file.path('qa', 'samplehucs.csv')) 

#new set, 3 from each of the top three timing groups for each priority for each region
# uses the qualifying HUCs from original sample plus new ones
hucs_sample_new <- read_csv(file.path('qa', 'sampledhucs_topthreegroups.csv')) 


### Looping version -----------------------------------------------

regions <- res %>% pull(Region) %>% unique()

#REGION (outer loop)
for (r in seq_along(regions)){
  
  this_reg <- regions[[r]]
  
  res_r <- res %>% 
    filter(Region == this_reg)
  
  
  #output 
  plot_folder <- file.path('plots', 'timeseries', this_reg)
  sampled_folder <- file.path(plot_folder, 'sampledhucs')
  sampled_folder_v2 <- file.path(plot_folder, 'sampledhucs_v2')
  
  dir.create(sampled_folder, recursive = TRUE)
  dir.create(sampled_folder_v2, recursive = TRUE)
  
  
  #priorities in this region (all same)
  priorities <- res_r %>% pull(Priority) %>% unique()
  
  #PRIORITY (first inner loop)
  for (p in seq_along(priorities)){
    
    this_priority <- priorities[[p]]
    
    res_r_p <- res_r %>% 
      filter(Priority == this_priority)
    
    # get the correct timing group for the different priorities
    # set as a known field name (to avoid passing field name as variable)
    if (this_priority == "Fire"){
      res_r_p <- res_r_p %>% 
        mutate(timing_group = timeFire) #timeFire fireGrpLbl
    } else if (this_priority == "WUI"){
      res_r_p <- res_r_p %>% 
        mutate(timing_group = timeWui) #timeWui wuiGrpLbl
    } else if (this_priority == "Hybrid"){
      res_r_p <- res_r_p %>% 
        mutate(timing_group = timeHybrid) #timeHybrid hybridGrpLbl
    # } else if (this_priority %in% c("baseline", "baseweather")){
    #   res_r_p <- res_r_p %>% 
    #     mutate(timing_group = "none")
    } else {
      stop("Unmatched priority timing group")
    }
    
    #get the correct original sampled hucs for this region, priority
    this_sample <- hucs_sample %>%
      filter(Region == this_reg,
             priority == tolower(this_priority))

    res_r_p_sample <- res_r_p %>%
      filter(HUC12 %in% this_sample$HUC12)
    
    
    #get the additional sampled hucs for this region, priority
    this_sample_new <- hucs_sample_new %>% 
      filter(Region == this_reg,
             grouping == this_priority)
    
    res_r_p_sample_new <- res_r_p %>% 
      filter(HUC12 %in% this_sample_new$HUC12)
    
    #get treatments in this region-priority
    trts <- res_r_p %>% pull(TxType) %>% unique()
    
    
    #TREATMENT TYPE (second inner loop)
    for (t in seq_along(trts)){
      
      this_trt <- trts[[t]]
      
      #get full and sampled datasets for this trt
      res_r_p_t <- res_r_p %>% 
        filter(TxType == this_trt)
      
      res_r_p_t_sample <- res_r_p_sample %>% 
        filter(TxType == this_trt)
      
      
      res_r_p_t_sample_new <- res_r_p_sample_new %>% 
        filter(TxType == this_trt)
      

      if (graph_boxplots) {
        #plot 1 : boxplot all HUCs, HACFL, timeseries
        p1 <- ggplot() +
          geom_boxplot(data = res_r_p_t,
                       mapping = aes(x = Year, y = HaCFL, group=Year)) +
          scale_x_continuous(breaks = year_breaks) +
          facet_wrap(~TxIntensity+timing_group, 
                     scales = box_scales, 
                     ncol = trt_cols) +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt))
        
        fn1 <- paste0(this_reg, '_', this_priority, '_', this_trt, '_',
                      'boxplot_', box_scales, '_HaCFL.jpg')
        ggsave(plot = p1,
               filename = file.path(plot_folder, fn1),
               width = 8, height = 8, units = 'in')
        
        #plot 1b: boxplot all HUCs, HaCFL/HaCBP, timeseries
        p1b <- ggplot() +
          geom_boxplot(data = res_r_p_t,
                       mapping = aes(x = Year, y = HaCFL/HaCBP, group=Year)) +
          scale_x_continuous(breaks = year_breaks) +
          facet_wrap(~TxIntensity+timing_group, 
                     scales = box_scales,
                     ncol = trt_cols) +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt))
        
        fn1b <- paste0(this_reg, '_', this_priority, '_', this_trt, '_',
                       'boxplot_', box_scales, '_HaCFLHaCBP.jpg')
        ggsave(plot = p1b,
               filename = file.path(plot_folder, fn1b),
               width = 8, height = 8, units = 'in')
        
        
        #plot 2 : boxplot all HUCs, expFlame, timeseries
        p2 <- ggplot() +
          geom_boxplot(data = res_r_p_t,
                       mapping = aes(x = Year, y = expFlame, group=Year)) +
          scale_x_continuous(breaks = year_breaks) +
          facet_wrap(~TxIntensity+timing_group, 
                     scales = box_scales,
                     ncol = trt_cols) + 
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt))
        
        fn2 <- paste0(this_reg, '_', this_priority, '_', this_trt, '_',
                      'boxplot_', box_scales, '_expFlame.jpg')
        ggsave(plot = p2,
               filename = file.path(plot_folder, fn2),
               width = 8, height = 8, units = 'in')
        
        
        #plot 3 : boxplot all HUCs, HaCBP, timeseries
        p3 <- ggplot() +
          geom_boxplot(data = res_r_p_t,
                       mapping = aes(x = Year, y = HaCBP, group=Year)) +
          scale_x_continuous(breaks = year_breaks) +
          facet_wrap(~TxIntensity+timing_group, 
                     scales = box_scales,
                     ncol = trt_cols) + 
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt))
        
        fn3 <- paste0(this_reg, '_', this_priority, '_', this_trt, '_',
                      'boxplot_', box_scales, '_HaCBP.jpg')
        ggsave(plot = p3,
               filename = file.path(plot_folder, fn3),
               width = 8, height = 8, units = 'in')
        
        
        #plot 4 : boxplot all HUCs, expBurn, timeseries
        p4 <- ggplot() +
          geom_boxplot(data = res_r_p_t,
                       mapping = aes(x = Year, y = expBurn, group=Year)) +
          scale_x_continuous(breaks = year_breaks) +
          facet_wrap(~TxIntensity+timing_group, 
                     scales = box_scales,
                     ncol = trt_cols) + 
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt))
        
        fn4 <- paste0(this_reg, '_', this_priority, '_', this_trt, '_',
                      'boxplot_', box_scales, '_expBurn.jpg')
        ggsave(plot = p4,
               filename = file.path(plot_folder, fn4),
               width = 8, height = 8, units = 'in')
        
        
        #plot 5 : boxplot all HUCs, active crown, timeseries
        p5 <- ggplot() +
          geom_boxplot(data = res_r_p_t,
                       mapping = aes(x = Year, y = expPcActive, group=Year)) +
          scale_x_continuous(breaks = year_breaks) +
          facet_wrap(~TxIntensity+timing_group, 
                     scales = box_scales,
                     ncol = trt_cols) + 
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt))
        
        fn5 <- paste0(this_reg, '_', this_priority, '_', this_trt, '_',
                      'boxplot_', box_scales, '_active.jpg')
        ggsave(plot = p5,
               filename = file.path(plot_folder, fn5),
               width = 8, height = 8, units = 'in')
        
        
        #plot 5b : boxplot all HUCs, surface, timeseries
        p5b <- ggplot() +
          geom_boxplot(data = res_r_p_t,
                       mapping = aes(x = Year, y = expPcSurface, group=Year)) +
          scale_x_continuous(breaks = year_breaks) +
          facet_wrap(~TxIntensity+timing_group, 
                     scales = box_scales,
                     ncol = trt_cols) + 
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt))
        
        fn5b <- paste0(this_reg, '_', this_priority, '_', this_trt, '_',
                       'boxplot_', box_scales, '_surface.jpg')
        ggsave(plot = p5b,
               filename = file.path(plot_folder, fn5b),
               width = 8, height = 8, units = 'in')
      } # end if(graph_boxplots)
  

      if(graph_original_sampled){
        #second set, selected HUCs only, time series line
        
        
        #plot 6: sampled line, HaCFL, timeseries
        p6 <- ggplot(data = res_r_p_t_sample,
                     mapping = aes(x=Year, y=HaCFL, color=TxIntensity)) +
          geom_jitter(shape = 1, height = 0, width = 0.3) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          facet_wrap(~paste0(timing_group, "\n", HUC12),
                     ncol = 2, scales='free_y') +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             "Selected HUCs"))
        
        fn6 <- paste0('sampled_', this_reg, '_', this_priority, '_', this_trt, '_',
                      'line_freey_', 'HaCFL.jpg')
        ggsave(plot = p6,
               filename = file.path(sampled_folder, fn6),
               width = 7, height = 7, units = 'in')
        
        # #plot 6b: samples line, HaCFL/HaCBP, timeseries
        # p6b <- ggplot(data = res_r_p_t_sample,
        #               mapping = aes(x=Year, y=HaCFL/HaCBP, color=TxIntensity)) +
        #   geom_jitter(shape = 1, height = 0, width = 0.3) +
        #   geom_line() +
        #   scale_x_continuous(breaks = year_breaks) +
        #   scale_color_manual("Intensity", values = i_colors) + 
        #   facet_wrap(~paste0(timing_group, " ", HUC12),
        #              ncol = 2, scales='free_y') +
        #   labs(title = paste(this_reg,
        #                      this_priority,
        #                      this_trt,
        #                      "Selected HUCs"))
        # 
        # fn6b <- paste0('sampled_', this_reg, '_', this_priority, '_', this_trt, '_',
        #                'line_freey_', 'HaCFLHaCBP.jpg')
        # ggsave(plot = p6b,
        #        filename = file.path(sampled_folder, fn6b),
        #        width = 7, height = 7, units = 'in')
        
        
        #plot 7: sampled line, expFlame, timeseries
        p7 <- ggplot(data = res_r_p_t_sample,
                     mapping = aes(x=Year, y=expFlame, color=TxIntensity)) +
          geom_jitter(shape = 1, height = 0, width = 0.3) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          facet_wrap(~paste0(timing_group, "\n", HUC12),
                     ncol = 2, scales='free_y') +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             "Selected HUCs"))
        
        fn7 <- paste0('sampled_', this_reg, '_', this_priority, '_', this_trt, '_',
                      'line_freey_', 'expFlame.jpg')
        ggsave(plot = p7,
               filename = file.path(sampled_folder, fn7),
               width = 7, height = 7, units = 'in')
        
        
        #plot 8: sampled line, HaCBP, timeseries
        p8 <- ggplot(data = res_r_p_t_sample,
                     mapping = aes(x=Year, y=HaCBP, color=TxIntensity)) +
          geom_jitter(shape = 1, height = 0, width = 0.3) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          facet_wrap(~paste0(timing_group, "\n", HUC12),
                     ncol = 2, scales='free_y') +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             "Selected HUCs"))
        
        fn8 <- paste0('sampled_', this_reg, '_', this_priority, '_', this_trt, '_',
                      'line_freey_', 'HaCBP.jpg')
        ggsave(plot = p8,
               filename = file.path(sampled_folder, fn8),
               width = 7, height = 7, units = 'in')
        
        
        #plot 9: sampled line, expBurn, timeseries
        p9 <- ggplot(data = res_r_p_t_sample,
                     mapping = aes(x=Year, y=expBurn, color=TxIntensity)) +
          geom_jitter(shape = 1, height = 0, width = 0.3) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          facet_wrap(~paste0(timing_group, "\n", HUC12),
                     ncol = 2, scales='free_y') +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             "Selected HUCs"))
        
        fn9 <- paste0('sampled_', this_reg, '_', this_priority, '_', this_trt, '_',
                      'line_freey_', 'expBurn.jpg')
        ggsave(plot = p9,
               filename = file.path(sampled_folder, fn9),
               width = 7, height = 7, units = 'in')
        
        #plot 9: sampled line, expPcActive, timeseries
        p10 <- ggplot(data = res_r_p_t_sample,
                      mapping = aes(x=Year, y=expPcActive, color=TxIntensity)) +
          geom_jitter(shape = 1, height = 0, width = 0.3) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          facet_wrap(~paste0(timing_group, "\n", HUC12),
                     ncol = 2, scales='free_y') +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             "Selected HUCs"))
        
        fn10 <- paste0('sampled_', this_reg, '_', this_priority, '_', this_trt, '_',
                       'line_freey_', 'expPcActive.jpg')
        ggsave(plot = p10,
               filename = file.path(sampled_folder, fn10),
               width = 7, height = 7, units = 'in')
      } # end if(graph_original_sampled)

      
      if(graph_new_sampled){
        
        #plot 11: sampled line, HaCFL, timeseries
        p11 <- ggplot(data = res_r_p_t_sample_new,
                     mapping = aes(x=Year, y=HaCFL, color=TxIntensity)) +
          geom_jitter(shape = 1, height = 0, width = 0.3) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          theme(legend.position = "bottom") + 
          facet_wrap(~paste0(timing_group, "\n", HUC12),
                     ncol = 3, scales='free_y') +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             "Selected HUCs"))
        
        fn11 <- paste0('sampledv2_', this_reg, '_', this_priority, '_', this_trt, '_',
                      'line_freey_', 'HaCFL.jpg')
        ggsave(plot = p11,
               filename = file.path(sampled_folder_v2, fn11),
               width = 7, height = 6, units = 'in')
        
        
        #plot 12: sampled line, expFlame, timeseries
        p12 <- ggplot(data = res_r_p_t_sample_new,
                     mapping = aes(x=Year, y=expFlame, color=TxIntensity)) +
          geom_jitter(shape = 1, height = 0, width = 0.3) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          theme(legend.position = "bottom") + 
          facet_wrap(~paste0(timing_group, "\n", HUC12),
                     ncol = 3, scales='free_y') +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             "Selected HUCs"))
        
        fn12 <- paste0('sampledv2_', this_reg, '_', this_priority, '_', this_trt, '_',
                      'line_freey_', 'expFlame.jpg')
        ggsave(plot = p12,
               filename = file.path(sampled_folder_v2, fn12),
               width = 7, height = 6, units = 'in')
        
        
        #plot 13: sampled line, HaCBP, timeseries
        p13 <- ggplot(data = res_r_p_t_sample_new,
                     mapping = aes(x=Year, y=HaCBP, color=TxIntensity)) +
          geom_jitter(shape = 1, height = 0, width = 0.3) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          theme(legend.position = "bottom") + 
          facet_wrap(~paste0(timing_group, "\n", HUC12),
                     ncol = 3, scales='free_y') +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             "Selected HUCs"))
        
        fn13 <- paste0('sampledv2_', this_reg, '_', this_priority, '_', this_trt, '_',
                      'line_freey_', 'HaCBP.jpg')
        ggsave(plot = p13,
               filename = file.path(sampled_folder_v2, fn13),
               width = 7, height = 6, units = 'in')
        
        
        #plot 14: sampled line, expBurn, timeseries
        p14 <- ggplot(data = res_r_p_t_sample_new,
                     mapping = aes(x=Year, y=expBurn, color=TxIntensity)) +
          geom_jitter(shape = 1, height = 0, width = 0.3) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          theme(legend.position = "bottom") + 
          facet_wrap(~paste0(timing_group, "\n", HUC12),
                     ncol = 3, scales='free_y') +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             "Selected HUCs"))
        
        fn14 <- paste0('sampledv2_', this_reg, '_', this_priority, '_', this_trt, '_',
                      'line_freey_', 'expBurn.jpg')
        ggsave(plot = p14,
               filename = file.path(sampled_folder_v2, fn14),
               width = 7, height = 6, units = 'in')
        
        #plot 15: sampled line, expPcActive, timeseries
        p15 <- ggplot(data = res_r_p_t_sample_new,
                      mapping = aes(x=Year, y=expPcActive, color=TxIntensity)) +
          geom_jitter(shape = 1, height = 0, width = 0.3) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          theme(legend.position = "bottom") + 
          facet_wrap(~paste0(timing_group, "\n", HUC12),
                     ncol = 3, scales='free_y') +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             "Selected HUCs"))
        
        fn15 <- paste0('sampledv2_', this_reg, '_', this_priority, '_', this_trt, '_',
                       'line_freey_', 'expPcActive.jpg')
        ggsave(plot = p15,
               filename = file.path(sampled_folder_v2, fn15),
               width = 7, height = 6, units = 'in')
        
        
        #fire type
        this_ft_res <- res_r_p_t_sample_new %>% 
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
                     ncol = 3, scales='free_y') + 
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             "Selected HUCs"))
        
        fn16 <- paste0('sampledv2_', this_reg, '_', this_priority, '_', this_trt, '_',
                      'line_freey_', 'firetype.jpg')
        ggsave(plot = p16,
               filename = file.path(sampled_folder_v2, fn16),
               width = 7, height = 6, units = 'in')
        
        
        
      } # end if(graph_new_sampled)
      
    } # end t trt
    
  } # end p priorities
  
} #end r regions
  


