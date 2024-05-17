# time series graphs

# all HUCs: boxplots
# sampled HUCs: line graphs

# all regions, all scenarios
# FVS results! 

# BASELINES: 
#version where baselines masquerade as a treatment intensity inside every priority/trt


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  viridis)

### user ---------------------------------------------------

#for the boxplots, free or fixed y axis? 
box_scales <- "fixed" #"fixed" #free_y

#flags for which graphs to make
graph_boxplots <- FALSE
graph_original_sampled <- FALSE
graph_new_sampled <- FALSE

graph_qa_sampled <- TRUE

### Results import -------------------------------------------

# can change between region-only and full datacube. 
# will overwrite
res_orig <- read_csv(file.path("results",
                               "absolute", #"datacube",
                               "SN_SNbl_SNbw_absolute_20240423.csv")) %>%
  mutate(HUC12 = as.character(HUC12))

# res_orig <- read_csv(file.path("results",
#                                "datacube", 
#                                "datacube_interim_sc_cc_sn_bl_bw_20240513.csv")) %>% 
#   mutate(HUC12 = as.character(HUC12)) 


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


#add treat year in later with crosswalk
huc_trt_yr <- res %>% 
  filter(!Priority == "baseline") %>% 
  filter(!Priority == "baseweather") %>% 
  dplyr::select(HUC12, Priority, timeFire, timeHybrid, timeWui) %>% 
  distinct() %>% 
  #treatment years
  separate_wider_delim(timeFire, "_", names=c("fire_trt_yr", NA)) %>% 
  separate_wider_delim(timeHybrid, "_", names=c("hybrid_trt_yr", NA)) %>% 
  mutate(fire_trt_yr = as.numeric(fire_trt_yr),
         hybrid_trt_yr = as.numeric(hybrid_trt_yr),
         wui_trt_yr = case_when(
           timeWui == "2024_2039_yr1to5_16to20" ~ 2024,
           timeWui == "2029_yr6to10" ~ 2029,
           timeWui == "2034_yr11to15" ~ 2034,
           timeWui == "Not treated" ~ 9999)) %>% 
  #treat year for priority
  mutate(trt_yr = case_when(
    Priority == "Fire" ~ fire_trt_yr,
    Priority == "Hybrid" ~ hybrid_trt_yr,
    Priority == "WUI" ~ wui_trt_yr)) %>%
  #final columns
  dplyr::select(HUC12, Priority, trt_yr)



#baseline/baseweather to be treated as an 'intensity' level PER ALL priorities, trts
res_bases <- res %>% 
  filter(TxIntensity %in% c("base", "bw")) %>% 
  #remove priorities, trts from bases
  dplyr::select(-Priority, -TxType)

#remove bases from rest of results (temporarily)
ress <- res %>% 
  filter(!TxIntensity %in% c("base", "bw"))

#get all combination of Priority and TxTypes
frame <- ress %>% 
  dplyr::select(Priority, TxType) %>% 
  distinct()

#duplicate bases values for all priority-txtype combos
res_bases <- frame %>% 
  cross_join(res_bases) 

#add back in
res <- bind_rows(ress, res_bases) %>% 
  left_join(huc_trt_yr, by = join_by(HUC12, Priority))


year_breaks <- c(2024, 2029, 2034, 2039)

#number of treatment time steps, for facetted boxplots correctly
trt_cols <- 4


#colors for intensities on line graphs
i_colors <- c("bw" = "grey50",
              "base" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[1],
              "500k" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[2],
              "1m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[3],
              "2m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[4])


## additional sampled hucs ---------------------------------------------

#original sampled hucs, 2 from each timing group for each priority for each region
hucs_sample <- read_csv(file.path('qa', 'samplehucs.csv')) 

#new set, 3 from each of the top three timing groups for each priority for each region
# uses the qualifying HUCs from original sample plus new ones
hucs_sample_new <- read_csv(file.path('qa', 'sampledhucs_topthreegroups.csv')) 


# troubleshooting SN HUCs with large pretreatment 2m-baseline differences
hucs_sample_qa <- readRDS("qa/scatter_noisyHUCs_top9priority.RDS") %>% 
  #add region to match the others, this is just SN
  mutate(Region = "SN",
         Priority = as.character(Priority))


### Looping version -----------------------------------------------

regions <- res %>% pull(Region) %>% unique()

#REGION (outer loop)
for (r in seq_along(regions)){
  
  this_reg <- regions[[r]]
  
  res_r <- res %>% 
    filter(Region == this_reg)
  
  
  #output 
  plot_folder <- file.path('plots', 'timeseries_FVS', this_reg)
  sampled_folder <- file.path(plot_folder, 'sampledhucs')
  sampled_folder_v2 <- file.path(plot_folder, 'sampledhucs_v2')
  sampled_folder_qa <- file.path(plot_folder, 'sampledhucs_qa')
  
  dir.create(sampled_folder, recursive = TRUE)
  dir.create(sampled_folder_v2, recursive = TRUE)
  dir.create(sampled_folder_qa, recursive = TRUE)
  
  
  #priorities in this region (all same)
  priorities <- res_r %>% pull(Priority) %>% unique()
  
  #PRIORITY (first inner loop)
  for (p in seq_along(priorities)){
    
    this_priority <- priorities[[p]]
    
    res_r_p <- res_r %>% 
      filter(Priority == this_priority)
    
    #treatment year / timing group
    res_r_p <- res_r_p %>% 
      mutate(timing_group = trt_yr)
    
    #get the correct sampled hucs for this region, priority
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
    

    #get the qa sampled hucs for this region, priority
    this_sample_qa <- hucs_sample_qa %>% 
      filter(Region == this_reg,
             Priority == this_priority)
    
    res_r_p_sample_qa <- res_r_p %>% 
      filter(HUC12 %in% this_sample_qa$HUC12)
    
    
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
      
      res_r_p_t_sample_qa <- res_r_p_sample_qa %>% 
        filter(TxType == this_trt)
      
      
      #BOXPLOTS
      if (graph_boxplots){
        #plot 1 : boxplot all HUCs, fuel_lt3, timeseries
        p1 <- ggplot() +
          geom_boxplot(data = res_r_p_t,
                       mapping = aes(x = Year, y = Fuel_lt3, group=Year)) +
          scale_x_continuous(breaks = year_breaks) +
          facet_wrap(~TxIntensity+timing_group, scales = box_scales) +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt))
        
        fn1 <- paste0(this_reg, '_', this_priority, '_', this_trt, '_',
                      'boxplot_', box_scales, '_fuellt3.jpg')
        ggsave(plot = p1,
               filename = file.path(plot_folder, fn1),
               width = 8, height = 8, units = 'in')
        
        #plot 2: boxplot all HUCs, fuel 3to6, timeseries
        p2 <- ggplot() +
          geom_boxplot(data = res_r_p_t,
                       mapping = aes(x = Year, y = Fuel_3to6, group=Year)) +
          scale_x_continuous(breaks = year_breaks) +
          facet_wrap(~TxIntensity+timing_group, scales = box_scales) +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt))
        
        fn2 <- paste0(this_reg, '_', this_priority, '_', this_trt, '_',
                      'boxplot_', box_scales, '_fuel3to6.jpg')
        ggsave(plot = p2,
               filename = file.path(plot_folder, fn2),
               width = 8, height = 8, units = 'in')
        
        
        #plot 3 : boxplot all HUCs, Fuel 6 to 12, timeseries
        p3 <- ggplot() +
          geom_boxplot(data = res_r_p_t,
                       mapping = aes(x = Year, y = Fuel_6to12, group=Year)) +
          scale_x_continuous(breaks = year_breaks) +
          facet_wrap(~TxIntensity+timing_group, scales = box_scales) + 
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt))
        
        fn3 <- paste0(this_reg, '_', this_priority, '_', this_trt, '_',
                      'boxplot_', box_scales, '_fuel6to12.jpg')
        ggsave(plot = p3,
               filename = file.path(plot_folder, fn3),
               width = 8, height = 8, units = 'in')
        
        
        p4 <- ggplot() +
          geom_boxplot(data = res_r_p_t,
                       mapping = aes(x = Year, y = SDI, group=Year)) +
          scale_x_continuous(breaks = year_breaks) +
          facet_wrap(~TxIntensity+timing_group, scales = box_scales) + 
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt))
        
        fn4 <- paste0(this_reg, '_', this_priority, '_', this_trt, '_',
                      'boxplot_', box_scales, '_SDI.jpg')
        ggsave(plot = p4,
               filename = file.path(plot_folder, fn4),
               width = 8, height = 8, units = 'in')
        
      } #end graph box
      
      
      #ORIGINAL SAMPLED HUCS
      if (graph_original_sampled){
        #second set, selected HUCs only, time series line
        
        #plot 6: sampled line, fuel less than 3, timeseries
        p6 <- ggplot(data = res_r_p_t_sample,
                     mapping = aes(x=Year, y=Fuel_lt3, color=TxIntensity)) +
          geom_jitter(shape = 1, height = 0, width = 0.3) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          facet_wrap(~paste0(timing_group, " ", HUC12),
                     ncol = 2, scales='free_y') +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             "Selected HUCs"))
        
        fn6 <- paste0('sampled_', this_reg, '_', this_priority, '_', this_trt, '_',
                      'line_freey_', 'fuellt3.jpg')
        ggsave(plot = p6,
               filename = file.path(sampled_folder, fn6),
               width = 7, height = 7, units = 'in')
        
        #plot 7: samples line, fuel 3 to 6, timeseries
        p7 <- ggplot(data = res_r_p_t_sample,
                     mapping = aes(x=Year, y=Fuel_3to6, color=TxIntensity)) +
          geom_jitter(shape = 1, height = 0, width = 0.3) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          facet_wrap(~paste0(timing_group, " ", HUC12),
                     ncol = 2, scales='free_y') +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             "Selected HUCs"))
        
        fn7 <- paste0('sampled_', this_reg, '_', this_priority, '_', this_trt, '_',
                      'line_freey_', 'fuel3to6.jpg')
        ggsave(plot = p7,
               filename = file.path(sampled_folder, fn7),
               width = 7, height = 7, units = 'in')
        
        
        #plot 8: sampled line, Fuel 6 to 12, timeseries
        p8 <- ggplot(data = res_r_p_t_sample,
                     mapping = aes(x=Year, y=Fuel_6to12, color=TxIntensity)) +
          geom_jitter(shape = 1, height = 0, width = 0.3) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          facet_wrap(~paste0(timing_group, " ", HUC12),
                     ncol = 2, scales='free_y') +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             "Selected HUCs"))
        
        fn8 <- paste0('sampled_', this_reg, '_', this_priority, '_', this_trt, '_',
                      'line_freey_', 'fuel6to12.jpg')
        ggsave(plot = p8,
               filename = file.path(sampled_folder, fn8),
               width = 7, height = 7, units = 'in')
        
        #plot 9: sampled line, SDI, timeseries
        p9 <- ggplot(data = res_r_p_t_sample,
                     mapping = aes(x=Year, y=SDI, color=TxIntensity)) +
          geom_jitter(shape = 1, height = 0, width = 0.3) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          facet_wrap(~paste0(timing_group, " ", HUC12),
                     ncol = 2, scales='free_y') +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             "Selected HUCs"))
        
        fn9 <- paste0('sampled_', this_reg, '_', this_priority, '_', this_trt, '_',
                      'line_freey_', 'SDI.jpg')
        ggsave(plot = p9,
               filename = file.path(sampled_folder, fn9),
               width = 7, height = 7, units = 'in')
      } #end graph original
      
      
      #NEW SAMPLED HUCS
      if (graph_new_sampled){
        # NEW sampled HUCS
        #plot 11: sampled line, mean, timeseries
        p11 <- ggplot(data = res_r_p_t_sample_new,
                      mapping = aes(x=Year, y=SDI, color=TxIntensity)) +
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
                             "Selected HUCs",
                             "SDI"))
        
        fn11 <- paste0('sampledv2_', this_reg, '_', this_priority, '_', this_trt, '_',
                       'line_freey_', 'SDI.jpg')
        ggsave(plot = p11,
               filename = file.path(sampled_folder_v2, fn11),
               width = 7, height = 6, units = 'in')
        
      } # end graph new
      
      #SPECIFIC QA HUCS
      if (graph_qa_sampled){
        #second set, selected HUCs only, time series line
        
        #plot 6: sampled line, fuel less than 3, timeseries
        p6 <- ggplot(data = res_r_p_t_sample_qa,
                     mapping = aes(x=Year, y=Fuel_lt3, color=TxIntensity)) +
          geom_jitter(shape = 1, height = 0, width = 0.3) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          facet_wrap(~paste0(timing_group, " ", HUC12),
                     ncol = 3, scales='free_y') +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             "Selected QA HUCs"))
        
        fn6 <- paste0('sampledqa_', this_reg, '_', this_priority, '_', this_trt, '_',
                      'line_freey_', 'fuellt3.jpg')
        ggsave(plot = p6,
               filename = file.path(sampled_folder_qa, fn6),
               width = 7, height = 7, units = 'in')
        
        #plot 7: samples line, fuel 3 to 6, timeseries
        p7 <- ggplot(data = res_r_p_t_sample_qa,
                     mapping = aes(x=Year, y=Fuel_3to6, color=TxIntensity)) +
          geom_jitter(shape = 1, height = 0, width = 0.3) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          facet_wrap(~paste0(timing_group, " ", HUC12),
                     ncol = 3, scales='free_y') +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             "Selected QA HUCs"))
        
        fn7 <- paste0('sampledqa_', this_reg, '_', this_priority, '_', this_trt, '_',
                      'line_freey_', 'fuel3to6.jpg')
        ggsave(plot = p7,
               filename = file.path(sampled_folder_qa, fn7),
               width = 7, height = 7, units = 'in')
        
        
        #plot 8: sampled line, Fuel 6 to 12, timeseries
        p8 <- ggplot(data = res_r_p_t_sample_qa,
                     mapping = aes(x=Year, y=Fuel_6to12, color=TxIntensity)) +
          geom_jitter(shape = 1, height = 0, width = 0.3) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          facet_wrap(~paste0(timing_group, " ", HUC12),
                     ncol = 3, scales='free_y') +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             "Selected QA HUCs"))
        
        fn8 <- paste0('sampledqa_', this_reg, '_', this_priority, '_', this_trt, '_',
                      'line_freey_', 'fuel6to12.jpg')
        ggsave(plot = p8,
               filename = file.path(sampled_folder_qa, fn8),
               width = 7, height = 7, units = 'in')
        
        #plot 9: sampled line, SDI, timeseries
        p9 <- ggplot(data = res_r_p_t_sample_qa,
                     mapping = aes(x=Year, y=SDI, color=TxIntensity)) +
          geom_jitter(shape = 1, height = 0, width = 0.3) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          facet_wrap(~paste0(timing_group, " ", HUC12),
                     ncol = 3, scales='free_y') +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             "Selected QA HUCs"))
        
        fn9 <- paste0('sampledqa_', this_reg, '_', this_priority, '_', this_trt, '_',
                      'line_freey_', 'SDI.jpg')
        ggsave(plot = p9,
               filename = file.path(sampled_folder_qa, fn9),
               width = 7, height = 7, units = 'in')
      } #end graph original
      
      
    } # end t trt
    
  } # end p priorities
  
} #end r regions
  


