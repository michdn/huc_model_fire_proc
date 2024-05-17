# time series graphs

# all HUCs: boxplots
# sampled HUCs: line graphs

# all regions, all scenarios

# BASELINES: 
#version where baselines masquerade as a treatment intensity inside every priority/trt


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  viridis)

### user ---------------------------------------------------

adj_to_use <- "ROS" # "FL" or "ROS"


#for the boxplots, free or fixed y axis? 
box_scales <- "fixed" #"fixed" #free_y


#flags for which graphs to make
graph_boxplots <- TRUE
graph_original_sampled <- TRUE
graph_new_sampled <- TRUE


### Results import -------------------------------------------

if (adj_to_use == "FL"){
  
  res_orig <- readRDS(file.path("qa", "SN_fuel_adjective_comparison_baseline.RDS")) %>% 
    #for now, filter out nonburn issue hucs
    filter(nonburn_coastal == FALSE)
  
} else if (adj_to_use == "ROS"){
  
  res_orig <- readRDS(file.path("qa", "SN_fuel_ROS_adjective_comparison_baseline.RDS")) %>% 
    #for now, filter out nonburn issue hucs
    filter(nonburn_coastal == FALSE)
  
}


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
  dplyr::select(-Priority, -TxType, -trt_yr)

#remove bases from rest of results (temporarily)
res <- res %>% 
  filter(!TxIntensity %in% c("base", "bw"))

#get trt yr to add in later
huc_trt_yr <- res %>% 
  dplyr::select(HUC12, Priority, trt_yr) %>% 
  distinct()

#get all combination of Priority and TxTypes
frame <- res %>% 
  dplyr::select(Priority, TxType) %>% 
  distinct()

#duplicate bases values for all priority-txtype combos
res_bases <- frame %>% 
  cross_join(res_bases) %>% 
  #add in trt yr for baseline masquerade
  left_join(huc_trt_yr, by = join_by("HUC12", "Priority"))

#add back in
res <- bind_rows(res, res_bases)


#graphing things
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


### Looping version -----------------------------------------------

regions <- "SN" #res %>% pull(Region) %>% unique()

#REGION (outer loop)
for (r in seq_along(regions)){
  
  this_reg <- regions[[r]]
  
  res_r <- res #%>% 
    #filter(Region == this_reg)
  
  
  #output 
  
  plot_folder <- file.path('plots', 
                           paste0('timeseries_fueladjmean_', adj_to_use), 
                           this_reg) 
  
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
    
    # # get the correct timing group for the different priorities
    # # set as a known field name (to avoid passing field name as variable)
    # if (this_priority == "Fire"){
    #   res_r_p <- res_r_p %>% 
    #     mutate(timing_group = timeFire) #timeFire fireGrpLbl
    # } else if (this_priority == "WUI"){
    #   res_r_p <- res_r_p %>% 
    #     mutate(timing_group = timeWui) #timeWui wuiGrpLbl
    # } else if (this_priority == "Hybrid"){
    #   res_r_p <- res_r_p %>% 
    #     mutate(timing_group = timeHybrid) #timeHybrid hybridGrpLbl
    # # } else if (this_priority %in% c("baseline", "baseweather")){
    # #   res_r_p <- res_r_p %>% 
    # #     mutate(timing_group = "none")
    # } else {
    #   stop("Unmatched priority timing group")
    # }
    
    res_r_p <- res_r_p %>% 
      mutate(timing_group = trt_yr)
    
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
        #plot 1 : boxplot all HUCs, mean, timeseries
        p1 <- ggplot() +
          geom_boxplot(data = res_r_p_t,
                       mapping = aes(x = Year, y = mean, group=Year)) +
          scale_x_continuous(breaks = year_breaks) +
          facet_wrap(~TxIntensity+timing_group, 
                     scales = box_scales, 
                     ncol = trt_cols) +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             adj_to_use,
                             "fuel adjectives mean"))
        
        fn1 <- paste0(this_reg, '_', this_priority, '_', this_trt, '_',
                      'boxplot_', box_scales, 
                      '_fueladjmean_', adj_to_use, '.jpg')
        ggsave(plot = p1,
               filename = file.path(plot_folder, fn1),
               width = 8, height = 8, units = 'in')
        
      } # end if(graph_boxplots)
  

      if(graph_original_sampled){
        #second set, selected HUCs only, time series line
        
        
        #plot 6: sampled line, mean, timeseries
        p6 <- ggplot(data = res_r_p_t_sample,
                     mapping = aes(x=Year, y=mean, color=TxIntensity)) +
          geom_jitter(shape = 1, height = 0, width = 0.3) +
          geom_line() +
          scale_x_continuous(breaks = year_breaks) +
          scale_color_manual("Intensity", values = i_colors) + 
          facet_wrap(~paste0(timing_group, "\n", HUC12),
                     ncol = 2, scales='free_y') +
          labs(title = paste(this_reg,
                             this_priority,
                             this_trt,
                             "Selected HUCs",
                             adj_to_use,
                             "Fuel adj mean"))
        
        fn6 <- paste0('sampled_', this_reg, '_', this_priority, '_', this_trt, '_',
                      'line_freey_', 'fueladjmean_', adj_to_use, '.jpg')
        ggsave(plot = p6,
               filename = file.path(sampled_folder, fn6),
               width = 7, height = 7, units = 'in')
        
      } # end if(graph_original_sampled)

      
      if(graph_new_sampled){
        
        #plot 11: sampled line, mean, timeseries
        p11 <- ggplot(data = res_r_p_t_sample_new,
                     mapping = aes(x=Year, y=mean, color=TxIntensity)) +
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
                             adj_to_use,
                             "fuel adj mean"))
        
        fn11 <- paste0('sampledv2_', this_reg, '_', this_priority, '_', this_trt, '_',
                      'line_freey_', 'fueladjmean_', adj_to_use, '.jpg')
        ggsave(plot = p11,
               filename = file.path(sampled_folder_v2, fn11),
               width = 7, height = 6, units = 'in')
        
        

      } # end if(graph_new_sampled)
      
    } # end t trt
    
  } # end p priorities
  
} #end r regions
  


