# time series graphs

# all HUCs: boxplots
# sampled HUCs: line graphs

# all regions, all scenarios
# HaCBP, HaCFL
# expBurn, expFlame


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### Results import -------------------------------------------

# can change between region-only and full datacube. 
# will overwrite
res_orig <- read_csv(file.path('results_csv', 
                               'datacube_weighted_20240212.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

### Data set up ------------------------------------------------

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

#relabeling groups
res <- res %>% 
  mutate(fireGrpLbl = case_when(
    fireGroup == 25 ~ "2024 (25)",
    fireGroup == 50 ~ "2029 (50)",
    fireGroup == 75 ~ "2034 (75)",
    fireGroup == 100 ~ "2039 (100)",
    .default = as.character(fireGroup)
  )) %>% 
  mutate(wuiGrpLbl = case_when(
    wuiGroup == 25 ~ "2024_2039 (25)",
    wuiGroup == 50 ~ "2029 (50)",
    wuiGroup == 75 ~ "2034 (75)",
    wuiGroup == 100 ~ "not treated",
    .default = as.character(wuiGroup)
  )) %>% 
  mutate(hybridGrpLbl = case_when(
    hybridGroup == 25 ~ "2024 (25)",
    hybridGroup == 50 ~ "2029 (50)",
    hybridGroup == 75 ~ "2034 (75)",
    hybridGroup == 100 ~ "2039 (100)",
    .default = as.character(hybridGroup)
  ))
  
year_breaks <- c(2024, 2029, 2034, 2039)

#sampled hucs
hucs_sample <- read_csv(file.path('qaqc', 'samplehucs.csv'))



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
  
  dir.create(sampled_folder, recursive = TRUE)
  
  
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
        mutate(timing_group = fireGrpLbl)
    } else if (this_priority == "WUI"){
      res_r_p <- res_r_p %>% 
        mutate(timing_group = wuiGrpLbl)
    } else if (this_priority == "Hybrid"){
      res_r_p <- res_r_p %>% 
        mutate(timing_group = hybridGrpLbl)
    } else {
      stop("Unmatched priority timing group")
    }
    
    #get the correct sampled hucs for this region, priority
    this_sample <- hucs_sample %>%
      filter(Region == this_reg,
             priority == tolower(this_priority))

    res_r_p_sample <- res_r_p %>%
      filter(HUC12 %in% this_sample$HUC12)
    
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
      
      #plot 1 : boxplot all HUCs, HACFL, timeseries
      p1 <- ggplot() +
        geom_boxplot(data = res_r_p_t,
                     mapping = aes(x = Year, y = HaCFL, group=Year)) +
        scale_x_continuous(breaks = year_breaks) +
        facet_wrap(~TxIntensity+timing_group, scales='free_y') +
        labs(title = paste(this_reg,
                           this_priority,
                           this_trt))

      fn1 <- paste0(this_reg, '_', this_priority, '_', this_trt, '_',
                    'boxplot_freey_', 'HaCFL.jpg')
      ggsave(plot = p1,
             filename = file.path(plot_folder, fn1),
             width = 8, height = 6, units = 'in')

      #plot 2 : boxplot all HUCs, expFlame, timeseries
      p2 <- ggplot() +
        geom_boxplot(data = res_r_p_t,
                     mapping = aes(x = Year, y = expFlame, group=Year)) +
        scale_x_continuous(breaks = year_breaks) +
        facet_wrap(~TxIntensity+timing_group, scales='free_y') +
        labs(title = paste(this_reg,
                           this_priority,
                           this_trt))

      fn2 <- paste0(this_reg, '_', this_priority, '_', this_trt, '_',
                    'boxplot_freey_', 'expFlame.jpg')
      ggsave(plot = p2,
             filename = file.path(plot_folder, fn2),
             width = 8, height = 6, units = 'in')


      #plot 3 : boxplot all HUCs, HaCBP, timeseries
      p3 <- ggplot() +
        geom_boxplot(data = res_r_p_t,
                     mapping = aes(x = Year, y = HaCBP, group=Year)) +
        scale_x_continuous(breaks = year_breaks) +
        facet_wrap(~TxIntensity+timing_group, scales='free_y') +
        labs(title = paste(this_reg,
                           this_priority,
                           this_trt))

      fn3 <- paste0(this_reg, '_', this_priority, '_', this_trt, '_',
                    'boxplot_freey_', 'HaCBP.jpg')
      ggsave(plot = p3,
             filename = file.path(plot_folder, fn3),
             width = 8, height = 6, units = 'in')


      #plot 4 : boxplot all HUCs, expBurn, timeseries
      p4 <- ggplot() +
        geom_boxplot(data = res_r_p_t,
                     mapping = aes(x = Year, y = expBurn, group=Year)) +
        scale_x_continuous(breaks = year_breaks) +
        facet_wrap(~TxIntensity+timing_group, scales='free_y') +
        labs(title = paste(this_reg,
                           this_priority,
                           this_trt))

      fn4 <- paste0(this_reg, '_', this_priority, '_', this_trt, '_',
                    'boxplot_freey_', 'expBurn.jpg')
      ggsave(plot = p4,
             filename = file.path(plot_folder, fn4),
             width = 8, height = 6, units = 'in')
      
      
      #second set, selected HUCs only, time series line

      #plot 5: sampled line, HaCFL, timeseries
      p5 <- ggplot(data = res_r_p_t_sample,
             mapping = aes(x=Year, y=HaCFL, color=TxIntensity)) +
        geom_jitter(shape = 1, height = 0, width = 0.3) +
        geom_line() +
        scale_x_continuous(breaks = year_breaks) +
        facet_wrap(~paste0(timing_group, " ", HUC12),
                   ncol = 2, scales='free_y') +
        labs(title = paste(this_reg,
                           this_priority,
                           this_trt,
                           "Selected HUCs"))

      fn5 <- paste0('sampled_', this_reg, '_', this_priority, '_', this_trt, '_',
                    'line_freey_', 'HaCFL.jpg')
      ggsave(plot = p5,
             filename = file.path(sampled_folder, fn5),
             width = 7, height = 7, units = 'in')


      #plot 6: sampled line, expFlame, timeseries
      p6 <- ggplot(data = res_r_p_t_sample,
             mapping = aes(x=Year, y=expFlame, color=TxIntensity)) +
        geom_jitter(shape = 1, height = 0, width = 0.3) +
        geom_line() +
        scale_x_continuous(breaks = year_breaks) +
        facet_wrap(~paste0(timing_group, " ", HUC12),
                   ncol = 2, scales='free_y') +
        labs(title = paste(this_reg,
                           this_priority,
                           this_trt,
                           "Selected HUCs"))

      fn6 <- paste0('sampled_', this_reg, '_', this_priority, '_', this_trt, '_',
                    'line_freey_', 'expFlame.jpg')
      ggsave(plot = p6,
             filename = file.path(sampled_folder, fn6),
             width = 7, height = 7, units = 'in')


      #plot 7: sampled line, HaCBP, timeseries
      p7 <- ggplot(data = res_r_p_t_sample,
             mapping = aes(x=Year, y=HaCBP, color=TxIntensity)) +
        geom_jitter(shape = 1, height = 0, width = 0.3) +
        geom_line() +
        scale_x_continuous(breaks = year_breaks) +
        facet_wrap(~paste0(timing_group, " ", HUC12),
                   ncol = 2, scales='free_y') +
        labs(title = paste(this_reg,
                           this_priority,
                           this_trt,
                           "Selected HUCs"))

      fn7 <- paste0('sampled_', this_reg, '_', this_priority, '_', this_trt, '_',
                    'line_freey_', 'HaCBP.jpg')
      ggsave(plot = p7,
             filename = file.path(sampled_folder, fn7),
             width = 7, height = 7, units = 'in')


      #plot 8: sampled line, expBurn, timeseries
      p8 <- ggplot(data = res_r_p_t_sample,
             mapping = aes(x=Year, y=expBurn, color=TxIntensity)) +
        geom_jitter(shape = 1, height = 0, width = 0.3) +
        geom_line() +
        scale_x_continuous(breaks = year_breaks) +
        facet_wrap(~paste0(timing_group, " ", HUC12),
                   ncol = 2, scales='free_y') +
        labs(title = paste(this_reg,
                           this_priority,
                           this_trt,
                           "Selected HUCs"))

      fn8 <- paste0('sampled_', this_reg, '_', this_priority, '_', this_trt, '_',
                    'line_freey_', 'expBurn.jpg')
      ggsave(plot = p8,
             filename = file.path(sampled_folder, fn8),
             width = 7, height = 7, units = 'in')
      
      
    } # end t trt
    
  } # end p priorities
  
} #end r regions
  


