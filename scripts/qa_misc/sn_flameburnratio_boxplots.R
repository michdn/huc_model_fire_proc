# SN per fire flame/burn ratio

# time series boxplots, 
# but now each HUC has 200 values not 1

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### user ---------------------------------------------------

#for the boxplots, free or fixed y axis? 
box_scales <- "fixed" #"fixed" #free_y

#output 
plot_folder <- file.path('plots', 'flameburnratio', "SN")
sampled_folder <- file.path(plot_folder, 'sampledhucs')

dir.create(sampled_folder, recursive = TRUE)


### Results import -------------------------------------------

res_orig <- readRDS("results/misc/SN_perfire_ratio_20240409.RDS") %>% 
  mutate(Year = as.numeric(Year))

# need to get timing group info
# cleanest way is from an interim datacube and create lookup table

cube <- read_csv(file.path("results",
                           "datacube", 
                           "datacube_interim_sc_cc_sn_20240403.csv")) %>% 
  mutate(HUC12 = as.character(HUC12)) %>% 
  filter(Region == "SN")

timegroups <- cube %>% 
  select(HUC12, timeFire, timeHybrid, timeWui) %>% 
  distinct()

#join to res_orig by HUC id
res <- res_orig %>% 
  left_join(timegroups, by = join_by("HUC12"))
  
  
  
### Data set up ------------------------------------------------

# Want priority order: 'Fire', 'WUI', 'Hybrid'
# Want intensity order: '500k', '1m', '2m'

res <- res %>% 
  #fix RFFC Hybrid here
  mutate(Priority = if_else(Priority == "RFFC", "Hybrid", Priority)) %>% 
  #For graphing in the correct order
  # make factor with set order (priority)
  mutate(Priority = as.factor(Priority),
         Priority = forcats::fct_relevel(Priority,
                                         "Fire", "WUI", "Hybrid"),
         #Make factor with set order (intensity))
         TxIntensity = as.factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                            "500k", "1m", "2m"))

year_breaks <- c(2024, 2029, 2034, 2039)

#sampled hucs
hucs_sample <- read_csv(file.path('qaqc', 'samplehucs.csv')) %>% 
  filter(Region == "SN")


### Looping version -----------------------------------------------

#priorities 
priorities <- res %>% pull(Priority) %>% unique()

#PRIORITY (first loop)
for (p in seq_along(priorities)){
  
  this_priority <- priorities[[p]]
  
  res_r_p <- res %>% 
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
  } else {
    stop("Unmatched priority timing group")
  }
  
  #get the correct sampled hucs for this region, priority
  this_sample <- hucs_sample %>%
    filter(priority == tolower(this_priority))
  
  res_r_p_sample <- res_r_p %>%
    filter(HUC12 %in% this_sample$HUC12)
  
  #get treatments in this region-priority
  trts <- res_r_p %>% pull(TxType) %>% unique()
  
  
  #TREATMENT TYPE (inner loop)
  for (t in seq_along(trts)){
    
    this_trt <- trts[[t]]
    
    #get full and sampled datasets for this trt
    res_r_p_t <- res_r_p %>% 
      filter(TxType == this_trt)
    
    res_r_p_t_sample <- res_r_p_sample %>% 
      filter(TxType == this_trt)
    

    #plot 1b: boxplot all HUCs, HaCFL/HaCBP, timeseries
    p1b <- ggplot() +
      geom_boxplot(data = res_r_p_t,
                   mapping = aes(x = Year, y = flame_burn_ratio, group=Year)) +
      scale_x_continuous(breaks = year_breaks) +
      facet_wrap(~TxIntensity+timing_group, scales = box_scales) +
      labs(title = paste("SN", 
                         this_priority,
                         this_trt))
    
    fn1b <- paste0('SN_', this_priority, '_', this_trt, '_',
                   'boxplot_', box_scales, '_flameburnratio.jpg')
    ggsave(plot = p1b,
           filename = file.path(plot_folder, fn1b),
           width = 8, height = 6, units = 'in')
    
    
    ## MULTIPLE HUCS IN HERE. FIX. 

    # #second set, selected HUCs only, boxplot (200 fires)
    # 
    # #plot 6b: samples boxplot, HaCFL/HaCBP, timeseries
    # p6b <- ggplot() + 
    #   geom_boxplot(data = res_r_p_t_sample,
    #               mapping = aes(x=Year, y=flame_burn_ratio, group=Year)) +
    #   scale_x_continuous(breaks = year_breaks) +
    #   facet_wrap(~TxIntensity+timing_group, scales = box_scales) +
    #   labs(title = paste("SN", 
    #                      this_priority,
    #                      this_trt, 
    #                      "Selected HUCs"))
    # 
    # 
    # fn6b <- paste0('sampled_SN_', this_priority, '_', this_trt, '_',
    #                'boxplot_', box_scales, '_flameburnratio.jpg')
    # ggsave(plot = p6b,
    #        filename = file.path(sampled_folder, fn6b),
    #        width = 7, height = 7, units = 'in')
    

  } # end t trt
  
} # end p priorities

