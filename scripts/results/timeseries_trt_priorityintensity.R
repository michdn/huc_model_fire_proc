# time series graphs

# all regions, all trt types

# each line graph: y = expBurn (metric), x = year
# priority = color
# intensity = line type

# loop regions, priorities


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### Results import -------------------------------------------

# can change between region-only and full datacube. 
# will overwrite
res_orig <- read_csv(file.path('results',
                          'absolute',
                          'CC_absolute_expanded_NOFVS_20240319.csv')) %>% 
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


year_breaks <- c(2024, 2029, 2034, 2039)


### Looping version -----------------------------------------------

regions <- res %>% pull(Region) %>% unique()

#REGION (outer loop)
for (r in seq_along(regions)){
  
  this_reg <- regions[[r]]
  
  res_r <- res %>% 
    filter(Region == this_reg)
  
  
  #output 
  plot_folder <- file.path('plots', 'timeseries_priorityintensity', this_reg)

  dir.create(plot_folder, recursive = TRUE)
  
  
  #trts in this region (may not be the same)
  trts <- res_r %>% pull(TxType) %>% unique()
  
  #TREATMENT TYPE (inner loop)
  for (t in seq_along(trts)){
    
    this_trt <- trts[[t]]
    
    res_r_t <- res_r %>% 
      filter(TxType == this_trt)
    
    res_r_t_mean <- res_r_t %>% 
      group_by(Year, TxIntensity, Priority) %>% 
      summarize(ave_expBurn = mean(expBurn),
                sum_expBurn = sum(expBurn),
                ave_expFlame = mean(expFlame),
                ave_expFlamehAc = mean(expFlame/hucAc),
                ave_active = mean(expPcActive))
    

      #plot 1: sum expBurn
      p1 <- ggplot() +
        geom_line(data = res_r_t_mean,
                  mapping = aes(x=Year, y=sum_expBurn, 
                                color=Priority, linetype=TxIntensity)) +
        scale_x_continuous(breaks = year_breaks) +
        scale_color_manual("Priority", values = c('#b81267', '#4daf4a', '#377eb8')) +
        scale_linetype_manual("Intensity", values = c('solid', 'dashed', 'dotted')) +
        labs(title = paste(this_reg,
                           this_trt),
             y = 'Sum of expBurn')

      fn1 <- paste0(this_reg, '_', this_trt, '_priorityintensity_sumexpBurn.jpg')
      
      ggsave(plot = p1,
             filename = file.path(plot_folder, fn1),
             width = 6, height = 5, units = 'in')

      #plot 2: average expFlame/HucAc
      p2 <- ggplot() +
        geom_line(data = res_r_t_mean,
                  mapping = aes(x=Year, y=ave_expFlamehAc, 
                                color=Priority, linetype=TxIntensity)) +
        scale_x_continuous(breaks = year_breaks) +
        scale_color_manual("Priority", values = c('#b81267', '#4daf4a', '#377eb8')) +
        scale_linetype_manual("Intensity", values = c('solid', 'dashed', 'dotted')) +
        labs(title = paste(this_reg,
                           this_trt),
             y = 'Mean of (expFlame/HUC area)')
      
      fn2 <- paste0(this_reg, '_', this_trt, '_priorityintensity_meanexpFlamehAc.jpg')
      
      ggsave(plot = p2,
             filename = file.path(plot_folder, fn2),
             width = 6, height = 5, units = 'in')
      
      #plot 2b: average expFlame
      p2b <- ggplot() +
        geom_line(data = res_r_t_mean,
                  mapping = aes(x=Year, y=ave_expFlame, 
                                color=Priority, linetype=TxIntensity)) +
        scale_x_continuous(breaks = year_breaks) +
        scale_color_manual("Priority", values = c('#b81267', '#4daf4a', '#377eb8')) +
        scale_linetype_manual("Intensity", values = c('solid', 'dashed', 'dotted')) +
        labs(title = paste(this_reg,
                           this_trt),
             y = 'Mean of expFlame')
      
      fn2b <- paste0(this_reg, '_', this_trt, '_priorityintensity_meanexpFlame.jpg')
      
      ggsave(plot = p2b,
             filename = file.path(plot_folder, fn2b),
             width = 6, height = 5, units = 'in')
      
      #plot 3: average active percent
      p3 <- ggplot() +
        geom_line(data = res_r_t_mean,
                  mapping = aes(x=Year, y=ave_active, 
                                color=Priority, linetype=TxIntensity)) +
        scale_x_continuous(breaks = year_breaks) +
        scale_color_manual("Priority", values = c('#b81267', '#4daf4a', '#377eb8')) +
        scale_linetype_manual("Intensity", values = c('solid', 'dashed', 'dotted')) +
        labs(title = paste(this_reg,
                           this_trt),
             y = 'Mean of active crown fire percent')
      
      fn3 <- paste0(this_reg, '_', this_trt, '_priorityintensity_meanactive.jpg')
      
      ggsave(plot = p3,
             filename = file.path(plot_folder, fn3),
             width = 6, height = 5, units = 'in')
      

  } # end p priorities
  
} #end r regions
  


