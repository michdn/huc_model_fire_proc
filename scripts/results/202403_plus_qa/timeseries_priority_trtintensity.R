# time series graphs

# all regions, all priorities

# each line graph: y = expBurn (metric), x = year
# trt = color
# intensity = line type

# loop regions, priorities


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### Results import -------------------------------------------

# can change between region-only and full datacube. 
# will overwrite
res_orig <- read_csv(file.path("results",
                               "datacube", 
                               "datacube_interim_sc_cc_sn_20240403.csv")) %>% 
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
  plot_folder <- file.path('plots', 'timeseries_trtintensity', this_reg)

  dir.create(plot_folder, recursive = TRUE)
  
  
  #priorities in this region (all same)
  priorities <- res_r %>% pull(Priority) %>% unique()
  
  #PRIORITY (inner loop)
  for (p in seq_along(priorities)){
    
    this_priority <- priorities[[p]]
    
    res_r_p <- res_r %>% 
      filter(Priority == this_priority)
    
    res_r_p_mean <- res_r_p %>% 
      group_by(Year, TxIntensity, TxType) %>% 
      mutate(hacflhacbp = HaCFL/HaCBP) %>% 
      summarize(ave_expBurn = mean(expBurn),
                sum_expBurn = sum(expBurn),
                ave_expFlame = mean(expFlame),
                ave_expFlamehAc = mean(expFlame/hucAc),
                ave_active = mean(expPcActive),
                #probably not correct
                ave_hacflhacbp = mean(hacflhacbp))
    

      #plot 1: sum expBurn
      p1 <- ggplot() +
        geom_line(data = res_r_p_mean,
                  mapping = aes(x=Year, y=sum_expBurn, 
                                color=TxType, linetype=TxIntensity)) +
        scale_x_continuous(breaks = year_breaks) +
        scale_color_manual("Treatment Type", values = c('#1b9e77', '#d95f02', '#7570b3')) +
        scale_linetype_manual("Intensity", values = c('solid', 'dashed', 'dotted')) +
        labs(title = paste(this_reg,
                           this_priority),
             y = 'Sum of expBurn')

      fn1 <- paste0(this_reg, '_', this_priority, '_trtintensity_sumexpBurn.jpg')
      
      ggsave(plot = p1,
             filename = file.path(plot_folder, fn1),
             width = 6, height = 5, units = 'in')

      #plot 2: average expFlame/hAc (/hAc so better comp when across regions)
      p2 <- ggplot() +
        geom_line(data = res_r_p_mean,
                  mapping = aes(x=Year, y=ave_expFlamehAc, 
                                color=TxType, linetype=TxIntensity)) +
        scale_x_continuous(breaks = year_breaks) +
        scale_color_manual("Treatment Type", values = c('#1b9e77', '#d95f02', '#7570b3')) +
        scale_linetype_manual("Intensity", values = c('solid', 'dashed', 'dotted')) +
        labs(title = paste(this_reg,
                           this_priority),
             y = 'Mean of (expFlame/HUC area)')
      
      fn2 <- paste0(this_reg, '_', this_priority, '_trtintensity_meanexpFlamehAc.jpg')
      
      ggsave(plot = p2,
             filename = file.path(plot_folder, fn2),
             width = 6, height = 5, units = 'in')
      
      #plot 2b: average expFlame
      p2b <- ggplot() +
        geom_line(data = res_r_p_mean,
                  mapping = aes(x=Year, y=ave_expFlame, 
                                color=TxType, linetype=TxIntensity)) +
        scale_x_continuous(breaks = year_breaks) +
        scale_color_manual("Treatment Type", values = c('#1b9e77', '#d95f02', '#7570b3')) +
        scale_linetype_manual("Intensity", values = c('solid', 'dashed', 'dotted')) +
        labs(title = paste(this_reg,
                           this_priority),
             y = 'Mean of expFlame')
      
      fn2b <- paste0(this_reg, '_', this_priority, '_trtintensity_meanexpFlame.jpg')
      
      ggsave(plot = p2b,
             filename = file.path(plot_folder, fn2b),
             width = 6, height = 5, units = 'in')
      
      
      #plot 3: average active crown %
      p3 <- ggplot() +
        geom_line(data = res_r_p_mean,
                  mapping = aes(x=Year, y=ave_active, 
                                color=TxType, linetype=TxIntensity)) +
        scale_x_continuous(breaks = year_breaks) +
        scale_color_manual("Treatment Type", values = c('#1b9e77', '#d95f02', '#7570b3')) +
        scale_linetype_manual("Intensity", values = c('solid', 'dashed', 'dotted')) +
        labs(title = paste(this_reg,
                           this_priority),
             y = 'Mean of active crown fire percent')
      
      fn3 <- paste0(this_reg, '_', this_priority, '_trtintensity_meanactive.jpg')
      
      ggsave(plot = p3,
             filename = file.path(plot_folder, fn3),
             width = 6, height = 5, units = 'in')
      
      
      #plot 4: average HaCFL/HaCBP
      # I don't think this might make sense to do
      p4 <- ggplot() +
        geom_line(data = res_r_p_mean,
                  mapping = aes(x=Year, y=ave_hacflhacbp,
                                color=TxType, linetype=TxIntensity)) +
        scale_x_continuous(breaks = year_breaks) +
        scale_color_manual("Treatment Type", values = c('#1b9e77', '#d95f02', '#7570b3')) +
        scale_linetype_manual("Intensity", values = c('solid', 'dashed', 'dotted')) +
        labs(title = paste(this_reg,
                           this_priority),
             y = 'Mean of (HaCFL/HaCBP)')

      fn4 <- paste0(this_reg, '_', this_priority, '_trtintensity_meanhacflhacbp.jpg')

      ggsave(plot = p4,
             filename = file.path(plot_folder, fn4),
             width = 6, height = 5, units = 'in')


  } # end p priorities
  
} #end r regions
  


