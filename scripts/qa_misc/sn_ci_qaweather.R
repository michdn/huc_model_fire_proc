### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  viridis)

## data -------------------------------------------------------

ci_raw <- read_csv("qa/qa_weather/CI/top200RH_CI_values.csv")

plot_folder <- file.path('qa', 'qa_weather', "CI")
dir.create(plot_folder, recursive = TRUE)


res_orig <- read_csv(file.path("results",
                               "absolute",
                               "SN_SNbl_SNbw_absolute_20240423.csv")) %>%
  mutate(HUC12 = as.character(HUC12)) 

bl <- res_orig %>% 
  filter(Priority == "baseline") %>% 
  dplyr::select(c(HUC12, Year, HaCFL)) %>% 
  rename(hacfl_base = HaCFL)
  
  
  ## prep ------------------------------------------------------

ci <- ci_raw %>% 
  separate_wider_delim(Scenario, "_",
                       names=c("run", "Region", 
                               "Priority", "TxIntensity", "TxType")) %>% 
  mutate(Priority = if_else(Priority == "RFFC", "Hybrid", Priority)) %>% 
  separate_wider_delim(HUC, "_",
                       names=c(NA, "HUC12")) %>% 
  mutate(Priority = as.factor(Priority),
         Priority = forcats::fct_relevel(Priority,
                                         "Fire", "WUI", "Hybrid"),
         #Make factor with set order (intensity))
         TxIntensity = as.factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                            "500k", "1m", "2m"))


i_colors <- c("Base" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[1],
              "500k" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[2],
              "1m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[3],
              "2m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[4])

year_breaks <- c(2024, 2029, 2034, 2039)


## graph diff_mean + CI -------------------------------------------

# like timeseries line graphs but add
#  geom_errorbar for CI

#don't need to be flexible here
priorities <- ci %>% pull(Priority) %>% unique()
trts <- ci %>% pull(TxType) %>% unique()


for (p in seq_along(priorities)){
  
  this_priority <- priorities[[p]]
  
  #TREATMENT TYPE
  for (t in seq_along(trts)){
    
    this_trt <- trts[[t]]
    
    this_ci <- ci %>% 
      filter(Priority == this_priority,
             TxType == this_trt)
    
    p1 <- ggplot(data = this_ci) +
      geom_line(mapping = aes(x=Year, y=diff_mean, 
                              color=TxIntensity)) +
      geom_point(mapping = aes(x=Year, y=diff_mean, 
                               color=TxIntensity)) +
      geom_errorbar(mapping=aes(x=Year,
                                ymin=diff_lower_CI,
                                ymax=diff_upper_CI,
                                color=TxIntensity)) + 
      scale_x_continuous(breaks = year_breaks) +
      scale_color_manual("Intensity", values = i_colors) + 
      facet_wrap(~HUC12,
                 ncol = 2, scales='free_y') +
      labs(title = paste("Top200RH", 
                         this_priority,
                         this_trt))
    
    fn1 <- paste0('plot_top200RH', "_", this_priority, '_', this_trt, '_',
                  'CI.jpg')
    ggsave(plot = p1,
           filename = file.path(plot_folder, fn1),
           width = 6, height = 3, units = 'in')
    
    
    
  } # end t
} # end p

## graph BASELINE HACFL + diff_mean + CI -------------------------------------------

# like timeseries line graphs but add
#  geom_errorbar for CI

cibl <- ci %>% 
  left_join(bl, by = join_by(HUC12, Year)) 

#don't need to be flexible here
priorities <- cibl %>% pull(Priority) %>% unique()
trts <- cibl %>% pull(TxType) %>% unique()


for (p in seq_along(priorities)){
  
  this_priority <- priorities[[p]]
  
  #TREATMENT TYPE
  for (t in seq_along(trts)){
    
    this_trt <- trts[[t]]
    
    this_cibl <- cibl %>% 
      filter(Priority == this_priority,
             TxType == this_trt)
    
    p1 <- ggplot(data = this_cibl) +
      #baseline
      geom_line(mapping = aes(x=Year, y=hacfl_base,
                              alpha="Base"),
                              color=plasma(n=4, begin=0.1, end = 0.9, 
                                           direction = 1)[1]) + 
      geom_point(mapping=aes(x=Year, y=hacfl_base,
                             alpha="Base"),
                             color=plasma(n=4, begin=0.1, end = 0.9, 
                                          direction = 1)[1]) +
      scale_alpha_manual("Intensity",values=c("Base"=1,
                                              "500k"=1,
                                              "1m"=1,
                                              "2m"=1)) + 
      #intensity CIs
      geom_line(mapping = aes(x=Year, y=hacfl_base+diff_mean, 
                              color=TxIntensity)) +
      geom_point(mapping = aes(x=Year, y=hacfl_base+diff_mean, 
                               color=TxIntensity)) +
      geom_errorbar(mapping=aes(x=Year,
                                ymin=hacfl_base+diff_lower_CI,
                                ymax=hacfl_base+diff_upper_CI,
                                color=TxIntensity)) + 
      scale_x_continuous(breaks = year_breaks) +
      scale_color_manual("Intensity", values = i_colors) + 
      facet_wrap(~HUC12,
                 ncol = 2, scales='free_y') +
      labs(title = paste("Top200RH", 
                         "Difference and CI from baseline: HaCFL:",
                         this_priority,
                         this_trt),
           y="HaCFL")
    
    fn1 <- paste0('plot_top200RH', "_", this_priority, '_', this_trt, '_',
                  'basediffCI.jpg')
    ggsave(plot = p1,
           filename = file.path(plot_folder, fn1),
           width = 6, height = 3, units = 'in')
    
    
    
  } # end t
} # end p
