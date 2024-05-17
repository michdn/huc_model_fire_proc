

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  viridis)

## data -------------------------------------------------------

#grp <- "top200Wind"
#grp <- "top200RH"
#grp <- "temp87to100percentile"
#grp <- "temp74to87percentile"
#grp <- "temp61to74percentile"
#grp <- "WindTempRH98-100_1k"
grp <- "extreme_wind_1k"


if (grp == "extreme_wind_1k"){
  res_orig <- read_csv(file.path("qa",
                                 "qa_weather", 
                                 paste0(grp, "_conditional_NOFVS_20240509.csv")
  )) %>% 
    mutate(HUC12 = as.character(HUC12)) 
  
} else if (grp == "WindTempRH98-100_1k"){
  res_orig <- read_csv(file.path("qa",
                                 "qa_weather", 
                                 paste0(grp, "_conditional_NOFVS_20240508.csv")
  )) %>% 
    mutate(HUC12 = as.character(HUC12)) 
  
} else {
  res_orig <- read_csv(file.path("qa",
                                 "qa_weather", 
                                 paste0(grp, "_conditional_NOFVS_20240502.csv")
  )) %>% 
    mutate(HUC12 = as.character(HUC12)) 
}

if (grp == "top200RH"){
  res_1k <- read_csv(file.path("qa",
                               "qa_weather",
                               # EDIT
                               paste0(grp, "_1k_conditional_NOFVS_20240503.csv"))) %>%
    mutate(HUC12 = as.character(HUC12)) %>%
    mutate(TxIntensity = paste0(TxIntensity, "1k"))
} else if (grp == "top200Wind"){
  res_1k <- read_csv(file.path("qa",
                               "qa_weather",
                               # EDIT
                               paste0(grp, "_1k_conditional_NOFVS_20240506.csv"))) %>%
    mutate(HUC12 = as.character(HUC12)) %>%
    mutate(TxIntensity = paste0(TxIntensity, "1k"))
}


if (!grp == "WindTempRH98-100_1k" & !grp == "extreme_wind_1k"){
  bl <- read_csv(file.path("qa", "qa_weather",
                           paste0(grp, "_baseline_conditional_NOFVS_20240503.csv"))) %>% 
    mutate(HUC12 = as.character(HUC12)) 
  
  bl1k <- read_csv(file.path("qa", "qa_weather",
                             paste0(grp, "_baseline_1k_conditional_NOFVS_20240503.csv"))) %>%
    mutate(HUC12 = as.character(HUC12)) %>%
    mutate(TxIntensity = "base1k")
  
  
  if (grp %in% c("top200RH", "top200Wind")){
    res <- bind_rows(res_orig, res_1k, bl, bl1k)
  } else {
    res <- bind_rows(res_orig, bl, bl1k)
  }
  
} else {
    res <- res_orig
  }



plot_folder <- file.path('qa', 'qa_weather', grp)
dir.create(plot_folder, recursive = TRUE)


#will throw messages for missing baselines/baseweather, fine. 
res <- res %>% 
  #For graphing in the correct order (generic, used in multiple places, with modifications)
  # make factor with set order (priority)
  mutate(Priority = as.factor(Priority),
         Priority = forcats::fct_relevel(Priority,
                                         "Fire", "WUI", "Hybrid", "baseline"),
         #Make factor with set order (intensity))
         TxIntensity = as.factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                            "base1k", "baseline", 
                                            "500k1k", "500k",
                                            "1m1k", "1m",
                                            "2m1k", "2m"),
         #recode so it fits on graphs
         TxIntensity = forcats::fct_recode(TxIntensity, "base" = "baseline"))

year_breaks <- c(2024, 2029, 2034, 2039)


#colors for intensities on line graphs
i_colors <- c("base1k" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[1],
              "base" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[1],
              "500k1k" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[2],
              "500k" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[2],
              "1m1k" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[3],
              "1m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[3],
              "2m1k" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[4], 
              "2m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[4])

i_line <- c("base1k" = "dotted",
            "base" = "solid",
            "500k1k" = "dotted",
            "500k" = "solid",
            "1m1k" = "dotted",
            "1m" = "solid",
            "2m1k" = "dotted", 
            "2m" = "solid")

ft_lines <-  c("surface" = "solid", 
               "passive_crown" = "dashed",
               "active_crown" = "dotted")



### prep and graph ---------------------------------------------------------

#baseline masquerade


#baseline/baseweather to be treated as an 'intensity' level PER ALL priorities, trts
res_bases <- res %>% 
  filter(TxIntensity %in% c("base", "base1k")) %>% 
  #remove priorities, trts from bases
  dplyr::select(-Priority, -TxType)

#remove bases from rest of results (temporarily)
res <- res %>% 
  filter(!TxIntensity %in% c("base", "base1k"))

#get all combination of Priority and TxTypes
frame <- res %>% 
  dplyr::select(Priority, TxType) %>% 
  distinct()

#duplicate bases values for all priority-txtype combos
res_bases <- frame %>% 
  cross_join(res_bases) 

#add back in
res <- res %>% 
  filter(!TxIntensity %in% c("base", "base1k")) %>%
  bind_rows(res_bases)


res <- res %>% 
  mutate(timing_group = case_when(
    Priority == "Fire" ~ timeFire,
    Priority == "Hybrid" ~ timeHybrid,
    Priority == "WUI" ~ timeWui
  ))




#don't need to be flexible here
priorities <- res %>% pull(Priority) %>% unique()
trts <- res %>% pull(TxType) %>% unique()


for (p in seq_along(priorities)){
  
  this_priority <- priorities[[p]]
  
  #TREATMENT TYPE
  for (t in seq_along(trts)){
    
    this_trt <- trts[[t]]
    
    this_res <- res %>% 
      filter(Priority == this_priority,
             TxType == this_trt)
    
    p1 <- ggplot(data = this_res,
                 mapping = aes(x=Year, y=HaCFL, 
                               color=TxIntensity,
                               linetype=TxIntensity)) +
      geom_jitter(shape = 1, height = 0, width = 0.3) +
      geom_line() +
      scale_x_continuous(breaks = year_breaks) +
      scale_color_manual("Intensity", values = i_colors) + 
      scale_linetype_manual("Intensity", values = i_line) +
      facet_wrap(~paste0(timing_group, "\n", HUC12),
                 ncol = 2, scales='free_y') +
      labs(title = paste(grp, 
                         this_priority,
                         this_trt))
    
    fn1 <- paste0('plot_', grp, "_", this_priority, '_', this_trt, '_',
                  'line_freey_', 'HaCFL.jpg')
    ggsave(plot = p1,
           filename = file.path(plot_folder, fn1),
           width = 6, height = 3, units = 'in')
    
    
    #fire type
    # only plain intensities
    this_ft_res <- this_res %>% 
      filter(TxIntensity %in% c("base", "500k", "1m", "2m")) %>%
      dplyr::select(HUC12, timing_group, Year, TxIntensity,
                    surface, passive_crown, active_crown) %>%
      pivot_longer(cols = c(surface, passive_crown, active_crown),
                   names_to = "firetype",
                   values_to = "mean_burned_frac")
    
    p2 <- ggplot() +
      geom_line(data = this_ft_res,
                mapping = aes(x=Year, y=mean_burned_frac, 
                              color=TxIntensity,
                              linetype=firetype)) + 
      scale_x_continuous(breaks = year_breaks) +
      scale_color_manual("Intensity", values = i_colors) + 
      scale_linetype_manual("Fire Type", values = ft_lines) + 
      facet_wrap(~paste0(timing_group, "\n", HUC12),
                 ncol = 2, scales='free_y') + 
      labs(title = paste(grp, 
                         this_priority,
                         this_trt))
      
    fn2 <- paste0('plot_', grp, "_", this_priority, '_', this_trt, '_',
                  'line_freey_', 'firetype.jpg')
    ggsave(plot = p2,
           filename = file.path(plot_folder, fn2),
           width = 6, height = 3, units = 'in')
    

    # ggplot() +
    #   geom_bar(data = ft_long,
    #            aes(x=Year, y=mean_burned_frac,
    #                fill=TxIntensity),
    #            position="dodge", stat="identity") +
    #   scale_fill_manual("Intensity", values = i_colors) + 
    #   scale_x_continuous(breaks = year_breaks) +
    #   facet_wrap(~paste0(timing_group, "\n", HUC12) + firetype,
    #              ncol = 3) + #, scales='free_y') 
    #   labs(title = paste(grp, 
    #                      this_priority,
    #                      this_trt))
    

    # ggplot() + 
    #   geom_bar(data = ft_long,
    #            aes(x=Year, y=mean_burned_frac,
    #                fill=firetype),
    #            position="stack", stat="identity") +
    #   scale_x_continuous(breaks = year_breaks) +
    #   facet_wrap(~paste0(timing_group, "\n", HUC12) + TxIntensity,
    #              ncol = 2, scales='free_y')
    
      
    # ft_l <- c("active_crown" = "solid",
    #           "passive_crown" = "dashed",
    #           "surface" = "dotted")
    # 
    # ggplot(data = ft_long,
    #        mapping = aes(x=Year, y=mean_burned_frac, 
    #                      color=TxIntensity,
    #                      linetype=firetype)) +
    #   geom_jitter(shape = 1, height = 0, width = 0.3) +
    #   geom_line() +
    #   scale_x_continuous(breaks = year_breaks) +
    #   scale_color_manual("Intensity", values = i_colors) + 
    #   scale_linetype_manual("Fire type", values = ft_l) +
    #   facet_wrap(~paste0(timing_group, "\n", HUC12),
    #              ncol = 2, scales='free_y') +
    #   labs(title = paste(grp, 
    #                      this_priority,
    #                      this_trt))
    
  } # end t
} # end p
