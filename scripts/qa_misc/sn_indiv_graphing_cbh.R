

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  viridis)

## data -------------------------------------------------------

grp <- "subset2"
res_orig <- read_csv(file.path("qa",
                               "qa_cbh", 
                               paste0(grp, "_conditional_NOFVS_20240521.csv")))

# grp <- "subset"
# res_orig <- read_csv(file.path("qa",
#                                "qa_cbh", 
#                                paste0(grp, "_conditional_NOFVS_20240520.csv")))


plot_folder <- file.path('qa', 'qa_cbh', grp)
dir.create(plot_folder, recursive = TRUE)


res <- res_orig %>% 
  #For graphing in the correct order (generic, used in multiple places, with modifications)
  # make factor with set order (priority)
  mutate(Priority = as.factor(Priority),
         Priority = forcats::fct_relevel(Priority,
                                         "Fire", "WUI", "Hybrid"),
         #Make factor with set order (intensity))
         TxIntensity = as.factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                            "500k","1m","2m"))

year_breaks <- c(2024, 2029, 2034, 2039)


#colors for intensities on line graphs
i_colors <- c("500k" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[2],
              "1m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[3],
              "2m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[4])

# i_line <- c("500k" = "solid",
#             "1m" = "solid",
#             "2m" = "solid")

#https://stackoverflow.com/questions/53071983/getting-n-shades-of-a-given-color-in-hex-code
#darken each color n times in increments of steps towards black
ExpandColors <- function(colors, n, steps = 11){
  if(n <= steps){
    suppressWarnings({
      sapply(colors, function(x){colorRampPalette(c(x, "#000000"))(steps)}) %>% 
        as.data.frame() %>% 
        filter(row_number() <= n) %>% 
        gather(key = original.color, value = expanded.color)
    })
  }else{
    warning("Select n < steps!")
  }
}

col_2m <- ExpandColors(i_colors["2m"], n=5) %>% 
  bind_cols(cbh_kept = c(10, 30, 50, 70, 90)) %>% 
  dplyr::select(cbh_kept, expanded.color)

colors_2m <- setNames(as.character(col_2m$expanded.color), col_2m$cbh_kept)

col_1m <- ExpandColors(i_colors["1m"], n=5) %>% 
  bind_cols(cbh_kept = c(10, 30, 50, 70, 90)) %>% 
  dplyr::select(cbh_kept, expanded.color)

colors_1m <- setNames(as.character(col_1m$expanded.color), col_1m$cbh_kept)

col_500k <- ExpandColors(i_colors["500k"], n=5) %>% 
  bind_cols(cbh_kept = c(10, 30, 50, 70, 90)) %>% 
  dplyr::select(cbh_kept, expanded.color)

colors_500k <- setNames(as.character(col_500k$expanded.color), col_500k$cbh_kept)


ft_lines <-  c("surface" = "solid", 
               "passive_crown" = "dashed",
               "active_crown" = "dotted")



### prep and graph ---------------------------------------------------------

#baseline masquerade


# #baseline/baseweather to be treated as an 'intensity' level PER ALL priorities, trts
# res_bases <- res %>% 
#   filter(TxIntensity %in% c("base", "base1k")) %>% 
#   #remove priorities, trts from bases
#   dplyr::select(-Priority, -TxType)
# 
# #remove bases from rest of results (temporarily)
# res <- res %>% 
#   filter(!TxIntensity %in% c("base", "base1k"))
# 
# #get all combination of Priority and TxTypes
# frame <- res %>% 
#   dplyr::select(Priority, TxType) %>% 
#   distinct()
# 
# #duplicate bases values for all priority-txtype combos
# res_bases <- frame %>% 
#   cross_join(res_bases) 
# 
# #add back in
# res <- res %>% 
#   filter(!TxIntensity %in% c("base", "base1k")) %>%
#   bind_rows(res_bases)


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
    
    # p1_2m <- ggplot(data = this_res %>% 
    #                   filter(TxIntensity == "2m"),
    #                 mapping = aes(x=Year, y=HaCFL, 
    #                               color=as.factor(cbh_kept))) +
    #   geom_point(shape = 1) +
    #   geom_line() +
    #   scale_x_continuous(breaks = year_breaks) +
    #   scale_color_manual("CBH kept", values = colors_2m) + 
    #   facet_wrap(~paste0(timing_group, "\n", HUC12),
    #              ncol = 2, scales='free_y') +
    #   labs(title = paste("CBH QA subset: ",
    #                      this_priority,
    #                      this_trt, 
    #                      "2m"))
    # 
    # 
    # p1_1m <- ggplot(data = this_res %>% 
    #                   filter(TxIntensity == "1m"),
    #                 mapping = aes(x=Year, y=HaCFL, 
    #                               color=as.factor(cbh_kept))) +
    #   geom_point(shape = 1) +
    #   geom_line() +
    #   scale_x_continuous(breaks = year_breaks) +
    #   scale_color_manual("CBH kept", values = colors_1m) + 
    #   facet_wrap(~paste0(timing_group, "\n", HUC12),
    #              ncol = 2, scales='free_y') +
    #   labs(title = paste("CBH QA subset: ",
    #                      this_priority,
    #                      this_trt, 
    #                      "1m"))
    # 
    # 
    # p1_500k <- ggplot(data = this_res %>% 
    #                   filter(TxIntensity == "500k"),
    #                 mapping = aes(x=Year, y=HaCFL, 
    #                               color=as.factor(cbh_kept))) +
    #   geom_point(shape = 1) +
    #   geom_line() +
    #   scale_x_continuous(breaks = year_breaks) +
    #   scale_color_manual("CBH kept", values = colors_500k) + 
    #   facet_wrap(~paste0(timing_group, "\n", HUC12),
    #              ncol = 2, scales='free_y') +
    #   labs(title = paste("CBH QA subset: ",
    #                      this_priority,
    #                      this_trt, 
    #                      "500k"))
    
    
    p1 <- ggplot(data = this_res,
                 mapping = aes(x=Year, y=HaCFL,color=TxIntensity)) + 
      geom_point(shape=1) + 
      geom_line() + 
        scale_x_continuous(breaks = year_breaks) +
        scale_color_manual("Intensity", values = i_colors) +
        facet_wrap(~paste0(timing_group, "\n", HUC12) + cbh_kept,
                   ncol = 2, scales='free_y', dir="v") +
        labs(title = paste("CBH QA subset: ",
                           this_priority,
                           this_trt))
      
    
    fn1 <- paste0('plot_CBH_', grp, "_", this_priority, '_', this_trt, '_',
                  'line_', 'HaCFL.jpg')
    
    
    
    ggsave(plot = p1,
           filename = file.path(plot_folder, fn1),
           width = 8, height = 10, units = 'in')
    
    
    
    
    
    
    
    #fire type
    this_ft_res <- this_res %>% 
      dplyr::select(HUC12, timing_group, cbh_kept, Year, TxIntensity, 
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
      facet_wrap(~paste0(timing_group, "\n", HUC12) + cbh_kept,
                 ncol = 2, scales='free_y', dir="v") + 
      labs(title = paste("CBH QA subset", 
                         this_priority,
                         this_trt))
    
    fn2 <- paste0('plot_CBH_', grp, "_", this_priority, '_', this_trt, '_',
                  'line_', 'firetype.jpg')
    ggsave(plot = p2,
           filename = file.path(plot_folder, fn2),
           width = 8, height = 10, units = 'in')
    
    
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
