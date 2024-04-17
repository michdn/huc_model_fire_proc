# Graphs of Dave's FVS flame length (stands)



### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### user ---------------------------------------------------

#for the boxplots, free or fixed y axis? 
box_scales <- "fixed" #"fixed" #free_y

### Results import -------------------------------------------

res_orig <- read_csv(file.path("results",
                               "absolute", #"datacube", 
                               "SN_SNbl_absolute_20240416.csv")) %>% 
  mutate(HUC12 = as.character(HUC12)) 

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


year_breaks <- c(2024, 2029, 2034, 2039)

#sampled hucs
hucs_sample <- read_csv(file.path('qa', 'samplehucs.csv'))

#colors from default color scheme (to match with expFlame colors)
def_colors <- scales::hue_pal()(4)
#2 : 500k, 3 : 1m, 4 : 2m. 

i_colors <- c("500k" = def_colors[2],
              "1m" = def_colors[3],
              "2m" = def_colors[4])

### FVS FL Data read in and bind ------------------------------------

flsev_files <- list.files(file.path("qa", "fl_sev_CSVs"),
                          full.names = TRUE,
                          pattern = "csv$")

flsev_collector <- list()

for (i in seq_along(flsev_files)){
  
  this_file <- flsev_files[[i]]
  
  this_res <- read_csv(this_file) %>% 
    mutate(HUC12 = as.character(HUC12)) %>% 
    # add in scenario info from file name
    mutate(filename = tools::file_path_sans_ext(basename(this_file))) %>% 
    separate(filename, into = c("run", "Region", 
                                "Priority", "TxIntensity", "TxType",
                                "avg", "by", "huc12")) %>% 
    select(-any_of(c("avg", "by", "huc12"))) %>% 
    mutate(Priority = if_else(Priority == "RFFC", "Hybrid", Priority))
  
  flsev_collector[[i]] <- this_res
}

flsev_all <- do.call(bind_rows, flsev_collector)


### Get timing groups ------------------------------------------- 

res <- flsev_all %>% 
  left_join(res %>% 
              dplyr::select(HUC12, Year, Region, 
                            Priority, TxIntensity, TxType,
                            timeFire, timeHybrid, timeWui),
            by = join_by(HUC12, Year, Region, 
                         Priority, TxIntensity, TxType))


### Loop plot, borrowed code -------------------------------------


regions <- res %>% pull(Region) %>% unique()

#REGION (outer loop)
for (r in seq_along(regions)){
  
  this_reg <- regions[[r]]
  
  res_r <- res %>% 
    filter(Region == this_reg)
  
  
  #output 
  plot_folder <- file.path('plots', 'timeseries_FVS_FL', this_reg)
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
      
      #plot 1 : boxplot all HUCs, FL_sev, timeseries
      p1 <- ggplot() +
        geom_boxplot(data = res_r_p_t,
                     mapping = aes(x = Year, y = FL_sev, group=Year)) +
        scale_x_continuous(breaks = year_breaks) +
        scale_color_manual(values = i_colors) + 
        facet_wrap(~TxIntensity+timing_group, scales = box_scales) +
        labs(title = paste(this_reg,
                           this_priority,
                           this_trt))
      
      fn1 <- paste0(this_reg, '_', this_priority, '_', this_trt, '_',
                    'boxplot_', box_scales, '_flsev.jpg')
      ggsave(plot = p1,
             filename = file.path(plot_folder, fn1),
             width = 8, height = 8, units = 'in')
      

      #second set, selected HUCs only, time series line
      
      #plot 6: sampled line, FL sev, timeseries
      p6 <- ggplot(data = res_r_p_t_sample,
                   mapping = aes(x=Year, y=FL_sev, color=TxIntensity)) +
        geom_jitter(shape = 1, height = 0, width = 0.3) +
        geom_line() +
        scale_x_continuous(breaks = year_breaks) +
        scale_color_manual(values = i_colors) + 
        facet_wrap(~paste0(timing_group, " ", HUC12),
                   ncol = 2, scales='free_y') +
        labs(title = paste(this_reg,
                           this_priority,
                           this_trt,
                           "Selected HUCs"))
      
      fn6 <- paste0('sampled_', this_reg, '_', this_priority, '_', this_trt, '_',
                    'line_freey_', 'flsev.jpg')
      ggsave(plot = p6,
             filename = file.path(sampled_folder, fn6),
             width = 7, height = 7, units = 'in')
      

    } # end t trt
    
  } # end p priorities
  
} #end r regions


