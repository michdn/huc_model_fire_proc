# script to look results by time SLICE

# per region, priority, trt

# facet by year
# x = intensity
# y = average expBurn

# color = treated (by year) vs untreated 


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### Base Data import -------------------------------------------

# res_orig <- read_csv(file.path('results_csv', 
#                                'datacube_weighted_20240212.csv')) %>% 
#   mutate(HUC12 = as.character(HUC12))

res_orig <- read_csv(file.path('run_202401_badblend',
                               'results_raw_extraction_test',
                               'SC_absolute_TEST20240307.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

### Data set up ------------------------------------------------

res <- res_orig %>% 
  #For graphing in the correct order
  # make factor with set order (priority)
  # Want priority order: 'Fire', 'WUI', 'Hybrid'
  # Want intensity order: '500k', '1m', '2m'
  mutate(Priority = as.factor(Priority),
         Priority = forcats::fct_relevel(Priority,
                                         "Fire", "WUI", "Hybrid"),
         #Make factor with set order (intensity))
         TxIntensity = as.factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                            "500k", "1m", "2m")) %>% 
  #make field to match on year for first treat
  mutate(fireGrpYr = case_when(
    fireGroup == 25 ~ 2024,
    fireGroup == 50 ~ 2029,
    fireGroup == 75 ~ 2034,
    fireGroup == 100 ~ 2039,
    .default = 9999
  )) %>% 
  mutate(wuiGrpYr = case_when(
    wuiGroup == 25 ~ 2024, #first treat
    wuiGroup == 50 ~ 2029,
    wuiGroup == 75 ~ 2034,
    wuiGroup == 100 ~ 9998, #never treated
    .default = 9999
  )) %>% 
  mutate(hybridGrpYr = case_when(
    hybridGroup == 25 ~ 2024,
    hybridGroup == 50 ~ 2029,
    hybridGroup == 75 ~ 2034,
    hybridGroup == 100 ~ 2039,
    .default = 9999
  ))


### Looping version -----------------------------------------------

regions <- res %>% pull(Region) %>% unique()

#REGION (outer loop)
for (r in seq_along(regions)){
  
  this_reg <- regions[[r]]
  
  res_r <- res %>% 
    filter(Region == this_reg)
  
  
  #output 
  plot_folder <- file.path('plots', 'timeslice_intensity', this_reg)

  dir.create(plot_folder, recursive = TRUE)
  
  
  #priorities in this region (all same)
  priorities <- res_r %>% pull(Priority) %>% unique()
  
  #PRIORITY (first inner loop)
  # only going in this order (priority then year) b/c using shell of other script
  for (p in seq_along(priorities)){
    
    this_priority <- priorities[[p]]
    
    res_r_p <- res_r %>% 
      filter(Priority == this_priority)
    
    # get the correct timing group for the different priorities
    # set as a known field name (to avoid passing field name as variable)
    if (this_priority == "Fire"){
      res_r_p <- res_r_p %>% 
        mutate(timing_year = fireGrpYr)
    } else if (this_priority == "WUI"){
      res_r_p <- res_r_p %>% 
        mutate(timing_year = wuiGrpYr)
    } else if (this_priority == "Hybrid"){
      res_r_p <- res_r_p %>% 
        mutate(timing_year = hybridGrpYr)
    } else {
      stop("Unmatched priority timing group")
    }
    

    #get trt type (may not be same)
    trts <- res_r %>% pull(TxType) %>% unique()
    
    
    #TRT (second inner loop)
    for (t in seq_along(trts)){
      
      this_trt <- trts[[t]]
      
      #get data for this region, priority, trt
      res_r_p_t <- res_r_p %>% 
        filter(TxType == this_trt)
      
      plot_label <- paste(this_reg, this_priority, this_trt)
      
      
      #summarize expBurn by treat vs untreat at this point in time
      #timing_year <= Year
      
      treated <- res_r_p_t %>% 
        #treatment year less than or is (that row's) Year
        filter(timing_year <= Year) %>% 
        mutate(treat_status = 'Treated')
      
      untreated <- res_r_p_t %>% 
        #treatment year is greater than this row's year
        filter(timing_year > Year) %>% 
        mutate(treat_status = 'Untreated')
      
      combo_summary <- treated %>% 
        bind_rows(untreated) %>% 
        #summarize
        group_by(treat_status, TxIntensity, Year) %>% 
        summarize(ave_expBurn = mean(expBurn))
        
      
      #plot ave expBurn
      burn_plot <- ggplot() +
        geom_line(data = combo_summary,
                  mapping = aes(x=TxIntensity,
                                y=ave_expBurn,
                                linetype=treat_status,
                                group=treat_status)) + 
        scale_linetype_manual("Treated by Year", values = c('solid', 'dotted')) +
        facet_wrap(~Year) +
        labs(title = plot_label,
             x = "Treatment Intensity",
             y = "Average expBurn")
      
      burn_file <- paste0(this_reg, '_', this_priority, '_', this_trt, '_',
                    'intensity_slices_expBurn.jpg')
      
      ggsave(plot = burn_plot,
             filename = file.path(plot_folder, burn_file),
             width = 5, height = 7, units = 'in')
      
      
      
      
    } #end y yrs
  } # end p priority
} # end r region
  
