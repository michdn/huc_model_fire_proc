# script to look at treatment groups by time SLICE



### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### User -------------------------------------------------------

# Flag for if baseline should be included
use_baseline <- FALSE

folder_out <- file.path('plots', 'timeslices')

### Base Data import -------------------------------------------

# res_orig <- read_csv(file.path("results",
#                                "datacube", 
#                                "datacube_interim_SNSCCC_20240617.csv")) %>% 
#   mutate(HUC12 = as.character(HUC12)) 

res_orig <- read_csv(file.path("results",
                               "absolute", 
                               "NC_absolute_20240628.csv")) %>% 
  mutate(HUC12 = as.character(HUC12)) 


### Data set up ------------------------------------------------

if (use_baseline){
  
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
  
  
  #baseline/baseweather to be treated as an "intensity" level PER ALL priorities, trts
  res_bases <- res %>% 
    filter(TxIntensity %in% c("base", "bw")) %>% 
    #remove priorities, trts from bases
    dplyr::select(-Priority, -TxType)
  
  #remove bases from rest of results (temporarily)
  res <- res %>% 
    filter(!TxIntensity %in% c("base", "bw"))
  
  #get all combination of Priority and TxTypes
  frame <- res %>% 
    dplyr::select(Priority, TxType) %>% 
    distinct()
  
  #duplicate bases values for all priority-txtype combos
  res_bases <- frame %>% 
    cross_join(res_bases) 
  
  #add back in
  res <- bind_rows(res, res_bases)
  
} else {
  res <- res_orig %>% 
    #remove any baseline
    filter(Priority %in% c("Fire", "WUI", "Hybrid")) %>% 
    #For graphing in the correct order (generic, used in multiple places, with modifications)
    # make factor with set order (priority)
    mutate(Priority = as_factor(Priority),
           Priority = forcats::fct_relevel(Priority,
                                           "Fire", "WUI", "Hybrid"),
           #Make factor with set order (intensity))
           TxIntensity = as_factor(TxIntensity),
           TxIntensity = forcats::fct_relevel(TxIntensity,
                                              "500k", "1m", "2m"))
}


# get the correct timing group for the different priorities
res <- res %>% 
  mutate(timing_group = case_when(
    Priority == "Fire" ~ timeFire, 
    Priority == "WUI" ~ timeWui,
    Priority == "Hybrid" ~ timeHybrid
  )) %>% 
  #and WUI 4th category is too long for graphs
  mutate(timing_group = if_else(timing_group=="2024_2039_yr1to5_16to20",
                                "2024_2039",
                                timing_group))



### Looping version -----------------------------------------------

regions <- res %>% pull(Region) %>% unique()

#REGION (outer loop)
for (r in seq_along(regions)){
  
  this_reg <- regions[[r]]
  
  res_r <- res %>% 
    filter(Region == this_reg)
  
  #output 
  plot_folder <- file.path(folder_out, 
                           this_reg)

  dir.create(plot_folder, recursive = TRUE)
  
  
  #priorities in this region (all same)
  priorities <- res_r %>% pull(Priority) %>% unique()
  

  #PRIORITY (first inner loop)
  # only going in this order (priority then year) b/c using shell of other script
  for (p in seq_along(priorities)){
    
    this_priority <- priorities[[p]]
    
    res_r_p <- res_r %>% 
      filter(Priority == this_priority)
    
    #get years (same years as in all)
    yrs <- res_r_p %>% pull(Year) %>% unique()
    
    
    #YEAR (second inner loop)
    for (y in seq_along(yrs)){
      
      this_year <- yrs[[y]]
      
      #get datasets for this year
      res_r_p_y <- res_r_p %>% 
        filter(Year == this_year)
      
      plot_label <- paste(this_reg, this_year, this_priority)
      
      #expBurn
      burn_plot <- ggplot() +
        geom_boxplot(data = res_r_p_y,
                     mapping = aes(x=TxIntensity, y=expBurn),
                     outlier.color = 'black',
                     outlier.shape = 16,
                     outlier.size = 2,
                     notch = TRUE) + 
        facet_wrap(~TxType+timing_group) +
        labs(title = plot_label,
             x = "Treatment Intensity") + 
        theme_bw()
      
      burn_file <- paste0(this_reg, '_', this_priority, '_', this_year, '_',
                    'boxplot_expBurn.jpg')
      
      ggsave(plot = burn_plot,
             filename = file.path(plot_folder, burn_file),
             width = 6, height = 7, units = 'in')
      
      
      #expFlame
      flame_plot <- ggplot() +
        geom_boxplot(data = res_r_p_y,
                     mapping = aes(x=TxIntensity, y=expFlame),
                     outlier.color = 'black',
                     outlier.shape = 16,
                     outlier.size = 2,
                     notch = TRUE) + 
        facet_wrap(~TxType+timing_group) +
        labs(title = plot_label,
             x = "Treatment Intensity") + 
        theme_bw()
      
      flame_file <- paste0(this_reg, '_', this_priority, '_', this_year, '_',
                          'boxplot_expFlame.jpg')
      
      ggsave(plot = flame_plot,
             filename = file.path(plot_folder, flame_file),
             width = 6, height = 7, units = 'in')
      
      #HaCBP
      hacbp_plot <- ggplot() +
        geom_boxplot(data = res_r_p_y,
                     mapping = aes(x=TxIntensity, y=HaCBP),
                     outlier.color = 'black',
                     outlier.shape = 16,
                     outlier.size = 2,
                     notch = TRUE) + 
        facet_wrap(~TxType+timing_group) +
        labs(title = plot_label,
             x = "Treatment Intensity") +
        theme_bw()
      
      hacbp_file <- paste0(this_reg, '_', this_priority, '_', this_year, '_',
                          'boxplot_hacbp.jpg')
      
      ggsave(plot = hacbp_plot,
             filename = file.path(plot_folder, hacbp_file),
             width = 6, height = 7, units = 'in')
      
      
      #HaCFL
      hacfl_plot <- ggplot() +
        geom_boxplot(data = res_r_p_y,
                     mapping = aes(x=TxIntensity, y=HaCFL),
                     outlier.color = 'black',
                     outlier.shape = 16,
                     outlier.size = 2,
                     notch = TRUE) +
        facet_wrap(~TxType+timing_group) +
        labs(title = plot_label,
             x = "Treatment Intensity") + 
        theme_bw()

      hacfl_file <- paste0(this_reg, '_', this_priority, '_', this_year, '_',
                           'boxplot_hacfl.jpg')

      ggsave(plot = hacfl_plot,
             filename = file.path(plot_folder, hacfl_file),
             width = 6, height = 7, units = 'in')
      
      
    } #end y yrs
  } # end p priority
} # end r region
  
