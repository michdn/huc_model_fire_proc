# script to look at treatment groups by time SLICE

# pulls out baseline (and baseweather?) and adds them as 'intensities' to each


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### Base Data import -------------------------------------------

res_orig <- read_csv(file.path("results",
                               "absolute", #"datacube", 
                               "SN_SNbl_absolute_20240410.csv")) %>% 
  mutate(HUC12 = as.character(HUC12)) 

# res_orig <- read_csv(file.path("results",
#                           "datacube", 
#                           "datacube_interim_sc_cc_sn_20240403.csv")) %>% 
#   mutate(HUC12 = as.character(HUC12)) 


### Data set up ------------------------------------------------

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

# #make clear labels
  # mutate(fireGrpLbl = case_when(
  #   fireGroup == 25 ~ "2024 (25)",
  #   fireGroup == 50 ~ "2029 (50)",
  #   fireGroup == 75 ~ "2034 (75)",
  #   fireGroup == 100 ~ "2039 (100)",
  #   .default = as.character(fireGroup)
  # )) %>% 
  # mutate(wuiGrpLbl = case_when(
  #   wuiGroup == 25 ~ "2024_2039 (25)",
  #   wuiGroup == 50 ~ "2029 (50)",
  #   wuiGroup == 75 ~ "2034 (75)",
  #   wuiGroup == 100 ~ "not treated",
  #   .default = as.character(wuiGroup)
  # )) %>% 
  # mutate(hybridGrpLbl = case_when(
  #   hybridGroup == 25 ~ "2024 (25)",
  #   hybridGroup == 50 ~ "2029 (50)",
  #   hybridGroup == 75 ~ "2034 (75)",
  #   hybridGroup == 100 ~ "2039 (100)",
  #   .default = as.character(hybridGroup)
  # ))

#NOTE: Versions past 2024-03-20 have timeFire, timeHybrid, timeWUI
# use these for EXTERNAL, but can use above (since already written) for internal

#baseline/baseweather to be treated as an 'intensity' level PER ALL priorities, trts
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


### Looping version -----------------------------------------------

regions <- res %>% pull(Region) %>% unique()

#REGION (outer loop)
for (r in seq_along(regions)){
  
  this_reg <- regions[[r]]
  
  res_r <- res %>% 
    filter(Region == this_reg)
  
  #output 
  plot_folder <- file.path('plots', 'timeslices', this_reg)

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
    

    #get years (same years as in all)
    yrs <- res_r_p %>% pull(Year) %>% unique()
    
    
    #YEAR (second inner loop)
    for (y in seq_along(yrs)){
      
      this_year <- yrs[[y]]
      
      #get full and sampled datasets for this year
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
             x = "Treatment Intensity")
      
      burn_file <- paste0(this_reg, '_', this_priority, '_', this_year, '_',
                    'boxplot_expBurn.jpg')
      
      ggsave(plot = burn_plot,
             filename = file.path(plot_folder, burn_file),
             width = 5.5, height = 7, units = 'in')
      
      
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
             x = "Treatment Intensity")
      
      flame_file <- paste0(this_reg, '_', this_priority, '_', this_year, '_',
                          'boxplot_expFlame.jpg')
      
      ggsave(plot = flame_plot,
             filename = file.path(plot_folder, flame_file),
             width = 5.5, height = 7, units = 'in')
      
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
             x = "Treatment Intensity")
      
      hacbp_file <- paste0(this_reg, '_', this_priority, '_', this_year, '_',
                          'boxplot_hacbp.jpg')
      
      ggsave(plot = hacbp_plot,
             filename = file.path(plot_folder, hacbp_file),
             width = 5.5, height = 7, units = 'in')
      
      
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
             x = "Treatment Intensity")

      hacfl_file <- paste0(this_reg, '_', this_priority, '_', this_year, '_',
                           'boxplot_hacfl.jpg')

      ggsave(plot = hacfl_plot,
             filename = file.path(plot_folder, hacfl_file),
             width = 5.5, height = 7, units = 'in')
      
      
    } #end y yrs
  } # end p priority
} # end r region
  











# ### Regions -------------------------------------------------------
# 
# rsc <- res %>% 
#   filter(Region == "SC")
# 
# rnc <- res %>% 
#   filter(Region == "NC")
# 
# rsn <- res %>% 
#   filter(Region == "SN")
# 
# rcc <- res %>% 
#   filter(Region == "CC")
# 
# 
# ### SC -------------------------------------------------------
# 
# ## Fire
# 
# # 2024
# 
# # groups test --
# 
# ggplot() +
#   geom_boxplot(data = rsc %>%
#                  filter(Year == "2024", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "SC 2024 Fire fireGroup",
#        x = "Treatment Intensity")
# 
# ggplot() +
#   geom_boxplot(data = rsc %>%
#                  filter(Year == "2024", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+wuiGroup) +
#   labs(title = "SC 2024 Fire wuiGroup",
#        x = "Treatment Intensity")
# 
# ggplot() +
#   geom_boxplot(data = rsc %>%
#                  filter(Year == "2024", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+hybridGroup) +
#   labs(title = "SC 2024 Fire hybridGroup",
#        x = "Treatment Intensity")
# 
# #nope, not a case of mismatched priority groups
# 
# #--
# 
# #expBurn
# ggplot() +
#   geom_boxplot(data = rsc %>%
#                  filter(Year == "2024", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expBurn),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "SC 2024 Fire",
#        x = "Treatment Intensity")
# 
# #expFlame
# ggplot() +
#   geom_boxplot(data = rsc %>%
#                  filter(Year == "2024", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "SC 2024 Fire",
#        x = "Treatment Intensity")
# 
# #flame/burn
# ggplot() +
#   geom_boxplot(data = rsc %>%
#                  filter(Year == "2024", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame/expBurn),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "SC 2024 Fire",
#        x = "Treatment Intensity")
# 
# #2029 
# ggplot() +
#   geom_boxplot(data = rsc %>%
#                  filter(Year == "2029", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "SC 2029 Fire",
#        x = "Treatment Intensity")
# 
# #2034
# ggplot() +
#   geom_boxplot(data = rsc %>%
#                  filter(Year == "2034", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "SC 2034 Fire",
#        x = "Treatment Intensity")
# 
# #2044
# ggplot() +
#   geom_boxplot(data = rsc %>%
#                  filter(Year == "2044", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "SC 2044 Fire",
#        x = "Treatment Intensity")
# 
# 
# # WUI
# ggplot() +
#   geom_boxplot(data = rsc %>%
#                  filter(Year == "2024", 
#                         Priority == "WUI"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+wuiGroup) +
#   labs(title = "SC 2024 WUI",
#        x = "Treatment Intensity")
# 
# #2029 
# ggplot() +
#   geom_boxplot(data = rsc %>%
#                  filter(Year == "2029", 
#                         Priority == "WUI"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+wuiGroup) +
#   labs(title = "SC 2029 WUI",
#        x = "Treatment Intensity")
# 
# #2034 
# ggplot() +
#   geom_boxplot(data = rsc %>%
#                  filter(Year == "2034", 
#                         Priority == "WUI"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+wuiGroup) +
#   labs(title = "SC 2034 WUI",
#        x = "Treatment Intensity")
# 
# #2044 
# ggplot() +
#   geom_boxplot(data = rsc %>%
#                  filter(Year == "2044", 
#                         Priority == "WUI"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+wuiGroup) +
#   labs(title = "SC 2044 WUI",
#        x = "Treatment Intensity")
# 
# 
# # Hybrid
# ggplot() +
#   geom_boxplot(data = rsc %>%
#                  filter(Year == "2024", 
#                         Priority == "Hybrid"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+hybridGroup) +
#   labs(title = "SC 2024 Hybrid",
#        x = "Treatment Intensity")
# 
# #2029 
# ggplot() +
#   geom_boxplot(data = rsc %>%
#                  filter(Year == "2029", 
#                         Priority == "Hybrid"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+hybridGroup) +
#   labs(title = "SC 2029 Hybrid",
#        x = "Treatment Intensity")
# 
# #2034 
# ggplot() +
#   geom_boxplot(data = rsc %>%
#                  filter(Year == "2034", 
#                         Priority == "Hybrid"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+hybridGroup) +
#   labs(title = "SC 2034 Hybrid",
#        x = "Treatment Intensity")
# 
# #2044 
# ggplot() +
#   geom_boxplot(data = rsc %>%
#                  filter(Year == "2044", 
#                         Priority == "Hybrid"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+hybridGroup) +
#   labs(title = "SC 2044 Hybrid",
#        x = "Treatment Intensity")
# 
# 
# ### NC -------------------------------------------------------
# 
# ## Fire
# 
# # 2024
# 
# #expBurn
# ggplot() +
#   geom_boxplot(data = rnc %>%
#                  filter(Year == "2024", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expBurn/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "NC 2024 Fire",
#        x = "Treatment Intensity")
# 
# #expFlame
# ggplot() +
#   geom_boxplot(data = rnc %>%
#                  filter(Year == "2024", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "NC 2024 Fire",
#        x = "Treatment Intensity")
# 
# #flame/burn
# ggplot() +
#   geom_boxplot(data = rnc %>%
#                  filter(Year == "2024", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame/expBurn),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "NC 2024 Fire",
#        x = "Treatment Intensity")
# 
# #2029 
# ggplot() +
#   geom_boxplot(data = rnc %>%
#                  filter(Year == "2029", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "NC 2029 Fire",
#        x = "Treatment Intensity")
# 
# #2034
# ggplot() +
#   geom_boxplot(data = rnc %>%
#                  filter(Year == "2034", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "NC 2034 Fire",
#        x = "Treatment Intensity")
# 
# #2044
# ggplot() +
#   geom_boxplot(data = rnc %>%
#                  filter(Year == "2044", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "NC 2044 Fire",
#        x = "Treatment Intensity")
# 
# 
# # WUI
# ggplot() +
#   geom_boxplot(data = rnc %>%
#                  filter(Year == "2024", 
#                         Priority == "WUI"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+wuiGroup) +
#   labs(title = "NC 2024 WUI",
#        x = "Treatment Intensity")
# 
# #2029 
# ggplot() +
#   geom_boxplot(data = rnc %>%
#                  filter(Year == "2029", 
#                         Priority == "WUI"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+wuiGroup) +
#   labs(title = "NC 2029 WUI",
#        x = "Treatment Intensity")
# 
# #2034 
# ggplot() +
#   geom_boxplot(data = rnc %>%
#                  filter(Year == "2034", 
#                         Priority == "WUI"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+wuiGroup) +
#   labs(title = "NC 2034 WUI",
#        x = "Treatment Intensity")
# 
# #2044 
# ggplot() +
#   geom_boxplot(data = rnc %>%
#                  filter(Year == "2044", 
#                         Priority == "WUI"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+wuiGroup) +
#   labs(title = "NC 2044 WUI",
#        x = "Treatment Intensity")
# 
# 
# # Hybrid
# ggplot() +
#   geom_boxplot(data = rnc %>%
#                  filter(Year == "2024", 
#                         Priority == "Hybrid"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+hybridGroup) +
#   labs(title = "NC 2024 Hybrid",
#        x = "Treatment Intensity")
# 
# #2029 
# ggplot() +
#   geom_boxplot(data = rnc %>%
#                  filter(Year == "2029", 
#                         Priority == "Hybrid"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+hybridGroup) +
#   labs(title = "NC 2029 Hybrid",
#        x = "Treatment Intensity")
# 
# #2034 
# ggplot() +
#   geom_boxplot(data = rnc %>%
#                  filter(Year == "2034", 
#                         Priority == "Hybrid"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+hybridGroup) +
#   labs(title = "NC 2034 Hybrid",
#        x = "Treatment Intensity")
# 
# #2044 
# ggplot() +
#   geom_boxplot(data = rnc %>%
#                  filter(Year == "2044", 
#                         Priority == "Hybrid"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+hybridGroup) +
#   labs(title = "NC 2044 Hybrid",
#        x = "Treatment Intensity")
# 
# 
# 
# 
# ### SN -------------------------------------------------------
# 
# ## Fire
# 
# # 2024
# 
# #expBurn
# ggplot() +
#   geom_boxplot(data = rsn %>%
#                  filter(Year == "2024", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expBurn),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "SN 2024 Fire",
#        x = "Treatment Intensity")
# 
# #expFlame
# ggplot() +
#   geom_boxplot(data = rsn %>%
#                  filter(Year == "2024", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "SN 2024 Fire",
#        x = "Treatment Intensity")
# 
# #flame/burn
# ggplot() +
#   geom_boxplot(data = rsn %>%
#                  filter(Year == "2024", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame/expBurn),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "SN 2024 Fire",
#        x = "Treatment Intensity")
# 
# #2029 
# ggplot() +
#   geom_boxplot(data = rsn %>%
#                  filter(Year == "2029", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "SN 2029 Fire",
#        x = "Treatment Intensity")
# 
# 
# ggplot() +
#   geom_boxplot(data = rsn %>%
#                  filter(Year == "2029", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expBurn),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "SN 2029 Fire",
#        x = "Treatment Intensity")
# 
# 
# #2034
# ggplot() +
#   geom_boxplot(data = rsn %>%
#                  filter(Year == "2034", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "SN 2034 Fire",
#        x = "Treatment Intensity")
# 
# #2044
# ggplot() +
#   geom_boxplot(data = rsn %>%
#                  filter(Year == "2044", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "SN 2044 Fire",
#        x = "Treatment Intensity")
# 
# 
# # WUI
# ggplot() +
#   geom_boxplot(data = rsn %>%
#                  filter(Year == "2024", 
#                         Priority == "WUI"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+wuiGroup) +
#   labs(title = "SN 2024 WUI",
#        x = "Treatment Intensity")
# 
# #2029 
# ggplot() +
#   geom_boxplot(data = rsn %>%
#                  filter(Year == "2029", 
#                         Priority == "WUI"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+wuiGroup) +
#   labs(title = "SN 2029 WUI",
#        x = "Treatment Intensity")
# 
# #2034 
# ggplot() +
#   geom_boxplot(data = rsn %>%
#                  filter(Year == "2034", 
#                         Priority == "WUI"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+wuiGroup) +
#   labs(title = "SN 2034 WUI",
#        x = "Treatment Intensity")
# 
# #2044 
# ggplot() +
#   geom_boxplot(data = rsn %>%
#                  filter(Year == "2044", 
#                         Priority == "WUI"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+wuiGroup) +
#   labs(title = "SN 2044 WUI",
#        x = "Treatment Intensity")
# 
# 
# # Hybrid
# ggplot() +
#   geom_boxplot(data = rsn %>%
#                  filter(Year == "2024", 
#                         Priority == "Hybrid"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+hybridGroup) +
#   labs(title = "SN 2024 Hybrid",
#        x = "Treatment Intensity")
# 
# #2029 
# ggplot() +
#   geom_boxplot(data = rsn %>%
#                  filter(Year == "2029", 
#                         Priority == "Hybrid"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+hybridGroup) +
#   labs(title = "SN 2029 Hybrid",
#        x = "Treatment Intensity")
# 
# #2034 
# ggplot() +
#   geom_boxplot(data = rsn %>%
#                  filter(Year == "2034", 
#                         Priority == "Hybrid"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+hybridGroup) +
#   labs(title = "SN 2034 Hybrid",
#        x = "Treatment Intensity")
# 
# #2044 
# ggplot() +
#   geom_boxplot(data = rsn %>%
#                  filter(Year == "2044", 
#                         Priority == "Hybrid"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+hybridGroup) +
#   labs(title = "SN 2044 Hybrid",
#        x = "Treatment Intensity")
# 
# ### CC -------------------------------------------------------
# 
# ## Fire
# 
# # 2024
# 
# #expBurn
# ggplot() +
#   geom_boxplot(data = rcc %>%
#                  filter(Year == "2024", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expBurn/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "CC 2024 Fire",
#        x = "Treatment Intensity")
# 
# #expFlame
# ggplot() +
#   geom_boxplot(data = rcc %>%
#                  filter(Year == "2024", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "CC 2024 Fire",
#        x = "Treatment Intensity")
# 
# #flame/burn
# ggplot() +
#   geom_boxplot(data = rcc %>%
#                  filter(Year == "2024", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame/expBurn),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "CC 2024 Fire",
#        x = "Treatment Intensity")
# 
# #2029 
# ggplot() +
#   geom_boxplot(data = rcc %>%
#                  filter(Year == "2029", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "CC 2029 Fire",
#        x = "Treatment Intensity")
# 
# #2034
# ggplot() +
#   geom_boxplot(data = rcc %>%
#                  filter(Year == "2034", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "CC 2034 Fire",
#        x = "Treatment Intensity")
# 
# #2044
# ggplot() +
#   geom_boxplot(data = rcc %>%
#                  filter(Year == "2044", 
#                         Priority == "Fire"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+fireGroup) +
#   labs(title = "CC 2044 Fire",
#        x = "Treatment Intensity")
# 
# 
# # WUI
# ggplot() +
#   geom_boxplot(data = rcc %>%
#                  filter(Year == "2024", 
#                         Priority == "WUI"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+wuiGroup) +
#   labs(title = "CC 2024 WUI",
#        x = "Treatment Intensity")
# 
# #2029 
# ggplot() +
#   geom_boxplot(data = rcc %>%
#                  filter(Year == "2029", 
#                         Priority == "WUI"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+wuiGroup) +
#   labs(title = "CC 2029 WUI",
#        x = "Treatment Intensity")
# 
# #2034 
# ggplot() +
#   geom_boxplot(data = rcc %>%
#                  filter(Year == "2034", 
#                         Priority == "WUI"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+wuiGroup) +
#   labs(title = "CC 2034 WUI",
#        x = "Treatment Intensity")
# 
# #2044 
# ggplot() +
#   geom_boxplot(data = rcc %>%
#                  filter(Year == "2044", 
#                         Priority == "WUI"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+wuiGroup) +
#   labs(title = "CC 2044 WUI",
#        x = "Treatment Intensity")
# 
# 
# # Hybrid
# ggplot() +
#   geom_boxplot(data = rcc %>%
#                  filter(Year == "2024", 
#                         Priority == "Hybrid"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+hybridGroup) +
#   labs(title = "CC 2024 Hybrid",
#        x = "Treatment Intensity")
# 
# #2029 
# ggplot() +
#   geom_boxplot(data = rcc %>%
#                  filter(Year == "2029", 
#                         Priority == "Hybrid"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+hybridGroup) +
#   labs(title = "CC 2029 Hybrid",
#        x = "Treatment Intensity")
# 
# #2034 
# ggplot() +
#   geom_boxplot(data = rcc %>%
#                  filter(Year == "2034", 
#                         Priority == "Hybrid"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+hybridGroup) +
#   labs(title = "CC 2034 Hybrid",
#        x = "Treatment Intensity")
# 
# #2044 
# ggplot() +
#   geom_boxplot(data = rcc %>%
#                  filter(Year == "2044", 
#                         Priority == "Hybrid"),
#                mapping = aes(x=TxIntensity, y=expFlame/hucAc),
#                outlier.color = 'black',
#                outlier.shape = 16,
#                outlier.size = 2,
#                notch = TRUE) + 
#   facet_wrap(~TxType+hybridGroup) +
#   labs(title = "CC 2044 Hybrid",
#        x = "Treatment Intensity")
# 
# 
# ## --------------------------------------
# 
# res %>% 
#   select(HUC12, fireGroup, wuiGroup, hybridGroup) %>% 
#   unique() %>% 
#   group_by(fireGroup, wuiGroup, hybridGroup) %>% 
#   summarize(count = n()) %>% 
#   View()
# 
# res %>% 
#   select(HUC12, fireGroup, wuiGroup, hybridGroup) %>% 
#   unique() %>% 
#   group_by(fireGroup) %>% 
#   summarize(firecount = n()) 
# 
# res %>% 
#   select(HUC12, fireGroup, wuiGroup, hybridGroup) %>% 
#   unique() %>% 
#   group_by(wuiGroup) %>% 
#   summarize(wuicount = n()) 
# 
# res %>% 
#   select(HUC12, fireGroup, wuiGroup, hybridGroup) %>% 
#   unique() %>% 
#   group_by(hybridGroup) %>% 
#   summarize(hybridcount = n()) 
# 
# 
# ## ----------------------------------------------------------
# 
# # res
# # rsc # 2m
# # #facet by time
# # 
# # ggplot() +
# #   geom_boxplot(data = rsc %>% 
# #     filter(Priority == "Fire", 
# #            TxIntensity == "2m"),
# #            #fireGroup == 50,
# #            #HUC12 == 180701020701),
# #     mapping = aes(x = Year, y = HaCFL, group=Year)) +
# #   facet_wrap(~TxType+fireGroup)
# # 
# # 
# # 
# # #180701020701 group50
# # 
# # ggplot() + 
# #   geom_line(data = rsc %>% 
# #               filter(Priority == "Fire",
# #                      TxIntensity == "2m"),
# #             mapping = aes(Year, y=HaCFL)) +
# #   facet_wrap(~TxType+fireGroup)
# 
# 
