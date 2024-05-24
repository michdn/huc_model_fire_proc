# firetype and HaCBP double checks

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### Input -------------------------------------------------

#region or folder name for variants/reruns #"SC", "SCbl", "SCbw", etc. 
reg_code <- "SN"

input_folder <- file.path('results', 'extracts')


cbp <- readRDS(file.path(input_folder, 
                         paste0(reg_code, '_cbp_all_fires_from_sql.RDS'))) 

# cfl <- readRDS(file.path(input_folder, 
#                          paste0(reg_code, '_cfl_all_fires_from_sql.RDS'))) 

cft_hist <- readRDS(file.path(input_folder, 
                              paste0(reg_code, '_cft_hist_from_sql.RDS')))


### Calculate per fire totals and compare -----------------------


cfthist_sum <- cft_hist %>% 
  group_by(HUC12, Region, 
           Priority, TxIntensity, TxType, 
           run, Year, mas_scenario,
           fire_i) %>% 
  summarize(sum_huc_burned_frac_in_ftype_bin = sum(huc_burned_frac_in_ftype_bin))


combo <- cbp %>% 
  left_join(cfthist_sum, 
            by = join_by(HUC12, Region, Priority, TxIntensity, TxType, 
                         run, Year, mas_scenario, fire_i))


combo_stats <- combo %>% 
  mutate(burn_frac_diff = huc_burned_frac - sum_huc_burned_frac_in_ftype_bin)

combo_stats$burn_frac_diff %>% summary()
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -0.0652484 -0.0011917 -0.0004593 -0.0009846  0.0000000  0.0000000 


### huc-scenario-year total instead of per fire total? --------------

hacbp <- cbp %>% 
  group_by(HUC12, Region, Priority, TxIntensity, TxType, run, Year, mas_scenario) %>% 
  summarize(HaCBP = mean(huc_burned_frac), .groups='drop')

hacft_hist_ft <- cft_hist %>% 
  group_by(HUC12, Region, Priority, TxIntensity, TxType, run, Year, mas_scenario, fire_type) %>% 
  summarize(burn_frac_ave = mean(huc_burned_frac_in_ftype_bin), .groups='drop') %>% 
  #now drop fire_type
  group_by(HUC12, Region, Priority, TxIntensity, TxType, run, Year, mas_scenario) %>% 
  summarize(burn_frac_ave_sum_alltype = sum(burn_frac_ave), .groups='drop')
  
