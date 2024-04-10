
# offshoot

#new metric 
#huc_avg_fl from ha_cfl_series 
# divided by huc_burned_frac from ha_cbp_series 
# matching on  fire_i (and the HUC12, scenario-year, etc.).

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### User settings ---------------------------------------------

reg_code <- "SN"

input_folder <- file.path('results', 'extracts')

#output_folder <- file.path('results', 'conditional')
#dir.create(output_folder, recursive = TRUE) 


### SQL extraction, every fire results -------------------------

cbp <- readRDS(file.path(input_folder, 
                         paste0(reg_code, '_cbp_all_fires_from_sql.RDS'))) 
cfl <- readRDS(file.path(input_folder, 
                         paste0(reg_code, '_cfl_all_fires_from_sql.RDS'))) 

### Join and calc per fire flame/burn ratio --------------------

joint <- cbp %>% 
  left_join(cfl,
            by = join_by(HUC12, Region, 
                         Priority, TxIntensity, TxType, 
                         run, Year, mas_scenario,
                         fire_i))

joint <- joint %>% 
  mutate(flame_burn_ratio = huc_avg_fl / huc_burned_frac)

summary(joint %>% pull(flame_burn_ratio))

#joint %>% 
#  filter(is.na(flame_burn_ratio))
# NaN when 0 / 0 

saveRDS(joint, "results/misc/SN_perfire_ratio_20240409.RDS")
