
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf)


### User settings ---------------------------------------------

reg_code <- "CC"

input_folder <- file.path('results', 'extracts')

cfl <- readRDS(file.path(input_folder, 
                          paste0(reg_code, '_cfl_all_fires_from_sql.RDS')))

this_cfl <- cfl %>% filter(HUC12 == "180600060410")

this_cfl %>%
  mutate(any_fl = if_else(huc_avg_fl > 0, 1, 0)) %>% 
  group_by(mas_scenario, Year) %>% 
  summarize(tot_fires = n(),
            fl_fires = sum(any_fl)) %>% View()


cbp <- readRDS(file.path(input_folder, 
                          paste0(reg_code, '_cbp_all_fires_from_sql.RDS')))

this_cbp <- cbp %>% filter(HUC12 == "180600060410")

this_cbp %>%
  mutate(any_fl = if_else(huc_burned_frac > 0, 1, 0)) %>% 
  group_by(mas_scenario, Year) %>% 
  summarize(tot_fires = n(),
            fl_fires = sum(any_fl)) %>% View()
