# Script to collate data for data cube

#"Fire modeling outputs: burn probability, flame length, 
#  % active crown fire, % passive crown fire, % surface fire"

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### User settings ---------------------------------------------

input_folder <- 'results_csv'

### Data import -------------------------------------------

#main data
res_sc <- read_csv(file.path(input_folder, 
                             'main_results_from_sql_SC.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

# res_cc_sn <- read_csv(file.path(input_folder, 
#                                 'main_results_from_sql_CC_SN.csv')) %>% 
#   mutate(HUC12 = as.character(HUC12))
# res_nc <- read_csv(file.path(input_folder, 
#                                 'main_results_from_sql_NC.csv')) %>% 
#   mutate(HUC12 = as.character(HUC12))

res <- res_sc #for now, until others available
#res <- bind_rows(res_sc, res_cc_sn, res_ns)

#fire type data
ft_sc <- read_csv(file.path(input_folder, 
                            '')) %>% 
  mutate(HUC12 = as.character(HUC12))