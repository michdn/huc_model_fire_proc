#qa things

#huc pulling
# group 50, 75 for 4 regions, for 3 priorities (random)
# (to be added to Anna pulling the top and bottom HUCs in group 25 and 100)

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### Base Data import -------------------------------------------

res <- read_csv(file.path('results_csv', 
                          'datacube_weighted_20240212.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))


hucs <- res %>% 
  dplyr::select(HUC12, Region, fireGroup, wuiGroup, hybridGroup) %>% 
  distinct()
#2837

## random pulls -----------------------------------------------

#set seed for reproducibility
set.seed(7)

#Fire
fire <- hucs %>% 
  select(HUC12, Region, fireGroup) %>% 
  filter(fireGroup == 50 | fireGroup == 75) %>% 
  group_by(Region, fireGroup) %>% 
  slice_sample(n = 2) %>% 
  mutate(grouping = "Fire") %>% 
  rename(group = fireGroup)

#will be overwriting one SC firegroup 50 with 180701020701 (in csv)

#set seed for reproducibility
set.seed(7)

#WUI
wui <- hucs %>% 
  select(HUC12, Region, wuiGroup) %>% 
  filter(wuiGroup == 50 | wuiGroup == 75) %>% 
  group_by(Region, wuiGroup) %>% 
  slice_sample(n = 2) %>% 
  mutate(grouping = "WUI") %>% 
  rename(group = wuiGroup)


#set seed for reproducibility
set.seed(7)

#Hybrid
hybrid <- hucs %>% 
  select(HUC12, Region, hybridGroup) %>% 
  filter(hybridGroup == 50 | hybridGroup == 75) %>% 
  group_by(Region, hybridGroup) %>% 
  slice_sample(n = 2) %>% 
  mutate(grouping = "Hybrid") %>% 
  rename(group = hybridGroup)


## combine & save

full <- bind_rows(fire, wui, hybrid)


write_csv(full, 
          file = file.path("qa", "trtgroups", "sampledhucs_grp5075.csv"))
