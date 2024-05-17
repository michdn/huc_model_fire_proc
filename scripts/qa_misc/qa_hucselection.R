#qa things

#huc pulling
# group 50, 75 for 4 regions, for 3 priorities (random)
# (to be added to Anna pulling the top and bottom HUCs in group 25 and 100)

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf)


### Data ------------------------------------------------------

#original full sampled hucs
hucs_sample <- read_csv(file.path('qa', 'samplehucs.csv')) 

#all hucs
hucs_shp <- st_read("data/data_huc/TxHucsTimingGroups.shp")

hucs <- hucs_shp %>%
  st_drop_geometry() %>%
  select(huc12, region, 
         fireGrp, hybrdGr, wuiGrop,
         timeFir, tmHybrd, timeWui) %>%
  rename(HUC12 = huc12,
         Region = region,
         fireGroup = fireGrp,
         hybridGroup = hybrdGr,
         wuiGroup = wuiGrop,
         timeFire = timeFir,
         timeHybrid = tmHybrd) %>% 
  #better (graph) sorting with yr_epoch format
  mutate(across(c(timeFire, timeHybrid, timeWui),
                ~ case_when(
                  . == "yr1to5" ~ paste0("2024_", .),
                  . == "yr6to10" ~ paste0("2029_", .),
                  . == "yr11to15" ~ paste0("2034_", .),
                  . == "yr16to20" ~ paste0("2039_", .),
                  . == "yr1to5_16to20" ~ paste0("2024_2039_", .),
                  . == "notTreated" ~ "Not treated"))) %>% 
  arrange(Region, HUC12)

#write_csv(hucs, file.path("qa", "hucs_timing_group_lookup.csv"))

### NEW SAMPLING ---------------------------------------------------------

#keep top three timing groups (2 HUCs per category)
#  priority = "fire"   "wui"    "hybrid"
hucs_keep_fire <- hucs_sample %>% 
  filter(!group == 100,
         priority == "fire") %>% 
  pull(HUC12)

hucs_keep_wui <- hucs_sample %>% 
  filter(!group == 100,
         priority == "wui") %>% 
  pull(HUC12)

hucs_keep_hybrid <- hucs_sample %>% 
  filter(!group == 100,
         priority == "hybrid") %>% 
  pull(HUC12)


#Fire new
set.seed(7)
fire <- hucs %>% 
  #not the ones we are keeping already
  filter(!HUC12 %in% hucs_keep_fire) %>% 
  select(HUC12, Region, timeFire) %>% 
  #not the last group
  filter(!timeFire == "2039_yr16to20") %>% 
  group_by(Region, timeFire) %>% 
  #just take one more per remaining 3 timing groups
  slice_sample(n = 1) %>% 
  mutate(grouping = "Fire") %>% 
  rename(group = timeFire)

#Hybrid new
set.seed(7)
hybrid <- hucs %>% 
  #not the ones we are keeping already
  filter(!HUC12 %in% hucs_keep_hybrid) %>% 
  select(HUC12, Region, timeHybrid) %>% 
  filter(!timeHybrid == "2039_yr16to20") %>% 
  group_by(Region, timeHybrid) %>% 
  #just take one more per remaining 3 timing groups
  slice_sample(n=1) %>% 
  mutate(grouping = "Hybrid") %>% 
  rename(group = timeHybrid)

#WUI new
set.seed(7)
wui <- hucs %>% 
  #not the ones we are keeping already
  filter(!HUC12 %in% hucs_keep_wui) %>% 
  select(HUC12, Region, timeWui) %>% 
  filter(!timeWui == "Not treated") %>% 
  group_by(Region, timeWui) %>% 
  slice_sample(n=1) %>% 
  mutate(grouping = "WUI") %>% 
  rename(group = timeWui)

#get kept HUCs 
fire_keep <- hucs %>% 
  filter(HUC12 %in% hucs_keep_fire) %>% 
  select(HUC12, Region, timeFire) %>% 
  mutate(grouping = "Fire") %>% 
  rename(group = timeFire)

hybrid_keep <- hucs %>% 
  filter(HUC12 %in% hucs_keep_hybrid) %>% 
  select(HUC12, Region, timeHybrid) %>% 
  mutate(grouping = "Hybrid") %>% 
  rename(group = timeHybrid)

wui_keep <- hucs %>% 
  filter(HUC12 %in% hucs_keep_wui) %>% 
  select(HUC12, Region, timeWui) %>% 
  mutate(grouping = "WUI") %>% 
  rename(group = timeWui)

#new huc sample set
hucs_sample_new <- bind_rows(
  fire,
  fire_keep,
  hybrid, 
  hybrid_keep,
  wui, 
  wui_keep) %>% 
  arrange(Region, grouping, group, HUC12)

write_csv(hucs_sample_new, file.path('qa', 'sampledhucs_topthreegroups.csv')) 

#### ORIGINAL PARTIAL SET -----------------------------------------------

# ### Base Data import -------------------------------------------
# 
# res <- read_csv(file.path('results_csv', 
#                           'datacube_weighted_20240212.csv')) %>% 
#   mutate(HUC12 = as.character(HUC12))
# 
# 
# hucs <- res %>% 
#   dplyr::select(HUC12, Region, fireGroup, wuiGroup, hybridGroup) %>% 
#   distinct()
# #2837

# ## random pulls -----------------------------------------------
# 
# #set seed for reproducibility
# set.seed(7)
# 
# #Fire
# fire <- hucs %>% 
#   select(HUC12, Region, fireGroup) %>% 
#   filter(fireGroup == 50 | fireGroup == 75) %>% 
#   group_by(Region, fireGroup) %>% 
#   slice_sample(n = 2) %>% 
#   mutate(grouping = "Fire") %>% 
#   rename(group = fireGroup)
# 
# #will be overwriting one SC firegroup 50 with 180701020701 (in csv)
# 
# #set seed for reproducibility
# set.seed(7)
# 
# #WUI
# wui <- hucs %>% 
#   select(HUC12, Region, wuiGroup) %>% 
#   filter(wuiGroup == 50 | wuiGroup == 75) %>% 
#   group_by(Region, wuiGroup) %>% 
#   slice_sample(n = 2) %>% 
#   mutate(grouping = "WUI") %>% 
#   rename(group = wuiGroup)
# 
# 
# #set seed for reproducibility
# set.seed(7)
# 
# #Hybrid
# hybrid <- hucs %>% 
#   select(HUC12, Region, hybridGroup) %>% 
#   filter(hybridGroup == 50 | hybridGroup == 75) %>% 
#   group_by(Region, hybridGroup) %>% 
#   slice_sample(n = 2) %>% 
#   mutate(grouping = "Hybrid") %>% 
#   rename(group = hybridGroup)
# 
# 
# ## combine & save
# 
# full <- bind_rows(fire, wui, hybrid)
# 
# 
# write_csv(full, 
#           file = file.path("qa", "trtgroups", "sampledhucs_grp5075.csv"))
# 

