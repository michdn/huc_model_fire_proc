# fuel inputs and treatment groups

# used only to id HUC to investigate
# see fuels_grps_ts.R for fuels work

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### Base Data import -------------------------------------------

res <- read_csv(file.path('results_csv', 
                               'datacube_weighted_20240212.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

# # Want priority order: 'Fire', 'WUI', 'Hybrid'
# # Want intensity order: '500k', '1m', '2m'
# 
# res <- res_orig %>% 
#   #For graphing in the correct order
#   # make factor with set order (priority)
#   mutate(Priority = as.factor(Priority),
#          Priority = forcats::fct_relevel(Priority,
#                                          "Fire", "WUI", "Hybrid"),
#          #Make factor with set order (intensity))
#          TxIntensity = as.factor(TxIntensity),
#          TxIntensity = forcats::fct_relevel(TxIntensity,
#                                             "500k", "1m", "2m"))


### data filtering --------------------------------------------------

res2024 <- res %>% 
  filter(Year == 2024)

r0_500k <- res2024 %>% 
  filter(TxIntensity == "500k") %>% 
  rename(expBurn_500k = expBurn, 
         expFlame_500k = expFlame,
         expAcf_500k = expPcActive,
         HaCBP_500k = HaCBP,
         HaCFL_500k = HaCFL,
         TSC_500k = TSC,
         SDI_500k = SDI)

r0_1m <- res2024 %>% 
  filter(TxIntensity == "1m") %>% 
  rename(expBurn_1m = expBurn, 
         expFlame_1m = expFlame,
         expAcf_1m = expPcActive,
         HaCBP_1m = HaCBP,
         HaCFL_1m = HaCFL,
         TSC_1m = TSC,
         SDI_1m = SDI)

r0_2m <- res2024 %>% 
  filter(TxIntensity == "2m") %>% 
  rename(expBurn_2m = expBurn, 
         expFlame_2m = expFlame,
         expAcf_2m = expPcActive,
         HaCBP_2m = HaCBP,
         HaCFL_2m = HaCFL,
         TSC_2m = TSC,
         SDI_2m = SDI)

r0_jt <- r0_500k %>% 
  left_join(r0_1m, 
            by = join_by(HUC12, Region, 
                         Priority, TxType, 
                         fireGroup, hybridGroup, wuiGroup)) %>% 
  left_join(r0_2m, 
            by = join_by(HUC12, Region, 
                         Priority, TxType, 
                         fireGroup, hybridGroup, wuiGroup)) %>% 
  dplyr::select(HUC12, Region, Priority, TxType,
                fireGroup, hybridGroup, wuiGroup,
                expBurn_500k, expBurn_1m, expBurn_2m,
                expFlame_500k, expFlame_1m, expFlame_2m,
                expAcf_500k, expAcf_1m, expAcf_2m,
                HaCBP_500k, HaCBP_1m, HaCBP_2m,
                HaCFL_500k, HaCFL_1m, HaCFL_2m,
                TSC_500k, TSC_1m, TSC_2m,
                SDI_500k, SDI_1m, SDI_2m)
  

# first find HUCs to examine
# in SC
# in 2024
# Fire Priority
# HUCs NOT in group25
# that had differences in expFlame between 500k, 1m, 2m scenario

r0scf <- r0_jt %>% 
  filter(Region == "SC",
         Priority == "Fire") %>% 
  select(-hybridGroup, -wuiGroup)

r0scf_g25 <- r0scf %>% 
  filter(fireGroup == 25)

r0scf_g50 <- r0scf %>% 
  filter(fireGroup == 50)

r0scf_g50 %>% 
  mutate(flame_500k2m = expFlame_500k - expFlame_2m) %>% 
  arrange(desc(flame_500k2m)) %>% View()

#HUC with highest diff: 180701020701 trt4