# Largest percent change by scenario (by region)

# Calculate percent change from Year 0 to Year 20
#  for each HUC for each scenario
# Summarize to each scenario

# Per region, see top n scenarios



# Uses output of results2_datacube


#ABONDONED, not useful to summarize to region


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### User settings ---------------------------------------------

input_folder <- 'results_csv'

### Base Data import -------------------------------------------

res <- read_csv(file.path(input_folder, 
                          'testing_qaqc',
                          'datacube_20230102a.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))


### Reformatting and factoring ------------------------------

# Want priority 'RFFC' to show as 'Hybrid'

res <- res %>% 
  #change name to Hybrid
  # name change will already be done in later versions of datacube,
  # but won't matter if here as well, it just won't do anything
  mutate(Priority = ifelse(Priority == 'RFFC', 'Hybrid', Priority)) 



### Year Data Prep -------------------------------------------

# 1. Make year datasets
# e.g. Split on Y2024 (year 0) and Y2029 (year 5)
# 2. Join on HUC12, Priority, TxIntensity, TxType, run
# Have single line with yr 0 and yr 5 HaCBP value for that
#  HUC, priority, TxIntensity, and TxType (and run)
# 3. Calculate percent change (from Year 0 to Year 5)
#  (yr5-yr0) / yr0

# 4. active crown fire has a lot of 0/0 for percent change
#  which is NaN, but want to interpret as 0 here. 

#baseline
res2024 <- res %>% 
  filter(Year == 2024)

#Year 5
res2029 <- res %>% 
  filter(Year == 2029)

res05 <- res2024 %>% 
  left_join(res2029, by = c("HUC12", "RRK", "Priority", "TxIntensity", "TxType")) %>% 
  #Note: .x is 2024, .y is 2029
  mutate(hacbp_diff = HaCBP.y - HaCBP.x, #2029-2024 value
         #percent CHANGE
         hacbp_pchange = hacbp_diff / HaCBP.x,
         #flame length
         hacfl_diff = HaCFL.y - HaCFL.x,
         hacfl_pchange = hacfl_diff / HaCFL.x,
         #active crown fire
         act_crown_diff = active_crown_fire_perc.y - active_crown_fire_perc.x,
         act_crown_pchange = act_crown_diff / active_crown_fire_perc.x,
         ac_pchange = ifelse(is.nan(act_crown_pchange) & act_crown_diff == 0, 0, act_crown_pchange)) %>% 
  select(HUC12, RRK, Priority, TxIntensity, TxType,  
         hacbp_pchange, hacfl_pchange, ac_pchange)


#Year 20
res2044 <- res %>% 
  filter(Year == 2044)

res020 <- res2024 %>% 
  left_join(res2044, by = c("HUC12", "RRK", "Priority", "TxIntensity", "TxType")) %>% 
  #Note: .x is 2024, .y is 2044
  mutate(hacbp_diff = HaCBP.y - HaCBP.x, #later year - 2024 value
         #percent CHANGE
         hacbp_pchange = hacbp_diff / HaCBP.x,
         #flame length
         hacfl_diff = HaCFL.y - HaCFL.x,
         hacfl_pchange = hacfl_diff / HaCFL.x,
         #active crown fire
         act_crown_diff = active_crown_fire_perc.y - active_crown_fire_perc.x,
         act_crown_pchange = act_crown_diff / active_crown_fire_perc.x,
         ac_pchange = ifelse(is.nan(act_crown_pchange) & act_crown_diff == 0, 0, act_crown_pchange)) %>% 
  select(HUC12, RRK, Priority, TxIntensity, TxType, 
         hacbp_pchange, hacfl_pchange, ac_pchange)


### Top scenarios HaCBP --------------------------------------------------------

# Average pchange per region per scenario


res020 %>% 
  group_by(RRK, Priority, TxIntensity, TxType) %>% 
  summarize(ave_cbp_pc = mean(hacbp_pchange, na.rm = TRUE),
            .groups = "drop") %>% 
  group_by(RRK) %>% 
  slice_min(order_by = ave_cbp_pc, n = 5)

# RRK   Priority TxIntensity TxType ave_cbp_pc
# <chr> <fct>    <fct>       <chr>       <dbl>
#   1 CC    WUI      2m          trt6       0.498 
# 2 CC    Fire     2m          trt6       0.502 
# 3 CC    Hybrid   2m          trt6       0.527 
# 4 CC    Hybrid   1m          trt1       0.527 
# 5 CC    Hybrid   500k        trt1       0.531 
# 6 NC    WUI      500k        trt1       0.0304
# 7 NC    WUI      500k        trt6       0.0320
# 8 NC    Hybrid   500k        trt1       0.0409
# 9 NC    WUI      1m          trt1       0.0410
# 10 NC    WUI      1m          trt6       0.0429
# 11 SC    WUI      500k        trt6       0.129 
# 12 SC    WUI      500k        trt1       0.130 
# 13 SC    WUI      500k        trt4       0.134 
# 14 SC    Hybrid   500k        trt6       0.136 
# 15 SC    Hybrid   500k        trt1       0.136 
# 16 SN    WUI      500k        trt6       0.0989
# 17 SN    WUI      500k        trt1       0.106 
# 18 SN    Fire     500k        trt6       0.108 
# 19 SN    Hybrid   500k        trt6       0.109 
# 20 SN    Hybrid   500k        trt1       0.120 


# Try by each factor separately, may be getting spurious? 


res020 %>% 
  group_by(RRK, Priority) %>% 
  summarize(ave_cbp_pc = mean(hacbp_pchange, na.rm = TRUE),
            .groups = "drop") %>% 
  group_by(RRK) %>% 
  slice_min(order_by = ave_cbp_pc, n = 3) #only 3
#top WUI: NC, SC, SN. Top Hybrid: CC


res020 %>% 
  group_by(RRK, TxIntensity) %>% 
  summarize(ave_cbp_pc = median(hacbp_pchange, na.rm = TRUE),
            .groups = "drop") %>% 
  group_by(RRK) %>% 
  slice_min(order_by = ave_cbp_pc, n = 3) #only 3
# 2m>1m>500k : CC
# 500k>1m>2m: NC, SC, SN ???????

res020 %>% 
  group_by(RRK, TxIntensity) %>% 
  summarize(ave_cbp_pc = median(hacbp_pchange, na.rm = TRUE),
            .groups = "drop") %>% 
  group_by(RRK) %>% 
  slice_min(order_by = ave_cbp_pc, n = 3) #only 3
# 500k better than 1m better than 2m


res020 %>% 
  group_by(RRK, TxType) %>% 
  summarize(ave_cbp_pc = mean(hacbp_pchange, na.rm = TRUE),
            .groups = "drop") %>% 
  group_by(RRK) %>% 
  slice_min(order_by = ave_cbp_pc, n = 3) #only 3
# 6, 1, 4: CC, SC, SN
# 1, 6, 4: NC


# # by 2?
# res020 %>% 
#   group_by(RRK, Priority, TxIntensity) %>% 
#   summarize(ave_cbp_pc = mean(hacbp_pchange, na.rm = TRUE),
#             .groups = "drop") %>% 
#   group_by(RRK) %>% 
#   slice_min(order_by = ave_cbp_pc, n = 3)



# CFL --------

res020 %>% 
  group_by(RRK, Priority, TxIntensity, TxType) %>% 
  summarize(mean_cfl_pc = mean(hacfl_pchange, na.rm = TRUE),
            .groups = "drop") %>% 
  group_by(RRK) %>% 
  slice_min(order_by = mean_cfl_pc, n = 5)



## FOR ANNA --------------------------------------------------------

res05 %>%
  group_by(RRK, Priority, TxIntensity, TxType) %>%
  summarize(median_cbp_pc = median(hacbp_pchange, na.rm = TRUE),
            mean_cbp_pc = mean(hacbp_pchange, na.rm = TRUE),
            min_cbp_pc = min(hacbp_pchange, na.rm = TRUE),
            max_cbp_pc = max(hacbp_pchange, na.rm = TRUE),
            #flame length
            median_cfl_pc = median(hacfl_pchange, na.rm = TRUE),
            mean_cfl_pc = mean(hacfl_pchange, na.rm = TRUE),
            min_cfl_pc = min(hacfl_pchange, na.rm = TRUE),
            max_cfl_pc = max(hacfl_pchange, na.rm = TRUE),
            #act crown fire
            median_ac_pc = median(ac_pchange, na.rm = TRUE),
            mean_ac_pc = mean(ac_pchange, na.rm = TRUE),
            min_ac_pc = min(ac_pchange, na.rm = TRUE),
            max_ac_pc = max(ac_pchange, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(RRK, Priority, TxIntensity, TxType) %>%
  write_csv(file.path("results_csv",
                      "round1",
                      "cbp_cfl_scenario_summaries_year5_year0_v2.csv"))


res020 %>%
  group_by(RRK, Priority, TxIntensity, TxType) %>%
  summarize(median_cbp_pc = median(hacbp_pchange, na.rm = TRUE),
            mean_cbp_pc = mean(hacbp_pchange, na.rm = TRUE),
            min_cbp_pc = min(hacbp_pchange, na.rm = TRUE),
            max_cbp_pc = max(hacbp_pchange, na.rm = TRUE),
            #flame length
            median_cfl_pc = median(hacfl_pchange, na.rm = TRUE),
            mean_cfl_pc = mean(hacfl_pchange, na.rm = TRUE),
            min_cfl_pc = min(hacfl_pchange, na.rm = TRUE),
            max_cfl_pc = max(hacfl_pchange, na.rm = TRUE),
            #act crown fire
            median_ac_pc = median(ac_pchange, na.rm = TRUE),
            mean_ac_pc = mean(ac_pchange, na.rm = TRUE),
            min_ac_pc = min(ac_pchange, na.rm = TRUE),
            max_ac_pc = max(ac_pchange, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(RRK, Priority, TxIntensity, TxType) %>%
  write_csv(file.path("results_csv",
                      "round1",
                      "cbp_cfl_scenario_summaries_year20_year0_v2.csv"))


## ------------------------------

res020 %>% 
  arrange(HUC12, Priority, TxType, TxIntensity) %>% View()

#check out SN 160501010301 ? 



