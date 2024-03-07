#Experimental script using CI on baseline vs future time steps
# Uses output of results2_datacube

# Idea:
# Test if later year point CI low is LESS or equal to 
#  Baseline Year 0 (per HUC per scenario)
# Binary flag : 1 Future time point is same or less than baseline
#               0 Future time point is greater than baseline
# (Where same/less than is a 'success' here)

# PER SCENARIO, per time difference (just Yr20 to Yr0 for now), 
#  count the number of lte HUCs on HaCBP (for now)

# PER REGION, Top n Scenarios? 




# TODO TODO

# total number of get percent of various things are incorrect. 
# Also, suspicion that higher/lower calculations with CIs might be incorrect. 





### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### User settings ---------------------------------------------

input_folder <- 'results_csv'

### Base Data import -------------------------------------------

res <- read_csv(file.path(input_folder, 
                          'datacube_expanded_20240119.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))


### Data prep ------------------------------

# Reformatting and factoring

# Want priority 'RFFC' to show as 'Hybrid'

# res <- res %>% 
#   #change name to Hybrid
#   # name change will already be done in later versions of datacube,
#   # but won't matter if here as well, it just won't do anything
#   mutate(Priority = ifelse(Priority == 'RFFC', 'Hybrid', Priority)) 

#get total number of HUCs per region, so that can calculate % of region later
reg_counts <- res %>% select(RRK, HUC12) %>% distinct() %>% 
  group_by(RRK) %>% summarize(region_hucs = n())

res <- res %>% 
  left_join(reg_counts, by = join_by(RRK))

### Year Data Prep -------------------------------------------

# 1. Make year datasets
# e.g. Split on Y2024 (year 0) and Y2029 (year 5)
# 2. Join on HUC12, Priority, TxIntensity, TxType, run
# Have single line with yr 0 and yr 5 HaCBP value for that
#  HUC, priority, TxIntensity, and TxType (and run)
# 3. Calculate flag categories


#baseline
res2024 <- res %>% 
  filter(Year == 2024)

#Year 5
res2029 <- res %>% 
  filter(Year == 2029)


res05 <- res2024 %>% 
  left_join(res2029, by = c("HUC12", "RRK", "region_hucs", "Priority", "TxIntensity", "TxType")) %>% 
  #Note: .x is 2024, .y is 2029
  mutate(hacbp_future_higher = ifelse(hacbp_ci_low.y > hacbp_ci_high.x, 1, 0),
         hacbp_future_lower = ifelse(hacbp_ci_high.y < hacbp_ci_low.x, 1, 0), 
         hacbp_future_lte = ifelse(hacbp_ci_low.y <= hacbp_ci_high.x, 1, 0),
         #flame length
         hacfl_future_higher = ifelse(hacfl_ci_low.y > hacfl_ci_high.x, 1, 0),
         hacfl_future_lower = ifelse(hacfl_ci_high.y < hacfl_ci_low.x, 1, 0),
         hacfl_future_lte = ifelse(hacfl_ci_low.y <= hacfl_ci_high.x, 1, 0)) %>% 
  select(HUC12, RRK, region_hucs, Priority, TxIntensity, TxType,  
         hacbp_future_higher, hacbp_future_lower, hacbp_future_lte,
         hacfl_future_higher, hacfl_future_lower, hacfl_future_lte)



#Year 20
res2044 <- res %>% 
  filter(Year == 2044)

res020 <- res2024 %>% 
  left_join(res2044, by = c("HUC12", "RRK", "region_hucs", "Priority", "TxIntensity", "TxType")) %>% 
  #Note: .x is 2024, .y is 2044
  mutate(hacbp_future_higher = ifelse(hacbp_ci_low.y > hacbp_ci_high.x, 1, 0),
         hacbp_future_lower = ifelse(hacbp_ci_high.y < hacbp_ci_low.x, 1, 0), 
         #flame length
         hacfl_future_higher = ifelse(hacfl_ci_low.y > hacfl_ci_high.x, 1, 0),
         hacfl_future_lower = ifelse(hacfl_ci_high.y < hacfl_ci_low.x, 1, 0)) 


 # select(HUC12, RRK, region_hucs, Priority, TxIntensity, TxType,  
 #         hacbp_future_higher, hacbp_future_lower,
 #         hacfl_future_higher, hacfl_future_lower)

#hacbp_future_lte = ifelse(hacbp_ci_low.y <= hacbp_ci_high.x, 1, 0),
#         hacfl_future_lte = ifelse(hacfl_ci_low.y <= hacfl_ci_high.x, 1, 0)) %>% 


### Summaries -------------------------------------------

res05 %>% 
  summarize(num_higher = sum(hacbp_future_higher, na.rm = TRUE),
            num_lower = sum(hacbp_future_lower, na.rm = TRUE))

# num_higher num_lower num_lte
# <dbl>     <dbl>   <dbl>
# 516       337   76069

(516+337)/76599*100
#1.11

#OLD
# num_higher num_lower num_lte
# <dbl>     <dbl>   <dbl>
#   1        508       335   76072

#,num_lte = sum(hacbp_future_lte, na.rm = TRUE)

res020 %>% 
  summarize(num_higher = sum(hacbp_future_higher, na.rm = TRUE),
            num_lower = sum(hacbp_future_lower, na.rm = TRUE))

# num_higher num_lower num_lte
# <dbl>     <dbl>   <dbl>
# 4874      1499   71712

(4874+1499)/76599*100
#8.31

#OLD
# num_higher num_lower num_lte
# <dbl>     <dbl>   <dbl>
#   1       4838      1501   71745

#,num_lte = sum(hacbp_future_lte, na.rm = TRUE)


res020 %>% 
  summarize(num_higher = sum(hacfl_future_higher, na.rm = TRUE),
            num_lower = sum(hacfl_future_lower, na.rm = TRUE),
            num_lte = sum(hacfl_future_lte, na.rm = TRUE))

#OLD
# num_higher num_lower num_lte
# <dbl>     <dbl>   <dbl>
#   1       3711     76583   72872


### Comparisons HACBP  --------------------------------------------------

sum020 <- res020 %>% 
  group_by(RRK, region_hucs, Priority, TxIntensity, TxType) %>% 
  summarize(num_lower = sum(hacbp_future_lower, na.rm = TRUE),
            num_higher = sum(hacbp_future_higher, na.rm = TRUE),
            num_lte = sum(hacbp_future_lte, na.rm = TRUE),
            .groups = "drop") #%>% 
#TODO TODO This is incorrect. region_hucs times scenarios 27
  #mutate(perc_lower = num_lower / region_hucs * 100,
  #       perc_higher = num_higher / region_hucs * 100,
  #       perc_lte = num_lte / region_hucs * 100) 

sum020 %>%  
  group_by(RRK) %>% 
  slice_max(order_by = perc_lower, n = 5) %>% 
  select(RRK, Priority, TxIntensity, TxType, num_lower, perc_lower) %>% View()

sum020 %>%  
  group_by(RRK) %>% 
  slice_max(order_by = perc_higher, n = 5) %>% 
  select(RRK, Priority, TxIntensity, TxType, num_higher, perc_higher) %>% View()

sum020 %>%  
  group_by(RRK) %>% 
  slice_max(order_by = perc_lte, n = 5) %>% 
  select(RRK, Priority, TxIntensity, TxType, num_lte, perc_lte) %>% View()




res020 %>% 
  ungroup() %>% 
  group_by(RRK, region_hucs, Priority) %>% 
  summarize(num_lower = sum(hacbp_future_lower, na.rm = TRUE),
            .groups = "drop") %>% 
  #mutate(perc_lower = num_lower / region_hucs * 100) %>% 
  select(-region_hucs) %>% 
  arrange(RRK, desc(num_lower))


res020 %>% 
  ungroup() %>% 
  group_by(RRK, region_hucs, TxIntensity) %>% 
  summarize(num_lower = sum(hacbp_future_lower, na.rm = TRUE),
            .groups = "drop") %>% 
  #mutate(perc_lower = num_lower / region_hucs * 100) %>% 
  select(-region_hucs) %>% 
  arrange(RRK, desc(num_lower))


res020 %>% 
  ungroup() %>% 
  group_by(RRK, region_hucs, TxType) %>% 
  summarize(num_lower = sum(hacbp_future_lower, na.rm = TRUE),
            .groups = "drop") %>% 
  #mutate(perc_lower = num_lower / region_hucs * 100) %>% 
  select(-region_hucs) %>% 
  arrange(RRK, desc(num_lower))


## CFL -----------------------------------------------------------

sum020_cfl <- res020 %>% 
  group_by(RRK, region_hucs, Priority, TxIntensity, TxType) %>% 
  summarize(num_lower = sum(hacfl_future_lower, na.rm = TRUE),
            num_higher = sum(hacfl_future_higher, na.rm = TRUE),
            num_lte = sum(hacfl_future_lte, na.rm = TRUE),
            .groups = "drop") #%>% 
  #mutate(perc_lower = num_lower / region_hucs * 100,
  #       perc_higher = num_higher / region_hucs * 100,
  #       perc_lte = num_lte / region_hucs * 100) 
sum020_cfl %>% View()

res020 %>% 
  ungroup() %>% 
  group_by(RRK, region_hucs, TxIntensity) %>% 
  summarize(num_lower = sum(hacfl_future_lower, na.rm = TRUE),
            .groups = "drop") %>% 
  #mutate(perc_lower = num_lower / (region_hucs * 27) * 100) %>% 
  select(-region_hucs) %>% 
  arrange(RRK, desc(num_lower))
