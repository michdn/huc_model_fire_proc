# qa script
# hucs that have inverted intensity responses
# based on expFlame
#  2m < 1m < 500k < baseline (should be)

# correlate (scatter plot?) with Dave's forest % of HUC

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)


### Results import -------------------------------------------

forest <- read_csv("qa/SN_TM_coverage_by_HUC.csv") %>% 
  mutate(HUC12 = as.character(HUC12))

#to remove ones affected by nonburn issue, until SNnbonly reruns are done and incorporated
nb_hucs <- readRDS("data/nonburnable_rerun_list.RDS")


res_orig <- read_csv(file.path("results",
                               "absolute",  
                               "SN_SNbl_absolute_20240416.csv")) %>% 
  mutate(HUC12 = as.character(HUC12)) 

#filter out nonburnable issue HUCs
res <- res_orig %>% 
  filter(!HUC12 %in% (nb_hucs %>% pull(huc12)))


res <- res %>% 
  #For graphing in the correct order (generic, used in multiple places, with modifications)
  # make factor with set order 
  mutate(Priority = as.factor(Priority),
         Priority = forcats::fct_relevel(Priority,
                                         "Fire", "WUI", "Hybrid", "baseline"), # , "baseweather"
         #Make factor with set order (intensity))
         TxIntensity = as.factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                            "baseline", "500k", "1m", "2m"), # "baseweather", 
         #recode so it fits on graphs
         #TxIntensity = forcats::fct_recode(TxIntensity, "bw" = "baseweather"),
         TxIntensity = forcats::fct_recode(TxIntensity, "base" = "baseline"))


#baseline to be treated as an 'intensity' level PER ALL priorities, trts
res_bl <- res %>% 
  filter(TxIntensity == "base") %>% 
  #remove priorities, trts from base
  dplyr::select(-Priority, -TxType)
#get all combination of Priority and TxTypes
frame <- res %>% 
  filter(!TxIntensity == "base") %>% 
  dplyr::select(Priority, TxType) %>% 
  distinct()
#duplicate bases values for all priority-txtype combos
res_bl_dup <- frame %>% 
  cross_join(res_bl) 

#recreate res with baselines masquerade
res <- res %>% 
  filter(!TxIntensity == "base") %>% 
  dplyr::bind_rows(res_bl_dup)


#trim down
# ONLY evaluate AT or POST treatment
res_trim <- res %>% 
  #treatment years
  separate_wider_delim(timeFire, "_", names=c("fire_trt_yr", NA)) %>% 
  separate_wider_delim(timeHybrid, "_", names=c("hybrid_trt_yr", NA)) %>% 
  mutate(fire_trt_yr = as.numeric(fire_trt_yr),
         hybrid_trt_yr = as.numeric(hybrid_trt_yr),
         wui_trt_yr = case_when(
           timeWui == "2024_2039_yr1to5_16to20" ~ 2024,
           timeWui == "2029_yr6to10" ~ 2029,
           timeWui == "2034_yr11to15" ~ 2034,
           timeWui == "Not treated" ~ 9999)) %>% 
  #treat year for priority
  mutate(trt_yr = case_when(
    Priority == "Fire" ~ fire_trt_yr,
    Priority == "Hybrid" ~ hybrid_trt_yr,
    Priority == "WUI" ~ wui_trt_yr)) %>% 
  #year is AT or AFTER treatment
  filter(Year >= trt_yr) %>% 
  #fields, for convenience
  dplyr::select(c(HUC12, Region, 
                  Priority, TxType, TxIntensity, 
                  Year, trt_yr,  
                  expFlame, HaCFL))

### intensity (incl baseline) ----------------------

res_base <- res_trim %>% 
  filter(TxIntensity == "base") %>% 
  rename(expFlame_base = expFlame,
         HaCFL_base = HaCFL) %>% 
  dplyr::select(-TxIntensity)

res_500k <- res_trim %>% 
  filter(TxIntensity == "500k") %>% 
  rename(expFlame_500k = expFlame,
         HaCFL_500k = HaCFL) %>% 
  dplyr::select(-TxIntensity)

res_1m <- res_trim %>% 
  filter(TxIntensity == "1m") %>% 
  rename(expFlame_1m = expFlame,
         HaCFL_1m = HaCFL) %>% 
  dplyr::select(-TxIntensity)

res_2m <- res_trim %>% 
  filter(TxIntensity == "2m") %>% 
  rename(expFlame_2m = expFlame,
         HaCFL_2m = HaCFL) %>% 
  dplyr::select(-TxIntensity)


#join back up for wide 
# maybe look at pivot_wider() one day
res_wide <- res_base %>%
  left_join(res_500k,
            by = join_by(HUC12, Region,
                         Priority, TxType, Year, trt_yr)) %>% 
  left_join(res_1m,
            by = join_by(HUC12, Region,
                         Priority, TxType, Year, trt_yr)) %>%
  left_join(res_2m,
            by = join_by(HUC12, Region,
                         Priority, TxType, Year, trt_yr))

### inversions -------------------------------------------------------------

#instead of binary, calc the percent increase from this expFlame to comparison expFlame

res_inversions <- res_wide %>% 
  mutate(pmax1m = pmax(expFlame_1m, expFlame_500k, expFlame_base, na.rm = TRUE),
         pmax500k = pmax(expFlame_500k, expFlame_base, na.rm = TRUE),
         i2m = if_else(expFlame_2m > pmax1m, ((expFlame_2m - pmax1m)/pmax1m) * 100, NA),
         i1m = if_else(expFlame_1m > pmax500k, ((expFlame_1m - pmax500k)/pmax500k) * 100, NA),
         i500k = if_else(expFlame_500k > expFlame_base, ((expFlame_500k - expFlame_base)/expFlame_base) * 100, NA),
         iany = if_else((is.na(i2m) & is.na(i1m) & is.na(i500k)), FALSE, TRUE))


#inversions and forest
resi <- res_inversions %>% 
  #only with any inversions
  filter(iany) %>% 
  #prep for making long
  dplyr::select(HUC12, Priority, TxType, Year, i2m, i1m, i500k) %>% 
  pivot_longer(cols = c(i2m, i1m, i500k)) %>% 
  #filter only inversions at that intensity
  filter(!is.na(value)) %>% 
  #rename to intensity level
  mutate(TxIntensity = case_when(
    name == "i2m" ~ "2m",
    name == "i1m" ~ "1m",
    name == "i500k" ~ "500k"
  )) %>% 
  rename(perc_incr = value) %>% 
  select(-name) %>% 
  left_join(forest, by = join_by("HUC12"))


### inverted HUCs to forest percentage ------------------------------------

#first pass, HUC level, count of all inversions
huci <- resi %>% 
  dplyr::select(HUC12, TM_coverage_percent) %>% 
  group_by(HUC12, TM_coverage_percent) %>% 
  summarize(count_inversions = n())

ggplot() + 
  geom_point(data = huci,
             mapping = aes(x=TM_coverage_percent,
                           y=count_inversions))


#HUC level, count of inversions > 0.01
huci01 <- resi %>% 
  filter(perc_incr > 0.01) %>% 
  dplyr::select(HUC12, TM_coverage_percent) %>% 
  group_by(HUC12, TM_coverage_percent) %>% 
  summarize(count_inversions = n())

ggplot() + 
  geom_point(data = huci01,
             mapping = aes(x=TM_coverage_percent,
                           y=count_inversions))


# value of inversions
ggplot() + 
  geom_point(data = resi,
             mapping = aes(x=TM_coverage_percent,
                           y=perc_incr)) + 
  labs(title = "SN Intensity inversion by forest cover",
       y = "Percent increase of inversion",
       x = "TreeMap coverage percentage") + 
  stat_smooth(data=resi,
              aes(x=TM_coverage_percent,
                  y=perc_incr),
              method=lm,
              geom="smooth",
              formula='y~x',
              fullrange = TRUE)
  
  




