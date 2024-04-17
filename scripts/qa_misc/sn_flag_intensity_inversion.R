# qa script
# tag hucs that have inverted intensity responses

# based on expFlame
#  2m < 1m < 500k < baseline
# if not, flag HUC-scenario-year

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### user settings -----------------------------------------

#set an above threshold to strip about-the-same-results 
threshold <- 1.01 # 1.01 = 1%


### Results import -------------------------------------------

#to remove ones affected by nonburn issue, until SNnbonly reruns are done and incorporated
nb_hucs <- readRDS("data/nonburnable_rerun_list.RDS")


res_orig <- read_csv(file.path("results",
                               "absolute", #"datacube", 
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

### flags -------------------------------------------------------------
# Should be: 2m < 1m < 500k < baseline
#    Flipped: baseline > 500k > 1m > 2m
# flag 2m: if 2m is greater than any of the others
# flag 1m: if 1m is greater than 500k or baseline
# flag 500k: if 500k is greater than baseline

res_flags <- res_wide %>% 
  mutate(pmin1m = pmin(expFlame_1m, expFlame_500k, expFlame_base, na.rm = TRUE),
         pmin500k = pmin(expFlame_500k, expFlame_base, na.rm = TRUE),
         t2m = if_else(expFlame_2m > pmin1m * threshold, TRUE, FALSE),
         t1m = if_else(expFlame_1m > pmin500k * threshold, TRUE, FALSE),
         t500k = if_else(expFlame_500k > expFlame_base * threshold, TRUE, FALSE),
         tcount = t2m + t1m + t500k,
         tany = if_else(tcount > 0, 1, tcount))

res_flags %>%
  summarize(count_any = sum(tany),
            count_2m = sum(t2m),
            count_1m = sum(t1m),
            count_500k = sum(t500k),
            count_total = n(),
            pany = count_any / count_total * 100)

res_flags %>% 
  group_by(Priority) %>% 
  summarize(count_any = sum(tany),
            count_2m = sum(t2m),
            count_1m = sum(t1m),
            count_500k = sum(t500k),
            count_total = n(),
            pany = count_any / count_total * 100)

res_flags %>% 
  group_by(TxType) %>% 
  summarize(count_any = sum(tany),
            count_2m = sum(t2m),
            count_1m = sum(t1m),
            count_500k = sum(t500k),
            count_total = n(),
            pany = count_any / count_total * 100)

res_flags %>% 
  group_by(Year) %>% 
  summarize(count_any = sum(tany),
            count_2m = sum(t2m),
            count_1m = sum(t1m),
            count_500k = sum(t500k),
            count_total = n(),
            pany = count_any / count_total * 100)

res_flags %>% 
  group_by(Priority, TxType, Year) %>% 
  summarize(count_any = sum(tany),
            count_2m = sum(t2m),
            count_1m = sum(t1m),
            count_500k = sum(t500k),
            count_total = n(),
            pany = count_any / count_total * 100,
            .groups = "drop") %>% 
  arrange(desc(pany))

### SAVE ------------------------------------------

#save out res_flags to use in other script for identifying which ones to investigate

stamp <- format(Sys.time(), "%Y%m%d")

saveRDS(res_flags, 
        file.path("qa", 
                  paste0("SN_intensity_inverted_flagged_", 
                         threshold, "_",
                         stamp, ".RDS")))

write_csv(res_flags, 
        file.path("qa", 
                  paste0("SN_intensity_inverted_flagged_", 
                         threshold, "_",
                         stamp, ".csv")))

