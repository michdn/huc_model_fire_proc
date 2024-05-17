
# regression 

# FBFM model and inversion

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  viridis)


### Results import -------------------------------------------

#FBFM pixel counts
fbfm <- readRDS(file.path("qa", "SN_FBFM_zonal_counts.RDS")) 


#to remove ones affected by nonburn issue, until SNnbonly reruns are done and incorporated
nb_hucs <- readRDS("data/nonburnable_rerun_list.RDS")


res_orig <- read_csv(file.path("results",
                               "absolute", #"datacube", 
                               "SN_SNbl_SNbw_absolute_20240423.csv")) %>% 
  mutate(HUC12 = as.character(HUC12)) 

#filter out nonburnable issue HUCs
res <- res_orig %>% 
  filter(!HUC12 %in% (nb_hucs %>% pull(huc12)))


res <- res %>% 
  #For graphing in the correct order (generic, used in multiple places, with modifications)
  # make factor with set order (priority)
  mutate(Priority = as.factor(Priority),
         Priority = forcats::fct_relevel(Priority,
                                         "Fire", "WUI", "Hybrid", "baseline", "baseweather"),
         #Make factor with set order (intensity))
         TxIntensity = as.factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                            "baseweather", "baseline", "500k", "1m", "2m"),
         #recode so it fits on graphs
         TxIntensity = forcats::fct_recode(TxIntensity, "bw" = "baseweather"),
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

# res_inversions <- res_wide %>% 
#   mutate(pmax1m = pmax(expFlame_1m, expFlame_500k, expFlame_base, na.rm = TRUE),
#          pmax500k = pmax(expFlame_500k, expFlame_base, na.rm = TRUE),
#          i2m = if_else(expFlame_2m > pmax1m, ((expFlame_2m - pmax1m)/pmax1m) * 100, NA),
#          i1m = if_else(expFlame_1m > pmax500k, ((expFlame_1m - pmax500k)/pmax500k) * 100, NA),
#          i500k = if_else(expFlame_500k > expFlame_base, ((expFlame_500k - expFlame_base)/expFlame_base) * 100, NA),
#          iany = if_else((is.na(i2m) & is.na(i1m) & is.na(i500k)), FALSE, TRUE))

#calculate for ALL, not just inverted
res_comparisons <- res_wide %>% 
  mutate(pmax1m = pmax(expFlame_1m, expFlame_500k, expFlame_base, na.rm = TRUE),
         pmax500k = pmax(expFlame_500k, expFlame_base, na.rm = TRUE),
         i2m = ((expFlame_2m - pmax1m)/pmax1m) * 100,
         i1m = ((expFlame_1m - pmax500k)/pmax500k) * 100,
         i500k = ((expFlame_500k - expFlame_base)/expFlame_base) * 100)


### inversions (or not) join with fbfm -----------------------------------------------

#inversions long
resi <- res_comparisons %>% 
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
  rename(perc_incr = value)


#fbfm adjustments, change to % of total

fbfm_prep <- fbfm %>% 
  mutate(fnb = f91 + f92 + f93 + f98 + f99) %>% 
  dplyr::select(-c(f91, f92, f93, f98, f99)) %>% 
  #calc percent total pixels of HUC, not raw pixel counts
  mutate(across(c(f101, f102, f103, f104, f121, f122, f123, f141, f142, f143,
                  f144, f145, f146, f147, f161, f162, f163, f164, f165,
                  f181, f182, f183, f184, f185, f186, f187, f188, f189,
                  f201, f202, f203, fnb),
                ~ ./ftotal))


res_fbfm <- resi %>% 
  left_join(fbfm_prep, by = join_by(HUC12, Priority, TxType, Year, TxIntensity))
  


### regression --------------------------------------------------------------

#HUC12??

reg_all <- lm(perc_incr ~ f101 + f102 + f103 + f104 + f121 + f122 + f123 + 
     f141 + f142 + f143 + f144 + f145 + f146 + f147 + f161 + f162 + f163 + f164 + f165 + 
     f181 + f182 + f183 + f184 + f185 + f186 + f187 + f188 + f189 +
     f201 + f202 + f203 + fnb,
   data = res_fbfm)

reg_pos <- lm(perc_incr ~ f101 + f102 + f103 + f104 + f121 + f122 + f123 + 
                f141 + f142 + f143 + f144 + f145 + f146 + f147 + f161 + f162 + f163 + f164 + f165 + 
                f181 + f182 + f183 + f184 + f185 + f186 + f187 + f188 + f189 +
                f201 + f202 + f203 + fnb,
              data = res_fbfm %>% filter(perc_incr > 0))

# hrm. not helpful? 


### scatter ------------------------------------------------------------------

# long and then loop to scatter plot all 

res_fbfm_long <- res_fbfm %>% 
  mutate(TxIntensity = as.factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                            "500k", "1m", "2m")) %>% 
  pivot_longer(cols = c(f101, f102, f103, f104, f121, f122, f123,  
                        f141, f142, f143, f144, f145, f146, f147, 
                        f161, f162, f163, f164, f165,  
                        f181, f182, f183, f184, f185, f186, f187, f188, f189, 
                        f201, f202, f203, fnb),
               names_to = "fbfm_category",
               values_to = "perc_huc") %>% 
  dplyr::select(-c(ftotal, name))

fbfm_codes <- c("f101", "f102", "f103", "f104", "f121", "f122", "f123",  
                "f141", "f142", "f143", "f144", "f145", "f146", "f147", 
                "f161", "f162", "f163", "f164", "f165",  
                "f181", "f182", "f183", "f184", "f185", "f186", "f187", "f188", "f189", 
                "f201", "f202", "f203", "fnb")

i_colors <- c("bw" = "grey50",
              "base" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[1],
              "500k" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[2],
              "1m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[3],
              "2m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[4])

for (i in seq_along(fbfm_codes)){
  
  this_fbfm_code <- fbfm_codes[[i]]
  
  this_data <- res_fbfm_long %>% 
    filter(fbfm_category == this_fbfm_code)
  
  pfbfm <- ggplot() + 
    geom_point(data = this_data, 
               mapping = aes(x=perc_huc, y=perc_incr, color=TxIntensity)) + 
    scale_color_manual(values = i_colors) + 
    geom_hline(yintercept = 0, color = "blue") + 
    labs(title = paste0("FBFM ", str_remove(this_fbfm_code, "f")),
         x = "Percent of HUC with this FBFM value",
         y = "Percent increase of inversion (if positive)")
  
  ggsave(plot = pfbfm, 
         filename = file.path("plots", "sn_fbfm", paste0(this_fbfm_code, ".jpg")),
         height = 6, width = 8, units = c("in"))
  
}


### compare to baseline and scatter plot DIFFERENCE to baseline ---------------

# use res_fbfm_long

# create similar structure but of baseline data  
# join including by HUC, Year, fbfm_category, then mutate with a single subtraction 


fbfm_bl <- fbfm_prep %>% 
  filter(Priority == "baseline")  %>% 
  pivot_longer(cols = c(f101, f102, f103, f104, f121, f122, f123,  
                        f141, f142, f143, f144, f145, f146, f147, 
                        f161, f162, f163, f164, f165,  
                        f181, f182, f183, f184, f185, f186, f187, f188, f189, 
                        f201, f202, f203, fnb),
               names_to = "fbfm_category",
               values_to = "perc_huc_baseline") %>% 
  dplyr::select(-c(ftotal, Priority, TxType, TxIntensity))

res_fbfm_bl <- res_fbfm_long %>% 
  left_join(fbfm_bl, by = join_by(HUC12, Year, fbfm_category)) %>% 
  mutate(perc_area_diff = perc_huc - perc_huc_baseline)


for (i in seq_along(fbfm_codes)){
  
  this_fbfm_code <- fbfm_codes[[i]]
  
  this_data <- res_fbfm_bl %>% 
    filter(fbfm_category == this_fbfm_code)
  
  pfbfmbl <- ggplot() + 
    geom_point(data = this_data, 
               mapping = aes(x=perc_area_diff, y=perc_incr, color=TxIntensity)) + 
    scale_color_manual(values = i_colors) + 
    geom_hline(yintercept = 0, color = "blue") + 
    labs(title = paste0("FBFM ", str_remove(this_fbfm_code, "f")),
         x = "FBFM category percent difference to baseline",
         y = "Percent increase of inversion (if positive)")
  
  ggsave(plot = pfbfmbl, 
         filename = file.path("plots", "sn_fbfm", paste0("bl_", this_fbfm_code, ".jpg")),
         height = 6, width = 8, units = c("in"))
  
}
