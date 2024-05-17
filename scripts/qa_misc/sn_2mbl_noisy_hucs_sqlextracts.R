
#investigate noisy HUCs (Fire priority)
# large (>10%) differences between baseline and 2m PREtreatment 


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)


### Data -----------------------------------------------------

noisy <- readRDS("qa/scatter_noisyHUCs_10_Fire.RDS")


input_folder <- file.path('results', 'extracts')

cfl_sn <- readRDS(file.path(input_folder, 
                         paste0("SN", '_cfl_all_fires_from_sql.RDS'))) 

cfl_bl <- readRDS(file.path(input_folder, 
                            paste0("SNbl", '_cfl_all_fires_from_sql.RDS'))) 

cfl_bw <- readRDS(file.path(input_folder, 
                            paste0("SNbw", '_cfl_all_fires_from_sql.RDS'))) 


res <- read_csv(file.path("results",
                          "absolute", #"datacube",
                          "SN_SNbl_SNbw_absolute_20240423.csv")) %>%
  mutate(HUC12 = as.character(HUC12))

huc_trt_yr <- res %>% 
  filter(Priority %in% c("Fire", "WUI", "Hybrid")) %>% 
  dplyr::select(HUC12, Priority, timeFire, timeHybrid, timeWui) %>% 
  distinct() %>% 
  #treatment years
  separate_wider_delim(timeFire, "_", names=c("fire_trt_yr_txt", NA)) %>% 
  separate_wider_delim(timeHybrid, "_", names=c("hybrid_trt_yr_txt", NA)) %>% 
  mutate(fire_trt_yr = as.numeric(fire_trt_yr_txt),
         hybrid_trt_yr = as.numeric(hybrid_trt_yr_txt),
         wui_trt_yr_txt = case_when(
           timeWui == "2024_2039_yr1to5_16to20" ~ "2024_2039",
           timeWui == "2029_yr6to10" ~ "2029",
           timeWui == "2034_yr11to15" ~ "2034",
           timeWui == "Not treated" ~ "Untreated"),
         wui_trt_yr = case_when(
           timeWui == "2024_2039_yr1to5_16to20" ~ 2024,
           timeWui == "2029_yr6to10" ~ 2029,
           timeWui == "2034_yr11to15" ~ 2034,
           timeWui == "Not treated" ~ 9999)) %>% 
  #treat year for priority
  mutate(trt_yr = case_when(
    Priority == "Fire" ~ fire_trt_yr,
    Priority == "Hybrid" ~ hybrid_trt_yr,
    Priority == "WUI" ~ wui_trt_yr),
    trt_yr_txt = case_when(
      Priority == "Fire" ~ fire_trt_yr_txt,
      Priority == "Hybrid" ~ hybrid_trt_yr_txt,
      Priority == "WUI" ~ wui_trt_yr_txt)) %>%
  #final columns
  dplyr::select(HUC12, Priority, trt_yr, trt_yr_txt)


### explore one -----------------------------------------------------------

huc1 <- noisy %>% arrange(desc(abs(hacfl_pc))) %>% slice(1) %>% pull(HUC12)

huc_trt_yr %>% 
  filter(HUC12 == huc1)

huc1_sn <- cfl_sn %>% filter(HUC12 == huc1)
huc1_bl <- cfl_bl %>% filter(HUC12 == huc1)
huc1_bw <- cfl_bw %>% filter(HUC12 == huc1)

huc1_sn %>%  
  filter(Priority == "Fire",
         TxType == "trt1") %>% 
  group_by(Year, TxIntensity) %>% 
  summarize(mean_haf = mean(huc_avg_fl))
# Year  TxIntensity mean_haf
# <chr> <chr>          <dbl>
#   1 2024  1m            0.0990
# 2 2024  2m            0.0990
# 3 2024  500k          0.0990
# 4 2029  1m            0.0983
# 5 2029  2m            0.0983
# 6 2029  500k          0.0983
# 7 2034  1m            0.0765
# 8 2034  2m            0.0708
# 9 2034  500k          0.0855
# 10 2039  1m            0.0987
# 11 2039  2m            0.103 
# 12 2039  500k          0.0980

huc1_bl %>% 
  group_by(Year) %>% 
  summarize(mean_haf = mean(huc_avg_fl))
# Year  mean_haf
# <chr>    <dbl>
#   1 2024    0.0680
# 2 2029    0.0675
# 3 2034    0.0623
# 4 2039    0.0661

huc1_sn %>% 
  filter(Priority == "Fire",
         TxType == "trt1",
         TxIntensity == "2m",
         Year == 2024) %>% 
  pull(huc_avg_fl) %>% 
  summary()

huc1_bl %>% 
  filter(Year == 2024) %>% 
  pull(huc_avg_fl) %>% 
  summary()

huc1_matched_firetrt1 <- huc1_sn %>% 
  filter(Priority == "Fire",
         TxType == "trt1") %>% 
  left_join(huc1_bl %>% 
              dplyr::select(HUC12, Year, fire_i, huc_avg_fl) %>% 
              rename(huc_avg_fl_base = huc_avg_fl), 
            by = join_by(HUC12, Year, fire_i))

ggplot() + 
  geom_point(data=huc1_matched_firetrt1,
             mapping=aes(x=huc_avg_fl_base, y=huc_avg_fl)) + 
  theme(aspect.ratio = 1) +
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=huc1_matched_firetrt1,
              mapping=aes(x=huc_avg_fl_base, y=huc_avg_fl),
              method=lm,
              geom="smooth",
              formula = 'y ~ x',
              fullrange = TRUE) + 
  labs(title=paste0("HUC: ", huc1, " Fire trt1 2m versus baseline")) + 
  facet_grid(~Year)


#compare aginst 'normal' HUC
huc2 <- "160402030901"

huc_trt_yr %>% 
  filter(HUC12 == huc2)

huc2_sn <- cfl_sn %>% filter(HUC12 == huc2)
huc2_bl <- cfl_bl %>% filter(HUC12 == huc2)
huc2_bw <- cfl_bw %>% filter(HUC12 == huc2)

huc2_sn %>%  
  filter(Priority == "Fire",
         TxType == "trt1") %>% 
  group_by(Year, TxIntensity) %>% 
  summarize(mean_haf = mean(huc_avg_fl))

huc2_bl %>% 
  group_by(Year) %>% 
  summarize(mean_haf = mean(huc_avg_fl))

huc2_sn %>% 
  filter(Priority == "Fire",
         TxType == "trt1",
         TxIntensity == "2m",
         Year == 2024) %>% 
  pull(huc_avg_fl) %>% 
  summary()

huc2_bl %>% 
  filter(Year == 2024) %>% 
  pull(huc_avg_fl) %>% 
  summary()

huc2_matched_firetrt1 <- huc2_sn %>% 
  filter(Priority == "Fire",
         TxType == "trt1") %>% 
  left_join(huc2_bl %>% 
              dplyr::select(HUC12, Year, fire_i, huc_avg_fl) %>% 
              rename(huc_avg_fl_base = huc_avg_fl), 
            by = join_by(HUC12, Year, fire_i))

ggplot() + 
  geom_point(data=huc2_matched_firetrt1,
             mapping=aes(x=huc_avg_fl_base, y=huc_avg_fl)) + 
  theme(aspect.ratio = 1) +
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=huc2_matched_firetrt1,
              mapping=aes(x=huc_avg_fl_base, y=huc_avg_fl),
              method=lm,
              geom="smooth",
              formula = 'y ~ x',
              fullrange = TRUE) + 
  labs(title=paste0("HUC: ", huc2, " Fire trt1 2m versus baseline")) + 
  facet_grid(~Year)
