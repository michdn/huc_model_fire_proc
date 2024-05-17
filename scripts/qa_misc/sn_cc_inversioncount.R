# inversion count 

# of 2m intensity compared to BASELINE

# SN vs CC

# by year relative to treatment
#  e.g. AT treatment year, 1 yr-period post, 2 yr-periods post, 3 yr-periods post

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### Data -----------------------------------------------------


sn <- read_csv(file.path("results",
                         "absolute", 
                         "SN_SNbl_SNbw_absolute_20240423.csv")) %>% 
  mutate(HUC12 = as.character(HUC12)) 

cc <- read_csv(file.path("results",
                         "absolute", 
                         "CC_CCbl_CCbw_absolute_20240510.csv")) %>% 
  mutate(HUC12 = as.character(HUC12))

sc <- read_csv(file.path("results",
                         "absolute", 
                         "SC_SCbl_SCbw_absolute_20240510.csv")) %>% 
  mutate(HUC12 = as.character(HUC12))


### 2m & Baseline -------------------------------------------

sn_bl <- sn %>% 
  filter(Priority == "baseline") %>% 
  dplyr::select(HUC12, Year, HaCFL) %>% 
  rename(hacfl_base = HaCFL)

sn_b2 <- sn %>% 
  #only 2m 
  filter(TxIntensity == "2m") %>% 
  #join with bl for single row
  left_join(sn_bl, by = join_by(HUC12, Year)) %>% 
  #calculate differences (base - 2m)
  mutate(hacfl_pc = (hacfl_base - HaCFL) / hacfl_base,
         hacfl_inv = if_else(hacfl_pc < 0, TRUE, FALSE),
         hacfl_inv001 = if_else(hacfl_pc < -0.01, TRUE, FALSE),
         hacfl_inv01= if_else(hacfl_pc < -0.1, TRUE, FALSE))


cc_bl <- cc %>% 
  filter(Priority == "baseline") %>% 
  dplyr::select(HUC12, Year, HaCFL) %>% 
  rename(hacfl_base = HaCFL)

cc_b2 <- cc %>% 
  #only 2m 
  filter(TxIntensity == "2m") %>% 
  #join with bl for single row
  left_join(cc_bl, by = join_by(HUC12, Year)) %>% 
  #calculate differences (base - 2m)
  mutate(hacfl_pc = (hacfl_base - HaCFL) / hacfl_base,
         hacfl_inv = if_else(hacfl_pc < 0, TRUE, FALSE),
         hacfl_inv001 = if_else(hacfl_pc < -0.01, TRUE, FALSE),
         hacfl_inv01= if_else(hacfl_pc < -0.1, TRUE, FALSE))


sc_bl <- sc %>% 
  filter(Priority == "baseline") %>% 
  dplyr::select(HUC12, Year, HaCFL) %>% 
  rename(hacfl_base = HaCFL)

sc_b2 <- sc %>% 
  #only 2m 
  filter(TxIntensity == "2m") %>% 
  #join with bl for single row
  left_join(sc_bl, by = join_by(HUC12, Year)) %>% 
  #calculate differences (base - 2m), percent change
  mutate(hacfl_pc = (hacfl_base - HaCFL) / hacfl_base,
         hacfl_inv = if_else(hacfl_pc < 0, TRUE, FALSE),
         hacfl_inv001 = if_else(hacfl_pc < -0.01, TRUE, FALSE),
         hacfl_inv01= if_else(hacfl_pc < -0.1, TRUE, FALSE))


b2 <- bind_rows(sn_b2, cc_b2, sc_b2)
  
### treatment year and years pre/post --------------------------

b2 <- b2 %>% 
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
    Priority == "WUI" ~ wui_trt_yr))

b2_filt <- b2 %>% 
  mutate(yrs_post_tx = Year - trt_yr) %>% 
  #at or after treatment
  filter(yrs_post_tx >= 0)

b2_sum <- b2_filt %>% 
  group_by(Region, yrs_post_tx) %>% 
  summarize(count_inv = sum(hacfl_inv),
            count_inv001 = sum(hacfl_inv001),
            count_inv01 = sum(hacfl_inv01),
            count_tot = n()) %>% 
  mutate(inv01_perc = count_inv01/count_tot*100) 

b2_sum

p_inv01 <- ggplot() + 
  geom_point(data=b2_sum,
             mapping=aes(x=yrs_post_tx,
                         y=inv01_perc,
                         color=Region)) + 
  geom_line(data=b2_sum,
             mapping=aes(x=yrs_post_tx,
                         y=inv01_perc,
                         color=Region)) + 
  labs(title = "Region comparison of >10% inversion of 2m intensity with baseline",
       subtitle = "At or after treatment",
       x = "Years Post Treatment",
       y = "Percent of HUC-2m-scenarios with thresholded inversion") + 
  theme_bw()

p_inv01

ggsave(plot=p_inv01, 
       filename="qa/region_comparison_2minversions_ataftertx.png")
