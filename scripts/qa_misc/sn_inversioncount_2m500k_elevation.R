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


elev_stats <- readRDS("qa/elev/hucs_elevation_stats.RDS")

### 2m & 500k -------------------------------------------

sn_500k <- sn %>% 
  filter(TxIntensity == "500k") %>% 
  dplyr::select(HUC12, Priority, TxType, Year, HaCFL) %>% 
  rename(hacfl_500k = HaCFL)

sn_500k2m <- sn %>% 
  #only 2m 
  filter(TxIntensity == "2m") %>% 
  #join with bl for single row
  left_join(sn_500k, by = join_by(HUC12, Year, Priority, TxType)) %>% 
  #calculate differences (base - 2m)
  mutate(hacfl_500k2m_pc = (hacfl_500k - HaCFL) / hacfl_500k,
         hacfl_inv = if_else(hacfl_500k2m_pc < 0, TRUE, FALSE),
         hacfl_inv001 = if_else(hacfl_500k2m_pc < -0.01, TRUE, FALSE),
         hacfl_inv01= if_else(hacfl_500k2m_pc < -0.1, TRUE, FALSE))

sn_elev <- sn_500k2m %>% 
  left_join(elev_stats, by = join_by(HUC12))

# relative to treatment year 

sn_s <- sn_elev %>% 
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
  #rel 
  mutate(rel_trt_yr = Year - trt_yr,
         pretx = if_else(rel_trt_yr < 0, TRUE, FALSE),
         attx = if_else(rel_trt_yr == 0, TRUE, FALSE),
         posttx = if_else(rel_trt_yr > 0, TRUE, FALSE),
         post5 = if_else(rel_trt_yr == 5, TRUE, FALSE),
         rel_tx = case_when(
           pretx == TRUE ~ "Pretreatment",
           attx == TRUE ~ "At treatment",
           post5 == TRUE ~ "Five years post",
           rel_trt_yr == 10 ~ "Ten years post",
           .default = NA), #"POST >= 10"),
         rel_tx = as.factor(rel_tx),
         rel_tx = forcats::fct_relevel(rel_tx, 
                                       "Pretreatment", "At treatment", "Five years post"))


### scatters at tx rel yr, midmontane vs not --------------------------

p_mm <- ggplot() + 
  geom_point(data = sn_s %>% 
               filter(!is.na(rel_tx),
                      !pretx,
                      Priority == "Fire"), 
             mapping=aes(x=mean_ft, y=hacfl_500k2m_pc, 
                         color=midmontane25007000,
                         shape=midmontane25007000)) + 
  #double plot midmontane to show up on top, hack!
  geom_point(data = sn_s %>% 
               filter(!is.na(rel_tx),
                      !pretx,
                      Priority == "Fire",
                      midmontane25007000), 
             mapping=aes(x=mean_ft, y=hacfl_500k2m_pc, 
                         color=midmontane25007000,
                         shape=midmontane25007000)) + 
  geom_hline(yintercept=0) + 
  scale_color_manual("Mid-montane\n(2500-7000 ft min/max)", 
                     values = c("TRUE" = "#ff7f00", 
                                "FALSE" = "royalblue")) + 
  scale_shape_manual("Mid-montane\n(2500-7000 ft min/max)", 
                     values = c("TRUE" = 19, 
                                "FALSE" = 1)) +
  labs(title = "SN Fire: 500k-2m/500k HaCFL inversions",
       subtitle = "Negative hacfl_500k2m_pc are INVERSIONS") + 
  facet_wrap(~rel_tx+TxType, dir="v") + 
  theme_bw()

p_mm

ggsave(plot=p_mm,
       filename=file.path("plots/midmontane_hacfl_inversions_SN_Fire.jpg"),
       height = 8, width = 12, units = "in")






### binary counts --------------------------------
# sns_sum <- sn_s %>% 
#   group_by(Region, yrs_rel_tx, midmontane25007000) %>% 
#   summarize(count_inv = sum(hacfl_inv),
#             #count_inv001 = sum(hacfl_inv001),
#             #count_inv01 = sum(hacfl_inv01),
#             count_tot = n()) %>% 
#   mutate(inv_perc = count_inv/count_tot*100) 
# 
# sns_sum
# 
# p_inv <- ggplot() + 
#   geom_point(data=sns_sum,
#              mapping=aes(x=yrs_rel_tx,
#                          y=inv_perc,
#                          color=midmontane25007000)) + 
#   geom_line(data=sns_sum,
#              mapping=aes(x=yrs_rel_tx,
#                          y=inv_perc,
#                          color=midmontane25007000)) + 
#   labs(title = "Comparison count of 2m to 500k inversions in mid-montane",
#        subtitle = "At or after treatment",
#        x = "Years Relative to Treatment",
#        y = "Percent of HUC-2m/500k-scenarios with inversion") + 
#   theme_bw()
# 
# p_inv
# 
# ggsave(plot=p_inv, 
#        filename="qa/region_comparison_2minversions_ataftertx.png")


