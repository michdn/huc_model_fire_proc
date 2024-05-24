# WUI treat 100 (untreated) and high fire
# Use to compare against? 

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### Results import -------------------------------------------

# can change between region-only and full datacube. 
# will overwrite
res_orig <- read_csv(file.path('results',
                               'absolute',
                               'SC_absolute_expanded_NOFVS_20240319.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

### Data set up ------------------------------------------------

# Want priority order: 'Fire', 'WUI', 'Hybrid'
# Want intensity order: '500k', '1m', '2m'

res <- res_orig %>% 
  #For graphing in the correct order
  # make factor with set order (priority)
  mutate(Priority = as.factor(Priority),
         Priority = forcats::fct_relevel(Priority,
                                         "Fire", "WUI", "Hybrid"),
         #Make factor with set order (intensity))
         TxIntensity = as.factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                            "500k", "1m", "2m"))

year_breaks <- c(2024, 2029, 2034, 2039)


### WUI 100 hucs w/ fire 25 or 50 ----------------------------------------

res_wui100 <- res %>% 
  filter(wuiGroup == 100,
         (fireGroup == 25 | fireGroup == 50))

res_wui100 %>% 
  group_by(mas_scenario, Year, fireGroup) %>% 
  summarize(count = n())
#19 firegroup 25, and 41 in firegroup 50

res_w100_f25 <- res_wui100 %>% 
  filter(fireGroup == 25)


### plot ts to see ------------------------------------------------------

#WUI priority
# x year
# 

# what to do with trt/intensity? (these were not treated)

# individual HUC graphs? 

ggplot() +
  geom_boxplot(data = res_w100_f25 %>% 
                 filter(Priority == "WUI"),
               mapping = aes(x = Year, y = expFlame, group=Year)) +
  scale_x_continuous(breaks = year_breaks) +
  facet_wrap(~TxIntensity+TxType, scales = 'fixed') + 
  labs(title = "SC")
