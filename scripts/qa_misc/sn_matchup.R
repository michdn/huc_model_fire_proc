# matching up new SN with old SN results 

# to see if we can shine light on the 2m = more fire issue

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### Base Data import -------------------------------------------

old <- read_csv(file.path("run_202401_badblend", 
                          "results_csv", 
                          "datacube_weighted_expanded_20240212.csv")) %>% 
  filter(Region == "SN")

new <- read_csv(file.path("results",
                               "datacube", 
                               "datacube_interim_sc_cc_sn_20240403.csv")) %>% 
  filter(Region == "SN")

yrs <- c(2024, 2029, 2034)

### Trim  ----------------------------------------------

new <- new %>% 
  #not the new time point, not trt6 some of which was redone
  filter(!Year == 2039,
         !TxType == "trt6") %>%
  dplyr::select(HUC12, Region,
                Priority, TxIntensity, TxType,
                Year,
                timeFire, timeHybrid, timeWui,
                TSC, SDI,
                HaCBP, HaCFL,
                expBurn, expFlame, expPcActive, expPcSurface)

#crosswalk for time groups

tg <- new %>% 
  select(HUC12, timeFire, timeHybrid, timeWui) %>% 
  distinct()


old <- old %>% 
  #not 2044, not trt6 (some redone)
  filter(!Year == 2044,
         !TxType == "trt6") %>% 
  #simplify columns
  dplyr::select(HUC12, Region, 
                Priority, TxIntensity, TxType,
                Year,
                #fireGroup, hybridGroup, wuiGroup,
                TSC, SDI,
                HaCBP, HaCFL, 
                #active_crown, surface, 
                expBurn, expFlame, expPcActive, expPcSurface) %>% 
  left_join(tg, by = join_by(HUC12))



## long ---------------------------------------------------------------

both <- bind_rows(old %>% mutate(status = "old"), 
                  new %>% mutate(status = "new")) %>% 
  mutate(status = as_factor(status),
         status = forcats::fct_relevel(status, "old", "new")) %>% 
  #For graphing in the correct order
  # make factor with set order (priority)
  mutate(Priority = as_factor(Priority),
         Priority = forcats::fct_relevel(Priority,
                                         "Fire", "WUI", "Hybrid"),
         #Make factor with set order (intensity))
         TxIntensity = as_factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                            "500k", "1m", "2m"))


## Explore

ggplot() +
  geom_boxplot(data = both %>% 
                 mutate(Year = factor(Year, levels=c(2024, 2029, 2034))) %>% 
                 filter(Priority == "Fire"),
               mapping = aes(x = Year, y = expFlame,
                             fill = status)) +
  facet_wrap(~TxType) + 
  labs(title = "SN Fire", subtitle = "Matching up old and new runs")

ggplot() +
  geom_boxplot(data = both %>% 
                 mutate(Year = factor(Year, levels=c(2024, 2029, 2034))) %>% 
                 filter(Priority == "WUI"),
               mapping = aes(x = Year, y = expFlame,
                             fill = status)) +
  facet_wrap(~TxType) + 
  labs(title = "SN WUI", subtitle = "Matching up old and new runs")

ggplot() +
  geom_boxplot(data = both %>% 
                 mutate(Year = factor(Year, levels=c(2024, 2029, 2034))) %>% 
                 filter(Priority == "Hybrid"),
               mapping = aes(x = Year, y = expFlame,
                             fill = status)) +
  facet_wrap(~TxType) + 
  labs(title = "SN Hybrid", subtitle = "Matching up old and new runs")

## treating timing

ggplot() +
  geom_boxplot(data = both %>% 
                 mutate(Year = factor(Year, levels=c(2024, 2029, 2034))) %>% 
                 filter(Priority == "Fire", 
                        TxType == "trt1",
                        !timeFire == "2039_yr16to20"),
               mapping = aes(x = Year, y = expFlame,
                             fill = status)) +
  facet_wrap(~timeFire) + 
  labs(title = "SN Fire trt1", subtitle = "Matching up old and new runs")

ggplot() +
  geom_boxplot(data = both %>% 
                 mutate(Year = factor(Year, levels=c(2024, 2029, 2034))) %>% 
                 filter(Priority == "Fire", 
                        TxType == "trt1",
                        !timeFire == "2039_yr16to20"),
               mapping = aes(x = Year, y = HaCFL/HaCBP,
                             fill = status)) +
  facet_wrap(~timeFire) + 
  labs(title = "SN Fire trt1", subtitle = "Matching up old and new runs")


## intensity, treatment timing -------------------------------------

ggplot() +
  geom_boxplot(data = both %>% 
                 mutate(Year = factor(Year, levels=c(2024, 2029, 2034))) %>% 
                 filter(Priority == "Fire", 
                        TxType == "trt1",
                        !timeFire == "2039_yr16to20"),
               mapping = aes(x = Year, y = expFlame,
                             fill = status)) +
  facet_wrap(~TxIntensity+timeFire) + 
  labs(title = "SN Fire trt1", subtitle = "Matching up old and new runs")


ggplot() +
  geom_boxplot(data = both %>% 
                 mutate(Year = factor(Year, levels=c(2024, 2029, 2034))) %>% 
                 filter(Priority == "Fire", 
                        TxType == "trt1",
                        !timeFire == "2039_yr16to20"),
               mapping = aes(x = Year, y = expFlame,
                             fill = TxIntensity)) +
  facet_wrap(~status+timeFire) + 
  labs(title = "SN Fire trt1", subtitle = "Matching up old and new runs")


## sampling hucs explore -----------------------------------------------

set.seed = 9
h6 <- new %>% pull(HUC12) %>% unique() %>% sample(6)

both6 <- both %>% 
  filter(HUC12 %in% h6)


ggplot(data = both6 %>% 
         filter(Priority == "Fire", 
                TxType == "trt1")) + 
  geom_line(aes(x=Year, y=expFlame, color = status, linetype = TxIntensity)) + 
  scale_x_continuous(breaks = yrs) +
  facet_wrap(~HUC12, scales = "free_y") 



## wide --------------------------------------------------------------

# #if joining rename old
# rename(TSC_old = TSC, SDI_old = SDI,
#        HaCBP_old = HaCBP, HaCFL_old = HaCFL,
#        #active_crown_old = active_crown, surface_old = surface,
#        expBurn_old = expBurn, expFlame_old = expFlame,
#        expPcActive_old = expPcActive, expPcSurface_old = expPcSurface) 


# jt <- new %>% 
#   #not the new time point, not trt6 some of which was redone
#   filter(!Year == 2039,
#          !TxType == "trt6") %>% 
#   dplyr::select(HUC12, Region, 
#                 Priority, TxIntensity, TxType,
#                 Year,
#                 timeFire, timeHybrid, timeWui,
#                 TSC, SDI,
#                 HaCBP, HaCFL, 
#                 expBurn, expFlame, expPcActive, expPcSurface) %>% 
#   left_join(old, 
#             by = join_by(HUC12, Region, Priority, TxIntensity, TxType, Year))
# 
# 
# #explore Fire trt1
# f1 <- jt %>% 
#   filter(Priority == "Fire",
#          TxType == "trt1")


