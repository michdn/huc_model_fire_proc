# script to explore Year 2024 treatment group effects
#  2024 includes treatments yr1-5 (treatment group = 25)

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### Base Data import -------------------------------------------

res_orig <- read_csv(file.path('results_csv', 
                               'datacube_weighted_20240212.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

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


### Regions -------------------------------------------------------

rsc <- res %>% 
  filter(Region == "SC")

rnc <- res %>% 
  filter(Region == "NC")

rsn <- res %>% 
  filter(Region == "SN")

rcc <- res %>% 
  filter(Region == "CC")


### SC -------------------------------------------------------

## Fire

# 2024

#TSC
ggplot() +
  geom_boxplot(data = rsc %>%
                 filter(Year == 2024, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "SC 2024 Fire",
       x = "Treatment Intensity")

ggplot() +
  geom_boxplot(data = rsc %>%
                 filter(Year == 2024, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=SDI),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "SC 2024 Fire",
       x = "Treatment Intensity")


#2029 
ggplot() +
  geom_boxplot(data = rsc %>%
                 filter(Year == 2029, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "SC 2029 Fire",
       x = "Treatment Intensity")

ggplot() +
  geom_boxplot(data = rsc %>%
                 filter(Year == 2029, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=SDI),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "SC 2029 Fire",
       x = "Treatment Intensity")


#2034
ggplot() +
  geom_boxplot(data = rsc %>%
                 filter(Year == 2034, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "SC 2034 Fire",
       x = "Treatment Intensity")


ggplot() +
  geom_boxplot(data = rsc %>%
                 filter(Year == 2034, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=SDI),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "SC 2034 Fire",
       x = "Treatment Intensity")


#2044
ggplot() +
  geom_boxplot(data = rsc %>%
                 filter(Year == 2044, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "SC 2044 Fire",
       x = "Treatment Intensity")


ggplot() +
  geom_boxplot(data = rsc %>%
                 filter(Year == 2044, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=SDI),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "SC 2044 Fire",
       x = "Treatment Intensity")



# WUI
ggplot() +
  geom_boxplot(data = rsc %>%
                 filter(Year == 2024, 
                        Priority == "WUI"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+wuiGroup) +
  labs(title = "SC 2024 WUI",
       x = "Treatment Intensity")

#2029 
ggplot() +
  geom_boxplot(data = rsc %>%
                 filter(Year == 2029, 
                        Priority == "WUI"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+wuiGroup) +
  labs(title = "SC 2029 WUI",
       x = "Treatment Intensity")

#2034 
ggplot() +
  geom_boxplot(data = rsc %>%
                 filter(Year == 2034, 
                        Priority == "WUI"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+wuiGroup) +
  labs(title = "SC 2034 WUI",
       x = "Treatment Intensity")

#2044 
ggplot() +
  geom_boxplot(data = rsc %>%
                 filter(Year == 2044, 
                        Priority == "WUI"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+wuiGroup) +
  labs(title = "SC 2044 WUI",
       x = "Treatment Intensity")


# Hybrid
ggplot() +
  geom_boxplot(data = rsc %>%
                 filter(Year == 2024, 
                        Priority == "Hybrid"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+hybridGroup) +
  labs(title = "SC 2024 Hybrid",
       x = "Treatment Intensity")

#2029 
ggplot() +
  geom_boxplot(data = rsc %>%
                 filter(Year == 2029, 
                        Priority == "Hybrid"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+hybridGroup) +
  labs(title = "SC 2029 Hybrid",
       x = "Treatment Intensity")

#2034 
ggplot() +
  geom_boxplot(data = rsc %>%
                 filter(Year == 2034, 
                        Priority == "Hybrid"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+hybridGroup) +
  labs(title = "SC 2034 Hybrid",
       x = "Treatment Intensity")

#2044 
ggplot() +
  geom_boxplot(data = rsc %>%
                 filter(Year == 2044, 
                        Priority == "Hybrid"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+hybridGroup) +
  labs(title = "SC 2044 Hybrid",
       x = "Treatment Intensity")


### NC -------------------------------------------------------

## Fire

#2024
ggplot() +
  geom_boxplot(data = rnc %>%
                 filter(Year == 2024, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "NC 2024 Fire",
       x = "Treatment Intensity")

ggplot() +
  geom_boxplot(data = rnc %>%
                 filter(Year == 2024, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=SDI),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "NC 2024 Fire",
       x = "Treatment Intensity")



#2029 
ggplot() +
  geom_boxplot(data = rnc %>%
                 filter(Year == 2029, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "NC 2029 Fire",
       x = "Treatment Intensity")

#2034
ggplot() +
  geom_boxplot(data = rnc %>%
                 filter(Year == 2034, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "NC 2034 Fire",
       x = "Treatment Intensity")

#2044
ggplot() +
  geom_boxplot(data = rnc %>%
                 filter(Year == 2044, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "NC 2044 Fire",
       x = "Treatment Intensity")


# WUI
ggplot() +
  geom_boxplot(data = rnc %>%
                 filter(Year == 2024, 
                        Priority == "WUI"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+wuiGroup) +
  labs(title = "NC 2024 WUI",
       x = "Treatment Intensity")

#2029 
ggplot() +
  geom_boxplot(data = rnc %>%
                 filter(Year == 2029, 
                        Priority == "WUI"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+wuiGroup) +
  labs(title = "NC 2029 WUI",
       x = "Treatment Intensity")

#2034 
ggplot() +
  geom_boxplot(data = rnc %>%
                 filter(Year == 2034, 
                        Priority == "WUI"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+wuiGroup) +
  labs(title = "NC 2034 WUI",
       x = "Treatment Intensity")

#2044 
ggplot() +
  geom_boxplot(data = rnc %>%
                 filter(Year == 2044, 
                        Priority == "WUI"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+wuiGroup) +
  labs(title = "NC 2044 WUI",
       x = "Treatment Intensity")


# Hybrid
ggplot() +
  geom_boxplot(data = rnc %>%
                 filter(Year == 2024, 
                        Priority == "Hybrid"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+hybridGroup) +
  labs(title = "NC 2024 Hybrid",
       x = "Treatment Intensity")

#2029 
ggplot() +
  geom_boxplot(data = rnc %>%
                 filter(Year == 2029, 
                        Priority == "Hybrid"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+hybridGroup) +
  labs(title = "NC 2029 Hybrid",
       x = "Treatment Intensity")

#2034 
ggplot() +
  geom_boxplot(data = rnc %>%
                 filter(Year == 2034, 
                        Priority == "Hybrid"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+hybridGroup) +
  labs(title = "NC 2034 Hybrid",
       x = "Treatment Intensity")

#2044 
ggplot() +
  geom_boxplot(data = rnc %>%
                 filter(Year == 2044, 
                        Priority == "Hybrid"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+hybridGroup) +
  labs(title = "NC 2044 Hybrid",
       x = "Treatment Intensity")




### SN -------------------------------------------------------

## Fire

# 2024

ggplot() +
  geom_boxplot(data = rsn %>%
                 filter(Year == 2024, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "SN 2024 Fire",
       x = "Treatment Intensity")

ggplot() +
  geom_boxplot(data = rsn %>%
                 filter(Year == 2024, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=SDI),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "SN 2024 Fire",
       x = "Treatment Intensity")


#2029 
ggplot() +
  geom_boxplot(data = rsn %>%
                 filter(Year == 2029, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "SN 2029 Fire",
       x = "Treatment Intensity")

#2034
ggplot() +
  geom_boxplot(data = rsn %>%
                 filter(Year == 2034, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "SN 2034 Fire",
       x = "Treatment Intensity")

#2044
ggplot() +
  geom_boxplot(data = rsn %>%
                 filter(Year == 2044, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "SN 2044 Fire",
       x = "Treatment Intensity")


# WUI
ggplot() +
  geom_boxplot(data = rsn %>%
                 filter(Year == 2024, 
                        Priority == "WUI"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+wuiGroup) +
  labs(title = "SN 2024 WUI",
       x = "Treatment Intensity")

#2029 
ggplot() +
  geom_boxplot(data = rsn %>%
                 filter(Year == 2029, 
                        Priority == "WUI"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+wuiGroup) +
  labs(title = "SN 2029 WUI",
       x = "Treatment Intensity")

#2034 
ggplot() +
  geom_boxplot(data = rsn %>%
                 filter(Year == 2034, 
                        Priority == "WUI"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+wuiGroup) +
  labs(title = "SN 2034 WUI",
       x = "Treatment Intensity")

#2044 
ggplot() +
  geom_boxplot(data = rsn %>%
                 filter(Year == 2044, 
                        Priority == "WUI"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+wuiGroup) +
  labs(title = "SN 2044 WUI",
       x = "Treatment Intensity")


# Hybrid
ggplot() +
  geom_boxplot(data = rsn %>%
                 filter(Year == 2024, 
                        Priority == "Hybrid"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+hybridGroup) +
  labs(title = "SN 2024 Hybrid",
       x = "Treatment Intensity")

#2029 
ggplot() +
  geom_boxplot(data = rsn %>%
                 filter(Year == 2029, 
                        Priority == "Hybrid"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+hybridGroup) +
  labs(title = "SN 2029 Hybrid",
       x = "Treatment Intensity")

#2034 
ggplot() +
  geom_boxplot(data = rsn %>%
                 filter(Year == 2034, 
                        Priority == "Hybrid"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+hybridGroup) +
  labs(title = "SN 2034 Hybrid",
       x = "Treatment Intensity")

#2044 
ggplot() +
  geom_boxplot(data = rsn %>%
                 filter(Year == 2044, 
                        Priority == "Hybrid"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+hybridGroup) +
  labs(title = "SN 2044 Hybrid",
       x = "Treatment Intensity")

### CC -------------------------------------------------------

## Fire

# 2024

ggplot() +
  geom_boxplot(data = rcc %>%
                 filter(Year == 2024, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "CC 2024 Fire",
       x = "Treatment Intensity")

ggplot() +
  geom_boxplot(data = rcc %>%
                 filter(Year == 2024, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=SDI),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "CC 2024 Fire",
       x = "Treatment Intensity")


#2029 
ggplot() +
  geom_boxplot(data = rcc %>%
                 filter(Year == 2029, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "CC 2029 Fire",
       x = "Treatment Intensity")

#2034
ggplot() +
  geom_boxplot(data = rcc %>%
                 filter(Year == 2034, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "CC 2034 Fire",
       x = "Treatment Intensity")

#2044
ggplot() +
  geom_boxplot(data = rcc %>%
                 filter(Year == 2044, 
                        Priority == "Fire"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+fireGroup) +
  labs(title = "CC 2044 Fire",
       x = "Treatment Intensity")


# WUI
ggplot() +
  geom_boxplot(data = rcc %>%
                 filter(Year == 2024, 
                        Priority == "WUI"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+wuiGroup) +
  labs(title = "CC 2024 WUI",
       x = "Treatment Intensity")

#2029 
ggplot() +
  geom_boxplot(data = rcc %>%
                 filter(Year == 2029, 
                        Priority == "WUI"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+wuiGroup) +
  labs(title = "CC 2029 WUI",
       x = "Treatment Intensity")

#2034 
ggplot() +
  geom_boxplot(data = rcc %>%
                 filter(Year == 2034, 
                        Priority == "WUI"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+wuiGroup) +
  labs(title = "CC 2034 WUI",
       x = "Treatment Intensity")

#2044 
ggplot() +
  geom_boxplot(data = rcc %>%
                 filter(Year == 2044, 
                        Priority == "WUI"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+wuiGroup) +
  labs(title = "CC 2044 WUI",
       x = "Treatment Intensity")


# Hybrid
ggplot() +
  geom_boxplot(data = rcc %>%
                 filter(Year == 2024, 
                        Priority == "Hybrid"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+hybridGroup) +
  labs(title = "CC 2024 Hybrid",
       x = "Treatment Intensity")

#2029 
ggplot() +
  geom_boxplot(data = rcc %>%
                 filter(Year == 2029, 
                        Priority == "Hybrid"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+hybridGroup) +
  labs(title = "CC 2029 Hybrid",
       x = "Treatment Intensity")

#2034 
ggplot() +
  geom_boxplot(data = rcc %>%
                 filter(Year == 2034, 
                        Priority == "Hybrid"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+hybridGroup) +
  labs(title = "CC 2034 Hybrid",
       x = "Treatment Intensity")

#2044 
ggplot() +
  geom_boxplot(data = rcc %>%
                 filter(Year == 2044, 
                        Priority == "Hybrid"),
               mapping = aes(x=TxIntensity, y=TSC),
               outlier.color = 'black',
               outlier.shape = 16,
               outlier.size = 2,
               notch = TRUE) + 
  facet_wrap(~TxType+hybridGroup) +
  labs(title = "CC 2044 Hybrid",
       x = "Treatment Intensity")


## --------------------------------------

res %>% 
  select(HUC12, fireGroup, wuiGroup, hybridGroup) %>% 
  unique() %>% 
  group_by(fireGroup, wuiGroup, hybridGroup) %>% 
  summarize(count = n()) %>% 
  View()

res %>% 
  select(HUC12, fireGroup, wuiGroup, hybridGroup) %>% 
  unique() %>% 
  group_by(fireGroup) %>% 
  summarize(firecount = n()) 

res %>% 
  select(HUC12, fireGroup, wuiGroup, hybridGroup) %>% 
  unique() %>% 
  group_by(wuiGroup) %>% 
  summarize(wuicount = n()) 

res %>% 
  select(HUC12, fireGroup, wuiGroup, hybridGroup) %>% 
  unique() %>% 
  group_by(hybridGroup) %>% 
  summarize(hybridcount = n()) 
