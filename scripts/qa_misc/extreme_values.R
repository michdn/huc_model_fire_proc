# look at extreme values

# CC has some in 2039. 


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### Base Data import -------------------------------------------

res <- read_csv(file.path('results',
                               'absolute',
                               'CC_absolute_expanded_NOFVS_20240319.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

### Search

res2039 <- res %>% 
  filter(Year == 2039)

res2039 %>% 
  arrange(desc(expBurn)) %>% 
  View()

# CC 	180600060410 
# sim yrs 0.1708782. ABP 95 acres. GF 16. (not extreme values, nor HaCBP, HaCFL)
# expBurn is ~5900, much higher than rest (~2000)
# hAc 55326, not extreme

