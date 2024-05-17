# FBFM model and inversion

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  viridis)


### Results import -------------------------------------------

#FBFM pixel counts
fbfm_raw <- readRDS(file.path("qa", "SN_FBFM_zonal_counts.RDS")) 

fbfm <- fbfm_raw %>% 
  mutate(across(c(f101, f102, f103, f104, f121, f122, f123, f141, f142, f143,
                  f144, f145, f146, f147, f161, f162, f163, f164, f165,
                  f181, f182, f183, f184, f185, f186, f187, f188, f189,
                  f201, f202, f203, 
                  f91, f92, f93, f98, f99),
                ~ ./ftotal))



### specific ones -------------------------------------------

t1 <- fbfm %>% 
  filter(HUC12 == "180200020102",
         Priority == "Fire",
         TxIntensity == "2m",
         Year == 2034,
         TxType %in% c("trt4", "trt6"))

t1 %>% 
  mutate(across(c(f101, f102, f103, f104, f121, f122, f123, f141, f142, f143,
                  f144, f145, f146, f147, f161, f162, f163, f164, f165,
                  f181, f182, f183, f184, f185, f186, f187, f188, f189,
                  f201, f202, f203, 
                  f91, f92, f93, f98, f99),
                ~ (.x) - lead(.x))) %>% 
  na.omit() %>% 
  select(-c(HUC12, Priority, TxType, TxIntensity, Year, ftotal)) %>% 
  select_if(colSums(.) %>% abs() > 0.009) %>% 
  pivot_longer(cols = everything(), names_to = "fbfm", values_to = "area_frac") %>% 
  arrange(area_frac) %>% 
  mutate(fbfm = str_remove(fbfm, "f")) %>% write_csv("qa/huc180200020102_Fire_2m_2039_trt4minus6_fbfm_1perc.csv")

