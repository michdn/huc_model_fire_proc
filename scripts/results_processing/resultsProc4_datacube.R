# Short script to bind all region absolute metrics together for final datacube

# Also adds veg proportion (from Anna)


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)


### User settings ---------------------------------------------

input_folder <- file.path("results", "absolute") 

output_folder <- file.path("results", "datacube")
dir.create(output_folder, recursive = TRUE) 


#region results
sn <- read_csv(file.path(input_folder, "SN_absolute_20240617.csv")) %>% 
  mutate(HUC12 = as.character(HUC12))

sc <- read_csv(file.path(input_folder, "SC_absolute_20240617.csv")) %>%
  mutate(HUC12 = as.character(HUC12))

cc <- read_csv(file.path(input_folder, "CC_absolute_20240617.csv")) %>%
mutate(HUC12 = as.character(HUC12))

# nc <- read_csv(file.path(input_folder, "NC_absolute_202406xx.csv")) %>% 
#   mutate(HUC12 = as.character(HUC12))

# Vegetation proportions
veg <- read_csv(file.path("data", "data_veg", 
                          "PropVegTypeForDatacube.csv")) %>% 
  rename(HUC12 = huc12) %>% 
  mutate(HUC12 = as.character(HUC12)) 



### Bind and add veg -------------------------------------------

cube_raw <- bind_rows(sn, sc, cc) #, nc

cube <- cube_raw %>% 
  left_join(veg, 
            by = join_by(HUC12)) %>% 
  #reorder
  dplyr::select(HUC12, Region, Priority, TxIntensity, TxType, Year, 
                mas_scenario, hucAc, timeFire, timeHybrid, timeWui,
                hardwoodSum, herbSum, shrubSum, softwoodSum,
                vegSum, propHard, propSoft, propShrub, propHerb,
                everything())


### save ---------------------------------------------------

stamp <- format(Sys.time(), "%Y%m%d")

write_csv(cube,
          file.path(output_folder,
                    paste0('datacube_', "interim_", stamp, '.csv')))
