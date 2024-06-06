# best scenario per HUC12
# Year 20

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf)

### User settings ---------------------------------------------

input_folder <- 'results_csv'

### Base Data import -------------------------------------------

res <- read_csv(file.path(input_folder, 
                          'datacube_weighted_expanded_20240205.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

#Year 20
res2044 <- res %>% 
  filter(Year == 2044)

#spatial
hucs_shp <- st_read("data/data_huc/TxPrctRankRrkWipRffc.shp")


### Lowest expected total burned acres -------------------------

lowest_burn <- res2044 %>% 
  dplyr::select(HUC12, Region, Priority, TxIntensity, TxType, expBurn) %>% 
  group_by(HUC12, Region) %>% 
  arrange(expBurn) %>% 
  slice(1)

lowest_burn %>% 
  group_by(Priority) %>% 
  summarize(count = n())

lowest_burn %>% 
  group_by(TxIntensity) %>% 
  summarize(count = n())

lowest_burn %>% 
  group_by(TxType) %>% 
  summarize(count = n())

lowest_burn %>% 
  group_by(Priority, TxIntensity, TxType) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% View()

lowest_burn %>% 
  group_by(Region, Priority, TxIntensity, TxType) %>% 
  summarize(count = n()) %>% 
  arrange(Region, desc(count)) %>% 
  ungroup() %>% 
  group_by(Region) %>% 
  slice(1)

#add spatial data and save out for QGIS plot
# hucs_lowest <- hucs_shp %>% 
#   left_join(lowest_burn %>% 
#               mutate(scenario = paste0(Priority, "-", TxIntensity, "-", TxType)),
#             by = join_by('huc12' == 'HUC12'))

hucs_lowest <- hucs_shp %>% 
  left_join(lowest_burn %>% 
              mutate(scenario = paste0(TxIntensity, "-", Priority, "-", TxType)),
            by = join_by('huc12' == 'HUC12'))

st_write(hucs_lowest, 
         dsn="results_spatial/scenario_lowest_burn_acres_v2.gpkg", 
         layer="scenario_lowest_burn_acres")


## Lowest flame length ----------------------------------------------------------

lowest_flame <- res2044 %>% 
  dplyr::select(HUC12, Region, Priority, TxIntensity, TxType, expFlame) %>% 
  group_by(HUC12, Region) %>% 
  arrange(expFlame) %>% 
  slice(1)

hucs_lowest_flame <- hucs_shp %>% 
  left_join(lowest_flame %>% 
              mutate(scenario = paste0(TxIntensity, "-", Priority, "-", TxType)),
            by = join_by('huc12' == 'HUC12'))

# st_write(hucs_lowest_flame,
#          dsn="results_spatial/scenario_lowest_flame_acres_ipt.gpkg",
#          layer="scenario_lowest_flame_ipt",
#          append=FALSE)

lb <- lowest_burn %>% 
  group_by(Region, Priority, TxIntensity, TxType) %>% 
  summarize(count_lb = n()) %>% 
  arrange(Region, desc(count_lb))

lf <- lowest_flame %>% 
  group_by(Region, Priority, TxIntensity, TxType) %>% 
  summarize(count_lf = n()) %>% 
  arrange(Region, desc(count_lf))

lb %>% 
  left_join(lf, by = c("Region", "Priority", "TxIntensity", "TxType")) %>% 
  mutate(diff = count_lb - count_lf) %>% 
  filter(!diff == 0) %>% View()

#definitely some differences when looking at expBurn vs expFlame
# seems to be more 500k scenario lowest in Flame than in Burn (quick check)


## lowest HaCBP check against expBurn -------------------------------------

lb <- lowest_burn %>% 
  group_by(Region, Priority, TxIntensity, TxType) %>% 
  summarize(count_lb = n()) %>% 
  arrange(Region, desc(count_lb))
  
lowest_hacbp <- res2044 %>% 
  dplyr::select(HUC12, Region, Priority, TxIntensity, TxType, HaCBP) %>% 
  group_by(HUC12, Region) %>% 
  arrange(HaCBP) %>% 
  slice(1)

lh <- lowest_hacbp %>% 
  group_by(Region, Priority, TxIntensity, TxType) %>% 
  summarize(count_lh = n()) %>% 
  arrange(Region, desc(count_lh))

lh %>% 
  left_join(lb, by = c("Region", "Priority", "TxIntensity", "TxType")) %>% 
  mutate(diff = count_lh - count_lb) %>% 
  filter(!diff == 0)

# Region Priority TxIntensity TxType count_lh count_lb  diff
# <chr>  <chr>    <chr>       <chr>     <int>    <int> <int>
#   1 CC     WUI      2m          trt1         28       27     1
# 2 CC     WUI      500k        trt1          7        8    -1

#not worried about 1 huc that shifted categories


## lowest HaCFL check against expFlame ------------------------------------

lf

lowest_hacfl <- res2044 %>% 
  dplyr::select(HUC12, Region, Priority, TxIntensity, TxType, HaCFL) %>% 
  group_by(HUC12, Region) %>% 
  arrange(HaCFL) %>% 
  slice(1)

lc <- lowest_hacfl %>% 
  group_by(Region, Priority, TxIntensity, TxType) %>% 
  summarize(count_lc = n()) %>% 
  arrange(Region, desc(count_lc))

lf %>% 
  left_join(lc, by = c("Region", "Priority", "TxIntensity", "TxType")) %>% 
  mutate(diff = count_lf - count_lc) %>% 
  filter(!diff == 0)

#single HUC flip as in HaCBP-expBurn (same scenarios). Not concerned. 
# Region Priority TxIntensity TxType count_lf count_lc  diff
# <chr>  <chr>    <chr>       <chr>     <int>    <int> <int>
#   1 CC     WUI      2m          trt1         29       30    -1
# 2 CC     WUI      500k        trt1          3        2     1

lowest_hacfl %>% 
  dplyr::select(-HaCFL) %>% 
  anti_join(lowest_flame %>% 
              dplyr::select(-expFlame),
            by = join_by(HUC12, Region, Priority, TxIntensity, TxType))

switchedhuc <- 180500040804

res2044 %>% 
  filter(HUC12 == switchedhuc,
         Priority == "WUI",
         TxType == "trt1") %>% View()

#ah. abp_sum = 0. exp* are 0, arranging&slicing going to be a toss-up. 


## zero abp ----------------------------------------------------------

res %>% 
  dplyr::select(HUC12, abp_sum) %>% 
  distinct() %>% 
  filter(abp_sum == 0)
# 1 HUC with abp_sum

