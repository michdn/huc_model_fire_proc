# script to count pixels of nonburnable fuels in base FM40



## library ---------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  terra,
  exactextractr)


## read in ---------------------------------------------

#fm40

# LANDFIRE 2.2.0 FM40, clipped to California extent
fm40ca <- terra::rast("qaqc/LC22_F40_220_CA.tif")


#hucs

hucs_all <- st_read("data/data_huc/TxHucsTimingGroups.shp") %>% 
  #immediately transform to raster crs EPSG:5070
  st_transform(st_crs(fm40ca))
  
  
hucs_coastal <- st_read("qaqc/coastal_water_hucs.gpkg") %>% 
  mutate(coastal = TRUE)


## nonburnable ----------------------------------------

#https://github.com/isciences/exactextractr/issues/39

# nonburnable: 91, 92, 93, 98, 99. Also -9999 (no data)

count_pixels <- exact_extract(fm40ca, 
                              hucs_all,
                              append_cols = c('huc12'),
                              fun = 'count')

count91 <- exact_extract(fm40ca, 
                         hucs_all,
                         append_cols = c('huc12'),
                         fun = function(value, coverage_fraction){
                           sum(coverage_fraction[value == 91], na.rm = TRUE)})

count92 <- exact_extract(fm40ca, 
                         hucs_all,
                         append_cols = c('huc12'),
                         fun = function(value, coverage_fraction){
                           sum(coverage_fraction[value == 92], na.rm = TRUE)})

count93 <- exact_extract(fm40ca, 
                         hucs_all,
                         append_cols = c('huc12'),
                         fun = function(value, coverage_fraction){
                           sum(coverage_fraction[value == 93], na.rm = TRUE)})

count98 <- exact_extract(fm40ca, 
                         hucs_all,
                         append_cols = c('huc12'),
                         fun = function(value, coverage_fraction){
                           sum(coverage_fraction[value == 98], na.rm = TRUE)})

count99 <- exact_extract(fm40ca, 
                         hucs_all,
                         append_cols = c('huc12'),
                         fun = function(value, coverage_fraction){
                           sum(coverage_fraction[value == 99], na.rm = TRUE)})

count9999 <- exactextractr::exact_extract(fm40ca, 
                                          hucs_all,
                                          append_cols = c('huc12'),
                                          fun = function(value, coverage_fraction){
                                            sum(coverage_fraction[value == -9999], na.rm = TRUE)})

#join all counts together
allcount <- count_pixels %>% 
  rename(count_pixels = count) %>% 
  left_join(count91 %>% 
              rename(count91 = result),
            by = join_by(huc12)) %>% 
  left_join(count92 %>% 
              rename(count92 = result),
            by = join_by(huc12)) %>% 
  left_join(count93 %>% 
              rename(count93 = result),
            by = join_by(huc12)) %>% 
  left_join(count98 %>% 
              rename(count98 = result),
            by = join_by(huc12)) %>% 
  left_join(count99 %>% 
              rename(count99 = result),
            by = join_by(huc12)) %>% 
  left_join(count9999 %>% 
              rename(count9999 = result),
            by = join_by(huc12))

head(allcount)

#calculate total nonburnable, and percentage
allcount <- allcount %>% 
  as_tibble() %>% 
  mutate(count_nonburn = count91 + count92 + count93 + count98 + count99 + count9999,
         nonburn_perc = count_nonburn / count_pixels * 100)

summary(allcount[["nonburn_perc"]])

#which nonburnable is largest?
allcount <- allcount %>% 
  mutate(maxnon = pmax(count91, count92, count93, count98, count99, count9999))
#https://stackoverflow.com/questions/71726392/how-to-return-the-name-of-a-column-if-its-value-matches-the-value-of-another-col
allcount <- allcount %>% 
  mutate(across(c(count91, count92, count93, count98, count99, count9999),
                ~case_when(. == maxnon ~ cur_column(),
                           TRUE ~ NA_character_),
                .names = "new_{col}")) %>% 
  unite(maxfield, starts_with("new"), na.rm=TRUE, sep=", ")


#add in coastal flag to see where coastal HUCs fall in here
allcount <- allcount %>% 
  left_join(hucs_coastal %>% 
              st_drop_geometry() %>% 
              select(huc12, coastal),
            by = join_by(huc12)) %>% 
  mutate(coastal = if_else(coastal, coastal, FALSE))

summary(allcount %>% filter(coastal) %>% pull(nonburn_perc))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 48.56   63.49   70.69   70.06   77.66   90.24         

# Only 1 coastal HUC below 50%

#coastals all count98? (yes)
allcount %>% filter(coastal) %>% pull(maxfield) %>% unique()

#problem HUC 
allcount %>% filter(huc12 == "180600060410")
#73% nonburnable, mostly count98

allcount %>% filter(nonburn_perc >= 73) %>% nrow()
# [1] 65
allcount %>% filter(nonburn_perc >= 73, coastal) %>% nrow()
# [1] 22


allcount %>% filter(nonburn_perc >= 50) %>% nrow()
# [1] 204
allcount %>% filter(nonburn_perc >= 50, coastal) %>% nrow()
# [1] 58

ggplot() + geom_histogram(data=allcount, mapping=aes(x=nonburn_perc))


nonburn50 <- allcount %>% 
  filter(nonburn_perc >= 50)

nb50 <- nonburn50 %>% pull(huc12)
#204

## Check 50%+ club against non-zero fires ---------------------------

#SC and CC available only. 

sc <- readRDS("results/extracts/SC_cbp_all_fires_from_sql.RDS")
cc <- readRDS("results/extracts/CC_cbp_all_fires_from_sql.RDS")
sn <- readRDS("results/extracts/SN_cbp_all_fires_from_sql.RDS")


sc_nb50 <- sc %>% 
  filter(HUC12 %in% nb50)
sc_nb50 %>% pull(HUC12) %>% unique() %>% length()
#83
cc_nb50 <- cc %>% 
  filter(HUC12 %in% nb50)
cc_nb50 %>% pull(HUC12) %>% unique() %>% length()
#51
sn_nb50 <- sn %>% 
  filter(HUC12 %in% nb50)
sn_nb50 %>% pull(HUC12) %>% unique() %>% length()
#46

scnb50_count <- sc_nb50 %>% 
  mutate(nonzero = if_else(huc_burned_frac > 0, 1, 0)) %>% 
  group_by(HUC12, mas_scenario, Year) %>% 
  summarize(count_nonzero = sum(nonzero)) 

ggplot() +
  geom_histogram(data=scnb50_count, mapping = aes(x = count_nonzero), bins = 20) + 
  labs(title = "SC HUCs 50%+ nonburnable (83 HUCs)", 
        subtitle = "Count of nonzero fires (out of 200)",
       y = "Count of scenario-years",
       x = "Count of nonzero fires")

ccnb50_count <- cc_nb50 %>% 
  mutate(nonzero = if_else(huc_burned_frac > 0, 1, 0)) %>% 
  group_by(HUC12, mas_scenario, Year) %>% 
  summarize(count_nonzero = sum(nonzero)) 

ggplot() +
  geom_histogram(data=ccnb50_count, mapping = aes(x = count_nonzero), bins=20) + 
  labs(title = "CC HUCs 50%+ nonburnable (51 HUCs)", 
       subtitle = "Count of nonzero fires (out of 200)",
       y = "Count of scenario-years",
       x = "Count of nonzero fires")


snnb50_count <- sn_nb50 %>% 
  mutate(nonzero = if_else(huc_burned_frac > 0, 1, 0)) %>% 
  group_by(HUC12, mas_scenario, Year) %>% 
  summarize(count_nonzero = sum(nonzero)) 

ggplot() +
  geom_histogram(data=snnb50_count, mapping = aes(x = count_nonzero), bins=20) + 
  labs(title = "SN HUCs 50%+ nonburnable (46 HUCs)", 
       subtitle = "Count of nonzero fires (out of 200)",
       y = "Count of scenario-years",
       x = "Count of nonzero fires")
