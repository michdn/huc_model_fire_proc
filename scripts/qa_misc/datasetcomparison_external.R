# Comparing absolute metrics to other datasets

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  raster,
  exactextractr,
  sf) 

### Data ----------------------------------------------------

res <- read_csv(file.path("results",
                               "datacube", 
                               "datacube_interim_sc_cc_20240328.csv")) %>% 
  mutate(HUC12 = as.character(HUC12))


hucs_shp <- st_read("data/data_huc/TxHucsTimingGroups.shp") %>% 
  #CHANGE WHEN NEEDED. For working with itnerim datasets
  filter(region %in% c("SC", "CC"))

#abp <- raster("other_datasets/ucsb_calfire/ucsb_burn_severity.tif")

whp <- raster("other_datasets/RDS-2015-0047-4_Data/Data/whp2023_GeoTIF/whp2023_cnt_conus.tif")
rrk <- raster("other_datasets/fireDynamics/fireDynamics/AnnualBurnProbability2022.tif")

### CRS prep -----------------------------------------------------

crs(whp)
crs(rrk)

#different. 

#hucs to the same crs  
hucs_whp <- st_transform(hucs_shp, st_crs(whp))
hucs_rrk <- st_transform(hucs_shp, st_crs(rrk))


### zonal summary -----------------------------------------------------

#extract the mean of the burn prob per huc
# (with partial pixel coverage support using exact_extract)
whp_hucs_mean <- exact_extract(whp, 
                         hucs_whp,
                         fun = 'mean', 
                         append_cols = c('huc12'),
                         progress = TRUE)

rrk_hucs_mean <- exact_extract(rrk, 
                               hucs_rrk,
                               fun = 'mean', 
                               append_cols = c('huc12'),
                               progress = TRUE)


whp_hucs_sum <- exact_extract(whp,
                               hucs_whp,
                               fun = 'sum',
                               append_cols = c('huc12'),
                               progress = TRUE)

rrk_hucs_sum <- exact_extract(rrk,
                               hucs_rrk,
                               fun = 'sum',
                               append_cols = c('huc12'),
                               progress = TRUE)

### MAS --------------------------------------------------

## 2024 

# Year 2024 contains yr 1-5 treatments. 
# Look at 500k scenarios (least treatment)
#  And take mean of expBurn of the rest of the scenario (priority-type)


r0 <- res %>% 
  #year 0
  filter(Year == 2024,
         TxIntensity == '500k') %>% 
  #group by HUC, plus other desired fields (that are the same per HUC)
  group_by(HUC12, Region, hucAc) %>% 
  #calculate max of expBurn across all 500k scenarios
  summarize(expBurn_mean = mean(expBurn, na.rm=TRUE),
            .groups = 'drop') %>% 
  #rename field to match shp id field
  rename(huc12 = HUC12)

# ## 2039
# r20 <- res %>% 
#   filter(Year == 2039,
#          TxIntensity == '2m') %>% 
#   #group by HUC, plus other desired fields (that are the same per HUC)
#   group_by(HUC12, Region, hucAc) %>% 
#   #calculate mean of expBurn across all 500k scenarios
#   summarize(expBurn_mean = mean(expBurn, na.rm=TRUE),
#             .groups = 'drop') %>% 
#   #rename field to match shp id field
#   rename(huc12 = HUC12)


### scatter plots ---------------------------------------------------

## WHP

whp_join <- whp_hucs_mean %>% 
  left_join(whp_hucs_sum, by = join_by(huc12)) %>% 
  left_join(r0, by = join_by(huc12))

whp_mas <- ggplot() +
  geom_point(data=whp_join,
             mapping=aes(x=mean, y=expBurn_mean),
             shape=1, color='darkblue') +
  theme(aspect.ratio = 1) + 
  # stat_smooth(data=whp_join,
  #             aes(x=mean, y=expBurn_mean),
  #             method=lm,
  #             formula='y ~ x',
  #             geom="smooth",
  #             se=FALSE,
  #             fullrange = TRUE) + 
  facet_wrap(~Region) + 
  labs(title='Comparison to WHP',
       subtitle = 'Per HUC by Region',
       x='WHP HUC-averaged wildfire hazard potential',
       y='MAS 2024-500k mean burned acres')

ggsave(plot = whp_mas, 
       filename = "plots/dscomp_whp_mas.jpg",
       width = 5, height = 3, units = 'in')

# whp_join20 <- whp_hucs_mean %>% 
#   left_join(whp_hucs_sum, by = join_by(huc12)) %>% 
#   left_join(r20, by = join_by(huc12))
# 
# ggplot() +
#   geom_point(data=whp_join20,
#              mapping=aes(x=mean, y=expBurn_mean),
#              shape=1, color='darkblue') +
#   theme(aspect.ratio = 1) + 
#   # stat_smooth(data=whp_join20,
#   #             aes(x=mean, y=expBurn_mean),
#   #             method=lm,
#   #             formula='y ~ x',
#   #             geom="smooth",
#   #             se=FALSE,
#   #             fullrange = TRUE) + 
#   facet_wrap(~Region) + 
#   labs(title='Comparison to WHP',
#        subtitle = 'Per HUC by Region',
#        x='WHP HUC-averaged wildfire hazard potential',
#        y='MAS 2044-2m mean burned acres')



## RRK

rrk_join <- rrk_hucs_mean %>% 
  left_join(rrk_hucs_sum, by = join_by(huc12)) %>% 
  left_join(r0, by = join_by(huc12)) %>% 
  mutate(sum_burned_ac = sum * 0.2223948)

#rrk had larger range
xy_limits_mean <- range(c(rrk_join$sum_burned_ac,
                          rrk_join$sum_burned_ac))

ggplot() +
  geom_point(data=rrk_join,
             mapping=aes(x=sum_burned_ac, y=expBurn_mean)) +
  theme(aspect.ratio = 1) + 
  facet_wrap(~Region) +
  scale_x_continuous(limits=xy_limits_mean) +
  scale_y_continuous(limits=xy_limits_mean)

  
# ggplot() +
#   geom_point(data=rrk_join,
#              mapping=aes(x=mean, y=expBurn_mean/hucAc),
#              shape=1, color='darkblue') +
#   theme(aspect.ratio = 1) + 
#   facet_wrap(~Region) + 
#   geom_abline(intercept=0, slope=1, color='black') + 
#   stat_smooth(data=rrk_join,
#               aes(x=mean, y=expBurn_mean/hucAc),
#               method=lm,
#               formula='y ~ x',
#               geom="smooth",
#               se=FALSE,
#               fullrange = TRUE) +
#   labs(title='Comparison to Fire Dynamics map',
#        subtitle = 'Per HUC by Region',
#        x='Fire Dynamics HUC-averaged burn probability',
#        y='MAS 2024 mean proportion burned')


plot_rrk_mas <- ggplot() +
  geom_point(data=rrk_join,
             mapping=aes(x=sum_burned_ac, y=expBurn_mean),
             shape=1, color='darkblue') +
  theme(aspect.ratio = 1) + 
  facet_wrap(~Region) + 
  geom_abline(intercept=0, slope=1, color='black') + 
  stat_smooth(data=rrk_join,
              aes(x=sum_burned_ac, y=expBurn_mean),
              method=lm,
              formula='y ~ x',
              geom="smooth",
              se=FALSE,
              fullrange = TRUE) +
  labs(title='Comparison to Fire Dynamics map',
       subtitle = 'Per HUC by Region',
       x='Fire Dynamics expected burned acres',
       y='MAS 2024-500k mean burned acres') +
  scale_x_continuous(limits=xy_limits_mean) +
  scale_y_continuous(limits=xy_limits_mean)


ggsave(plot = plot_rrk_mas, 
       filename = "plots/dscomp_rrk_mas.jpg",
       width = 5, height = 3, units = 'in')


  
## RRK and WHP

rrk_whp <- rrk_hucs_mean %>% 
  rename(rrk_mean = mean) %>% 
  left_join(rrk_hucs_sum, by = join_by(huc12)) %>% 
  rename(rrk_sum = sum) %>% 
  mutate(sum_burned_ac = rrk_sum * 0.2223948) %>% 
  left_join(whp_hucs_mean, by = join_by(huc12)) %>% 
  rename(whp_mean = mean) %>% 
  #r0 just to get Region
  left_join(r0, by = join_by(huc12))
  
plot_rrk_whp <- ggplot() +
  geom_point(data=rrk_whp,
             mapping=aes(x=whp_mean, y=rrk_mean),
             shape=1, color='darkblue') +
  theme(aspect.ratio = 1) + 
  facet_wrap(~Region) + 
  # stat_smooth(data=rrk_whp,
  #             aes(x=rrk_mean, y=whp_mean),
  #             method=lm,
  #             formula='y ~ x',
  #             geom="smooth",
  #             se=FALSE,
  #             fullrange = TRUE) +
  labs(title='Comparison WHP to Fire Dynamics map',
       subtitle = 'Per HUC by Region',
       x='Wildfire hazard potential',
       y='Fire Dynamics mean burn probability') 

ggsave(plot = plot_rrk_whp, 
       filename = "plots/dscomp_rrk_whp.jpg",
       width = 5, height = 3, units = 'in')

