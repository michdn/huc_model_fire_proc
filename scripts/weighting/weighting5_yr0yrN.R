# Year n to Year 0 baseline

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### Data import & processing ------------------------------------

res_orig <- read_csv(file.path('results_csv', 
                          'datacube_weighted_20240205.csv')) %>% 
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

#Year 0 baseline
res2024 <- res %>% 
  filter(Year == 2024) %>% 
  rename(HaCBP_y0 = HaCBP,
         HaCFL_y0 = HaCFL,
         expBurn_y0 = expBurn,
         expFlame_y0 = expFlame,
         expPcActive_y0 = expPcActive)

#Year 5
res2029 <- res %>% 
  filter(Year == 2029) %>% 
  rename(HaCBP_y5 = HaCBP,
         HaCFL_y5 = HaCFL,
         expBurn_y5 = expBurn,
         expFlame_y5 = expFlame,
         expPcActive_y5 = expPcActive)

#Year 10
res2034 <- res %>% 
  filter(Year == 2034) %>% 
  rename(HaCBP_y10 = HaCBP,
         HaCFL_y10 = HaCFL,
         expBurn_y10 = expBurn,
         expFlame_y10 = expFlame,
         expPcActive_y10 = expPcActive)

#Year 20
res2044 <- res %>% 
  filter(Year == 2044) %>% 
  rename(HaCBP_y20 = HaCBP,
         HaCFL_y20 = HaCFL,
         expBurn_y20 = expBurn,
         expFlame_y20 = expFlame,
         expPcActive_y20 = expPcActive)


res_jt <- res2024 %>% 
  left_join(res2029, by = join_by(HUC12, Region, 
                                  Priority, TxIntensity, TxType,
                                  fireGroup, hybridGroup, wuiGroup)) %>% 
  left_join(res2034, by = join_by(HUC12, Region, 
                                Priority, TxIntensity, TxType, 
                                fireGroup, hybridGroup, wuiGroup)) %>% 
  left_join(res2044, by = join_by(HUC12, Region, 
                               Priority, TxIntensity, TxType, 
                               fireGroup, hybridGroup, wuiGroup))
#will have a lot of duplicate .x, .y, .z etc. fields, but ok


## full scatters -------------------------------------------------------

ggplot() +
  geom_point(data=res_jt,
             mapping=aes(x=HaCBP_y0, y=HaCBP_y5),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res_jt,
              aes(x=HaCBP_y0, y=HaCBP_y5),
              method=lm,
              geom="smooth",
              formula = 'y ~ x',
              se = FALSE, 
              fullrange = TRUE) + 
  labs(title="Year 0 to Year 5: HaCBP",
       x="Year 0",
       y="Year 5")

ggplot() +
  geom_point(data=res_jt,
             mapping=aes(x=HaCBP_y0, y=HaCBP_y10),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res_jt,
              aes(x=HaCBP_y0, y=HaCBP_y10),
              method=lm,
              geom="smooth",
              formula = 'y ~ x',
              se = FALSE, 
              fullrange = TRUE) + 
  labs(title="Year 0 to Year 10: HaCBP",
       x="Year 0",
       y="Year 10")

ggplot() +
  geom_point(data=res_jt,
             mapping=aes(x=HaCBP_y0, y=HaCBP_y20),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res_jt,
              aes(x=HaCBP_y0, y=HaCBP_y20),
              method=lm,
              geom="smooth",
              formula = 'y ~ x',
              se = FALSE, 
              fullrange = TRUE) + 
  labs(title="Year 0 to Year 20: HaCBP",
       x="Year 0",
       y="Year 20")

#Okay, effects are completely washed out by looking at everything together. 

#checking expBurn too
ggplot() +
  geom_point(data=res_jt,
             mapping=aes(x=expBurn_y0, y=expBurn_y5),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res_jt,
              aes(x=expBurn_y0, y=expBurn_y5),
              method=lm,
              geom="smooth",
              formula = 'y ~ x',
              se = FALSE, 
              fullrange = TRUE) + 
  labs(title="Year 0 to Year 5: expBurn",
       x="Year 0",
       y="Year 5")

ggplot() +
  geom_point(data=res_jt,
             mapping=aes(x=expBurn_y0, y=expBurn_y10),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res_jt,
              aes(x=expBurn_y0, y=expBurn_y10),
              method=lm,
              geom="smooth",
              formula = 'y ~ x',
              se = FALSE, 
              fullrange = TRUE) + 
  labs(title="Year 0 to Year 10: expBurn",
       x="Year 0",
       y="Year 10")

#large outliers in Year 20, distorts scatter plot, need to set limits
xy_limits_yr20 <- range(c(res_jt$expBurn_y0, 
                          res_jt$expBurn_y20),
                        na.rm=TRUE)

ggplot() +
  geom_point(data=res_jt,
             mapping=aes(x=expBurn_y0, y=expBurn_y20),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res_jt,
              aes(x=expBurn_y0, y=expBurn_y20),
              method=lm,
              geom="smooth",
              formula = 'y ~ x',
              se = FALSE, 
              fullrange = TRUE) + 
  labs(title="Year 0 to Year 20: expBurn",
       x="Year 0",
       y="Year 20") +
  scale_x_continuous(limits=xy_limits_yr20) +
  scale_y_continuous(limits=xy_limits_yr20)

#what region are the outliers in? 
ggplot() +
  geom_point(data=res_jt,
             mapping=aes(x=expBurn_y0, y=expBurn_y20),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res_jt,
              aes(x=expBurn_y0, y=expBurn_y20),
              method=lm,
              geom="smooth",
              formula = 'y ~ x',
              se = FALSE, 
              fullrange = TRUE) + 
  labs(title="Year 0 to Year 20: expBurn",
       x="Year 0",
       y="Year 20") +
  facet_wrap(~Region)
#NC has extreme outliers, and a few moderate outliers in CC


### Region SC ------------------------------------------------------

rsc <- res_jt %>% 
  filter(Region == "SC")

xy_limits_sc20 <- range(c(rsc$expBurn_y0, 
                        rsc$expBurn_y20),
                        na.rm=TRUE)

#Intensity facet scatter
ggplot() +
  geom_point(data=rsc,
             mapping=aes(x=expBurn_y0, y=expBurn_y20),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=rsc,
              aes(x=expBurn_y0, y=expBurn_y20),
              method=lm,
              geom="smooth",
              formula = 'y ~ x',
              se = FALSE, 
              fullrange = TRUE) + 
  labs(title="Year 0 to Year 20: expBurn",
       x="Year 0",
       y="Year 20") +
  facet_wrap(~TxIntensity) +
  scale_x_continuous(limits=xy_limits_sc20) +
  scale_y_continuous(limits=xy_limits_sc20)

#effects of treatments still washed out? 


## FIRE

rscfire <- rsc %>% 
  filter(Priority == "Fire")

#really large facet just to see
ggplot() +
  geom_point(data = rscfire,
             mapping = aes(x=expBurn_y0, y=expBurn_y20),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=rscfire,
              aes(x=expBurn_y0, y=expBurn_y20),
              method=lm,
              formula = 'y ~ x',
              geom="smooth",
              se=FALSE,
              fullrange = TRUE) + 
  labs(title="SC, Year 20, Fire Priority: Expected Burned Acres",
       subtitle = "By intensity, treatment type and fireGroup",
       x="Year 0",
       y="Year 20") +
  facet_wrap(~TxIntensity+TxType+fireGroup) +
  scale_x_continuous(limits=xy_limits_sc20) +
  scale_y_continuous(limits=xy_limits_sc20)
#too many

ggplot() +
  geom_point(data = rscfire,
             mapping = aes(x=expBurn_y0, y=expBurn_y20),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=rscfire,
              aes(x=expBurn_y0, y=expBurn_y20),
              method=lm,
              formula = 'y ~ x',
              geom="smooth",
              se=FALSE,
              fullrange = TRUE) + 
  labs(title="SC, Year 0 to Year 20, Fire Priority: Expected Burned Acres",
       subtitle = "By intensity, treatment type",
       x="Year 0",
       y="Year 20") +
  facet_wrap(~TxIntensity+TxType) +
  scale_x_continuous(limits=xy_limits_sc20) +
  scale_y_continuous(limits=xy_limits_sc20)


#split by treatment type?
ggplot() +
  geom_point(data = rscfire %>% 
               filter(TxType == "trt1"),
             mapping = aes(x=expBurn_y0, y=expBurn_y20),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=rscfire,
              aes(x=expBurn_y0, y=expBurn_y20),
              method=lm,
              formula = 'y ~ x',
              geom="smooth",
              se=FALSE,
              fullrange = TRUE) + 
  labs(title="SC, Year 20, Fire Priority, trt1: Expected Burned Acres",
       subtitle = "By intensity and fireGroup",
       x="Year 0",
       y="Year 20") +
  facet_wrap(~TxIntensity+fireGroup) +
  scale_x_continuous(limits=xy_limits_sc20) +
  scale_y_continuous(limits=xy_limits_sc20)

ggplot() +
  geom_point(data = rscfire %>% 
               filter(TxType == "trt1"),
             mapping = aes(x=HaCBP_y0, y=HaCBP_y20),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=rscfire,
              aes(x=HaCBP_y0, y=HaCBP_y20),
              method=lm,
              formula = 'y ~ x',
              geom="smooth",
              se=FALSE,
              fullrange = TRUE) + 
  labs(title="SC, Year 20, Fire Priority, trt1: HaCBP",
       subtitle = "By intensity and fireGroup",
       x="Year 0",
       y="Year 20") +
  facet_wrap(~TxIntensity+fireGroup)


## Year 0 exploration ----------------------------------------------------

res2024

ggplot() +
  geom_point(data=res2024,
             mapping=aes(x=TxIntensity, y=HaCBP_y0))

ggplot() +
  geom_point(data=res2024,
             mapping=aes(x=TxType, y=HaCBP_y0))

ggplot() +
  geom_point(data=res2024,
             mapping=aes(x=Priority, y=HaCBP_y0))


ggplot() +
  geom_point(data=res2024,
             mapping=aes(x=Priority, y=HaCBP_y0)) +
  facet_wrap(~TxIntensity+TxType)

#all look same/similar which is what we expect. 

