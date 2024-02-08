# Highly filtered experiment

# Year 20
# SC

# Priority: 3 analyses Fire, WUI, Hybrid

# intensity: 1m-500k, 2m-500k
# type: facet?
# groupPriority (relevant one): facet? 

# Feedback that it too confusing to show 500k vs 2m
#  should show Year 0 baseline instead
# But that was weather changes as well, so keeping 

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### Base Data import -------------------------------------------

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


#Year 20
res2044 <- res %>% 
  filter(Year == 2044)


### Intensity comparison calculations ------------------------
#Calculate differences here, to be filtered more later

r20_500k <- res2044 %>% 
  filter(TxIntensity == "500k") %>% 
  rename(expBurn_500k = expBurn, 
         expFlame_500k = expFlame,
         expAcf_500k = expPcActive,
         HaCBP_500k = HaCBP,
         HaCFL_500k = HaCFL)

r20_1m <- res2044 %>% 
  filter(TxIntensity == "1m") %>% 
  rename(expBurn_1m = expBurn, 
         expFlame_1m = expFlame,
         expAcf_1m = expPcActive,
         HaCBP_1m = HaCBP,
         HaCFL_1m = HaCFL)

r20_2m <- res2044 %>% 
  filter(TxIntensity == "2m") %>% 
  rename(expBurn_2m = expBurn, 
         expFlame_2m = expFlame,
         expAcf_2m = expPcActive,
         HaCBP_2m = HaCBP,
         HaCFL_2m = HaCFL)

r20_jt <- r20_500k %>% 
  left_join(r20_1m, by = join_by(HUC12, Region, Priority, TxType, fireGroup, hybridGroup, wuiGroup)) %>% 
  left_join(r20_2m, by = join_by(HUC12, Region, Priority, TxType, fireGroup, hybridGroup, wuiGroup)) 


r20_diff <- r20_jt %>% 
  # dplyr::select(HUC12, Region, Priority, TxType, 
  #               fireGroup, hybridGroup, wuiGroup,
  #               expBurn_500k, expBurn_1m, expBurn_2m,
  #               expFlame_500k, expFlame_1m, expFlame_2m,
  #               expAcf_500k, expAcf_1m, expAcf_2m) %>% 
  mutate(burn2m500k = expBurn_2m - expBurn_500k,
         burn1m500k = expBurn_1m - expBurn_500k,
         flame2m500k = expFlame_2m - expFlame_500k,
         flame1m500k = expFlame_1m - expFlame_500k,
         acf2m500k = expAcf_2m - expAcf_500k,
         acf1m500k = expAcf_1m - expAcf_500k)


### SC -------------------------------------------------------

#SC only
r20sc <- r20_diff %>% 
  filter(Region == "SC")


### SC Priority: Fire -------------------------------------------

r20scfire <- r20sc %>% 
  filter(Priority == "Fire")

r20scfire

ggplot() +
  geom_jitter(data = r20scfire,
             mapping = aes(x=fireGroup, y=burn2m500k),
             height = 0) +
  scale_x_continuous(breaks=seq(0,100,25)) +
  facet_wrap(~TxType) +
  labs(title = "SC Region, Year 20, Priority Fire") +
  xlab("expBurn 2m - expBurn 500k") 


ggplot() +
  geom_jitter(data = r20scfire,
              mapping = aes(x=fireGroup, y=burn1m500k),
              height = 0) +
  scale_x_continuous(breaks=seq(0,100,25)) +
  facet_wrap(~TxType) +
  labs(title = "SC Region, Year 20, Priority Fire") + 
  xlab("expBurn 1m - expBurn 500k") 


ggplot() +
  geom_jitter(data = r20scfire,
              mapping = aes(x=fireGroup, y=flame2m500k),
              height = 0) +
  scale_x_continuous(breaks=seq(0,100,25)) +
  facet_wrap(~TxType) +
  labs(title = "SC Region, Year 20, Priority Fire") +
  xlab("expFlame 2m - expFlame 500k") 


ggplot() +
  geom_jitter(data = r20scfire,
              mapping = aes(x=fireGroup, y=flame1m500k),
              height = 0) +
  scale_x_continuous(breaks=seq(0,100,25)) +
  facet_wrap(~TxType) +
  labs(title = "SC Region, Year 20, Priority Fire")

ggplot() +
  geom_jitter(data = r20scfire,
              mapping = aes(x=fireGroup, y=acf2m500k),
              height = 0) +
  scale_x_continuous(breaks=seq(0,100,25)) +
  facet_wrap(~TxType) +
  labs(title = "SC Region, Year 20, Priority Fire") + 
  xlab("expPcActive 2m - expPcActive 500k") 


#scatter faceted 

ggplot() +
  geom_point(data = r20scfire,
             mapping = aes(x=expBurn_500k, y=expBurn_2m),
                     shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=r20scfire,
              aes(x=expBurn_500k, y=expBurn_2m),
              method=lm,
              formula = 'y ~ x',
              geom="smooth",
              se=FALSE,
              fullrange = TRUE) + 
  labs(title="SC, Year 20, Fire Priority: Expected Burned Acres",
       subtitle = "By treatment type and fireGroup",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") +
  facet_wrap(~TxType+fireGroup)


ggplot() +
  geom_point(data = r20scfire,
             mapping = aes(x=expFlame_500k, y=expFlame_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=r20scfire,
              aes(x=expFlame_500k, y=expFlame_2m),
              method=lm,
              formula = 'y ~ x',
              geom="smooth",
              se=FALSE,
              fullrange = TRUE) + 
  labs(title="SC, Year 20, Fire Priority: Expected Total Flame Length",
       subtitle = "By treatment type and fireGroup",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") +
  facet_wrap(~TxType+fireGroup)


### SC Priority: WUI -------------------------------------------

rwui <- rsc %>% 
  filter(Priority == "WUI")

rwui

ggplot() +
  geom_jitter(data = rwui,
              mapping = aes(x=wuiGroup, y=burn2m500k),
              height = 0) +
  scale_x_continuous(breaks=seq(0,100,25)) +
  facet_wrap(~TxType) +
  labs(title = "SC Region, Year 20, Priority WUI")

### SC Priority: Hybrid -------------------------------------------

rhybrid <- rsc %>% 
  filter(Priority == "Hybrid")

rhybrid

ggplot() +
  geom_jitter(data = rhybrid,
              mapping = aes(x=hybridGroup, y=burn2m500k),
              height = 0) +
  scale_x_continuous(breaks=seq(0,100,25)) +
  facet_wrap(~TxType) +
  labs(title = "SC Region, Year 20, Priority Hybrid")


### NC ---------------------------------------------------------

#NC only
rnc <- rjtdiff %>% 
  filter(Region == "NC")


### NC Priority: Fire -------------------------------------------

rncfire <- rnc %>% 
  filter(Priority == "Fire")

rncfire

ggplot() +
  geom_jitter(data = rncfire,
              mapping = aes(x=fireGroup, y=burn2m500k),
              height = 0) +
  scale_x_continuous(breaks=seq(0,100,25)) +
  facet_wrap(~TxType) +
  labs(title = "NC Region, Year 20, Priority Fire") +
  ylab("expBurn 2m - expBurn 500k") +
  coord_cartesian(ylim = c(-500,500))

### SN ---------------------------------------------------------

rsn <- r20_diff %>% 
  filter(Region == "SN")


### SN Priority: Fire -------------------------------------------

rsnfire <- rsn %>% 
  filter(Priority == "Fire")

rsnfire

ggplot() +
  geom_jitter(data = rsnfire,
              mapping = aes(x=fireGroup, y=burn2m500k),
              height = 0) +
  scale_x_continuous(breaks=seq(0,100,25)) +
  facet_wrap(~TxType) +
  labs(title = "SN Region, Year 20, Priority Fire") +
  ylab("expBurn 2m - expBurn 500k") 


ggplot() + 
  geom_point(data=rsnfire,
             mapping=aes(x=HaCBP_500k, y=HaCBP_2m), 
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=rsnfire,
              aes(x=HaCBP_500k, y=HaCBP_2m),
              method=lm,
              formula = 'y ~ x',
              geom="smooth",
              se=FALSE,
              fullrange = TRUE) + 
  labs(title="SC, Year 10, Fire Priority: HaCBP",
       subtitle = "By treatment type and fireGroup",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") +
  facet_wrap(~TxType+fireGroup)


### CC ---------------------------------------------------------

rcc <- rjtdiff %>% 
  filter(Region == "CC")


### CC Priority: Fire -------------------------------------------

rccfire <- rcc %>% 
  filter(Priority == "Fire")

rccfire

ggplot() +
  geom_jitter(data = rccfire,
              mapping = aes(x=fireGroup, y=burn2m500k),
              height = 0) +
  scale_x_continuous(breaks=seq(0,100,25)) +
  facet_wrap(~TxType) +
  labs(title = "CC Region, Year 20, Priority Fire") +
  ylab("expBurn 2m - expBurn 500k") + 
  coord_cartesian(ylim = c(-1000,1000))



### Year 10 -------------------------------------------------------
res2034 <- res %>% 
  filter(Year == 2034)

r10_500k <- res2034 %>% 
  filter(TxIntensity == "500k") %>% 
  rename(expBurn_500k = expBurn, 
         expFlame_500k = expFlame,
         expAcf_500k = expPcActive,
         HaCBP_500k = HaCBP,
         HaCFL_500k = HaCFL)

r10_1m <- res2034 %>% 
  filter(TxIntensity == "1m") %>% 
  rename(expBurn_1m = expBurn, 
         expFlame_1m = expFlame,
         expAcf_1m = expPcActive,
         HaCBP_1m = HaCBP,
         HaCFL_1m = HaCFL)

r10_2m <- res2034 %>% 
  filter(TxIntensity == "2m") %>% 
  rename(expBurn_2m = expBurn, 
         expFlame_2m = expFlame,
         expAcf_2m = expPcActive,
         HaCBP_2m = HaCBP,
         HaCFL_2m = HaCFL)

r10 <- r10_500k %>% 
  left_join(r10_1m, by = join_by(HUC12, Region, Priority, TxType, 
                                 fireGroup, hybridGroup, wuiGroup)) %>% 
  left_join(r10_2m, by = join_by(HUC12, Region, Priority, TxType, 
                                 fireGroup, hybridGroup, wuiGroup)) %>% 
  dplyr::select(HUC12, Region, Priority, TxType, 
                fireGroup, hybridGroup, wuiGroup,
                expBurn_500k, expBurn_1m, expBurn_2m,
                expFlame_500k, expFlame_1m, expFlame_2m,
                expAcf_500k, expAcf_1m, expAcf_2m)
  
### YR10 SC -------------------------------------------------------
r10sc <- r10 %>% 
  filter(Region == "SC")

### YR10 SC Fire --------------------------------------------------
r10scfire <- r10sc %>% 
  filter(Priority == "Fire")

r10scfire

ggplot() +
  geom_point(data = r10scfire,
             mapping = aes(x=expBurn_500k, y=expBurn_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=r10scfire,
              aes(x=expBurn_500k, y=expBurn_2m),
              method=lm,
              formula = 'y ~ x',
              geom="smooth",
              se=FALSE,
              fullrange = TRUE) + 
  labs(title="SC, Year 10, Fire Priority: Expected Burned Acres",
       subtitle = "By treatment type and fireGroup",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") +
  facet_wrap(~TxType+fireGroup)


### Year 5 -------------------------------------------------------
res2029 <- res %>% 
  filter(Year == 2029)

r5_500k <- res2029 %>% 
  filter(TxIntensity == "500k") %>% 
  rename(expBurn_500k = expBurn, 
         expFlame_500k = expFlame,
         expAcf_500k = expPcActive,
         HaCBP_500k = HaCBP,
         HaCFL_500k = HaCFL)

r5_1m <- res2029 %>% 
  filter(TxIntensity == "1m") %>% 
  rename(expBurn_1m = expBurn, 
         expFlame_1m = expFlame,
         expAcf_1m = expPcActive,
         HaCBP_1m = HaCBP,
         HaCFL_1m = HaCFL)

r5_2m <- res2029 %>% 
  filter(TxIntensity == "2m") %>% 
  rename(expBurn_2m = expBurn, 
         expFlame_2m = expFlame,
         expAcf_2m = expPcActive,
         HaCBP_2m = HaCBP,
         HaCFL_2m = HaCFL)

r5 <- r5_500k %>% 
  left_join(r5_1m, by = join_by(HUC12, Region, Priority, TxType, 
                                 fireGroup, hybridGroup, wuiGroup)) %>% 
  left_join(r5_2m, by = join_by(HUC12, Region, Priority, TxType, 
                                  fireGroup, hybridGroup, wuiGroup)) #%>% 
  # dplyr::select(HUC12, Region, Priority, TxType, 
  #               fireGroup, hybridGroup, wuiGroup,
  #               expBurn_500k, expBurn_1m, expBurn_2m,
  #               expFlame_500k, expFlame_1m, expFlame_2m,
  #               expAcf_500k, expAcf_1m, expAcf_2m)


### YR5 SC -------------------------------------------------------
r5sc <- r5 %>% 
  filter(Region == "SC")

### YR5 SC Fire --------------------------------------------------
r5scfire <- r5sc %>% 
  filter(Priority == "Fire")

r5scfire

ggplot() +
  geom_point(data = r5scfire,
             mapping = aes(x=expBurn_500k, y=expBurn_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=r5scfire,
              aes(x=expBurn_500k, y=expBurn_2m),
              method=lm,
              formula = 'y ~ x',
              geom="smooth",
              se=FALSE,
              fullrange = TRUE) + 
  labs(title="SC, Year 5, Fire Priority: Expected Burned Acres",
       subtitle = "By treatment type and fireGroup",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") +
  facet_wrap(~TxType+fireGroup)

### Year 0 -------------------------------------------------------
res2024 <- res %>% 
  filter(Year == 2024)

r0_500k <- res2024 %>% 
  filter(TxIntensity == "500k") %>% 
  rename(expBurn_500k = expBurn, 
         expFlame_500k = expFlame,
         HaCBP_500k = HaCBP,
         HaCFL_500k = HaCFL)

r0_1m <- res2024 %>% 
  filter(TxIntensity == "1m") %>% 
  rename(expBurn_1m = expBurn, 
         expFlame_1m = expFlame,
         HaCBP_1m = HaCBP,
         HaCFL_1m = HaCFL)

r0_2m <- res2024 %>% 
  filter(TxIntensity == "2m") %>% 
  rename(expBurn_2m = expBurn, 
         expFlame_2m = expFlame,
         HaCBP_2m = HaCBP,
         HaCFL_2m = HaCFL)

r0 <- r0_500k %>% 
  left_join(r0_1m, by = join_by(HUC12, Region, Priority, TxType, 
                                fireGroup, hybridGroup, wuiGroup)) %>% 
  left_join(r0_2m, by = join_by(HUC12, Region, Priority, TxType, 
                                fireGroup, hybridGroup, wuiGroup))
  # dplyr::select(HUC12, Region, Priority, TxType, 
  #               fireGroup, hybridGroup, wuiGroup,
  #               expBurn_500k, expBurn_1m, expBurn_2m,
  #               expFlame_500k, expFlame_1m, expFlame_2m)


### YR0 SC -------------------------------------------------------
r0sc <- r0 %>% 
  filter(Region == "SC")

### YR0 SC Fire --------------------------------------------------
r0scfire <- r0sc %>% 
  filter(Priority == "Fire")

r0scfire

ggplot() +
  geom_point(data = r0scfire,
             mapping = aes(x=expBurn_500k, y=expBurn_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=r0scfire,
              aes(x=expBurn_500k, y=expBurn_2m),
              method=lm,
              formula = 'y ~ x',
              geom="smooth",
              se=FALSE,
              fullrange = TRUE) + 
  labs(title="SC, Year 0, Fire Priority: Expected Burned Acres",
       subtitle = "By treatment type and fireGroup",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") +
  facet_wrap(~TxType+fireGroup)

#um

ggplot() +
  geom_point(data = r0scfire,
             mapping = aes(x=HaCBP_500k, y=HaCBP_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=r0scfire,
              aes(x=HaCBP_500k, y=HaCBP_2m),
              method=lm,
              formula = 'y ~ x',
              geom="smooth",
              se=FALSE,
              fullrange = TRUE) + 
  labs(title="SC, Year 0, Fire Priority: HaCBP",
       subtitle = "By treatment type and fireGroup",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") +
  facet_wrap(~TxType+fireGroup)


ggplot() +
  geom_point(data = r0scfire,
             mapping = aes(x=expFlame_500k, y=expFlame_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=r0scfire,
              aes(x=expFlame_500k, y=expFlame_2m),
              method=lm,
              formula = 'y ~ x',
              geom="smooth",
              se=FALSE,
              fullrange = TRUE) + 
  labs(title="SC, Year 0, Fire Priority: Expected Total Flame Length",
       subtitle = "By treatment type and fireGroup",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") +
  facet_wrap(~TxType+fireGroup)

#boxplot
res2024

#weird lines
ggplot() +
  geom_bar(data=res2024 %>% 
             filter(Region == "SC",
                    Priority == "Fire"),
           mapping=aes(x=TxIntensity, y=expBurn),
           stat = 'identity') +
  labs(title="SC Year 0 Fire Priority: Sum Total Expected Burned Acres",
       x="Intensity",
       y="Sum total expected burned acres") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) + 
  facet_wrap(~TxType+fireGroup) 

rscfire_sum <- res2024 %>% 
  filter(Region == "SC",
         Priority == "Fire") %>% 
  group_by(TxIntensity, TxType, fireGroup) %>% 
  summarize(expBurn_sum = sum(expBurn), .groups = "drop")


ggplot() +
  geom_col(data=rscfire_sum,
           mapping=aes(x=TxIntensity, y=expBurn_sum)) +
  labs(title="SC Year 0 Fire Priority: Sum Total Expected Burned Acres",
       subtitle = "By Treatment type and fireGroup", 
       x="Intensity",
       y="Sum total expected burned acres") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) + 
  facet_wrap(~TxType+fireGroup) 



### YR 0 scatters ---------------------------------------------

ggplot() +
  geom_point(data = r0,
             mapping = aes(x=expBurn_500k, y=expBurn_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=r0,
              aes(x=expBurn_500k, y=expBurn_2m),
              method=lm,
              formula = 'y ~ x',
              geom="smooth",
              se=FALSE,
              fullrange = TRUE) + 
  labs(title="Year 0: Expected Burned Acres",
       #subtitle = "",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") 

ggplot() +
  geom_point(data = r0,
             mapping = aes(x=expBurn_500k, y=expBurn_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=r0,
              aes(x=expBurn_500k, y=expBurn_2m),
              method=lm,
              formula = 'y ~ x',
              geom="smooth",
              se=FALSE,
              fullrange = TRUE) + 
  labs(title="Year 0: Expected Burned Acres",
       #subtitle = "",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") +
  facet_wrap(~Region)

ggplot() +
  geom_point(data = r0,
             mapping = aes(x=expBurn_500k, y=expBurn_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=r0,
              aes(x=expBurn_500k, y=expBurn_2m),
              method=lm,
              formula = 'y ~ x',
              geom="smooth",
              se=FALSE,
              fullrange = TRUE) + 
  labs(title="Year 0: Expected Burned Acres",
       #subtitle = "",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") +
  facet_wrap(~Priority+TxType)


ggplot() +
  geom_point(data = r0,
             mapping = aes(x=HaCBP_500k, y=HaCBP_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=r0,
              aes(x=HaCBP_500k, y=HaCBP_2m),
              method=lm,
              formula = 'y ~ x',
              geom="smooth",
              se=FALSE,
              fullrange = TRUE) + 
  labs(title="Year 0: HaCBP",
       #subtitle = "",
       x="500k Acres Treated",
       y="2.3 Million Acres Treated") 

ggplot() +
  geom_point(data = r0,
             mapping = aes(x=HaCBP_500k, y=HaCBP_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=r0,
              aes(x=HaCBP_500k, y=HaCBP_2m),
              method=lm,
              formula = 'y ~ x',
              geom="smooth",
              se=FALSE,
              fullrange = TRUE) + 
  labs(title="Year 0: HaCBP",
       #subtitle = "",
       x="500k Acres Treated",
       y="2.3 Million Acres Treated") +
  facet_wrap(~Priority+TxType)

