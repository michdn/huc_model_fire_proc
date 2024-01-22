# compare scenarios in a given year

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### User settings ---------------------------------------------

input_folder <- 'results_csv'

### Base Data import -------------------------------------------

res <- read_csv(file.path(input_folder, 
                          'datacube_expanded_20240119.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

#Year 5
res2029 <- res %>% 
  filter(Year == 2029)

#Year 20
res2044 <- res %>% 
  filter(Year == 2044)

### Year 20 scenarios --------------------------------------------------

#compare 2.3m to 1m to 500k

res2044_500k <- res2044 %>% 
  filter(TxIntensity == "500k") %>% 
  rename(hacbp_500k = HaCBP)

res2044_1m <- res2044 %>% 
  filter(TxIntensity == "1m") %>% 
  rename(hacbp_1m = HaCBP)

res2044_2m <- res2044 %>% 
  filter(TxIntensity == "2m") %>% 
  rename(hacbp_2m = HaCBP)

res2044_jt <- res2044_500k %>% 
  left_join(res2044_1m, by = join_by(HUC12, RRK, Priority, TxType)) %>% 
  left_join(res2044_2m, by = join_by(HUC12, RRK, Priority, TxType)) 


res2044_trim <- res2044_jt %>% 
  select(HUC12, RRK, Priority, TxType, hacbp_500k, hacbp_1m, hacbp_2m)

#overall
ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=log10(hacbp_500k), y=log10(hacbp_2m), color=RRK),
             shape=1) +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) 

ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=hacbp_500k, y=hacbp_2m, color=RRK),
             shape=1) +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) 


#priority
ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=log10(hacbp_500k), y=log10(hacbp_2m), color=RRK),
             shape=1) +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  facet_wrap(~Priority)

#type
ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=log10(hacbp_500k), y=log10(hacbp_2m), color=RRK),
             shape=1) +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  facet_wrap(~TxType)

#priority & type
ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=log10(hacbp_500k), y=log10(hacbp_2m), color=RRK),
             shape=1) +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  facet_wrap(~Priority+TxType)

ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=hacbp_500k, y=hacbp_2m, color=RRK),
             shape=1) +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  facet_wrap(~Priority+TxType)


#overall by region
ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=log10(hacbp_500k), y=log10(hacbp_2m), color=RRK),
             shape=1) +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) +
  facet_wrap(~RRK)

ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=hacbp_500k, y=hacbp_2m, color=RRK),
             shape=1) +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) +
  facet_wrap(~RRK)
