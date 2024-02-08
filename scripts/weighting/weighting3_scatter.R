# Script to produce scatter plots of the absolute metrics
#  comparing 500k scenarios to 2.3m scenarios

# Explore efficiency: 500k, 1m, 2m

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### Base Data import -------------------------------------------

res <- read_csv(file.path('results_csv', 
                          'datacube_weighted_20240205.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

#Year 20
res2044 <- res %>% 
  filter(Year == 2044)

### Splitting scenarios ---------------------------------------

#compare 2.3m to 1m to 500k

res2044_500k <- res2044 %>% 
  filter(TxIntensity == "500k") %>% 
  rename(expBurn_500k = expBurn, 
         expFlame_500k = expFlame,
         expAcf_500k = expPcActive)

res2044_1m <- res2044 %>% 
  filter(TxIntensity == "1m") %>% 
  rename(expBurn_1m = expBurn, 
         expFlame_1m = expFlame,
         expAcf_1m = expPcActive)

res2044_2m <- res2044 %>% 
  filter(TxIntensity == "2m") %>% 
  rename(expBurn_2m = expBurn, 
         expFlame_2m = expFlame,
         expAcf_2m = expPcActive)

res2044_jt <- res2044_500k %>% 
  left_join(res2044_1m, by = join_by(HUC12, Region, Priority, TxType)) %>% 
  left_join(res2044_2m, by = join_by(HUC12, Region, Priority, TxType)) 


res2044_trim <- res2044_jt %>% 
  dplyr::select(HUC12, Region, Priority, TxType, 
         expBurn_500k, expBurn_1m, expBurn_2m,
         expFlame_500k, expFlame_1m, expFlame_2m,
         expAcf_500k, expAcf_1m, expAcf_2m)

res2044_trim

### expBurn -----------------------------------------------------------

#overall
plot_yr20_expBurn_overall <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=expBurn_500k, y=expBurn_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2044_trim,
              aes(x=expBurn_500k, y=expBurn_2m),
              method=lm,
              geom="smooth",
              fullrange = TRUE) + 
  labs(title="Year 20: Expected Burned Acres",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated")
plot_yr20_expBurn_overall

ggsave(plot = plot_yr20_expBurn_overall,
       filename = "year20_expBurn_500vs2_scatter_overall.jpg",
       path = "plots_weighted",
       width = 4.5, height = 4, units = "in")

#priority, type
plot_yr20_expBurn_pt <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=expBurn_500k, y=expBurn_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2044_trim,
              aes(x=expBurn_500k, y=expBurn_2m),
              method=lm,
              geom="smooth",
              fullrange = TRUE) + 
  labs(title="Year 20: Expected Burned Acres",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") +
  facet_wrap(~Priority+TxType)
plot_yr20_expBurn_pt

ggsave(plot = plot_yr20_expBurn_pt,
       filename = "year20_expBurn_500vs2_scatter_prioritytype.jpg",
       path = "plots_weighted",
       width = 6, height = 7, units = "in")


#region
plot_yr20_expBurn_reg <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=expBurn_500k, y=expBurn_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2044_trim,
              aes(x=expBurn_500k, y=expBurn_2m),
              method=lm,
              geom="smooth",
              fullrange = TRUE) + 
  labs(title="Year 20: Expected Burned Acres",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") +
  facet_wrap(~Region)
plot_yr20_expBurn_reg

ggsave(plot = plot_yr20_expBurn_reg,
       filename = "year20_expBurn_500vs2_scatter_region.jpg",
       path = "plots_weighted",
       width = 6, height = 7, units = "in")


## expFlame -------------------------------------------------------

#overall
plot_yr20_expFlame_overall <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=expFlame_500k, y=expFlame_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2044_trim,
              aes(x=expFlame_500k, y=expFlame_2m),
              method=lm,
              geom="smooth",
              fullrange = TRUE) + 
  labs(title="Year 20: Expected Total Flame Length",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated")
plot_yr20_expFlame_overall

ggsave(plot = plot_yr20_expFlame_overall,
       filename = "year20_expFlame_500vs2_scatter_overall.jpg",
       path = "plots_weighted",
       width = 4.5, height = 4, units = "in")

#priority, type
plot_yr20_expFlame_pt <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=expFlame_500k, y=expFlame_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2044_trim,
              aes(x=expFlame_500k, y=expFlame_2m),
              method=lm,
              geom="smooth",
              fullrange = TRUE) + 
  labs(title="Year 20: Expected Total Flame Length",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") +
  facet_wrap(~Priority+TxType)
plot_yr20_expFlame_pt

ggsave(plot = plot_yr20_expFlame_pt,
       filename = "year20_expFlame_500vs2_scatter_prioritytype.jpg",
       path = "plots_weighted",
       width = 6, height = 7, units = "in")


#region
plot_yr20_expFlame_reg <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=expFlame_500k, y=expFlame_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2044_trim,
              aes(x=expFlame_500k, y=expFlame_2m),
              method=lm,
              geom="smooth",
              fullrange = TRUE) + 
  labs(title="Year 20: Expected Total Flame Length",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") +
  facet_wrap(~Region)
plot_yr20_expFlame_reg

ggsave(plot = plot_yr20_expFlame_reg,
       filename = "year20_expFlame_500vs2_scatter_region.jpg",
       path = "plots_weighted",
       width = 6, height = 7, units = "in")


## Active crown fire ----------------------------------------------------

#overall
plot_yr20_expPcActive_overall <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=expAcf_500k, y=expAcf_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2044_trim,
              aes(x=expAcf_500k, y=expAcf_2m),
              method=lm,
              geom="smooth",
              fullrange = TRUE) + 
  labs(title="Year 20: Expected Burn Acres by Active Crown Fire",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated")
plot_yr20_expPcActive_overall

ggsave(plot = plot_yr20_expPcActive_overall,
       filename = "year20_expPcActive_500vs2_scatter_overall.jpg",
       path = "plots_weighted",
       width = 4.5, height = 4, units = "in")

#priority, type
plot_yr20_expPcActive_pt <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=expAcf_500k, y=expAcf_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2044_trim,
              aes(x=expAcf_500k, y=expAcf_2m),
              method=lm,
              geom="smooth",
              fullrange = TRUE) + 
  labs(title="Year 20: Expected Burn Acres by Active Crown Fire",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") +
  facet_wrap(~Priority+TxType)
plot_yr20_expPcActive_pt

ggsave(plot = plot_yr20_expPcActive_pt,
       filename = "year20_expPcActive_500vs2_scatter_prioritytype.jpg",
       path = "plots_weighted",
       width = 6, height = 7, units = "in")


#region
plot_yr20_expPcActive_reg <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=expAcf_500k, y=expAcf_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2044_trim,
              aes(x=expAcf_500k, y=expAcf_2m),
              method=lm,
              geom="smooth",
              fullrange = TRUE) + 
  labs(title="Year 20: Expected Burn Acres by Active Crown Fire",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") +
  facet_wrap(~Region)
plot_yr20_expPcActive_reg

ggsave(plot = plot_yr20_expPcActive_reg,
       filename = "year20_expPcActive_500vs2_scatter_region.jpg",
       path = "plots_weighted",
       width = 6, height = 7, units = "in")
