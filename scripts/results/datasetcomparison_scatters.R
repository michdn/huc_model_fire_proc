# Scatter plots 500k vs 2m, final year

# Expecting FULL (4 region datacube)
# Overall, facet region, facet Priority/trt

# expBurn, expFlame, conditional

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### Base Data import -------------------------------------------

folder_out <- file.path("plots", 
                        "dataset_comp",
                        "scatter")
dir.create(folder_out, recursive = TRUE)


res <- read_csv(file.path("results",
                          "datacube", 
                          "datacube_interim_SNSCCC_20240617.csv")) %>% 
  mutate(HUC12 = as.character(HUC12)) 


#End year
res2039 <- res %>% 
  filter(Year == 2039)

### Splitting scenarios ---------------------------------------

#compare 2.3m to 500k, include 1m for potential other comparisons

res2039_500k <- res2039 %>% 
  filter(TxIntensity == "500k") %>% 
  rename(expBurn_500k = expBurn, 
         expFlame_500k = expFlame,
         expAcf_500k = expPcActive,
         hacbp_500k = HaCBP,
         hacfl_500k = HaCFL)

res2039_1m <- res2039 %>% 
  filter(TxIntensity == "1m") %>% 
  rename(expBurn_1m = expBurn, 
         expFlame_1m = expFlame,
         expAcf_1m = expPcActive,
         hacbp_1m = HaCBP,
         hacfl_1m = HaCFL)

res2039_2m <- res2039 %>% 
  filter(TxIntensity == "2m") %>% 
  rename(expBurn_2m = expBurn, 
         expFlame_2m = expFlame,
         expAcf_2m = expPcActive,
         hacbp_2m = HaCBP,
         hacfl_2m = HaCFL)

res2039_jt <- res2039_500k %>% 
  left_join(res2039_1m, by = join_by(HUC12, Region, Priority, TxType)) %>% 
  left_join(res2039_2m, by = join_by(HUC12, Region, Priority, TxType)) 

res2039_trim <- res2039_jt %>% 
  dplyr::select(HUC12, Region, Priority, TxType, 
                expBurn_500k, expBurn_1m, expBurn_2m,
                expFlame_500k, expFlame_1m, expFlame_2m,
                expAcf_500k, expAcf_1m, expAcf_2m,
                hacbp_500k, hacbp_1m, hacbp_2m,
                hacfl_500k, hacfl_1m, hacfl_2m) %>% 
  mutate(region_label = case_when(
    Region == "CC" ~ "Central Coast",
    Region == "NC" ~ "North Coast",
    Region == "SC" ~ "South Coast",
    Region == "SN" ~ "Sierra Nevada"))


res2039_trim


### High level -----------------------------------------------------------

### expBurn 

#overall
plot_yr2039_expBurn_overall <- ggplot() + 
  geom_point(data=res2039_trim, 
             aes(x=expBurn_500k, y=expBurn_2m),
             shape=1, color="blue") +
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2039_trim,
              aes(x=expBurn_500k, y=expBurn_2m),
              method=lm,
              geom="smooth",
              formula='y~x',
              fullrange = TRUE) + 
  labs(title="Year 2039: Expected Burned Acres",
       x="500k Acres Treated",
       y="2.3 Million Acres Treated") + 
  theme_bw() + 
  theme(aspect.ratio = 1)
plot_yr2039_expBurn_overall

ggsave(plot = plot_yr2039_expBurn_overall,
       filename = "yr2039_expBurn_500vs2_scatter_overall.jpg",
       path = folder_out,
       width = 4.5, height = 4, units = "in")

#priority, type
plot_yr2039_expBurn_pt <- ggplot() + 
  geom_point(data=res2039_trim, 
             aes(x=expBurn_500k, y=expBurn_2m),
             shape=1, color="blue") +
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2039_trim,
              aes(x=expBurn_500k, y=expBurn_2m),
              method=lm,
              geom="smooth",
              formula="y~x",
              fullrange = TRUE) + 
  labs(title="Year 2039: Expected Burned Acres",
       x="500k Acres Treated",
       y="2.3 Million Acres Treated") +
  facet_wrap(~Priority+TxType) + 
  theme_bw() +   
  theme(aspect.ratio = 1)
plot_yr2039_expBurn_pt

ggsave(plot = plot_yr2039_expBurn_pt,
       filename = "yr2039_expBurn_500vs2_scatter_prioritytype.jpg",
       path = folder_out,
       width = 6, height = 7, units = "in")


#region
plot_yr2039_expBurn_reg <- ggplot() + 
  geom_point(data=res2039_trim, 
             aes(x=expBurn_500k, y=expBurn_2m),
             shape=1, color="blue") +
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2039_trim,
              aes(x=expBurn_500k, y=expBurn_2m),
              method=lm,
              geom="smooth",
              formula="y~x",
              fullrange = TRUE) + 
  labs(title="Year 2039: Expected Burned Acres",
       x="500k Acres Treated",
       y="2.3 Million Acres Treated") +
  facet_wrap(~region_label) + 
  theme_bw() + 
  theme(aspect.ratio = 1)
plot_yr2039_expBurn_reg

ggsave(plot = plot_yr2039_expBurn_reg,
       filename = "yr2039_expBurn_500vs2_scatter_region.jpg",
       path = folder_out,
       width = 7.25, height = 3, units = "in")


## expFlame

#overall
plot_yr2039_expFlame_overall <- ggplot() + 
  geom_point(data=res2039_trim, 
             aes(x=expFlame_500k, y=expFlame_2m),
             shape=1, color="blue") +
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2039_trim,
              aes(x=expFlame_500k, y=expFlame_2m),
              method=lm,
              geom="smooth",
              formula="y~x",
              fullrange = TRUE) + 
  labs(title="Year 2039: Expected Total Flame Index",
       x="500k Acres Treated",
       y="2.3 Million Acres Treated") + 
  theme_bw() + 
  theme(aspect.ratio = 1)
plot_yr2039_expFlame_overall

ggsave(plot = plot_yr2039_expFlame_overall,
       filename = "yr2039_expFlame_500vs2_scatter_overall.jpg",
       path = folder_out,
       width = 4.5, height = 4, units = "in")

#priority, type
plot_yr2039_expFlame_pt <- ggplot() + 
  geom_point(data=res2039_trim, 
             aes(x=expFlame_500k, y=expFlame_2m),
             shape=1, color="blue") +
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2039_trim,
              aes(x=expFlame_500k, y=expFlame_2m),
              method=lm,
              geom="smooth",
              formula="y~x",
              fullrange = TRUE) + 
  labs(title="Year 2039: Expected Total Flame Index",
       x="500k Acres Treated",
       y="2.3 Million Acres Treated") +
  facet_wrap(~Priority+TxType) + 
  theme_bw() + 
  theme(aspect.ratio = 1)
plot_yr2039_expFlame_pt

ggsave(plot = plot_yr2039_expFlame_pt,
       filename = "yr2039_expFlame_500vs2_scatter_prioritytype.jpg",
       path = folder_out,
       width = 6, height = 7, units = "in")


#region
plot_yr2039_expFlame_reg <- ggplot() + 
  geom_point(data=res2039_trim, 
             aes(x=expFlame_500k, y=expFlame_2m),
             shape=1, color="blue") +
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2039_trim,
              aes(x=expFlame_500k, y=expFlame_2m),
              method=lm,
              geom="smooth",
              formula="y~x",
              fullrange = TRUE) + 
  labs(title="Year 2039: Expected Total Flame Index",
       x="500k Acres Treated",
       y="2.3 Million Acres Treated") +
  facet_wrap(~region_label) + 
  theme_bw() + 
  theme(aspect.ratio = 1)
plot_yr2039_expFlame_reg

ggsave(plot = plot_yr2039_expFlame_reg,
       filename = "yr2039_expFlame_500vs2_scatter_region.jpg",
       path = folder_out,
       width = 7.25, height = 3, units = "in")


## Active crown fire 

#overall
plot_yr2039_expPcActive_overall <- ggplot() + 
  geom_point(data=res2039_trim, 
             aes(x=expAcf_500k, y=expAcf_2m),
             shape=1, color="blue") +
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2039_trim,
              aes(x=expAcf_500k, y=expAcf_2m),
              method=lm,
              geom="smooth",
              formula="y~x",
              fullrange = TRUE) + 
  labs(title="Year 2039: Expected Burn Acres by Active Crown Fire",
       x="500k Acres Treated",
       y="2.3 Million Acres Treated") + 
  theme_bw() + 
  theme(aspect.ratio = 1)
plot_yr2039_expPcActive_overall

ggsave(plot = plot_yr2039_expPcActive_overall,
       filename = "yr2039_expPcActive_500vs2_scatter_overall.jpg",
       path = folder_out,
       width = 4.5, height = 4, units = "in")

#priority, type
plot_yr2039_expPcActive_pt <- ggplot() + 
  geom_point(data=res2039_trim, 
             aes(x=expAcf_500k, y=expAcf_2m),
             shape=1, color="blue") +
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2039_trim,
              aes(x=expAcf_500k, y=expAcf_2m),
              method=lm,
              geom="smooth",
              formula="y~x",
              fullrange = TRUE) + 
  labs(title="Year 2039: Expected Burn Acres by Active Crown Fire",
       x="500k Acres Treated",
       y="2.3 Million Acres Treated") +
  facet_wrap(~Priority+TxType) + 
  theme_bw() + 
  theme(aspect.ratio = 1)
plot_yr2039_expPcActive_pt

ggsave(plot = plot_yr2039_expPcActive_pt,
       filename = "yr2039_expPcActive_500vs2_scatter_prioritytype.jpg",
       path = folder_out,
       width = 6, height = 7, units = "in")


#region
plot_yr2039_expPcActive_reg <- ggplot() + 
  geom_point(data=res2039_trim, 
             aes(x=expAcf_500k, y=expAcf_2m),
             shape=1, color="blue") +
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2039_trim,
              aes(x=expAcf_500k, y=expAcf_2m),
              method=lm,
              geom="smooth",
              formula="y~x", 
              fullrange = TRUE) + 
  labs(title="Year 2039: Expected Burn Acres by Active Crown Fire",
       x="500k Acres Treated",
       y="2.3 Million Acres Treated") +
  facet_wrap(~region_label) + 
  theme_bw() + 
  theme(aspect.ratio = 1)
plot_yr2039_expPcActive_reg

ggsave(plot = plot_yr2039_expPcActive_reg,
       filename = "yr2039_expPcActive_500vs2_scatter_region.jpg",
       path = folder_out,
       width = 7.25, height = 3, units = "in")

## HaCBP

#overall
plot_yr2039_hacbp_overall <- ggplot() + 
  geom_point(data=res2039_trim, 
             aes(x=hacbp_500k, y=hacbp_2m),
             shape=1, color="blue") +
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2039_trim,
              aes(x=hacbp_500k, y=hacbp_2m),
              method=lm,
              geom="smooth",
              formula="y~x",
              fullrange = TRUE) + 
  labs(title="Year 2039: HaCBP",
       x="500k Acres Treated",
       y="2.3 Million Acres Treated") + 
  theme_bw() + 
  theme(aspect.ratio = 1)
plot_yr2039_hacbp_overall

ggsave(plot = plot_yr2039_hacbp_overall,
       filename = "yr2039_hacbp_500vs2_scatter_overall.jpg",
       path = folder_out,
       width = 4.5, height = 4, units = "in")

#priority, type
plot_yr2039_hacbp_pt <- ggplot() + 
  geom_point(data=res2039_trim, 
             aes(x=hacbp_500k, y=hacbp_2m),
             shape=1, color="blue") +
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2039_trim,
              aes(x=hacbp_500k, y=hacbp_2m),
              method=lm,
              geom="smooth",
              formula="y~x",
              fullrange = TRUE) + 
  labs(title="Year 2039: HaCBP",
       x="500k Acres Treated",
       y="2.3 Million Acres Treated") +
  facet_wrap(~Priority+TxType) + 
  theme_bw() + 
  theme(aspect.ratio = 1)
plot_yr2039_hacbp_pt

ggsave(plot = plot_yr2039_hacbp_pt,
       filename = "yr2039_hacbp_500vs2_scatter_prioritytype.jpg",
       path = folder_out,
       width = 6, height = 7, units = "in")


#region
plot_yr2039_hacbp_reg <- ggplot() + 
  geom_point(data=res2039_trim, 
             aes(x=hacbp_500k, y=hacbp_2m),
             shape=1, color="blue") +
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2039_trim,
              aes(x=hacbp_500k, y=hacbp_2m),
              method=lm,
              geom="smooth",
              formula="y~x",
              fullrange = TRUE) + 
  labs(title="Year 2039: HaCBP",
       x="500k Acres Treated",
       y="2.3 Million Acres Treated") +
  facet_wrap(~region_label) + 
  theme_bw() + 
  theme(aspect.ratio = 1)
plot_yr2039_hacbp_reg

ggsave(plot = plot_yr2039_hacbp_reg,
       filename = "yr2039_hacbp_500vs2_scatter_region.jpg",
       path = folder_out,
       width = 7.25, height = 3, units = "in")


## HaCFL

#overall
plot_yr2039_hacfl_overall <- ggplot() + 
  geom_point(data=res2039_trim, 
             aes(x=hacfl_500k, y=hacfl_2m),
             shape=1, color="blue") +
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2039_trim,
              aes(x=hacfl_500k, y=hacfl_2m),
              method=lm,
              geom="smooth",
              formula="y~x",
              fullrange = TRUE) + 
  labs(title="Year 2039: HaCFL",
       x="500k Acres Treated",
       y="2.3 Million Acres Treated") + 
  theme_bw() + 
  theme(aspect.ratio = 1)
plot_yr2039_hacfl_overall

ggsave(plot = plot_yr2039_hacfl_overall,
       filename = "yr2039_hacfl_500vs2_scatter_overall.jpg",
       path = folder_out,
       width = 4.5, height = 4, units = "in")

#priority, type
plot_yr2039_hacfl_pt <- ggplot() + 
  geom_point(data=res2039_trim, 
             aes(x=hacfl_500k, y=hacfl_2m),
             shape=1, color="blue") +
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2039_trim,
              aes(x=hacfl_500k, y=hacfl_2m),
              method=lm,
              geom="smooth",
              formula="y~x",
              fullrange = TRUE) + 
  labs(title="Year 2039: HaCFL",
       x="500k Acres Treated",
       y="2.3 Million Acres Treated") +
  facet_wrap(~Priority+TxType) + 
  theme_bw() + 
  theme(aspect.ratio = 1)
plot_yr2039_hacfl_pt

ggsave(plot = plot_yr2039_hacfl_pt,
       filename = "yr2039_hacfl_500vs2_scatter_prioritytype.jpg",
       path = folder_out,
       width = 6, height = 7, units = "in")


#region
plot_yr2039_hacfl_reg <- ggplot() + 
  geom_point(data=res2039_trim, 
             aes(x=hacfl_500k, y=hacfl_2m),
             shape=1, color="blue") +
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2039_trim,
              aes(x=hacfl_500k, y=hacfl_2m),
              method=lm,
              geom="smooth",
              formula="y~x",
              fullrange = TRUE) + 
  labs(title="Year 2039: HaCFL",
       x="500k Acres Treated",
       y="2.3 Million Acres Treated") +
  facet_wrap(~region_label) + 
  theme_bw() + 
  theme(aspect.ratio = 1)
plot_yr2039_hacfl_reg

ggsave(plot = plot_yr2039_hacfl_reg,
       filename = "yr2039_hacfl_500vs2_scatter_region.jpg",
       path = folder_out,
       width = 7.25, height = 3, units = "in")


### Extra: cond vs abs ------------------------------------------------------

ggplot() + 
  geom_point(data=res2039, 
             aes(x=HaCBP, y=expBurn),
             shape=1, color="blue") +
  geom_abline(intercept=0, slope=1) + 
  labs(title="Year 2039: HaCBP & ExpBurn") + 
  theme_bw() + 
  theme(aspect.ratio = 1)

ggplot() + 
  geom_point(data=res2039, 
             aes(x=HaCFL, y=expFlame),
             shape=1, color="blue") +
  geom_abline(intercept=0, slope=1) + 
  labs(title="Year 2039: HaCFL & ExpFlame") + 
  theme_bw() + 
  theme(aspect.ratio = 1)
  