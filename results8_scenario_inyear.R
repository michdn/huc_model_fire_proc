# compare scenarios in a given year

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### User settings ---------------------------------------------

input_folder <- 'results_csv'

### Base Data import -------------------------------------------

res <- read_csv(file.path(input_folder, 
                          'datacube_expanded_20240124.csv')) %>% 
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
  rename(hacbp_500k = HaCBP, 
         hacfl_500k = HaCFL,
         acf_500k = active_crown_perc,
         fire_size_500k = ave_fire_size_ac)

res2044_1m <- res2044 %>% 
  filter(TxIntensity == "1m") %>% 
  rename(hacbp_1m = HaCBP,
         hacfl_1m = HaCFL,
         acf_1m = active_crown_perc,
         fire_size_1m = ave_fire_size_ac)

res2044_2m <- res2044 %>% 
  filter(TxIntensity == "2m") %>% 
  rename(hacbp_2m = HaCBP,
         hacfl_2m = HaCFL,
         acf_2m = active_crown_perc,
         fire_size_2m = ave_fire_size_ac)

res2044_jt <- res2044_500k %>% 
  left_join(res2044_1m, by = join_by(HUC12, RRK, Priority, TxType)) %>% 
  left_join(res2044_2m, by = join_by(HUC12, RRK, Priority, TxType)) 


res2044_trim <- res2044_jt %>% 
  select(HUC12, RRK, Priority, TxType, 
         hacbp_500k, hacbp_1m, hacbp_2m,
         hacfl_500k, hacfl_1m, hacfl_2m,
         acf_500k, acf_1m, acf_2m,
         fire_size_500k, fire_size_1m, fire_size_2m)

res2044_trim

### HaCBP ------------------------------------------------------------

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

plot_yr20_hacbp_overall <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=hacbp_500k, y=hacbp_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2044_trim,
            aes(x=hacbp_500k, y=hacbp_2m),
            method=lm,
            geom="smooth",
            fullrange = TRUE) + 
  labs(title="Year 20: Conditional Burn Probability",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated")
plot_yr20_hacbp_overall

ggsave(plot = plot_yr20_hacbp_overall,
       filename = "year20_hacbp_500vs2_scatter_formatted.jpg",
       path = "plots",
       width = 4.5, height = 4, units = "in")



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

plot_yr20_hacbp_prioritytype <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=hacbp_500k, y=hacbp_2m),
             shape=1, color = "blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2044_trim,
              aes(x=hacbp_500k, y=hacbp_2m),
              method=lm,
              geom="smooth",
              fullrange = TRUE) + 
  labs(title="Year 20: Conditional Burn Probability",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") + 
  facet_wrap(~Priority+TxType)
plot_yr20_hacbp_prioritytype

ggsave(plot = plot_yr20_hacbp_prioritytype,
       filename = "year20_hacbp_500vs2_scatter_prioritytype_formatted.jpg",
       path = "plots",
       width = 6, height = 7, units = "in")


#overall by region
ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=log10(hacbp_500k), y=log10(hacbp_2m), color=RRK),
             shape=1) +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) +
  facet_wrap(~RRK)

plot_yr20_hacbp_region <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=hacbp_500k, y=hacbp_2m),
             shape=1, color = "blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) +
  stat_smooth(data=res2044_trim,
              aes(x=hacbp_500k, y=hacbp_2m),
              method=lm,
              geom="smooth",
              fullrange = TRUE) + 
  labs(title="Year 20: Conditional Burn Probability",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") + 
  facet_wrap(~RRK)
plot_yr20_hacbp_region

ggsave(plot = plot_yr20_hacbp_region,
       filename = "year20_hacbp_500vs2_scatter_region_formatted.jpg",
       path = "plots",
       width = 6, height = 7, units = "in")


# ## with regression lines for treatment -------------------------------------
# 
# #cc
# res2044_cc <- res2044_trim %>% filter(RRK == "CC")
# 
# plot_hacbp_cc <- ggplot() + 
#   geom_point(data=res2044_cc, 
#              aes(x=hacbp_500k, y=hacbp_2m),
#              shape=1) +
#   theme(aspect.ratio = 1) + 
#   geom_abline(intercept=0, slope=1) + 
#   stat_smooth(data=res2044_cc, 
#             aes(x=hacbp_500k, y=hacbp_2m), 
#             method=lm, 
#             geom="smooth", 
#             fullrange = TRUE) + 
#   xlab("HaCBP at 500k Intensity") + 
#   ylab("HaCBP at 2.3m Intensity") + 
#   labs(title="Central Coast by Priority") +
#   facet_wrap(~Priority)
# 
# 
# # attempt with treatment by color with their own regression line
# # INCORRECT. WOULD NEED TO DO REGRESSION BY TRT AND BY PRIORITY
# 
# #regressions
# reg_trt1 <- lm(formula = hacbp_2m ~ hacbp_500k, 
#                data=res2044_cc %>% filter(TxType == "trt1"))
# reg_trt4 <- lm(formula = hacbp_2m ~ hacbp_500k, 
#                data=res2044_cc %>% filter(TxType == "trt4"))
# reg_trt6 <- lm(formula = hacbp_2m ~ hacbp_500k, 
#                data=res2044_cc %>% filter(TxType == "trt6"))
# #get intercept and slope values 
# coeff_trt1 <- coefficients(reg_trt1)           
# intercept_trt1 <- coeff_trt1[1] 
# slope_trt1 <- coeff_trt1[2]
# coeff_trt4 <- coefficients(reg_trt4)           
# intercept_trt4 <- coeff_trt4[1] 
# slope_trt4 <- coeff_trt4[2]
# coeff_trt6 <- coefficients(reg_trt6)           
# intercept_trt6 <- coeff_trt6[1] 
# slope_trt6 <- coeff_trt6[2]
# 
# 
# plot_hacbp_cc_trt <- ggplot() + 
#   geom_point(data=res2044_cc, 
#              aes(x=hacbp_500k, y=hacbp_2m, 
#                  color=TxType),
#              shape=1) +
#   theme(aspect.ratio = 1) + 
#   scale_color_manual("Treatment Type", 
#                      values = c("#1b9e77", "#d95f02", "#7570b3")) + 
#   geom_abline(intercept=0, slope=1) + 
#   geom_abline(intercept=intercept_trt1, 
#               slope=slope_trt1, 
#               color="#1b9e77") + 
#   geom_abline(intercept=intercept_trt4, 
#               slope=slope_trt4, 
#               color="#d95f02") + 
#   geom_abline(intercept=intercept_trt4, 
#               slope=slope_trt4, 
#               color="#7570b3") + 
#   # geom_abline(intercept = intercept, slope = slope, color="red",  
#   #             linetype="dashed", size=1.5)
#   xlab("HaCBP at 500k Intensity") + 
#   ylab("HaCBP at 2.3m Intensity") + 
#   labs(title="Central Coast by Priority") +
#   facet_wrap(~Priority)
# 
# plot_hacbp_cc_trt
# 
# ggsave(plot = plot_hacbp_cc,
#        filename = "year20_hacbp_500vs2_scatter_cc.jpg",
#        path = "plots",
#        width = 8, height = 3.5, units = "in")
# 
# 
# ggsave(plot = plot_hacbp_cc_trt,
#        filename = "year20_hacbp_500vs2_scatter_cc_trt.jpg",
#        path = "plots",
#        width = 8, height = 3.5, units = "in")
# 
# 
# # function version 
# 
# plot_hacbp_500vs2_region <- function(df, reg_code, title, year_prefix){
#   
#   this_df <- df %>% filter(RRK == reg_code)
#   
#   plot_hacbp <- ggplot() + 
#     geom_point(data=this_df, 
#                aes(x=hacbp_500k, y=hacbp_2m),
#                shape=1) +
#     theme(aspect.ratio = 1) + 
#     geom_abline(intercept=0, slope=1) + 
#     stat_smooth(data=this_df, 
#                 aes(x=hacbp_500k, y=hacbp_2m), 
#                 method=lm, 
#                 geom="smooth", 
#                 fullrange = TRUE) + 
#     xlab("HaCBP at 500k Intensity") + 
#     ylab("HaCBP at 2.3m Intensity") + 
#     labs(title=title) +
#     facet_wrap(~Priority)
#   
#   
#   #treatment by color with their own regression line
#   
#   #regressions
#   reg_trt1 <- lm(formula = hacbp_2m ~ hacbp_500k, 
#                  data=this_df %>% filter(TxType == "trt1"))
#   reg_trt4 <- lm(formula = hacbp_2m ~ hacbp_500k, 
#                  data=this_df %>% filter(TxType == "trt4"))
#   reg_trt6 <- lm(formula = hacbp_2m ~ hacbp_500k, 
#                  data=this_df %>% filter(TxType == "trt6"))
#   #get intercept and slope values 
#   coeff_trt1 <- coefficients(reg_trt1)           
#   intercept_trt1 <- coeff_trt1[1] 
#   slope_trt1 <- coeff_trt1[2]
#   coeff_trt4 <- coefficients(reg_trt4)           
#   intercept_trt4 <- coeff_trt4[1] 
#   slope_trt4 <- coeff_trt4[2]
#   coeff_trt6 <- coefficients(reg_trt6)           
#   intercept_trt6 <- coeff_trt6[1] 
#   slope_trt6 <- coeff_trt6[2]
#   
#   
#   plot_hacbp_trt <- ggplot() + 
#     geom_point(data=this_df, 
#                aes(x=hacbp_500k, y=hacbp_2m, 
#                    color=TxType),
#                shape=1) +
#     theme(aspect.ratio = 1) + 
#     scale_color_manual("Treatment Type", 
#                        values = c("#1b9e77", "#d95f02", "#7570b3")) + 
#     geom_abline(intercept=0, slope=1) + 
#     geom_abline(intercept=intercept_trt1, 
#                 slope=slope_trt1, 
#                 color="#1b9e77") + 
#     geom_abline(intercept=intercept_trt4, 
#                 slope=slope_trt4, 
#                 color="#d95f02") + 
#     geom_abline(intercept=intercept_trt4, 
#                 slope=slope_trt4, 
#                 color="#7570b3") + 
#     # geom_abline(intercept = intercept, slope = slope, color="red",  
#     #             linetype="dashed", size=1.5)
#     xlab("HaCBP at 500k Intensity") + 
#     ylab("HaCBP at 2.3m Intensity") + 
#     labs(title=title) +
#     facet_wrap(~Priority)
#   
#   plot_hacbp_cc_trt
#   
#   ggsave(plot = plot_hacbp,
#          filename = paste0(year_prefix, 
#                            "_hacbp_500vs2_scatter_", 
#                            tolower(reg_code), 
#                            ".jpg"),
#          path = "plots",
#          width = 8, height = 3.5, units = "in")
#   
#   ggsave(plot = plot_hacbp_trt,
#          filename = paste0(year_prefix,
#                            "_hacbp_500vs2_scatter_", 
#                            tolower(reg_code), 
#                            "_trt.jpg"),
#          path = "plots",
#          width = 8, height = 3.5, units = "in")
#   
#   
# }
# 
# plot_hacbp_500vs2_region(df = res2044_trim, 
#                          reg_code = "CC",
#                          title = "Central Coast by Priority", 
#                          year_prefix = "year20")


## HaCFL -----------------------------------------------------------------

#overall
ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=log10(hacfl_500k), y=log10(hacfl_2m), color=RRK),
             shape=1) +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) 

plot_yr20_hacfl_overall <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=hacfl_500k, y=hacfl_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2044_trim,
              aes(x=hacfl_500k, y=hacfl_2m),
              method=lm,
              geom="smooth",
              fullrange = TRUE) + 
  labs(title="Year 20: Conditional Flame Length",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated")
plot_yr20_hacfl_overall

ggsave(plot = plot_yr20_hacfl_overall,
       filename = "year20_hacfl_500vs2_scatter_formatted.jpg",
       path = "plots",
       width = 4.5, height = 4, units = "in")


#priority & type
ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=log10(hacfl_500k), y=log10(hacfl_2m), color=RRK),
             shape=1) +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  facet_wrap(~Priority+TxType)

plot_yr20_hacfl_prioritytype <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=hacfl_500k, y=hacfl_2m),
             shape=1, color = "blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2044_trim,
              aes(x=hacfl_500k, y=hacfl_2m),
              method=lm,
              geom="smooth",
              fullrange = TRUE) + 
  labs(title="Year 20: Conditional Flame Length",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") + 
  facet_wrap(~Priority+TxType)
plot_yr20_hacfl_prioritytype

ggsave(plot = plot_yr20_hacfl_prioritytype,
       filename = "year20_hacfl_500vs2_scatter_prioritytype_formatted.jpg",
       path = "plots",
       width = 6, height = 7, units = "in")


#overall by region
plot_yr20_hacfl_region <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=hacfl_500k, y=hacfl_2m),
             shape=1, color = "blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) +
  stat_smooth(data=res2044_trim,
              aes(x=hacfl_500k, y=hacfl_2m),
              method=lm,
              geom="smooth",
              fullrange = TRUE) + 
  labs(title="Year 20: Conditional Flame Length",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") + 
  facet_wrap(~RRK)
plot_yr20_hacfl_region

ggsave(plot = plot_yr20_hacfl_region,
       filename = "year20_hacfl_500vs2_scatter_region_formatted.jpg",
       path = "plots",
       width = 6, height = 7, units = "in")


## Active crown fire ------------------------------------------------------

plot_yr20_acf_overall <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=acf_500k, y=acf_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2044_trim,
              aes(x=acf_500k, y=acf_2m),
              method=lm,
              geom="smooth",
              fullrange = TRUE) + 
  labs(title="Year 20: Active Crown Fire Percentage",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated")
plot_yr20_acf_overall

ggsave(plot = plot_yr20_acf_overall,
       filename = "year20_acf_500vs2_scatter_formatted.jpg",
       path = "plots",
       width = 4.5, height = 4, units = "in")


#priority & type
plot_yr20_acf_prioritytype <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=acf_500k, y=acf_2m),
             shape=1, color = "blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2044_trim,
              aes(x=acf_500k, y=acf_2m),
              method=lm,
              geom="smooth",
              fullrange = TRUE) + 
  labs(title="Year 20: Active Crown Fire Percentage",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") + 
  facet_wrap(~Priority+TxType)
plot_yr20_acf_prioritytype

ggsave(plot = plot_yr20_acf_prioritytype,
       filename = "year20_acf_500vs2_scatter_prioritytype_formatted.jpg",
       path = "plots",
       width = 6, height = 7, units = "in")


#overall by region
plot_yr20_acf_region <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=acf_500k, y=acf_2m),
             shape=1, color = "blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) +
  stat_smooth(data=res2044_trim,
              aes(x=acf_500k, y=acf_2m),
              method=lm,
              geom="smooth",
              fullrange = TRUE) + 
  labs(title="Year 20: Active Crown Fire Percentage",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") + 
  facet_wrap(~RRK)
plot_yr20_acf_region

ggsave(plot = plot_yr20_acf_region,
       filename = "year20_acf_500vs2_scatter_region_formatted.jpg",
       path = "plots",
       width = 6, height = 7, units = "in")



## Fire size --------------------------------------------------------------


#need to set ticks manually so they match here. 

xy_limits_size <- range(c(res2044_trim$fire_size_500k, 
                          res2044_trim$fire_size_2m))

plot_yr20_size_overall <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=fire_size_500k, y=fire_size_2m),
             shape=1, color="blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2044_trim,
              aes(x=fire_size_500k, y=fire_size_2m),
              method=lm,
              geom="smooth",
              fullrange = TRUE) + 
  labs(title="Year 20: Average Fire Size (Acres)",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") +
  scale_x_continuous(limits=xy_limits_size) +
  scale_y_continuous(limits = xy_limits_size)
plot_yr20_size_overall


ggsave(plot = plot_yr20_size_overall,
       filename = "year20_firesize_500vs2_scatter_formatted.jpg",
       path = "plots",
       width = 4.5, height = 4, units = "in")


#priority & type
plot_yr20_size_prioritytype <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=fire_size_500k, y=fire_size_2m),
             shape=1, color = "blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) + 
  stat_smooth(data=res2044_trim,
              aes(x=fire_size_500k, y=fire_size_2m),
              method=lm,
              geom="smooth",
              fullrange = TRUE) + 
  labs(title="Year 20: Average Fire Size (Acres)",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") + 
  facet_wrap(~Priority+TxType) + 
  scale_x_continuous(limits=xy_limits_size) +
  scale_y_continuous(limits = xy_limits_size)

plot_yr20_size_prioritytype

ggsave(plot = plot_yr20_size_prioritytype,
       filename = "year20_firesize_500vs2_scatter_prioritytype_formatted.jpg",
       path = "plots",
       width = 6, height = 7, units = "in")


#overall by region
plot_yr20_size_region <- ggplot() + 
  geom_point(data=res2044_trim, 
             aes(x=fire_size_500k, y=fire_size_2m),
             shape=1, color = "blue") +
  theme(aspect.ratio = 1) + 
  geom_abline(intercept=0, slope=1) +
  stat_smooth(data=res2044_trim,
              aes(x=fire_size_500k, y=fire_size_2m),
              method=lm,
              geom="smooth",
              fullrange = TRUE) + 
  labs(title="Year 20: Average Fire Size (Acres)",
       x="Baseline (500k Acres Treated)",
       y="2.3 Million Acres Treated") + 
  facet_wrap(~RRK) +
  scale_x_continuous(limits=xy_limits_size) +
  scale_y_continuous(limits = xy_limits_size)

plot_yr20_size_region

ggsave(plot = plot_yr20_size_region,
       filename = "year20_firesize_500vs2_scatter_region_formatted.jpg",
       path = "plots",
       width = 6, height = 7, units = "in")

