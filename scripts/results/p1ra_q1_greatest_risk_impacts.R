#' ---
#' title: "Regional greatest fire risks and greatest impacts"
#' output: html_document
#' ---

#' Requested Phase 1 Analyses
#'
#' 1. What regions faced the greatest fire risk and where did treatments have the greatest impacts?
#' 
#' 1.1 By Region, what was the relative change in Conditional Flame Length and Active Crown Fire from the 500 to 1m to 2.3m acre treatments?


#NOTE: Half translated for spin() but too many issues, abandoned that aspect. Treat as normal .R script for now. 

### Libraries -------------------------------------------------
#+ libraries, echo=FALSE
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  viridis,
  sf,
  viridis,
  ggspatial, 
  gridExtra)

#knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

### User settings -----------------------------------------
#+ user, echo=FALSE
folder_out <- file.path("phase1", "q1_risk")
dir.create(folder_out, recursive = TRUE)


### Data import -------------------------------------------
#+ import, echo=FALSE
res_orig <- read_csv(file.path("results",
                               "202403_runs",
                               "datacube", 
                               "datacube_interim_sc_cc_sn_bl_bw_20240513.csv")) %>% 
  #no baselines here/now
  filter(Priority %in% c("Fire", "WUI", "Hybrid")) %>% 
  #fix exception case (will be fixed in future absolute script runs)
  mutate(across(c(expPcSurface, expPcPassive, expPcActive), ~ replace_na(., 0))) %>% 
  mutate(HUC12 = as.character(HUC12)) 

#spatial
hucs_spatial <- st_read("data/data_huc/TxHucsTimingGroups.shp") %>% 
  rename(HUC12 = huc12)
#Simplified for faster drawing, testing only
hucs_simpl <- sf::st_simplify(hucs_spatial, preserveTopology = TRUE, dTolerance = 200)
#region boundaries for map
region_spatial <- hucs_spatial %>% 
  group_by(region) %>% 
  summarize()

# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
states <- st_read("data/tl_2023_us_state/tl_2023_us_state.shp")
ca <- states %>% filter(STUSPS == "CA")


### Data set up ---------------------------------------------
#+ dataprep, echo=FALSE

# Get all intensities in one row
res_wide <- res_orig %>%
  #adding timing group
  mutate(timing_group = case_when(
    Priority == "Fire" ~ timeFire, 
    Priority == "WUI" ~ timeWui,
    Priority == "Hybrid" ~ timeHybrid
  )) %>% 
  #and WUI 4th category is too long for graphs
  mutate(timing_group = if_else(timing_group=="2024_2039_yr1to5_16to20",
                                "2024_2039",
                                timing_group)) %>% 
  #also calc treatment year (untreated WUI will be NA)
  separate_wider_delim(cols=timing_group, delim="_", 
                       names=c("trt_yr", NA),
                       #too few for the "Not treated" WUI 4th
                       too_few="align_end",
                       cols_remove=FALSE) %>% 
  #selecting desired and pivot wider for all Intensities in one row
  dplyr::select(HUC12, Region, 
                Priority, TxIntensity, TxType,
                Year, timing_group, trt_yr, 
                expFlame, expBurn, expPcActive) %>% 
  pivot_wider(names_from = TxIntensity,
              values_from = c(expFlame, expBurn, expPcActive)) 

#calculate differences from 500k to 1m, 500k to 2m, 1m to 2m. 
# (uncertain which ones exactly would needed for which plots)
res_wide <- res_wide %>% 
  #calculate differences
  mutate(expFlame_1m500k = expFlame_1m - expFlame_500k,
         expFlame_2m500k = expFlame_2m - expFlame_500k,
         expFlame_2m1m = expFlame_2m - expFlame_1m,
         #active as well
         expPcActive_1m500k = expPcActive_1m - expPcActive_500k,
         expPcActive_2m500k = expPcActive_2m - expPcActive_500k,
         expPcActive_2m1m = expPcActive_2m - expPcActive_1m)




### greatest risk -------------------------------------------------
#+ allyrsrisk, echo=FALSE

#' ##Greatest risk
#' To describe greatest risk, we will use the 500k scenarios. 

# Greatest fire risk: No baseline to use. USE 500k AS pseudo-BASELINE
# Average {expFlame} 500k scenarios, per HUC per year. 
# Then ave over year (1 value per HUC). Then by region and/or map HUCs. 

# REstrict to 2024? 
# Did first over all years, then 2024 to compare. Select or both. 
# SELECTED ALL YEARS. 

## All years
fl_mean_500k <- res_wide %>% 
  #by HUC and year
  group_by(Region, HUC12, Year) %>% 
  # mean of all 500k scenarios (priority and treatment types)
  summarize(mean_flame_500k = mean(expFlame_500k),
            mean_act_500k = mean(expPcActive_500k), 
            .groups="drop") %>% 
  # group by only HUC (collapsing year now)
  group_by(Region, HUC12) %>% 
  summarize(mean_flame_500k = mean(mean_flame_500k), 
            mean_act_500k = mean(mean_act_500k),
            .groups="drop")
#2063 = 1148 + 480 + 435

# risk summary values by region
risk_sum_tbl <- fl_mean_500k %>% 
  group_by(Region) %>% 
  summarize(`Ave flame index` = round(mean(mean_flame_500k), 2),
            `Ave active crown percent` = round(mean(mean_act_500k), 2)) %>% 
  arrange(desc(`Ave flame index`))


# map risk (mean expFlame)
hucs_fl_mean_500k <- hucs_spatial %>% 
  left_join(fl_mean_500k, by = join_by(HUC12))

plot_risk_overall_flame <- ggplot() +
  #California
  geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
  #HUCs
  geom_sf(data = hucs_fl_mean_500k, 
          #color the HUC by overall mean expFlame
          aes(fill = mean_flame_500k),
          color = "grey90", size = 0.01) +
  #regions
  geom_sf(data = region_spatial,
          color = "grey30", fill = NA, size = 0.1) + 
  #use viridis color scheme
  # scale_fill_viridis("Average expFlame", option="rocket", direction=-1, 
  #                    #dealing with NAs and labeling
  #                    na.value="grey95",
  #                    limits=range(c(fl_mean_500k$mean_flame_500k, 
  #                                   fl_2024_500k$mean_flame_500k))) + 
  scale_fill_viridis("Average expFlame", option="rocket", direction=-1, 
                     #dealing with NAs and labeling
                     na.value="grey95") + 
  #plot settings for a nicer map
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        strip.background = element_blank(),
        strip.text = element_text(size = 13),
        axis.text = element_text(family = "sans"),
        legend.position = "right",
        legend.direction = "vertical") +
  #add a scale bar
  ggspatial::annotation_scale(
    location = "bl", bar_cols = c("grey50", "white")) +
  #add a north arrow
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    height = unit(0.6, "cm"), width = unit(0.6, "cm"),
    pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
    style = ggspatial::north_arrow_minimal()) +
  #add a plot title
  labs(title = "Fire risk: Average flame index",
       subtitle = "Over all years in all 500k scenarios")



plot_risk_overall_active <- ggplot() +
  #California
  geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
  #HUCs
  geom_sf(data = hucs_fl_mean_500k, 
          #color the HUC by overall mean expPcActive
          aes(fill = mean_act_500k),
          color = "grey90", size = 0.01) +
  #regions
  geom_sf(data = region_spatial,
          color = "grey30", fill = NA, size = 0.1) + 
  #use viridis color scheme
  # scale_fill_viridis("Average expPcActive", option="magma", direction=-1, 
  #                    #dealing with NAs and labeling
  #                    na.value="grey95",
  #                    limits=range(c(fl_mean_500k$mean_act_500k, 
  #                                   fl_2024_500k$mean_act_500k))) + 
  scale_fill_viridis("Average expPcActive", option="magma", direction=-1, 
                     #dealing with NAs and labeling
                     na.value="grey95") + 
  #plot settings for a nicer map
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        strip.background = element_blank(),
        strip.text = element_text(size = 13),
        axis.text = element_text(family = "sans"),
        legend.position = "right",
        legend.direction = "vertical") +
  #add a scale bar
  ggspatial::annotation_scale(
    location = "bl", bar_cols = c("grey50", "white")) +
  #add a north arrow
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    height = unit(0.6, "cm"), width = unit(0.6, "cm"),
    pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
    style = ggspatial::north_arrow_minimal()) +
  #add a plot title
  labs(title = "Fire risk: Average percent of active crown fire",
       subtitle = "Over all years in all 500k scenarios")


# USE ALL YEAR VERSION

maps_risk <- arrangeGrob(plot_risk_overall_flame,
                         plot_risk_overall_active,
                         nrow=2)

ggsave(plot=maps_risk,
       filename=file.path(folder_out, 
                          "risk_maps_500k_allyears.jpg"),
       height=11, width=7, units=c("in"))

rst_grob <- tableGrob(risk_sum_tbl, rows = rep("", nrow(risk_sum_tbl)))
plot(rst_grob)
rst_title <- "Average values of 500k scenarios over all years"
ggsave(plot=grid.arrange(top=rst_title, rst_grob),
       filename = file.path(folder_out, 
                            "risk_summary_table_allyrs.jpg"),
       height=1.75, width=5, units=c("in"))


# #+ yr2024risk, echo=FALSE
# #compare to 2024 only and see how much/if differs.... 
# 
# fl_2024_500k <- res_wide %>% 
#   #limit to 2024 only
#   filter(Year == 2024) %>% 
#   #by HUC 
#   group_by(Region, HUC12) %>% 
#   # mean of all 500k scenarios (priority and treatment types)
#   summarize(mean_flame_500k = mean(expFlame_500k),
#             mean_act_500k = mean(expPcActive_500k), 
#             .groups="drop")
# #2063 = 1148 + 480 + 435
# 
# # risk summary values by region
# fl_2024_500k %>% 
#   group_by(Region) %>% 
#   summarize(region_overall_mean_flame = mean(mean_flame_500k),
#             region_overall_mean_actpc = mean(mean_act_500k)) %>% 
#   arrange(desc(region_overall_mean_flame)) %>% 
#   knitr::kable()
# 
# # with March run. flame slightly lower, act slightly higher, across SC,CC,SN in 2024
# 
# # 2024 map risk (mean expFlame)
# hucs_fl_2024_500k <- hucs_simpl %>% 
#   left_join(fl_2024_500k, by = join_by(HUC12))
# 
# plot_risk_2024_flame <- ggplot() +
#   #California
#   geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
#   #HUCs
#   geom_sf(data = hucs_fl_2024_500k, 
#           #color the HUC by overall mean expFlame
#           aes(fill = mean_flame_500k),
#           color = "grey90", size = 0.01) +
#   #use viridis color scheme
#   scale_fill_viridis("Average expFlame", option="rocket", direction=-1, 
#                      #dealing with NAs and labeling
#                      na.value="grey95",
#                      limits=range(c(fl_mean_500k$mean_flame_500k, 
#                                     fl_2024_500k$mean_flame_500k))) + 
#   #plot settings for a nicer map
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         panel.background = element_blank(),
#         legend.title = element_text(size = 11),
#         legend.text = element_text(size = 10),
#         strip.background = element_blank(),
#         strip.text = element_text(size = 13),
#         axis.text = element_text(family = "sans"),
#         legend.position = "right",
#         legend.direction = "vertical") +
#   #add a scale bar
#   ggspatial::annotation_scale(
#     location = "bl", bar_cols = c("grey50", "white")) +
#   #add a north arrow
#   ggspatial::annotation_north_arrow(
#     location = "tr", which_north = "true",
#     height = unit(0.6, "cm"), width = unit(0.6, "cm"),
#     pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
#     style = ggspatial::north_arrow_minimal()) +
#   #add a plot title
#   labs(title = "Average flame index over all 500k scenarios",
#        subtitle = "In 2024")
# 
# 
# plot_risk_2024_active <- ggplot() +
#   #California
#   geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
#   #HUCs
#   geom_sf(data = hucs_fl_2024_500k, 
#           #color the HUC by overall mean expPcActive
#           aes(fill = mean_act_500k),
#           color = "grey90", size = 0.01) +
#   #use viridis color scheme
#   scale_fill_viridis("Average expPcActive", option="magma", direction=-1, 
#                      #dealing with NAs and labeling
#                      na.value="grey95",
#                      limits=range(c(fl_mean_500k$mean_act_500k, 
#                                     fl_2024_500k$mean_act_500k))) + 
#   #plot settings for a nicer map
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         panel.background = element_blank(),
#         legend.title = element_text(size = 11),
#         legend.text = element_text(size = 10),
#         strip.background = element_blank(),
#         strip.text = element_text(size = 13),
#         axis.text = element_text(family = "sans"),
#         legend.position = "right",
#         legend.direction = "vertical") +
#   #add a scale bar
#   ggspatial::annotation_scale(
#     location = "bl", bar_cols = c("grey50", "white")) +
#   #add a north arrow
#   ggspatial::annotation_north_arrow(
#     location = "tr", which_north = "true",
#     height = unit(0.6, "cm"), width = unit(0.6, "cm"),
#     pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
#     style = ggspatial::north_arrow_minimal()) +
#   #add a plot title
#   labs(title = "Average percent of active crown fire in all 500k scenarios in 2024")
# 
# 
# #Watch out for changing scales. 
# # ave expFlame higher in all years. Pattern is pretty similar though. 
# # expPcActive higher in 2024. Pattern similar. 
# # If showing both, use the synced ranges (limits=() added above). 
# 
# #range(c(fl_mean_500k$mean_flame_500k, fl_2024_500k$mean_flame_500k))
# #range(c(fl_mean_500k$mean_act_500k, fl_2024_500k$mean_act_500k))


### reductions ----------------------------------------------------------

#   By Region, what was the relative change in Conditional Flame Length and Active Crown Fire from the 500 to 1m to 2.3m acre treatments?

# ? how deal with years/timing
#   at (initial) treatment year? (timeslice of greatest impact)
#   at or after? (though then bias towards later treated hucs)
#   average of at or after? (also still some bias towards later treated hucs)
#   average of all 4 years? (overall result with changing priorities)
#   * do both (initial) treatment year and average of all 4 years...
# ? how deal with various priorities/treatment types? 
#   * max of reduction across priorities/treatments? the 'best' reduction? (min value since negative)
#   mean? Feels like it would wash out the impacts 
#   Perhaps groupby priority+trt and take highest 'best' aggregate? A naive 'best' scenario? 


## Treatment year 

maxredx_2m <- res_wide %>% 
  #treatment year only
  filter(Year == trt_yr) %>% 
  #group by HUC and get max reduction from 500k to 2m (will do 1m later)
  # max reduction would be MINIMUM value (smallest, since negative)
  group_by(Region, HUC12) %>% 
  slice_min(order_by=expFlame_2m500k, n=1)

maxredx_1m <- res_wide %>% 
  #treatment year only
  filter(Year == trt_yr) %>% 
  #group by HUC and get max reduction from 500k to 1m 
  group_by(Region, HUC12) %>% 
  slice_min(order_by=expFlame_1m500k, n=1)

#by region summary. Reductions. 

#Get 500k and 2m values, percent decrease. 
maxredx_fl_reg <- left_join(maxredx_2m %>% 
                           group_by(Region) %>% 
                           summarize(mean_fl_maxredx_2m = mean(expFlame_2m500k),
                                     mean_fl_500k = mean(expFlame_500k),
                                     mean_fl_2m = mean(expFlame_2m),
                                     mean_fl_maxredx_2m_comp = (mean_fl_2m - mean_fl_500k)) %>% 
                           mutate(maxredx_fl_2m500k_pc = (mean_fl_2m - mean_fl_500k)/mean_fl_500k*100), 
                         maxredx_1m %>% 
                           group_by(Region) %>% 
                           summarize(mean_fl_maxredx_1m = mean(expFlame_1m500k),
                                     mean_fl_500k = mean(expFlame_500k),
                                     mean_fl_1m = mean(expFlame_1m),
                                     mean_fl_maxredx_1m_comp = (mean_fl_1m - mean_fl_500k)) %>% 
                           mutate(maxredx_fl_1m500k_pc = (mean_fl_1m - mean_fl_500k)/mean_fl_500k*100), 
                         by = join_by(Region))
#maxredx_fl_reg %>% View()
#Note: mean_fl_500k is different between these, b/c the treatment zones are
#      different by Priority, so we are looking at a slightly different 
#      spatial distribution of treatment zones (since we are looking at 
#      multiple priority-trt combinations here). 

#Average max reduction of 2m and 1m scenarios compared to 500k scenarios, by region
# percent decrease
maxredx_fl_reg %>% 
  dplyr::select(Region, maxredx_fl_2m500k_pc, maxredx_fl_1m500k_pc)


## Average all 4 years (as opposed to treatment year)

maxredx_2m_4yr <- res_wide %>% 
  #group by and summarize 4 years
  group_by(Region, HUC12, Priority, TxType) %>% 
  summarize(mean4yr_expFlame_2m500k = mean(expFlame_2m500k),
            mean4yr_expFlame_2m = mean(expFlame_2m),
            mean4yr_expFlame_500k = mean(expFlame_500k),
            .groups = "drop") %>% 
  #group by HUC and get max reduction from 500k to 2m (will do 1m later)
  # max reduction would be MINIMUM value (smallest, since negative)
  group_by(Region, HUC12) %>% 
  slice_min(order_by=mean4yr_expFlame_2m500k, n=1)

maxredx_1m_4yr <- res_wide %>% 
  #group by and summarize 4 years
  group_by(Region, HUC12, Priority, TxType) %>% 
  summarize(mean4yr_expFlame_1m500k = mean(expFlame_1m500k),
            mean4yr_expFlame_1m = mean(expFlame_1m),
            mean4yr_expFlame_500k = mean(expFlame_500k),
            .groups = "drop") %>% 
  #group by HUC and get max reduction from 500k to 1m 
  # max reduction would be MINIMUM value (smallest, since negative)
  group_by(Region, HUC12) %>% 
  slice_min(order_by=mean4yr_expFlame_1m500k, n=1)

#by region summary. Reductions. 

#Get 500k and 2m values, percent decrease. 
maxredx_fl_reg_4yr <- left_join(maxredx_2m_4yr %>% 
                              group_by(Region) %>% 
                              summarize(mean_fl_maxredx_2m = mean(mean4yr_expFlame_2m500k),
                                        mean_fl_500k = mean(mean4yr_expFlame_500k),
                                        mean_fl_2m = mean(mean4yr_expFlame_2m),
                                        mean_fl_maxredx_2m_comp = (mean_fl_2m - mean_fl_500k)) %>% 
                              mutate(maxredx_fl_2m500k_pc = (mean_fl_2m - mean_fl_500k)/mean_fl_500k*100), 
                            maxredx_1m_4yr %>% 
                              group_by(Region) %>% 
                              summarize(mean_fl_maxredx_1m = mean(mean4yr_expFlame_1m500k),
                                        mean_fl_500k = mean(mean4yr_expFlame_500k),
                                        mean_fl_1m = mean(mean4yr_expFlame_1m),
                                        mean_fl_maxredx_1m_comp = (mean_fl_1m - mean_fl_500k)) %>% 
                              mutate(maxredx_fl_1m500k_pc = (mean_fl_1m - mean_fl_500k)/mean_fl_500k*100), 
                            by = join_by(Region))
#maxredx_fl_reg %>% View()
#Note: mean_fl_500k is different between these, b/c the treatment zones are
#      different by Priority, so we are looking at a slightly different 
#      spatial distribution of treatment zones (since we are looking at 
#      multiple priority-trt combinations here). 

#Average max reduction of 2m and 1m scenarios compared to 500k scenarios, by region
# percent decrease
maxredx_fl_reg_4yr %>% 
  dplyr::select(Region, maxredx_fl_2m500k_pc, maxredx_fl_1m500k_pc)

redx_sum_tbl <- maxredx_fl_reg_4yr %>% 
  dplyr::select(Region, maxredx_fl_2m500k_pc, maxredx_fl_1m500k_pc) %>% 
  rename(`2m ave`=maxredx_fl_2m500k_pc,
         `1m ave`=maxredx_fl_1m500k_pc) %>% 
  left_join(maxredx_fl_reg %>% 
              dplyr::select(Region, maxredx_fl_2m500k_pc, maxredx_fl_1m500k_pc) %>% 
              rename(`2m at treatment`=maxredx_fl_2m500k_pc,
                     `1m at treatment`=maxredx_fl_1m500k_pc),
            by = join_by(Region)) %>% 
  mutate(across(.cols = where(is.numeric), 
                ~ round(., 1)))

redx_title <- "Region average of max percent reduction in flame length, any scenario"
redx_grob <- tableGrob(redx_sum_tbl, rows=rep("", nrow(redx_sum_tbl)))

ggsave(plot=grid.arrange(top=redx_title, redx_grob),
       filename = file.path(folder_out, 
                            "best_reduction_summary_table_allyrs.jpg"),
       height=1.75, width=5.5, units=c("in"))


## map maxredx 2m
hucs_maxredx_2m <- hucs_spatial %>% 
  left_join(maxredx_2m_4yr %>% 
              mutate(pc_redx = mean4yr_expFlame_2m500k/mean4yr_expFlame_500k*100), 
            by = join_by(HUC12))

map_maxredx_2m <- ggplot() +
  #California
  geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
  #HUCs
  geom_sf(data = hucs_maxredx_2m, 
          #color the HUC
          aes(fill = pc_redx),
          color = "grey90", size = 0.01) +
  #regions
  geom_sf(data = region_spatial,
          color = "grey30", fill = NA, size = 0.1) + 
  #use viridis color scheme
  scale_fill_viridis("Max percent reduction in flame index\n2m to 500k, any scenario", 
                     option="mako", direction=1, 
                     #dealing with NAs and labeling
                     na.value="grey95") + 
  #plot settings for a nicer map
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        strip.background = element_blank(),
        strip.text = element_text(size = 13),
        axis.text = element_text(family = "sans"),
        legend.position = "right",
        legend.direction = "vertical") +
  #add a scale bar
  ggspatial::annotation_scale(
    location = "bl", bar_cols = c("grey50", "white")) +
  #add a north arrow
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    height = unit(0.6, "cm"), width = unit(0.6, "cm"),
    pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
    style = ggspatial::north_arrow_minimal()) +
  #add a plot title
  labs(title = "Greatest impacts: Percent reduction in flame index from 500k to 2m scenarios",
       subtitle = "Max reduction of any priority or treatment, average over all years")


ggsave(plot=map_maxredx_2m,
       filename=file.path(folder_out,
                          "map_max_reduction_flame_2m500k.jpg"),
       height=7, width=8, units=c("in"))
