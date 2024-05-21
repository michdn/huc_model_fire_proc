# Mapping CBH stats & inversions
#  similar to SDI maps in sn_adjective_20240426.R

#NO BASELINE HERE. Baseline issue is still a problem. Readd later if wanted

# actually switched to scatters, not a map

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  viridis,
  sf,
  ggspatial,
  scales,
  gridExtra)


### data import -------------------------------------------

res_orig <- read_csv(file.path("results",
                               "absolute", #"datacube",
                               "SN_SNbl_SNbw_absolute_20240423.csv")) %>%
  mutate(HUC12 = as.character(HUC12)) %>% 
  filter(!Priority == "baseweather") %>% 
  filter(!Priority == "baseline") %>% 
  mutate(Priority = as.factor(Priority),
         Priority = forcats::fct_relevel(Priority,
                                         "Fire", "WUI", "Hybrid"),
         #Make factor with set order (intensity))
         TxIntensity = as.factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                            "500k", "1m", "2m"))

#nonburn issue
nb_hucs <- readRDS("data/nonburnable_rerun_list.RDS") %>% 
  filter(region == "SN")

res <- res_orig %>% 
  mutate(nonburn_coastal = if_else(HUC12 %in% nb_hucs$huc12, TRUE, FALSE)) %>% 
  filter(nonburn_coastal == FALSE)



# CBH stats
cbh <- readRDS(file.path("qa", "SN_scenario_zonal_cbh.RDS")) %>% 
  #to match res
  mutate(Priority = if_else(Priority == "RFFC", "Hybrid", Priority)) %>% 
  mutate(Priority = as.factor(Priority),
         Priority = forcats::fct_relevel(Priority,
                                         "Fire", "WUI", "Hybrid"),
         #Make factor with set order (intensity))
         TxIntensity = as.factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                            "500k", "1m", "2m")) %>% 
  #don't need since have above and would get duplicated in joining and don't want in key
  dplyr::select(-nonburn_coastal)

#res join
res <- res %>% 
  left_join(cbh, by = join_by(HUC12, Priority, TxIntensity, TxType, Year))


#REM that CBH has trt_yr


#spatial
hucs_shp <- st_read("data/data_huc/TxHucsTimingGroups.shp") %>% 
  rename(HUC12 = huc12)
#Simplified for faster drawing 
hucs_simpl <- sf::st_simplify(hucs_shp, preserveTopology = TRUE, dTolerance = 200)


### data prep ---------------------------------------------------------------

#calc HaCFL inversions from results
res500k <- res %>% 
  filter(TxIntensity == "500k") %>% 
  dplyr::select(HUC12, Priority, TxType, Year, HaCFL) %>% 
  rename(hacfl_500k = HaCFL)

res2m <- res %>% 
  #only desired for convenience
  dplyr::select(HUC12, Priority, TxType, TxIntensity, Year, HaCFL) %>% 
  #just 2m intensity 
  filter(TxIntensity == "2m") %>% 
  # compare with 500k version
  left_join(res500k, by = join_by(HUC12, Priority, TxType, Year)) %>% 
  #calc diff/inversion. 500k-2m/500k. 2m SHOULD be smaller, so NEGATIVE when inversion
  mutate(pc500k2m = (hacfl_500k - HaCFL)/hacfl_500k)


#get 2m CBH and join
res_cbh <- res2m %>% 
  left_join(cbh %>% filter(TxIntensity == "2m"), 
            by = join_by(HUC12, Priority, TxType, TxIntensity, Year)) %>% 
  #handy precalcs
  mutate(rel_trt_yr = Year - trt_yr,
         pretx = if_else(rel_trt_yr < 0, TRUE, FALSE),
         attx = if_else(rel_trt_yr == 0, TRUE, FALSE),
         posttx = if_else(rel_trt_yr > 0, TRUE, FALSE),
         post5 = if_else(rel_trt_yr == 5, TRUE, FALSE),
         rel_tx = case_when(
           pretx == TRUE ~ "Pretreatment",
           attx == TRUE ~ "At treatment",
           post5 == TRUE ~ "Five years post",
           .default = NA), #"POST >= 10"),
         rel_tx = as.factor(rel_tx),
         rel_tx = forcats::fct_relevel(rel_tx, 
                                       "Pretreatment", "At treatment", "Five years post"), 
         inversion = if_else(pc500k2m < 0, TRUE, FALSE)) %>% 
  dplyr::select(HUC12, Priority, TxType, TxIntensity, Year, trt_yr, rel_trt_yr, everything())
  

res_cbh


### scatter ----------------------------------

p_mean <- ggplot() + 
  geom_point(data = res_cbh %>% 
               filter(Priority == "Fire",
                      !is.na(rel_tx)),
             mapping=aes(x=cbh_ave, y=pc500k2m,
                         color=TxType,
                         shape=inversion),
             size=1) + 
  geom_hline(yintercept = 0) + 
  labs(title = "SN Fire 500k-2m/500k inversions vs HUC Mean CBH",
       y = "Percent change 500k-2m/500k (frac)",
       x = "HUC Mean of CBH") + 
  scale_color_brewer(palette = "Dark2") + 
  scale_shape_manual(values=c("TRUE"=6, "FALSE"=1)) + 
  facet_wrap(~rel_tx, ncol=1) +
  theme_bw()


ggsave(plot=p_mean,
       filename=file.path("plots", "cbh", "cbh_mean_inversions_facetreltxyr_fire.jpg"),
       height = 6, width = 6, units = "in")



# p_max <- ggplot() + 
#   geom_point(data = res_cbh %>% 
#                filter(Priority == "Fire",
#                       !is.na(rel_tx)),
#              mapping=aes(x=cbh_max, y=pc500k2m,
#                          color=TxType,
#                          shape=inversion)) + 
#   geom_hline(yintercept = 0) + 
#   labs(title = "SN Fire 500k-2m/500k inversions vs HUC Mean CBH",
#        y = "Percent change 500k-2m/500k (frac)",
#        x = "HUC Max of CBH") + 
#   scale_color_brewer(palette = "Dark2") + 
#   scale_shape_manual(values=c("TRUE"=6, "FALSE"=1)) + 
#   facet_wrap(~rel_tx, ncol=1) +
#   theme_bw()
# 
# 
# ggsave(plot=p_max,
#        filename=file.path("plots", "cbh", "cbh_max_inversions_facetreltxyr_fire.jpg"),
#        height = 6, width = 6, units = "in")


# ### ATTEMPT1 -----------------------------------------------------------------------------------------------
# 
# ### prep -----------------------------------------------------
# 
# 
# #CBH stats will be per scenario including intensity. 
# # so separate out by intensity level - show inversion or not (binary) & then stat. 
# 
# # withholding filtering trt_yr until later. 
# 
# cbh_allyrs <- res %>% 
#   #needed columns
#   dplyr::select(HUC12, Priority, trt_yr, TxType, TxIntensity, Year, HaCFL, starts_with("cbh")) %>% 
#   #add rank order
#   arrange(Year, HUC12, Priority, TxType, TxIntensity) %>% 
#   group_by(Year, HUC12, Priority, TxType) %>% 
#   #break ties with first since in order of TxIntensity. default 'average' is a mean which fails to compare well to other ranks
#   mutate(hacfl_rank = rank(HaCFL, ties="first"),
#          cbh_mean_rank = rank(cbh_ave, ties="first")) %>% 
#   #create intensity rank (ascending order, 2m should be smallest value)
#   mutate(intensity_rank = case_when(
#     #TxIntensity == "base" ~ 4,
#     TxIntensity == "500k" ~ 3,
#     TxIntensity == "1m" ~ 2,
#     TxIntensity == "2m" ~ 1)) %>% 
#   #flag if ranks mismatches
#   mutate(i_h_flag = if_else(!hacfl_rank == intensity_rank, TRUE, FALSE),
#          #not certain if needed or useful, but just to see
#          i_cmean_flag = if_else(!cbh_mean_rank == intensity_rank, TRUE, FALSE))
# 
# 
# 
# ### CBH inversion maps ------------------------------------------------------
# 
# 
# cbh_sum_attx <- cbh_allyrs %>%
#   #filter year of treatment
#   filter(Year == trt_yr) %>% 
#   #summarize
#   group_by(HUC12, Priority, TxType) %>% 
#   summarize(count_icmean = sum(i_cmean_flag), .groups = "drop") %>% 
#   #create color categories
#   mutate(icmean_cat = case_when(
#     count_icmean == 0 ~ "Zero",
#     count_icmean <= 2  ~ "One/Two",
#     count_icmean > 2 ~ "Multiple"
#   ))
# 
# 
# cbh_sum_post5 <- cbh_allyrs %>%
#   #filter year post treatment (5 yrs post)
#   filter(Year == trt_yr + 5) %>% 
#   #summarize
#   group_by(HUC12, Priority, TxType) %>% 
#   summarize(count_icmean = sum(i_cmean_flag), .groups = "drop") %>% 
#   #create color categories
#   mutate(icmean_cat = case_when(
#     count_icmean == 0 ~ "Zero",
#     count_icmean <= 2  ~ "One/Two",
#     count_icmean > 2 ~ "Multiple"
#   ))
# 
# 
# stoplight_colors <- c("Zero" = "lightseagreen",
#                       "One/Two" = "gold",
#                       "Multiple" = "red3")
# 
# 
# ## loop on priority 
# 
# priorities <- cbh_sum_attx %>% pull(Priority) %>% unique()
# 
# for (p in seq_along(priorities)){
#   
#   this_priority <- priorities[[p]]
#   
#   #--
#   #at year of treatment
#   this_cbh_sum_attx <- cbh_sum_attx %>% 
#     filter(Priority == this_priority)
#   
#   #join with spatial data
#   this_shp_attx <- hucs_simpl %>% 
#     filter(region == "SN") %>% 
#     #removing the nonburn HUCs
#     inner_join(this_cbh_sum_attx, by = join_by(HUC12))
#   
#   #map
#   p_this_icmean_attx <- ggplot() +
#     #HUCs
#     geom_sf(data = this_shp_attx, 
#             aes(fill = icmean_cat),
#             color = "grey90", size = 0.01) +
#     scale_fill_manual("CBH mean inversions", 
#                       values = stoplight_colors, 
#                       na.value = "grey80") + 
#     theme_bw() +
#     theme(panel.grid = element_blank(),
#           panel.background = element_blank(),
#           legend.title = element_text(size = 11),
#           legend.text = element_text(size = 10),
#           strip.background = element_blank(),
#           strip.text = element_text(size = 13),
#           axis.text = element_text(family = 'sans'),
#           legend.position = "right",
#           legend.direction = "vertical") +
#     ggspatial::annotation_scale(
#       location = "bl", bar_cols = c("grey50", "white")) +
#     ggspatial::annotation_north_arrow(
#       location = "tr", which_north = "true",
#       height = unit(0.6, "cm"), width = unit(0.6, "cm"),
#       pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
#       style = ggspatial::north_arrow_minimal()) +
#     labs(title = paste0("CBH mean intensity inversions: ", 
#                         this_priority),
#          subtitle = "At treatment year for that HUC12") + 
#     facet_wrap(~TxType)
#   
#   
#   ggsave(plot = p_this_icmean_attx,
#          filename = file.path("plots", "maps", "cbh",
#                               paste0("SN_cbh_inversions_attx_", 
#                                      this_priority, ".jpg")),
#          height = 5, width = 8, units = c("in"))
#   
#   # -- 
#   
#   #At 5 years POST treatment
#   this_cbh_sum_post5 <- cbh_sum_post5 %>% 
#     filter(Priority == this_priority)
#   
#   #join with spatial data
#   this_shp_post5 <- hucs_simpl %>% 
#     filter(region == "SN") %>% 
#     #removing the nonburn HUCs
#     inner_join(this_cbh_sum_post5, by = join_by(HUC12))
#   
#   #map
#   p_this_icmean_post5 <- ggplot() +
#     #HUCs
#     geom_sf(data = this_shp_post5, 
#             aes(fill = icmean_cat),
#             color = "grey90", size = 0.01) +
#     scale_fill_manual("CBH mean inversions", 
#                       values = stoplight_colors, 
#                       na.value = "grey80") + 
#     theme_bw() +
#     theme(panel.grid = element_blank(),
#           panel.background = element_blank(),
#           legend.title = element_text(size = 11),
#           legend.text = element_text(size = 10),
#           strip.background = element_blank(),
#           strip.text = element_text(size = 13),
#           axis.text = element_text(family = 'sans'),
#           legend.position = "right",
#           legend.direction = "vertical") +
#     ggspatial::annotation_scale(
#       location = "bl", bar_cols = c("grey50", "white")) +
#     ggspatial::annotation_north_arrow(
#       location = "tr", which_north = "true",
#       height = unit(0.6, "cm"), width = unit(0.6, "cm"),
#       pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
#       style = ggspatial::north_arrow_minimal()) +
#     labs(title = paste0("CBH mean intensity inversions: ", 
#                         this_priority),
#          subtitle = "5 years post treatment year for that HUC12") + 
#     facet_wrap(~TxType)
#   
#   
#   ggsave(plot = p_this_icmean_post5,
#          filename = file.path("plots", "maps", "cbh",
#                               paste0("SN_cbh_inversions_post5_", 
#                                      this_priority, ".jpg")),
#          height = 5, width = 8, units = c("in"))
#   
#   
# }
# 
# ### CBH with HaCFL inversions --------------------------------------------
# 
# # per full scenario, display CBH stat
# # facet on intensity and stat???
# # max, mean
# 
# # year of treatment, and 5 years post treatment
# 
# # Need long form with cbh stat for facetting
# cbh_allyrs_long <- cbh_allyrs %>% 
#   pivot_longer(cols=starts_with("cbh"), 
#                names_to = "cbh_stat", 
#                values_to = "cbh_value") %>% 
#   filter(cbh_stat %in% c("cbh_ave", "cbh_max")) 
# 
# 
# #need consistent ranges of mean and max together, and all years tx or later
# # could be picky and redo with attx and just post5, but hopefully this will be close enough
# cbh_limits <- range(cbh_allyrs_long %>% 
#                       filter(Year >= trt_yr) %>% 
#                       pull(cbh_value))
# 
# 
# priorities <- cbh_allyrs %>% pull(Priority) %>% unique() %>% sort()
# trts <- cbh_allyrs %>% pull(TxType) %>% unique() %>% sort()
# 
# for (p in seq_along(priorities)){
#   
#   this_priority <- priorities[[p]]
#   
#   #TREATMENT TYPE
#   for (t in seq_along(trts)){
#     
#     this_trt <- trts[[t]]
#     
#     #--
#     # AT TX
#     
#     this_cbh_attx <- cbh_allyrs_long %>%
#       filter(Year == trt_yr) %>% 
#       #this priority and trt
#       filter(Priority == this_priority,
#              TxType == this_trt)
#     
#     #join with spatial data
#     # NOT simpl since using color=NA, and slivers
#     this_shp_attx <- hucs_shp %>% 
#       filter(region == "SN") %>% 
#       #removing the nonburn HUCs
#       inner_join(this_cbh_attx, by = join_by(HUC12))
#     
#     
#     #map
#     # WILL TAKE A WHILE b/c not using simpl and lots of facets
#     p_cbh_attx <- ggplot() +
#       #HUCs
#       geom_sf(data = this_shp_attx, 
#               aes(fill = cbh_value),
#               color=NA) +
#       scale_fill_viridis(limits = cbh_limits) + #NOPE, need separate limits by cbh_stat somehow.... 
#       geom_sf(data = this_shp_attx %>%  
#                 filter(i_h_flag == TRUE),
#               aes(color = i_h_flag),
#               fill=NA,
#               size=0.01) + 
#       scale_color_manual("HaCFL Inversion", 
#                          values = "red") + 
#       theme_bw() +
#       theme(panel.grid = element_blank(),
#             panel.background = element_blank(),
#             legend.title = element_text(size = 11),
#             legend.text = element_text(size = 10),
#             strip.background = element_blank(),
#             strip.text = element_text(size = 13),
#             axis.text = element_text(family = 'sans'),
#             legend.position = "right",
#             legend.direction = "vertical") +
#       ggspatial::annotation_scale(
#         location = "bl", bar_cols = c("grey50", "white")) +
#       ggspatial::annotation_north_arrow(
#         location = "tr", which_north = "true",
#         height = unit(0.6, "cm"), width = unit(0.6, "cm"),
#         pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
#         style = ggspatial::north_arrow_minimal()) +
#       labs(title = paste0("CBH stats with HaCFL inversions flagged by Intensity: ", 
#                           this_priority, " ", 
#                           this_trt),
#            subtitle = "At treatment year for that HUC12") + 
#       facet_wrap(~cbh_stat+TxIntensity, ncol=3, nrow=2)
#     
#     ggsave(plot = p_cbh_attx,
#            filename = file.path("plots", "maps", "cbh", 
#                                 paste0("SN_cbh_meanmax_hacfl_intensity_inversions_", 
#                                        this_priority, "_",
#                                        this_trt, 
#                                        "_ATTX.jpg")),
#            height = 9, width = 8, units = c("in"))
#     
#     # --
#     # POST 5 TX
#     this_cbh_post5 <- cbh_allyrs_long %>%
#       filter(Year == (trt_yr+5)) %>% 
#       #this priority and trt
#       filter(Priority == this_priority,
#              TxType == this_trt)
#     
#     #join with spatial data
#     # NOT simpl since using color=NA, and slivers
#     this_shp_post5 <- hucs_shp %>% 
#       filter(region == "SN") %>% 
#       #removing the nonburn HUCs
#       inner_join(this_cbh_post5, by = join_by(HUC12))
#     
#     
#     #map
#     # WILL TAKE A WHILE b/c not using simpl and lots of facets
#     p_cbh_post5 <- ggplot() +
#       #HUCs
#       geom_sf(data = this_shp_post5, 
#               aes(fill = cbh_value),
#               color=NA) +
#       scale_fill_viridis(limits = cbh_limits) + 
#       geom_sf(data = this_shp_post5 %>%  # CHECK IF STILL WORKS WITH LONG TODO<<>>
#                 filter(i_h_flag == TRUE),
#               aes(color = i_h_flag),
#               fill=NA,
#               size=0.01) + 
#       scale_color_manual("HaCFL Inversion", 
#                          values = "red") + 
#       theme_bw() +
#       theme(panel.grid = element_blank(),
#             panel.background = element_blank(),
#             legend.title = element_text(size = 11),
#             legend.text = element_text(size = 10),
#             strip.background = element_blank(),
#             strip.text = element_text(size = 13),
#             axis.text = element_text(family = 'sans'),
#             legend.position = "right",
#             legend.direction = "vertical") +
#       ggspatial::annotation_scale(
#         location = "bl", bar_cols = c("grey50", "white")) +
#       ggspatial::annotation_north_arrow(
#         location = "tr", which_north = "true",
#         height = unit(0.6, "cm"), width = unit(0.6, "cm"),
#         pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
#         style = ggspatial::north_arrow_minimal()) +
#       labs(title = paste0("CBH mean and max values with HaCFL inversions flagged by Intensity: ", 
#                           this_priority, " ", 
#                           this_trt),
#            subtitle = "5 years post treatment for that HUC12") + 
#       facet_wrap(~cbh_stat+TxIntensity, ncol=3, nrow=2)
#     
#     ggsave(plot = p_cbh_post5,
#            filename = file.path("plots", "maps", "cbh", 
#                                 paste0("SN_cbh_meanmax_hacfl_intensity_inversions_", 
#                                        this_priority, "_",
#                                        this_trt, 
#                                        "_POST5.jpg")),
#            height = 9, width = 8, units = c("in"))
#     
#   }#end trt
#   
# }#end priority
