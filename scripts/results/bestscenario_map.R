# Script for potential "best" scenario maps

# - By region.
# - Use absolute metrics: expFlame, expBurn, expPcActive
# - Within that HUC-reltxyr, for all scenarios, rank each separately. (See years/timing below) (Ties can be average)
# - Add ranks within HUC-reltxyr for a final HUC-reltxyr rank. (Each weighted equally.)
# - Scenario with smallest summed rank is the 'best' in that HUC-reltxyr. (Ties by ... expFlame?)
# 
# Years/timing:
#   - Treatment effects fade over time, so just looking at 2039 will bias towards treated later HUCs.
# - Combining all four years is odd because HUCs are treated at various time points and we would be including 'pretreatment' years (where there would be a ranking order but meaningless).
# - Going BY YEAR is incorrect across Priorities, as they are treated in different years. 
# - By RELATIVE TO TREATMENT YEAR. Likely comparing DIFFERENT ABSOLUTE years within a HUC.
# - 1) At year of treatment - would have values for all treated HUCs
# - 2) 5 years post treatment - include HUCs treated in 2024, 2029, or 2034 in ALL Priorities, otherwise mismatched number of rankings to compare. 
# 
# Effects map: 
#   - Show difference in combined ranking (HUC-reltxyr).
# -  VS ranking of 500k scenario (might be itself if 500k scenario best).
# 
# Arguments could be made for separating out Priorities (i.e. treatment year issues) instead.... 


# WORK IN PROGRESS
# - ONLY does at year of treatment
# - reconsider how to handle partially missing scenarios for HUC-reltrtyr?
# - maybe normalized rank index from 3-81 to 1-100? 


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  ggspatial,
  viridis,
  gridExtra)

### Base Data import -------------------------------------------

# res_orig <- read_csv(file.path("results",
#                                "202403_runs",
#                                "datacube", 
#                                "datacube_interim_sc_cc_sn_bl_bw_20240513.csv")) %>% 
#   mutate(HUC12 = as.character(HUC12)) %>% 
#   filter(!Priority %in% c("baseline", "baseweather")) 

res_orig <- read_csv(file.path("results",
                               "datacube",
                               "datacube_interim_SNSCCC_20240617.csv")) %>%
  mutate(HUC12 = as.character(HUC12))


#spatial
hucs_spatial <- st_read("data/data_huc/TxHucsTimingGroups.shp") %>% 
  rename(HUC12 = huc12)
#Simplified for faster drawing, testing only
#hucs_simpl <- sf::st_simplify(hucs_spatial, preserveTopology = TRUE, dTolerance = 200)

#If CA state boundary wanted, download from 
# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
#states <- st_read("data/tl_2023_us_state/tl_2023_us_state.shp")
#ca <- states %>% filter(STUSPS == "CA")


folder_out <- file.path("plots", "maps_best")
dir.create(folder_out)

### Data processing and ranking -----------------------------------

# Create scenario labels and order
res <- res_orig %>% 
  #create nice label for trt
  mutate(trt_desc = case_when(
    TxType == "trt1" ~ "Mod thin removal",
    TxType == "trt4" ~ "Heavy thinning",
    TxType == "trt6" ~ "Mastication", 
    TxType == "trt7" ~ "Prescribed fire",
    .default = "TRT DESC ERROR"
  )) %>% 
  #create combined scenario label, FACTOR
  # arrange as desired factor order to avoid making 27 factor list
  #  (default is to make the order as it appears)
  mutate(Priority = as_factor(Priority),
         Priority = forcats::fct_relevel(Priority,
                                         "Fire", "WUI", "Hybrid"),
         #Make factor with set order (intensity))
         TxIntensity = as_factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                            "500k", "1m", "2m")) %>% 
  arrange(TxIntensity, TxType, Priority) %>% 
  mutate(scenario = paste0(TxIntensity, "_", trt_desc, "_", Priority),
         scenario = as_factor(scenario))


# Define relative to treatment year
res <- res %>% 
  #treat years
  separate_wider_delim(timeFire, "_", names=c("fire_trt_yr", NA)) %>% 
  separate_wider_delim(timeHybrid, "_", names=c("hybrid_trt_yr", NA)) %>% 
  mutate(fire_trt_yr = as.numeric(fire_trt_yr),
         hybrid_trt_yr = as.numeric(hybrid_trt_yr),
         wui_trt_yr = case_when(
           timeWui == "2024_2039_yr1to5_16to20" ~ 2024,
           timeWui == "2029_yr6to10" ~ 2029,
           timeWui == "2034_yr11to15" ~ 2034,
           timeWui == "Not treated" ~ 9999)) %>% 
  #treat year for priority
  mutate(trt_yr = case_when(
    Priority == "Fire" ~ fire_trt_yr,
    Priority == "Hybrid" ~ hybrid_trt_yr,
    Priority == "WUI" ~ wui_trt_yr)) %>% 
  #relative to treatment year 
  mutate(rel_trt_yr = Year - trt_yr,
         #making factors and more levels than planned, just in case
         rel_tx = case_when(
           rel_trt_yr < 0 ~ "Pretreatment",
           rel_trt_yr == 0 ~ "At treatment",
           rel_trt_yr == 5 ~ "Five years post",
           rel_trt_yr == 10 ~ "Ten years post",
           .default = NA), 
         rel_tx = as.factor(rel_tx),
         rel_tx = forcats::fct_relevel(rel_tx, 
                                       "Pretreatment", "At treatment", 
                                       "Five years post", "Ten years post"))


# Rankings across scenarios within a HUC by rel_trt_yr
# (Doing all years, will filter later)
res_r_raw <- res %>% 
  dplyr::select(Region, HUC12, Priority, TxIntensity, TxType, 
                scenario, Year, rel_trt_yr, rel_tx, 
                expFlame, expBurn, expPcActive) %>% 
  group_by(HUC12, rel_trt_yr) %>% 
  mutate(rank_flame = rank(expFlame, ties.method = "average"),
         rank_burn = rank(expBurn, ties.method = "average"),
         rank_active = rank(expPcActive, ties.method = "average"),
         rank_sum = rank_flame + rank_burn + rank_active) %>% 
  ungroup()
#arrange(HUC12, rel_trt_yr, rank_flame) %>% View()

# Create list of 'good' HUC12-rel_trt_yr combinations to keep
# Should have 27 rows (9 for each Priority)
# Should be at or post treatment
# (Filtering for 0, 5, 10, 15 yrs post tx will be later)
res_r_counts <- res_r_raw %>% 
  group_by(Region, HUC12, rel_trt_yr) %>% 
  summarize(count=n(), .groups = "drop") %>% 
  filter(count == 27,
         rel_trt_yr >= 0)

res_r_counts %>% group_by(Region, rel_trt_yr) %>% summarize(count_hucs = n())
# SC
# rel_trt_yr count_hucs
#           0        360
#           5        255
#          10        113
#          15         19

#Select full (27 counts post tx) combinations using join
res_r <- res_r_counts %>% 
  left_join(res_r_raw, by = join_by(Region, HUC12, rel_trt_yr)) %>% 
  dplyr::select(-count)


### Selecting top results for region -------------------------------------

# WITH NEW DATA CHECK!! THE TIE BREAKS ARE FROM MARCH SC RUNS





res_best <- res_r %>% 
  #selecting top (lowest) summed rank by rel_trt_yr, 
  # ties broken by expFlame, expPcActive, expBurn, [still 31 HUC-reltrtyrs with ties in old SC. 107 in new SC,CC,SN]
  # then by TxIntensity (500k>1m>2m), [still 30 old SC. still 107 new SC,CC,SN]
  # then by TxType (1, 4, 6/7), [still 5 old SC. 54 new SC,CC,SN]
  # then by Priority (Fire, WUI, Hybrid)
  group_by(Region, HUC12, rel_trt_yr) %>% 
  slice_min(order_by = tibble(rank_sum, 
                              expFlame, expPcActive, expBurn,
                              TxIntensity, TxType, Priority), 
            n = 1)

# res_best %>%
#   group_by(HUC12, rel_trt_yr) %>%
#   summarize(count_rows=n()) %>%
#   filter(!count_rows == 1) 


### Loop by region ----------------------------------------------

regions <- res %>% pull(Region) %>% unique() %>% sort()

#REGION (outer loop)
for (r in seq_along(regions)){
  
  this_reg <- regions[[r]]
  
  this_reg_label <- case_when(
    this_reg == "CC" ~ "Central Coast",
    this_reg == "NC" ~ "North Coast",
    this_reg == "SC" ~ "South Coast",
    this_reg == "SN" ~ "Sierra Nevada",
  )
  
  res_best_reg <- res_best %>% 
    filter(Region == this_reg)
  
  ### Break out by rel_trt_yr and map -----------------------------
  
  res_attx <- res_best_reg %>% 
    filter(rel_trt_yr == 0)
  
  hucs_attx <- hucs_spatial %>% #hucs_simpl %>%  
    filter(region == this_reg) %>% 
    left_join(res_attx,
               by = join_by("HUC12"))
  
  
  plot_attx <- ggplot() +
    #California
    #geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
    #HUCs
    geom_sf(data = hucs_attx, 
            #color the HUC by scenario
            aes(fill = scenario),
            color = "grey90", size = 0.01) +
    #use viridis color scheme
    scale_fill_viridis("Scenario", discrete=TRUE, option="magma", direction=1, 
                       #dealing with NAs and labeling
                       na.value="grey95",
                       labels=function(breaks){
                         breaks[is.na(breaks)] <- "Not treated in all scenarios"; breaks}) + 
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
    labs(title = paste0(this_reg_label, ": Best scenario at year of treatment"),
         subtitle = "Using flame index, expected burned acres, and active crown fire",
         caption = "Only includes HUCs treated in across all scenarios.")
  
  #plot_attx
  
  
  ### Effect maps ----------------------------------------------------------------
  
  res_attx_efx <- res_attx %>% 
    left_join(res_r %>% 
                filter(Region == this_reg,
                       TxIntensity == "500k") %>% 
                dplyr::select(HUC12, rel_trt_yr, Priority, TxType, rank_sum) %>% 
                rename(rank_sum_500k = rank_sum),
              by = join_by(HUC12, rel_trt_yr, Priority, TxType)) %>% 
    # NEGATIVE is IMPROVEMENT over 500k. 
    mutate(rank_diff = rank_sum - rank_sum_500k)
  
  
  hucs_attx_efx <- hucs_spatial %>% #hucs_simpl %>% 
    filter(region == this_reg) %>% 
    left_join(res_attx_efx,
              by = join_by("HUC12"))
  
  
  plot_attx_efx <- ggplot() +
    #California
    #geom_sf(data = ca, color = "grey50", fill = NA, size = 0.1) + 
    #HUCs
    geom_sf(data = hucs_attx_efx, 
            #color the HUC by scenario
            aes(fill = rank_diff),
            color = "grey90", size = 0.01) +
    #use viridis color scheme
    scale_fill_viridis("Rank index difference\nNegative is improvement", 
                       option="mako",
                       #dealing with NAs 
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
    labs(title = paste0(this_reg_label, 
                        ": Rank index comparison of best scenario to 500k scenario at year of treatment"),
      subtitle = "Rank index combines flame index, expected burned acres, and active crown fire",
      caption = "Only includes HUCs treated in across all scenarios.\nRank index values range from 3 to 81.")
  
  #plot_attx_efx
  
  
  ### Combine maps ------------------------------------------------------
  
  # In order to align maps, we will separate maps from legends
  #https://stackoverflow.com/questions/16963137/ggplot2-multiple-plots-with-different-variables-in-a-single-row-single-groupin
  get_legend <- function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  plot_attx_leg <- get_legend(plot_attx)
  plot_attx_efx_leg <- get_legend(plot_attx_efx)
  
  plot_attx_noleg <- plot_attx + 
    theme(legend.position="none")
  plot_attx_efx_noleg <- plot_attx_efx + 
    theme(legend.position = "none")
  
  # grob_widths = case_when(
  #   this_reg == "CC" ~ c(2/3, 1/3),
  #   this_reg == "NC" ~ c(2/3, 1/3),
  #   this_reg == "SC" ~ c(1/2, 1/2),
  #   this_reg == "SN" ~ c(1/2, 1/2),
  # )
  
  
  attx <- arrangeGrob(plot_attx_noleg, plot_attx_leg,
                      plot_attx_efx_noleg, plot_attx_efx_leg,
                      ncol=2, 
                      widths=c(1/2, 1/2)) 
  
  #note highly dependent on shape of view pane
  #plot(attx)
  
  plot_height = case_when(
    this_reg == "CC" ~ 8.5,
    this_reg == "NC" ~ 9,
    this_reg == "SC" ~ 8.5,
    this_reg == "SN" ~ 12,
  )
  plot_width = case_when(
    this_reg == "CC" ~ 9.5,
    this_reg == "NC" ~ 10,
    this_reg == "SC" ~ 11,
    this_reg == "SN" ~ 9,
  )
  
  
  ggsave(plot=attx,
         filename=file.path(folder_out,
                            paste0(this_reg, "_attx_best.jpg")),
         height = plot_height,
         width = plot_width, 
         units = c("in"))
  
  
}

