
# Fuel adjectives


# 2. Fuel adjective changes at year of treatment
#  a. example HUCs
#   filter Year = year of treatment, HUC12 in sampled HUCs. 
#   histo bar of pos and neg per category
#   maybe just Fire priority? 
#   facet by trt?
#   color by intensity? can I get groups going correctly for that? 

# b. map
#    filter Year = year of treatment. 
# Rank order fuel adj huc mean (per HUC per scenario) by intensity
# Rank order HaCFL (per HuC per scenario) by intensity
# Count mismatches. 0 = perfect alignment green, 1 = yellow, 2+ = red. 
# One map for each Priority facet for trt

# c. map 
# all 4 years. ???


# 3. Identify HUCs with incr adjectives 
#  degree of shift. 

# Compare those against intensity diff (inversion). Or straight against HaCFL? 
# 

## 2024-05-03: Add SDI map


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
  #For graphing in the correct order (generic, used in multiple places, with modifications)
  # make factor with set order (priority)
  filter(!Priority == "baseweather") %>% 
  mutate(Priority = as.factor(Priority),
         Priority = forcats::fct_relevel(Priority,
                                         "Fire", "WUI", "Hybrid", "baseline"),
         #Make factor with set order (intensity))
         TxIntensity = as.factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                            "baseline", "500k", "1m", "2m"),
         #recode so it fits on graphs
         TxIntensity = forcats::fct_recode(TxIntensity, "base" = "baseline"))

nb_hucs <- readRDS("data/nonburnable_rerun_list.RDS") %>% 
  filter(region == "SN")

res <- res_orig %>% 
  mutate(nonburn_coastal = if_else(HUC12 %in% nb_hucs$huc12, TRUE, FALSE)) %>% 
  filter(nonburn_coastal == FALSE)

#add treat year in later with crosswalk
huc_trt_yr <- res %>% 
  filter(!Priority == "baseline") %>% 
  filter(!Priority == "baseweather") %>% 
  dplyr::select(HUC12, Priority, timeFire, timeHybrid, timeWui) %>% 
  distinct() %>% 
  #treatment years
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
  #final columns
  dplyr::select(HUC12, Priority, trt_yr)


#Fuel adjectives
fueladj <- readRDS(file.path("qa", "SN_fuel_adjective_comparison_baseline.RDS")) %>% 
  #for now, filter out nonburn issue hucs
  filter(nonburn_coastal == FALSE) %>% 
  #For graphing in the correct order (generic, used in multiple places, with modifications)
  # make factor with set order (priority)
  mutate(Priority = as.factor(Priority),
         Priority = forcats::fct_relevel(Priority,
                                         "Fire", "WUI", "Hybrid", "baseline", "baseweather"),
         #Make factor with set order (intensity))
         TxIntensity = as.factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                            "baseweather", "baseline", "500k", "1m", "2m"),
         #recode so it fits on graphs
         TxIntensity = forcats::fct_recode(TxIntensity, "bw" = "baseweather"),
         TxIntensity = forcats::fct_recode(TxIntensity, "base" = "baseline"))


#original sampled hucs, 2 from each timing group for each priority for each region
hucs_sample <- read_csv(file.path('qa', 'samplehucs.csv'))  %>% 
  filter(Region == "SN")

#new set, 3 from each of the top three timing groups for each priority for each region
# uses the qualifying HUCs from original sample plus new ones
hucs_sample_new <- read_csv(file.path('qa', 'sampledhucs_topthreegroups.csv')) %>% 
  filter(Region == "SN")


#colors for intensities
i_colors <- c("bw" = "grey50",
              "base" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[1],
              "500k" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[2],
              "1m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[3],
              "2m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[4])

year_breaks <- c(2024, 2029, 2034, 2039)


#spatial
hucs_shp <- st_read("data/data_huc/TxHucsTimingGroups.shp") %>% 
  rename(HUC12 = huc12)
#Simplified for faster drawing 
hucs_simpl <- sf::st_simplify(hucs_shp, preserveTopology = TRUE, dTolerance = 200)

# #If CA state boundary wanted, download from 
# # https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
# # https://www2.census.gov/geo/tiger/TIGER2023/STATE/tl_2023_us_state.zip
# states <- st_read("data/tl_2023_us_state/tl_2023_us_state.shp")
# ca <- states %>% filter(STUSPS == "CA")


### 2. Fuel adjective changes at year of treatment ---------------------------
#  a. example HUCs
#   filter Year = year of treatment, HUC12 in sampled HUCs. 
#   histo bar of pos and neg per category
#   maybe just Fire priority? 
#   facet by trt?
#   color by intensity? can I get groups going correctly for that? 

fuel_long <- fueladj %>% 
  #year of treatment
  filter(Year == trt_yr) %>% 
  #convert to percentage of total area (pixels)
  mutate(across(c(VL_diff, L_diff, M_diff, H_diff, VH_diff, X_diff), ~ ./tot_pixels)) %>% 
  #select wanted
  dplyr::select(HUC12, Priority, TxType, TxIntensity, Year,
                VL_diff, L_diff, M_diff, H_diff, VH_diff, X_diff) %>% 
  #pivot long
  pivot_longer(cols= c(VL_diff, L_diff, M_diff, H_diff, VH_diff, X_diff),
               names_to = "adjective",
               values_to = "area_fraction") %>% 
  #factor adjective so correct order
  separate_wider_delim(col = adjective, names=c("adjective", NA), delim="_") %>% 
  mutate(adjective = as.factor(adjective),
         adjective = forcats::fct_relevel(adjective,
                                          "VL", "L", "M", "H", "VH", "X")) 


#testing with Fire trt1
#this_priority <- "Fire"
#this_trt <- "trt1"

plot_folder <- file.path('plots', 'bar_fueladj_trtyr', "SN")
#sampled_folder <- file.path(plot_folder, 'sampledhucs')
#sampled_folder_v2 <- file.path(plot_folder, 'sampledhucs_v2')

#dir.create(sampled_folder, recursive = TRUE)
#dir.create(sampled_folder_v2, recursive = TRUE)
dir.create(plot_folder, recursive=TRUE)

#priorities in this region (all same)
priorities <- fuel_long %>% pull(Priority) %>% unique()
trts <- fuel_long %>% pull(TxType) %>% unique()


#PRIORITY (first inner loop)
for (p in seq_along(priorities)){
  
  this_priority <- priorities[[p]]
  
  
  for (t in seq_along(trts)){
    
    this_trt <- trts[[t]]
    
    this_adj <- fuel_long %>% 
      filter(Priority == this_priority,
             TxType == this_trt) %>% 
      #sampled
      filter(HUC12 %in% (hucs_sample_new %>% 
                           #filter(priority == tolower(this_priority)) %>% 
                           filter(grouping == this_priority) %>% 
                           pull(HUC12)))
    
    
    p1 <- ggplot() + 
      geom_col(data = this_adj,
               mapping = aes(x = adjective, y = area_fraction, 
                             fill = TxIntensity),
               position = position_dodge()) + 
      geom_hline(yintercept = 0, color = "black") + 
      facet_wrap(~paste0(Year, " - ", HUC12), ncol=3) + 
      theme_bw() + 
      scale_fill_manual("Intensity", values = i_colors) + 
      labs(title = paste(this_priority, this_trt) ,
           subtitle = "Change in fuel adjectives at treatment year compared to baseline")
    
    fn1 <- paste0('sampled_SN_', this_priority, '_', this_trt, '_',
                  'fueladj_trtyr.jpg')
    ggsave(plot = p1,
           filename = file.path(plot_folder, fn1),
           width = 7, height = 6, units = 'in')
    
    
  } #end trt
} # end priority 


### 2b. Map rank order ------------------------------------------------------------

# b. map
#    filter Year = year of treatment. 
# Rank order fuel adj huc mean (per HUC per scenario per year) by intensity
# Rank order HaCFL (per HUC per scenario per year) by intensity
# Count mismatches. 0 = perfect alignment green, some = yellow, a lot = red. 
# One map for each Priority-trt (9 maps) 
#  (for now year of treatment. may want to look at other years later) 

#b1. Where are INVERSIONS happening? (Just GF, inversion)
#b2. where are fueladj inversions happening? 
#b3. where are fueladj - hacfl rank mismatches happening? 

#b1
#known related to forests, so expect that spatial pattern

#baseline/baseweather to be treated as an 'intensity' level PER ALL priorities, trts
res_bases <- res %>% 
  #only baseline, removing baseweather here
  filter(TxIntensity %in% c("base")) %>% 
  #remove priorities, trts from bases
  dplyr::select(-Priority, -TxType)

#remove bases from rest of results (temporarily)
ress <- res %>% 
  filter(!TxIntensity %in% c("base", "bw"))

#get all combination of Priority and TxTypes
frame <- ress %>% 
  dplyr::select(Priority, TxType) %>% 
  distinct()

#duplicate bases values for all priority-txtype combos
res_bases <- frame %>% 
  cross_join(res_bases)

#add back in
resb <- bind_rows(ress, res_bases) %>% 
  #add in trt yr for all
  left_join(huc_trt_yr, by = join_by("HUC12", "Priority")) %>% 
  #needed columns
  dplyr::select(HUC12, Priority, trt_yr, TxType, TxIntensity, Year, HaCFL) %>% 
  #year of treatment (note NA in baseline)
  filter(Year == trt_yr) %>% 
  #add rank order
  arrange(HUC12, Priority, TxType, TxIntensity) %>% 
  group_by(HUC12, Priority, TxType) %>% 
  mutate(hacfl_rank = rank(HaCFL)) %>% 
  #create intensity rank (ascending order, 2m should be smallest value)
  mutate(intensity_rank = case_when(
    TxIntensity == "base" ~ 4,
    TxIntensity == "500k" ~ 3,
    TxIntensity == "1m" ~ 2,
    TxIntensity == "2m" ~ 1)) %>% 
  #flag if ranks mismatches (inversion of some sort)
  mutate(i_h_flag = if_else(!hacfl_rank == intensity_rank, TRUE, FALSE))

resb_sum <- resb %>% 
  group_by(HUC12, Priority, TxType) %>% 
  summarize(count_ih = sum(i_h_flag), .groups = "drop") %>% 
  #create color categories
  mutate(ih_cat = case_when(
    count_ih == 0 ~ "Zero",
    count_ih <= 2  ~ "One/Two",
    count_ih > 2 ~ "Multiple"
  ))

stoplight_colors <- c("Zero" = "lightseagreen",
                      "One/Two" = "gold",
                      "Multiple" = "red3")


## loop on priority 

#this_priority <- "Fire"
priorities <- resb_sum %>% pull(Priority) %>% unique()

for (p in seq_along(priorities)){
  
  this_priority <- priorities[[p]]
  
  this_resb_sum <- resb_sum %>% 
    filter(Priority == this_priority)

  #join with spatial data
  this_shp <- hucs_simpl %>% 
    filter(region == "SN") %>% 
    #removing the nonburn HUCs
    inner_join(this_resb_sum, by = join_by(HUC12))
  
  #map
  p_this_ih <- ggplot() +
    #HUCs
    geom_sf(data = this_shp, 
            aes(fill = ih_cat),
            color = "grey90", size = 0.01) +
    scale_fill_manual("HaCFL inversions", 
                      values = stoplight_colors, 
                      na.value = "grey80") + 
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 10),
          strip.background = element_blank(),
          strip.text = element_text(size = 13),
          axis.text = element_text(family = 'sans'),
          legend.position = "right",
          legend.direction = "vertical") +
    ggspatial::annotation_scale(
      location = "bl", bar_cols = c("grey50", "white")) +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      height = unit(0.6, "cm"), width = unit(0.6, "cm"),
      pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
      style = ggspatial::north_arrow_minimal()) +
    labs(title = paste0("HaCFL Intensity inversions: ", 
                        this_priority),
         subtitle = "At treatment year for that HUC12") + 
    facet_wrap(~TxType)
  
  
  ggsave(plot = p_this_ih,
         filename = file.path("plots", "maps", 
                              paste0("SN_intensity_hacfl_inversions_", 
                                     this_priority, ".jpg")),
         height = 5, width = 8, units = c("in"))
  
}






#b2 fuel adjective inversions
fas <- fueladj %>% 
  filter(!TxIntensity == "base") %>% 
  dplyr::select(HUC12, Priority, trt_yr, TxType, TxIntensity, Year, mean) 

#get trt yr to add in later for baseline masquerade
huc_trt_yr <- fas %>% 
  dplyr::select(HUC12, Priority, trt_yr) %>% 
  distinct()

#grab baseline to masquerade as intensity level
fabases <- fueladj %>% 
  filter(TxIntensity == "base") %>% 
  #no priority or trt
  dplyr::select(HUC12, TxIntensity, mean, Year) 

#get all combination of Priority and TxTypes
frame <- fueladj %>% 
  filter(!TxIntensity == "base") %>% 
  dplyr::select(Priority, TxType) %>% 
  distinct()

#duplicate bases values for all priority-txtype combos
fab <- frame %>% 
  cross_join(fabases) %>% 
  #add in trt yr for baseline masquerade
  left_join(huc_trt_yr, by = join_by("HUC12", "Priority"))

#add back in and filter to treatment year
fasb <- bind_rows(fas, fab) %>% 
  #year of treatment (note NA in baseline)
  filter(Year == trt_yr) %>% 
  rename(fuel_adj_mean = mean) %>% 
  #add rank order
  arrange(HUC12, Priority, TxType, TxIntensity) %>% 
  group_by(HUC12, Priority, TxType) %>% 
  mutate(fuel_adj_rank = rank(fuel_adj_mean)) %>% 
  #create intensity rank (ascending order, 2m should be smallest value)
  mutate(intensity_rank = case_when(
    TxIntensity == "base" ~ 4,
    TxIntensity == "500k" ~ 3,
    TxIntensity == "1m" ~ 2,
    TxIntensity == "2m" ~ 1)) %>% 
  #flag if ranks mismatches (inversion of some sort)
  mutate(i_fa_flag = if_else(!fuel_adj_rank == intensity_rank, TRUE, FALSE))



fasb_sum <- fasb %>% 
  group_by(HUC12, Priority, TxType) %>% 
  summarize(count_ifa = sum(i_fa_flag), .groups = "drop") %>% 
  #create color categories
  mutate(ifa_cat = case_when(
    count_ifa == 0 ~ "Zero",
    count_ifa <= 2  ~ "One/Two",
    count_ifa > 2 ~ "Multiple"
  ))



## loop on priority 

#this_priority <- "Fire"
priorities <- fasb_sum %>% pull(Priority) %>% unique()

for (p in seq_along(priorities)){
  
  this_priority <- priorities[[p]]
  
  this_fasb_sum <- fasb_sum %>% 
    filter(Priority == this_priority)
  
  #join with spatial data
  this_shp <- hucs_simpl %>% 
    filter(region == "SN") %>% 
    #removing the nonburn HUCs
    inner_join(this_fasb_sum, by = join_by(HUC12))
  
  #map
  p_this_ifa <- ggplot() +
    #HUCs
    geom_sf(data = this_shp, 
            aes(fill = ifa_cat),
            color = "grey90", size = 0.01) +
    scale_fill_manual("Fuel adjective inversions", 
                      values = stoplight_colors, 
                      na.value = "grey80") + 
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 10),
          strip.background = element_blank(),
          strip.text = element_text(size = 13),
          axis.text = element_text(family = 'sans'),
          legend.position = "right",
          legend.direction = "vertical") +
    ggspatial::annotation_scale(
      location = "bl", bar_cols = c("grey50", "white")) +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      height = unit(0.6, "cm"), width = unit(0.6, "cm"),
      pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
      style = ggspatial::north_arrow_minimal()) +
    labs(title = paste0("Fuel Adjective Intensity inversions: ", 
                        this_priority),
         subtitle = "At treatment year for that HUC12") + 
    facet_wrap(~TxType)
  
  
  ggsave(plot = p_this_ifa,
         filename = file.path("plots", "maps", 
                              paste0("SN_intensity_fueladjective_inversions_", 
                                     this_priority, ".jpg")),
         height = 5, width = 8, units = c("in"))
  
}

### b3. 
# where are fueladj - hacfl rank mismatches happening? 
# (irrespective of intensity inversions)

refa <- resb %>% 
  dplyr::select(-intensity_rank, -i_h_flag) %>% 
  left_join(fasb %>% 
              dplyr::select(-intensity_rank, -i_fa_flag),
            by = join_by(HUC12, Priority, trt_yr, TxType, TxIntensity, Year)) %>% 
  mutate(h_fa_flag = if_else(!fuel_adj_rank == hacfl_rank, TRUE, FALSE))



refa_sum <- refa %>% 
  group_by(HUC12, Priority, TxType) %>% 
  summarize(count_hfa = sum(h_fa_flag), .groups = "drop") %>% 
  #create color categories
  mutate(hfa_cat = case_when(
    count_hfa == 0 ~ "Zero",
    count_hfa <= 2  ~ "One/Two",
    count_hfa > 2 ~ "Multiple"
  ))

## loop on priority 

#this_priority <- "Fire"
priorities <- fasb_sum %>% pull(Priority) %>% unique()

for (p in seq_along(priorities)){
  
  this_priority <- priorities[[p]]
  
  this_refa_sum <- refa_sum %>% 
    filter(Priority == this_priority)
  
  #join with spatial data
  this_shp <- hucs_simpl %>% 
    filter(region == "SN") %>% 
    #removing the nonburn HUCs
    inner_join(this_refa_sum, by = join_by(HUC12))
  
  #map
  p_this_hfa <- ggplot() +
    #HUCs
    geom_sf(data = this_shp, 
            aes(fill = hfa_cat),
            color = "grey90", size = 0.01) +
    scale_fill_manual("Fuel adjective & HaCFL\nrank mismatches", 
                      values = stoplight_colors, 
                      na.value = "grey80") + 
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 10),
          strip.background = element_blank(),
          strip.text = element_text(size = 13),
          axis.text = element_text(family = 'sans'),
          legend.position = "right",
          legend.direction = "vertical") +
    ggspatial::annotation_scale(
      location = "bl", bar_cols = c("grey50", "white")) +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      height = unit(0.6, "cm"), width = unit(0.6, "cm"),
      pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
      style = ggspatial::north_arrow_minimal()) +
    labs(title = paste0("Fuel Adjective versus HaCFL rankings: ", 
                        this_priority),
         subtitle = "At treatment year for that HUC12") + 
    facet_wrap(~TxType)
  
  
  ggsave(plot = p_this_hfa,
         filename = file.path("plots", "maps", 
                              paste0("SN_hacfl_fueladj_rankmismatches_", 
                                     this_priority, ".jpg")),
         height = 5, width = 8, units = c("in"))
  
}

### 3. Identify 'worst' fuel inversions (from baseline) ------------------------

# 3. Identify HUCs with incr adjectives 
#  degree of shift. 
#  percentage change of fuel adj mean from BASELINE. 
#   (reason: e.g. 2m and 1m could both be really high over baseline, but if
#                2m is only a little higher than 1m it'll look like a small inversion
#                but what we really want is comparison to no treatment)

# multiplot? 3a. HaCFL, b. fuel mean, c. HUC location. marrangeGrid to multi pdf

# any year! not just year of treatment

fi <- fueladj %>% 
  dplyr::select(HUC12, Priority, trt_yr, TxType, TxIntensity, Year, mean, mean_bl) %>%  
  rename(fa_mean = mean,
         fa_base = mean_bl) %>% 
  filter(!Priority == "baseline") %>% 
  #baseline masquerade
  bind_rows(fab %>% 
              #match set up to scenario. (baseline both same value)
              mutate(fa_base = mean) %>% 
              rename(fa_mean = mean)) %>% 
  #find percent change from baseline
  mutate(fa_perc_change = (fa_mean - fa_base) / fa_base * 100)


fi %>% filter(!TxIntensity == "base") %>% pull(fa_perc_change) %>% summary()
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -23.630185  -2.684661  -0.210873  -1.719351   0.001663  16.341600

#all years
ggplot() + geom_histogram(data=fi %>% filter(!TxIntensity == "base"), 
                          mapping=aes(x=fa_perc_change))

#treatment year or after
ggplot() + 
  geom_histogram(data=fi %>% filter(!Priority == "baseline", Year >= trt_yr),
                 mapping=aes(x=fa_perc_change),
                 bins = 60) + 
  labs(title = "SN Fuel Adjectives changes at or post treatment",
       y = "Count of HUC-Scenario-Years",
       x = "Percent change of fuel adjective mean from baseline")

#identifying top instances to investigate
fi_sel <- fi %>% 
  arrange(desc(fa_perc_change)) %>% 
  #take top 10
  slice(1:10)

fi_sel$HUC12 %>% unique() %>% length()
# 6 unique HUCs


#gridfire results for joining/plotting
ri <- bind_rows(ress, res_bases) %>% 
  #add in trt yr for all
  left_join(huc_trt_yr, by = join_by("HUC12", "Priority")) %>% 
  dplyr::select(HUC12, Priority, TxIntensity, TxType, Year, trt_yr, HaCFL, expFlame)

#fuel changes
fl <- fueladj %>% 
  #convert to percentage of total area (pixels)
  mutate(across(c(VL_diff, L_diff, M_diff, H_diff, VH_diff, X_diff), ~ ./tot_pixels)) %>% 
  #select wanted
  dplyr::select(HUC12, Priority, TxType, TxIntensity, Year,
                VL_diff, L_diff, M_diff, H_diff, VH_diff, X_diff) %>% 
  #pivot long
  pivot_longer(cols= c(VL_diff, L_diff, M_diff, H_diff, VH_diff, X_diff),
               names_to = "adjective",
               values_to = "area_fraction") %>% 
  #factor adjective so correct order
  separate_wider_delim(col = adjective, names=c("adjective", NA), delim="_") %>% 
  mutate(adjective = as.factor(adjective),
         adjective = forcats::fct_relevel(adjective,
                                          "VL", "L", "M", "H", "VH", "X")) 

#loop on each entry of top fuel inversions
pfi_collector <- list()

for (i in 1:nrow(fi_sel)){
  
  this_row <- fi_sel[i,]
  this_huc <- this_row[["HUC12"]]
  this_priority <- this_row[["Priority"]]
  this_trt <- this_row[["TxType"]]
  this_intensity <- this_row[["TxIntensity"]]
  this_year <- this_row[["Year"]]
  this_trtyr <- this_row[["trt_yr"]] #for this HUC-Priority
  
  all_title <- paste("HUC", this_huc, this_priority, this_trt, this_year)
  
  #plot fuels changes in this HUC-scenario-year from baseline
  p1 <- ggplot() + 
    geom_col(data = fl %>% 
               filter(HUC12 == this_huc,
                      Priority == this_priority,
                      TxType == this_trt,
                      Year == this_year),
             mapping = aes(x = adjective, y = area_fraction, 
                           fill = TxIntensity),
             position = position_dodge()) + 
    geom_hline(yintercept = 0, color = "black") + 
    theme_bw() + 
    scale_fill_manual("Intensity", values = i_colors) + 
    labs(title = "Change in fuel adjectives to baseline",
         subtitle = this_year)  
    #theme(title = element_text(size = 10))
  
  #plot location
  p2 <- ggplot() +
    #HUCs
    geom_sf(data = hucs_simpl %>% 
              filter(region == "SN"), 
            color = "grey90", size = 0.01) +
    geom_sf(data = hucs_simpl %>% 
              filter(HUC12 == this_huc),
            color = "red", fill="red") + 
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 10),
          strip.background = element_blank(),
          strip.text = element_text(size = 13),
          axis.text = element_text(family = 'sans'),
          legend.position = "right",
          legend.direction = "vertical") +
    ggspatial::annotation_scale(
      location = "bl", bar_cols = c("grey50", "white")) +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      height = unit(0.6, "cm"), width = unit(0.6, "cm"),
      pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
      style = ggspatial::north_arrow_minimal())   

  #plot mean fuel timeseries (all years)
  p3 <- ggplot(data = fi %>% 
                 filter(HUC12 == this_huc,
                        Priority == this_priority,
                        TxType == this_trt),
               mapping = aes(x=Year, y=fa_mean, color=TxIntensity)) +
    geom_jitter(shape = 1, height = 0, width = 0.3) +
    geom_line() +
    scale_x_continuous(breaks = year_breaks) +
    scale_color_manual("Intensity", values = i_colors) + 
    labs(title = "Fuel adjective mean, all years",
         subtitle = paste0("Treat year: ", this_trtyr),
         y = "Average coded fuel adjective")
  
  
  #plot gridfire HaCFL timeseries (all years)
  p4 <- ggplot(data = ri %>% 
                 filter(HUC12 == this_huc,
                        Priority == this_priority,
                        TxType == this_trt),
               mapping = aes(x=Year, y=HaCFL, color=TxIntensity)) +
    geom_jitter(shape = 1, height = 0, width = 0.3) +
    geom_line() +
    scale_x_continuous(breaks = year_breaks) +
    scale_color_manual("Intensity", values = i_colors) + 
    labs(title = "Gridfire results, all years",
         subtitle = paste0("Treat year: ", this_trtyr))
  
  
  #arrange plots
  # p5 <- arrangeGrob(arrangeGrob(p1, p3, p4), 
  #                   p2, 
  #                   ncol = 2, top = all_title)
  
  p5 <- arrangeGrob(p1, p3, p2, p4, ncol = 2, top = all_title)
  
  pfi_collector[[i]] <- p5
}

plots_to_pdf <- marrangeGrob(pfi_collector, nrow = 1, ncol = 1)

ggsave(plots_to_pdf,
       filename = file.path("plots", 
                            "adjectives", 
                            "top_fuel_adjective_inversions_investigation.pdf"))


### 2024-05-03 SDI Maps --------------------------------------------------------

#SDI will be per scenario including intensity. 
# so separate out by intensity level - show inversion or not (binary) & then SDI. 
 

#baseline/baseweather to be treated as an 'intensity' level PER ALL priorities, trts
res_bases <- res %>% 
  #only baseline, removing baseweather here
  filter(TxIntensity %in% c("base")) %>% 
  #remove priorities, trts from bases
  dplyr::select(-Priority, -TxType)

#remove bases from rest of results (temporarily)
ress <- res %>% 
  filter(!TxIntensity %in% c("base", "bw"))

#get all combination of Priority and TxTypes
frame <- ress %>% 
  dplyr::select(Priority, TxType) %>% 
  distinct()

#duplicate bases values for all priority-txtype combos
res_bases <- frame %>% 
  cross_join(res_bases)

#add back in
sdi <- bind_rows(ress, res_bases) %>% 
  #add in trt yr for all
  left_join(huc_trt_yr, by = join_by("HUC12", "Priority")) %>% 
  #needed columns
  dplyr::select(HUC12, Priority, trt_yr, TxType, TxIntensity, Year, HaCFL, SDI) %>% 
  #year of treatment 
  filter(Year == trt_yr) %>% 
  #add rank order
  arrange(HUC12, Priority, TxType, TxIntensity) %>% 
  group_by(HUC12, Priority, TxType) %>% 
  mutate(hacfl_rank = rank(HaCFL),
         sdi_rank = rank(SDI)) %>% 
  #create intensity rank (ascending order, 2m should be smallest value)
  mutate(intensity_rank = case_when(
    TxIntensity == "base" ~ 4,
    TxIntensity == "500k" ~ 3,
    TxIntensity == "1m" ~ 2,
    TxIntensity == "2m" ~ 1)) %>% 
  #flag if ranks mismatches
  mutate(i_h_flag = if_else(!hacfl_rank == intensity_rank, TRUE, FALSE),
         i_s_flag = if_else(!sdi_rank == intensity_rank, TRUE, FALSE))


#SDI inversion map 

sdi_sum <- sdi %>% 
  group_by(HUC12, Priority, TxType) %>% 
  summarize(count_isa = sum(i_s_flag), .groups = "drop") %>% 
  #create color categories
  mutate(isa_cat = case_when(
    count_isa == 0 ~ "Zero",
    count_isa <= 2  ~ "One/Two",
    count_isa > 2 ~ "Multiple"
  ))

stoplight_colors <- c("Zero" = "lightseagreen",
                      "One/Two" = "gold",
                      "Multiple" = "red3")


## loop on priority 

priorities <- sdi_sum %>% pull(Priority) %>% unique()

for (p in seq_along(priorities)){
  
  this_priority <- priorities[[p]]
  
  this_sdi_sum <- sdi_sum %>% 
    filter(Priority == this_priority)
  
  #join with spatial data
  this_shp <- hucs_simpl %>% 
    filter(region == "SN") %>% 
    #removing the nonburn HUCs
    inner_join(this_sdi_sum, by = join_by(HUC12))
  
  #map
  p_this_isa <- ggplot() +
    #HUCs
    geom_sf(data = this_shp, 
            aes(fill = isa_cat),
            color = "grey90", size = 0.01) +
    scale_fill_manual("SDI inversions", 
                      values = stoplight_colors, 
                      na.value = "grey80") + 
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 10),
          strip.background = element_blank(),
          strip.text = element_text(size = 13),
          axis.text = element_text(family = 'sans'),
          legend.position = "right",
          legend.direction = "vertical") +
    ggspatial::annotation_scale(
      location = "bl", bar_cols = c("grey50", "white")) +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      height = unit(0.6, "cm"), width = unit(0.6, "cm"),
      pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
      style = ggspatial::north_arrow_minimal()) +
    labs(title = paste0("SDI Intensity inversions: ", 
                        this_priority),
         subtitle = "At treatment year for that HUC12") + 
    facet_wrap(~TxType)
  
  
  ggsave(plot = p_this_isa,
         filename = file.path("plots", "maps", 
                              paste0("SN_intensity_sdi_inversions_", 
                                     this_priority, ".jpg")),
         height = 5, width = 8, units = c("in"))
  
}


# per intensity level, display SDI value??
# do not want to display base level (inversion will be caught in the other)
#filter(!TxIntensity == "base")
# by trt, display 500k, 1m, 2m as facets probably
# outline inversions? 
#  hacfl: red, SDI: blue, both: purple ? 

sdi_i <- sdi %>% 
  filter(!TxIntensity == "base")
# %>% 
#   #create combo flag
#   mutate(flag_combo = case_when(
#     i_h_flag == TRUE & i_s_flag == TRUE ~ "Both",
#     i_h_flag == TRUE & i_s_flag == FALSE ~ "HaCFL inversion",
#     i_h_flag == FALSE & i_s_flag == TRUE ~ "SDI inversion",
#     i_h_flag == FALSE & i_h_flag == FALSE ~ "None"
#   ))

#need consistent ranges
sdi_limits <- range(sdi_i[["SDI"]])

priorities <- sdi %>% pull(Priority) %>% unique()
trts <- sdi %>% pull(TxType) %>% unique()

for (p in seq_along(priorities)){
  
  this_priority <- priorities[[p]]
  
  #TREATMENT TYPE
  for (t in seq_along(trts)){
    
    this_trt <- trts[[t]]
    
    this_sdi <- sdi_i %>% 
      filter(Priority == this_priority,
             TxType == this_trt)
    
    #join with spatial data
    # NOT simpl since using color=NA
    this_shp <- hucs_shp %>% 
      filter(region == "SN") %>% 
      #removing the nonburn HUCs
      inner_join(this_sdi, by = join_by(HUC12))
    
    #map
    p_sdi <- ggplot() +
      #HUCs
      geom_sf(data = this_shp, 
              aes(fill = SDI),
              color=NA) +
      scale_fill_viridis(limits = sdi_limits) + 
      geom_sf(data = this_shp %>% 
                filter(i_h_flag == TRUE),
              aes(color = i_h_flag),
              fill=NA,
              size=0.01) + 
      scale_color_manual("HaCFL Inversion", 
                         values = "red") + 
      theme_bw() +
      theme(panel.grid = element_blank(),
            panel.background = element_blank(),
            legend.title = element_text(size = 11),
            legend.text = element_text(size = 10),
            strip.background = element_blank(),
            strip.text = element_text(size = 13),
            axis.text = element_text(family = 'sans'),
            legend.position = "right",
            legend.direction = "vertical") +
      ggspatial::annotation_scale(
        location = "bl", bar_cols = c("grey50", "white")) +
      ggspatial::annotation_north_arrow(
        location = "tr", which_north = "true",
        height = unit(0.6, "cm"), width = unit(0.6, "cm"),
        pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
        style = ggspatial::north_arrow_minimal()) +
      labs(title = paste0("SDI values with HaCFL inversions flagged by Intensity: ", 
                          this_priority, " ", 
                          this_trt),
           subtitle = "At treatment year for that HUC12") + 
      facet_wrap(~TxIntensity)
    
    ggsave(plot = p_sdi,
           filename = file.path("plots", "maps", 
                                paste0("SN_SDI_intensity_inversions_", 
                                       this_priority, "_",
                                       this_trt, 
                                       ".jpg")),
           height = 5, width = 8, units = c("in"))
    
    
  }#end trt

}#end priority
