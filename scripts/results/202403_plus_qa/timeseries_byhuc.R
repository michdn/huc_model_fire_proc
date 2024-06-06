# timeseries BY INDIVIDUAL HUC
# ALL HUCS
# designed for per region files

# output is massive multipage pdf

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  viridis,
  sf,
  ggspatial,
  #scales,
  gridExtra)

### user ---------------------------------------------------

#flags add fuel adjective graphs or not
#  Only SN (at the moment) has fuel adjective data
graph_fueladj <- TRUE

#filter out the nonburn hucs? 
filter_nonburn <- TRUE

out_folder <- file.path("plots", "timeseries_byhuc")
dir.create(out_folder, recursive = TRUE)

### data ----------------------------------------------------

#gridfire results
res <- read_csv(file.path("results",
                          "absolute", #"datacube",
                          "SN_SNbl_SNbw_absolute_20240423.csv")) %>%
  mutate(HUC12 = as.character(HUC12)) %>% 
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

if (filter_nonburn){
  nb_hucs <- readRDS("data/nonburnable_rerun_list.RDS") 
  
  res <- res %>% 
    mutate(nonburn_coastal = if_else(HUC12 %in% nb_hucs$huc12, TRUE, FALSE)) %>% 
    filter(nonburn_coastal == FALSE)
} 

# noisy <- readRDS("qa/scatter_noisyHUCs.RDS")
# res <- res %>%
#   filter(HUC12 %in% noisy$HUC12)


#Fuel adjectives
if (graph_fueladj){
  fueladj <- readRDS(file.path("qa", "SN_fuel_adjective_comparison_baseline.RDS")) %>% 
    #For graphing in the correct order (generic, used in multiple places, with modifications)
    # make factor with set order (priority)
    mutate(Priority = as.factor(Priority),
           Priority = forcats::fct_relevel(Priority,
                                           "Fire", "WUI", "Hybrid", "baseline"),
           #Make factor with set order (intensity))
           TxIntensity = as.factor(TxIntensity),
           TxIntensity = forcats::fct_relevel(TxIntensity,
                                              "baseline", "500k", "1m", "2m"),
           #recode so it fits on graphs
           TxIntensity = forcats::fct_recode(TxIntensity, "base" = "baseline"))
  
  if (filter_nonburn){
    fueladj <- fueladj %>%   
      #filter out nonburn issue hucs
      filter(nonburn_coastal == FALSE)
  }
}

#spatial
hucs_shp <- st_read("data/data_huc/TxHucsTimingGroups.shp") %>% 
  rename(HUC12 = huc12)
#Simplified for faster drawing 
hucs_simpl <- sf::st_simplify(hucs_shp, preserveTopology = TRUE, dTolerance = 200)


### setup prep -----------------------------------------------------------------------

#colors for intensities
i_colors <- c("bw" = "grey50",
              "base" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[1],
              "500k" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[2],
              "1m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[3],
              "2m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[4])

year_breaks <- c(2024, 2029, 2034, 2039)


### baseline masquerades -----------------------------------------------------

# gridfire

#add treat year in later with crosswalk
huc_trt_yr <- res %>% 
  filter(!TxIntensity %in% c("base", "bw")) %>% 
  dplyr::select(HUC12, Priority, timeFire, timeHybrid, timeWui) %>% 
  distinct() %>% 
  #treatment years
  separate_wider_delim(timeFire, "_", names=c("fire_trt_yr_txt", NA)) %>% 
  separate_wider_delim(timeHybrid, "_", names=c("hybrid_trt_yr_txt", NA)) %>% 
  mutate(fire_trt_yr = as.numeric(fire_trt_yr_txt),
         hybrid_trt_yr = as.numeric(hybrid_trt_yr_txt),
         wui_trt_yr_txt = case_when(
           timeWui == "2024_2039_yr1to5_16to20" ~ "2024_2039",
           timeWui == "2029_yr6to10" ~ "2029",
           timeWui == "2034_yr11to15" ~ "2034",
           timeWui == "Not treated" ~ "Untreated"),
         wui_trt_yr = case_when(
           timeWui == "2024_2039_yr1to5_16to20" ~ 2024,
           timeWui == "2029_yr6to10" ~ 2029,
           timeWui == "2034_yr11to15" ~ 2034,
           timeWui == "Not treated" ~ 9999)) %>% 
  #treat year for priority
  mutate(trt_yr = case_when(
    Priority == "Fire" ~ fire_trt_yr,
    Priority == "Hybrid" ~ hybrid_trt_yr,
    Priority == "WUI" ~ wui_trt_yr),
    trt_yr_txt = case_when(
      Priority == "Fire" ~ fire_trt_yr_txt,
      Priority == "Hybrid" ~ hybrid_trt_yr_txt,
      Priority == "WUI" ~ wui_trt_yr_txt)) %>%
  #final columns
  dplyr::select(HUC12, Priority, trt_yr, trt_yr_txt)


#baseline/baseweather to be treated as an 'intensity' level PER ALL priorities, trts
res_bases <- res %>% 
  #baseline, baseweather 
  filter(TxIntensity %in% c("base", "bw")) %>% 
  #remove priorities, trts from bases
  dplyr::select(-Priority, -TxType)

#remove bases from rest of results (temporarily), scenarios only
ress <- res %>% 
  filter(!TxIntensity %in% c("base", "bw"))

#get all combination of Priority and TxTypes
frame <- ress %>% 
  dplyr::select(Priority, TxType) %>% 
  distinct()

#duplicate bases values for all priority-txtype combos
res_bases <- frame %>% 
  cross_join(res_bases)


res_sb <- bind_rows(ress, res_bases) %>% 
  #add in trt yr for all
  left_join(huc_trt_yr, by = join_by("HUC12", "Priority")) 



# fuel adj
#  (note baseweather is not relevant/included/existing)

if (graph_fueladj){
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
  
  
  fa_sb <- fueladj %>% 
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
  
} # end if graph_fueladj


# LOOP OF GRAPHING ---------------------------------------------------------
huc_list <- res_sb %>% pull(HUC12) %>% unique() %>% sort()

#initialize collector
pagegraph_collector <- vector("list", length = length(huc_list))

for (i in seq_along(huc_list)){
  
  this_huc <- huc_list[i]
  
  this_res <- res_sb %>% 
    filter(HUC12 == this_huc)
  
  
  p_hacfl <- ggplot(data = this_res,
                    mapping = aes(x=Year, y=HaCFL, color=TxIntensity)) +
    geom_jitter(shape = 1, height = 0, width = 0.3) +
    geom_line() +
    scale_x_continuous(breaks = year_breaks) +
    scale_color_manual("Intensity", values = i_colors) + 
    facet_wrap(~ paste0(Priority, " (treated ", trt_yr_txt, ")" ) + TxType,
               labeller = function (labels) {
                 labels <- lapply(labels, as.character)
                 list(do.call(paste, c(labels, list(sep = "\n"))))
               }) +
    theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0.1),
          legend.position = "bottom") + 
    labs(title = "Fire modeling: Conditional flame length")
  
  # p_expflame <- ggplot(data = this_res,
  #                      mapping = aes(x=Year, y=expFlame, color=TxIntensity)) +
  #   geom_jitter(shape = 1, height = 0, width = 0.3) +
  #   geom_line() +
  #   scale_x_continuous(breaks = year_breaks) +
  #   scale_color_manual("Intensity", values = i_colors) + 
  #   facet_wrap(~ paste0(Priority, " (treated ", trt_yr_txt, ")" ) + TxType,
  #              labeller = function (labels) {
  #                labels <- lapply(labels, as.character)
  #                list(do.call(paste, c(labels, list(sep = "\n"))))
  #              }) +
  #   theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0.1))
  
    #labs(title = "Fire modeling: Flame index")
  
  # p_hacbp <- ggplot(data = this_res,
  #                   mapping = aes(x=Year, y=HaCBP, color=TxIntensity)) +
  #   geom_jitter(shape = 1, height = 0, width = 0.3) +
  #   geom_line() +
  #   scale_x_continuous(breaks = year_breaks) +
  #   scale_color_manual("Intensity", values = i_colors) + 
  #   facet_wrap(~ paste0(Priority, " (treat year ", trt_yr_txt, ")" ) + TxType) + 
  #   labs(title = "Fire modeling: Conditional burn probability")
  # 
  # p_expBurn <- ggplot(data = this_res,
  #                     mapping = aes(x=Year, y=expBurn, color=TxIntensity)) +
  #   geom_jitter(shape = 1, height = 0, width = 0.3) +
  #   geom_line() +
  #   scale_x_continuous(breaks = year_breaks) +
  #   scale_color_manual("Intensity", values = i_colors) + 
  #   facet_wrap(~ paste0(Priority, " (treat year ", trt_yr_txt, ")" ) + TxType) + 
  #   labs(title = "Fire modeling: Expected burned acres")
  # 
  # 
  # p_exppcactive <- ggplot(data = this_res,
  #                         mapping = aes(x=Year, y=expPcActive, color=TxIntensity)) +
  #   geom_jitter(shape = 1, height = 0, width = 0.3) +
  #   geom_line() +
  #   scale_x_continuous(breaks = year_breaks) +
  #   scale_color_manual("Intensity", values = i_colors) + 
  #   facet_wrap(~ paste0(Priority, " (treat year ", trt_yr_txt, ")" ) + TxType) + 
  #   labs(title = "Fire modeling: Percent active crown fire")
  
  
  #locator
  this_spatial <- hucs_simpl %>% 
    filter(HUC12 == this_huc)
  
  p_loc <- ggplot() +
    #HUCs
    geom_sf(data = hucs_simpl, #%>% filter(region == this_spatial[["region"]]), 
            color = "grey90", size = 0.01) +
    geom_sf(data = this_spatial,
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
  
  if (graph_fueladj){
    
    this_fa <- fa_sb %>% 
      filter(HUC12 == this_huc)
    
    p_fa <- ggplot(data = this_fa,
                   mapping = aes(x=Year, y=fa_mean, color=TxIntensity)) +
      geom_jitter(shape = 1, height = 0, width = 0.3) +
      geom_line() +
      scale_x_continuous(breaks = year_breaks) +
      scale_color_manual("Intensity", values = i_colors) + 
      facet_wrap(~paste0(Priority, " (treated ", trt_yr, ")" ) + TxType,
                 labeller = function (labels) {
                   labels <- lapply(labels, as.character)
                   list(do.call(paste, c(labels, list(sep = "\n"))))
                 }) +
      labs(title = "FL Fuel adjective mean",
           y = "Average coded FL fuel adj") + 
      theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0.1),
            legend.position = "bottom")
    
  }
  
  #arrange plots
  if (graph_fueladj){
    #gridfire and fuel adj plots
    # huc_plots <- arrangeGrob(p_hacfl, p_expflame, 
    #                          p_hacbp, p_expBurn, 
    #                          p_exppcactive, p_loc,
    #                          p_fa, 
    #                          ncol = 2, 
    #                          top = paste0("HUC: ", this_huc))
    huc_plots <- arrangeGrob(p_hacfl, #p_loc, 
                             p_fa, 
                             ncol = 2, 
                             top = paste0("HUC: ", this_huc))
    
    
  } else {
    #just gridfire
    # huc_plots <- arrangeGrob(p_hacfl, p_expflame, 
    #                          p_hacbp, p_expBurn, 
    #                          p_exppcactive, p_loc,
    #                          ncol = 2, 
    #                          top = paste0("HUC: ", this_huc))
    huc_plots <- arrangeGrob(p_hacfl,  
                             p_loc,
                             ncol = 2, 
                             top = paste0("HUC: ", this_huc))
    
    
  }
  
  #add page plots to collector
  pagegraph_collector[[i]] <- huc_plots
  
  if (i%%100 == 0){
    print(paste0(i, " of ", length(huc_list), " at ", Sys.time()))
  }
  
  
} # end i huc loop

plots_to_pdf <- marrangeGrob(pagegraph_collector, 
                             nrow = 1, ncol = 1, 
                             top=NULL)

#fn1 <- "NOISY10_2mbl_huc_hacfl"
fn1 <- "huc_hacfl"
fn2 <- if_else(graph_fueladj, "_FLfueladj_", "_")
fn3 <- "timeseries"
fn4 <- if_else(filter_nonburn, "_nbremoved", "")

filenm <- paste0(fn1, fn2, fn3, fn4, ".pdf")

ggsave(plots_to_pdf,
       filename = file.path(out_folder, 
                            filenm),
       width = 11, height = 8.5, units = c("in"))
