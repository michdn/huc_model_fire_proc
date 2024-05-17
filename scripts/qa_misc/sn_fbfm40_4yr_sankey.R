# Sankey/alluvial plots
#  of FBFM40 values over 4 years
#  by HUC-scenario


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  ggplot2,
  #viridis,
  gridExtra,
  ggfittext,
  ggalluvial) 


### Data ----------------------------

#zonal pixel counts from 4yrs collapsed {111222333444}
fbfm <- readRDS(file.path("qa", "SN_FBFM_4yrcollapsed_zonal_counts.RDS")) %>% 
#For graphing in the correct order (generic, used in multiple places, with modifications)
  #Make factor with set order (intensity))
  mutate(TxIntensity = as.factor(TxIntensity),
       TxIntensity = forcats::fct_relevel(TxIntensity,
                                          "500k", "1m", "2m"))
       

#fuel adjectives for labeling
fa <- read_csv("qa/fbfm_adjective_original_crosswalk.csv") %>% 
  dplyr::select(FBFM, ROS, FL) %>% 
  mutate(FBFM = as.character(FBFM)) %>% 
  unite(col = fbfm_adj, 
        FBFM, ROS, FL,
        sep="_", 
        remove=FALSE)
#add in nonburns
nonburn <- tibble(FBFM = c("091", "092", "093", "098", "099"),
                        ROS = rep("NB", 5),
                        FL = rep("NB", 5)) %>% 
  mutate(fbfm_adj = paste0(FBFM, "_NB"))
#final fa crosswalk
fadj <- bind_rows(nonburn, fa)


#official FBFM colors
colors_raw <- read_csv("qa/fbfm_colors.csv") %>% 
  #as character and 3 digits across
  mutate(fbfm_value = as.character(fbfm_value),
         fbfm_value = str_pad(fbfm_value, width=3, side="left", pad="0")) %>% 
  #add in fuel adj
  left_join(fadj, by=join_by("fbfm_value" == "FBFM")) %>% 
  #remove -9999
  dplyr::filter(!fbfm_value == "-9999")

#without adj
#colors_fbfm <- setNames(as.character(colors_raw$color_hex), colors_raw$fbfm_value) 
#with adj
colors_fbfm2 <- setNames(as.character(colors_raw$color_hex), colors_raw$fbfm_adj)


#example hucs
#new set, 3 from each of the top three timing groups for each priority for each region
# uses the qualifying HUCs from original sample plus new ones
hucs_sample_new <- read_csv(file.path("qa", "sampledhucs_topthreegroups.csv")) %>% 
  filter(Region == "SN")


### Example HUCs loop trts next to each other ------------------------------------------

#only fire
selected_priority <- "Fire"

#create a tibble with each combination in row (no nested loops)
frame <- fbfm %>% 
  dplyr::select(Priority, TxIntensity) %>% 
  #only fire
  filter(Priority == selected_priority) %>% 
  distinct()

sample <- hucs_sample_new %>% 
  filter(grouping == selected_priority) %>% 
  dplyr::select(HUC12, group) %>% 
  cross_join(frame) %>% 
  arrange(group, HUC12, Priority, TxIntensity)

#loop
p_collector <- list()
for (i in 1:nrow(sample)){

  this_row <- sample[i,]

  this_huc <- this_row[["HUC12"]]
  this_timinggroup <- this_row[["group"]]
  this_priority <- this_row[["Priority"]]
  this_intensity <- this_row[["TxIntensity"]]

  #filter to this dataset HUC-Priority-Intensity, all trts
  this_fbfm <- fbfm %>%
    filter(HUC12 == this_huc,
           Priority == this_priority,
           TxIntensity == this_intensity)

  
  #pivot long
  this_fbfm_long <- this_fbfm %>% 
    #remove total, not using here, will be working with pixel counts
    dplyr::select(-"ftotal") %>% 
    #long for all fbfm40 combinations
    pivot_longer(cols = starts_with("f"),
                 names_to = "fbfm4yrs",
                 values_to = "pixels") %>% 
    #remove "f" string in preparation for separation
    mutate(fbfm4yrs = str_remove(fbfm4yrs, pattern="f"),
           #left pad nonburns (2 digit fbfms)
           fbfm4yrs = str_pad(fbfm4yrs, width=12, side="left", pad="0")) %>% 
    #separate into each year
    separate_wider_position(cols=fbfm4yrs, widths=c(year_2024=3, 
                                                    year_2029=3, 
                                                    year_2034=3, 
                                                    year_2039=3)) %>% 
    #remove all zero pixel entries from this huc-scenario
    filter(pixels > 0)
  
  # go to lodes form (even longer)
  this_fbfm_lodes <- this_fbfm_long %>% 
    to_lodes_form(axes=c("year_2024", "year_2029", "year_2034", "year_2039"),
                  id="cohort") %>% 
    #fix year names
    mutate(x = str_remove(x, "year_")) %>% 
    #add in fuel adj names
    rename(FBFM = stratum) %>% 
    left_join(fadj, by = join_by(FBFM))
  
  #plot! 
  this_plot <- ggplot(data=this_fbfm_lodes,
         aes(x=x, y=pixels, stratum=fbfm_adj, alluvium=cohort,
             fill=fbfm_adj, label=FBFM)) +
    geom_flow() + 
    geom_stratum(width=1/2) + 
    #geom_text(stat="stratum", min.y=1000, color="white") + 
    ggfittext::geom_fit_text(stat="stratum", min.size=5, width=1/2,
                             color="white", show.legend=FALSE) + 
    scale_fill_manual("FBFM40_ROS_FL", values = colors_fbfm2) + 
    labs(title=paste0("HUC", this_huc, " (", this_timinggroup, ") : ", 
                      this_priority, " ", this_intensity),
         x="") + 
    theme_bw() + 
    facet_wrap(~TxType, nrow=3)
  
  p_collector[[i]] <- this_plot


} # end i row


plots_to_pdf <- marrangeGrob(p_collector, nrow = 1, ncol = 1, top = NULL)

ggsave(plots_to_pdf,
       filename = file.path("plots", 
                            "sankey_fbfm", 
                            "fbfm_4yrs_sankey_sampledhucs_fire_trt.pdf"),
       height=11, width=8.5, units="in")


### Example HUCs loop INTENSITIES next to each other ------------------------------------------

#only fire
selected_priority <- "Fire"

#create a tibble with each combination in row (no nested loops)
frame <- fbfm %>% 
  dplyr::select(Priority, TxType) %>% 
  #only fire
  filter(Priority == selected_priority) %>% 
  distinct()

sample <- hucs_sample_new %>% 
  filter(grouping == selected_priority) %>% 
  dplyr::select(HUC12, group) %>% 
  cross_join(frame) %>% 
  arrange(group, HUC12, Priority, TxType)

#loop
p_collector <- list()
for (i in 1:nrow(sample)){
  
  this_row <- sample[i,]
  
  this_huc <- this_row[["HUC12"]]
  this_timinggroup <- this_row[["group"]]
  this_priority <- this_row[["Priority"]]
  this_trt <- this_row[["TxType"]]
  
  #filter to this dataset HUC-Priority-Intensity, all trts
  this_fbfm <- fbfm %>%
    filter(HUC12 == this_huc,
           Priority == this_priority,
           TxType == this_trt)
  
  
  #pivot long
  this_fbfm_long <- this_fbfm %>% 
    #remove total, not using here, will be working with pixel counts
    dplyr::select(-"ftotal") %>% 
    #long for all fbfm40 combinations
    pivot_longer(cols = starts_with("f"),
                 names_to = "fbfm4yrs",
                 values_to = "pixels") %>% 
    #remove "f" string in preparation for separation
    mutate(fbfm4yrs = str_remove(fbfm4yrs, pattern="f"),
           #left pad nonburns (2 digit fbfms)
           fbfm4yrs = str_pad(fbfm4yrs, width=12, side="left", pad="0")) %>% 
    #separate into each year
    separate_wider_position(cols=fbfm4yrs, widths=c(year_2024=3, 
                                                    year_2029=3, 
                                                    year_2034=3, 
                                                    year_2039=3)) %>% 
    #remove all zero pixel entries from this huc-scenario
    filter(pixels > 0)
  
  # go to lodes form (even longer)
  this_fbfm_lodes <- this_fbfm_long %>% 
    to_lodes_form(axes=c("year_2024", "year_2029", "year_2034", "year_2039"),
                  id="cohort") %>% 
    #fix year names
    mutate(x = str_remove(x, "year_")) %>% 
    #add in fuel adj names
    rename(FBFM = stratum) %>% 
    left_join(fadj, by = join_by(FBFM))
  
  #plot! 
  this_plot <- ggplot(data=this_fbfm_lodes,
                      aes(x=x, y=pixels, stratum=fbfm_adj, alluvium=cohort,
                          fill=fbfm_adj, label=FBFM)) +
    geom_flow() + 
    geom_stratum(width=1/2) + 
    #geom_text(stat="stratum", min.y=1000, color="white") + 
    ggfittext::geom_fit_text(stat="stratum", min.size=5, width=1/2,
                             color="white", show.legend=FALSE) + 
    scale_fill_manual("FBFM40_ROS_FL", values = colors_fbfm2) + 
    labs(title=paste0("HUC", this_huc, " (", this_timinggroup, ") : ", 
                      this_priority, " ", this_trt),
         x="") + 
    theme_bw() + 
    facet_wrap(~TxIntensity, nrow=3)
  
  p_collector[[i]] <- this_plot
  
  
} # end i row


plots_to_pdf <- marrangeGrob(p_collector, nrow = 1, ncol = 1, top = NULL)

ggsave(plots_to_pdf,
       filename = file.path("plots", 
                            "sankey_fbfm", 
                            "fbfm_4yrs_sankey_sampledhucs_fire_intensity.pdf"),
       height=11, width=8.5, units="in")


# ### Test case ---------
# 
# #test case
# this_huc <- "180201250901"
# this_priority <- "Fire"
# this_trt <- "trt1"
# this_intensity <- "2m"
# 
# this_fbfm <- fbfm %>% 
#   filter(HUC12 == this_huc,
#          Priority == this_priority,
#          TxType == this_trt,
#          TxIntensity == this_intensity)
# 
# #pivot long
# this_fbfm_long <- this_fbfm %>% 
#   #remove total, not using here, will be working with pixel counts
#   dplyr::select(-"ftotal") %>% 
#   #long for all fbfm40 combinations
#   pivot_longer(cols = starts_with("f"),
#                names_to = "fbfm4yrs",
#                values_to = "pixels") %>% 
#   #remove "f" string in preparation for separation
#   mutate(fbfm4yrs = str_remove(fbfm4yrs, pattern="f"),
#          #left pad nonburns (2 digit fbfms)
#          fbfm4yrs = str_pad(fbfm4yrs, width=12, side="left", pad="0")) %>% 
#   #separate into each year
#   separate_wider_position(cols=fbfm4yrs, widths=c(year_2024=3, 
#                                                   year_2029=3, 
#                                                   year_2034=3, 
#                                                   year_2039=3)) %>% 
#   #remove all zero pixel entries from this huc-scenario
#   filter(pixels > 0)
# 
# 
# # go to lodes form (even longer)
# this_fbfm_lodes <- this_fbfm_long %>% 
#   to_lodes_form(axes=c("year_2024", "year_2029", "year_2034", "year_2039"),
#                 id="cohort") %>% 
#   #fix year names
#   mutate(x = str_remove(x, "year_")) %>% 
#   #add in fuel adj names
#   rename(FBFM = stratum) %>% 
#   left_join(fadj, by = join_by(FBFM))
# 
# #plot! 
# ggplot(data=this_fbfm_lodes,
#        aes(x=x, y=pixels, stratum=fbfm_adj, alluvium=cohort,
#            fill=fbfm_adj, label=FBFM)) +
#   geom_flow() + 
#   geom_stratum(width=1/2) + 
#   #geom_text(stat="stratum", min.y=1000, color="white") + 
#   ggfittext::geom_fit_text(stat="stratum", min.size=5, width=1/2,
#                            color="white", show.legend=FALSE) + 
#   scale_fill_manual("FBFM40_ROS_FL", values = colors_fbfm2) + 
#   labs(title=paste0("HUC", this_huc, ": ", 
#                     this_priority, " ", this_trt, " ", this_intensity),
#        x="") + 
#   theme_bw()
