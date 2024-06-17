# 2m to 500k comparison HaCFL
# At various years relative to treatment

# Inversions will be positive 

# All scenarios
# (breakouts below??)

### Libraries -------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  viridis)

### User settings -------------------------------------------

#percent change threshold for inversions of HaCFL


# BROKEN FOR MAR VS JUN. FIx later if needed


inv_threshold <- NA

inv_color <- "gold"
inv_text_color <- "gold3"

### Results import -------------------------------------------

mar <- read_csv(file.path("results",
                          "202403_runs",
                          "datacube", 
                          "datacube_interim_sc_cc_sn_bl_bw_20240513.csv")) %>% 
  mutate(HUC12 = as.character(HUC12))


jun <- read_csv(file.path("results",
                               "datacube", 
                               "datacube_interim_SNSCCC_20240617.csv")) %>% 
  mutate(HUC12 = as.character(HUC12)) 


### Data set up ---------------------------------------------

#March
mar_r500k <- mar %>% 
  filter(TxIntensity == "500k") %>% 
  dplyr::select(Region, HUC12, Priority, TxType, Year, HaCFL) %>% 
  rename(mar_hacfl_500k = HaCFL)

mar_2m500k <- mar %>% 
  #only 2m 
  filter(TxIntensity == "2m") %>% 
  rename(mar_hacfl_2m = HaCFL) %>% 
  #join with 500k for single row
  left_join(mar_r500k, by = join_by(Region, HUC12, Year, Priority, TxType)) %>% 
  #calculate differences (2m - 500k) / 500k * 100 as percent change
  # Inversions are POSITIVE
  mutate(mar_hacfl_2m500k_pc = (mar_hacfl_2m - mar_hacfl_500k) / mar_hacfl_500k * 100) %>% 
  #limited cols
  dplyr::select(Region, HUC12, Year, Priority, TxType, mar_hacfl_2m500k_pc)


#June
jun_r500k <- jun %>% 
  filter(TxIntensity == "500k") %>% 
  dplyr::select(Region, HUC12, Priority, TxType, Year, HaCFL) %>% 
  rename(jun_hacfl_500k = HaCFL)

jun_2m500k <- jun %>% 
  #only 2m 
  filter(TxIntensity == "2m") %>% 
  rename(jun_hacfl_2m = HaCFL) %>% 
  #join with 500k for single row
  left_join(jun_r500k, by = join_by(Region, HUC12, Year, Priority, TxType)) %>% 
  #calculate differences (2m - 500k) / 500k * 100 as percent change
  # Inversions are POSITIVE
  mutate(jun_hacfl_2m500k_pc = (jun_hacfl_2m - jun_hacfl_500k) / jun_hacfl_500k * 100)

#join and relative to treatment year
res_rel <- jun_2m500k %>% 
  left_join(mar_2m500k) %>% 
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
  #relative to treatment year 
  mutate(rel_trt_yr = Year - trt_yr,
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


### Scatter plot facets ------------------------------------------------------

regions <- res_rel %>% pull(Region) %>% unique() %>% sort()

#REGION (outer loop)
for (r in seq_along(regions)){
  
  this_reg <- regions[[r]]
  
  this_reg_label <- case_when(
    this_reg == "CC" ~ "Central Coast",
    this_reg == "NC" ~ "North Coast",
    this_reg == "SC" ~ "South Coast",
    this_reg == "SN" ~ "Sierra Nevada",
  )
  
  res_r <- res_rel %>% 
    filter(Region == this_reg)
  
  
  #output 
  plot_folder <- file.path('plots', 'scatter_inversions_mar_jun', this_reg)
  dir.create(plot_folder, recursive = TRUE)
  
  
  #priorities in this region (all same)
  priorities <- res_r %>% pull(Priority) %>% unique() %>% sort()
  
  #PRIORITY (first inner loop)
  for (p in seq_along(priorities)){
    
    this_priority <- priorities[[p]]
    
    res_r_p <- res_r %>% 
      filter(Priority == this_priority)
    
    #prep for counting (to put on graph)
    t1_prep <- res_r_p %>% 
      filter(!is.na(rel_tx), 
             !rel_tx == "Pretreatment") %>% 
      mutate(x=mar_hacfl_2m500k_pc,
             y=jun_hacfl_2m500k_pc,
             x_neg = if_else(x < 0, 1, 0),
             y_neg = if_else(y < 0, 1, 0)) %>% 
      group_by(rel_tx, TxType, x_neg, y_neg) %>% 
      summarize(.groups="drop",
                count=n(),
                max_x=max(x),
                min_x=min(x),
                max_y=max(y),
                min_y=min(y)) %>% 
      mutate(max_x=max(max_x),
             min_x=min(min_x),
             max_y=max(max_y),
             min_y=min(min_y)) %>% 
      arrange(TxType, rel_tx)
    
    t_r <- t1_prep %>% filter(x_neg==0, y_neg==0)
    b_r <- t1_prep %>% filter(x_neg==0, y_neg==1)
    b_l <- t1_prep %>% filter(x_neg==1, y_neg==1)
    t_l <- t1_prep %>% filter(x_neg==1, y_neg==0)
    
    
    
    #plotting
    p_inv <- ggplot() + 
      geom_point(data = res_r_p %>% 
                   filter(!is.na(rel_tx),
                          !rel_tx == "Pretreatment"),
                 mapping = aes(x = mar_hacfl_2m500k_pc, 
                               y = jun_hacfl_2m500k_pc,
                               color = TxType),
                 shape = 1) + 
      geom_hline(yintercept = 0) + 
      geom_vline(xintercept = 0) + 
      scale_color_brewer("Treatment", palette = "Dark2") + 
      labs(title = paste0(this_reg_label, " - ",
                  "Priority: ", this_priority),
           subtitle = "Facets by treatment type and relative year to treatment",
           caption = "Positive values are INVERSIONS where 2m is worse than 500k",
           y = "JUNE HaCFL: (2m-500k)/500k*100",
           x = "MARCH HaCFL: (2m-500k)/500k*100") + 
      facet_wrap(~rel_tx + TxType, dir="v") + 
      geom_label(data=t_r,
                 mapping=aes(label=count, x=max_x, y=max_y), 
                 size=3, hjust="inward", alpha=0.5) +
      geom_label(data=b_r,
                 mapping=aes(label=count, x=max_x, y=min_y), 
                 size=3, hjust="inward", alpha=0.5) +
      geom_label(data=b_l,
                 mapping=aes(label=count, x=min_x, y=min_y), 
                 size=3, hjust="inward", alpha=0.5) +
      geom_label(data=t_l,
                 mapping=aes(label=count, x=min_x, y=max_y), 
                 size=3, hjust="inward", alpha=0.5) +
      theme_bw() + 
      theme(aspect.ratio = 1)
    
    fn <- paste0(this_reg, '_', this_priority, '_HaCFL_inversions_marjun.jpg')
    
    ggsave(plot = p_inv,
           filename = file.path(plot_folder, fn),
           width = 8, height = 9, units = 'in')
    
    
    # plot 2, WITH threshold
    if (is.numeric(inv_threshold)){
      
      #prep for counting (to put on graph)
      # ONLY HaCFL inversions above threshold!!
      #need to calculate min/maxes BEFORE threshold filtering
      
      t2_prep <- t1_prep %>% 
        rename(count_nothresh = count) %>% 
        left_join(res_r_p %>%
                    #threshold
                    filter(jun_hacfl_2m500k_pc >= inv_threshold |
                             mar_hacfl_2m500k_pc >= inv_threshold) %>% 
                    #regular set up
                    filter(!is.na(rel_tx), 
                           !rel_tx == "Pretreatment") %>% 
                    mutate(x=mar_hacfl_2m500k_pc,
                           y=jun_hacfl_2m500k_pc,
                           x_neg = if_else(x < 0, 1, 0),
                           y_neg = if_else(y < 0, 1, 0)) %>% 
                    group_by(rel_tx, TxType, x_neg, y_neg) %>% 
                    summarize(.groups="drop",
                              count=n()),
                  by = join_by(rel_tx, TxType, x_neg, y_neg)) %>% 
        #drop NAs just to remove warnings
        drop_na(count)
      
      t_r <- t2_prep %>% filter(x_neg==0, y_neg==0)
      b_r <- t2_prep %>% filter(x_neg==0, y_neg==1)
      b_l <- t2_prep %>% filter(x_neg==1, y_neg==1)
      t_l <- t2_prep %>% filter(x_neg==1, y_neg==0)
      
      
      
      #plotting
      p_inv_thresh <- ggplot() + 
        #non threshold inversion points, colors
        geom_point(data = res_r_p %>% 
                     filter(mar_hacfl_2m500k_pc < inv_threshold,
                            jun_hacfl_2m500k_pc) %>% 
                     filter(!is.na(rel_tx),
                            !rel_tx == "Pretreatment"),
                   mapping = aes(x = hacbp_2m500k_pc, 
                                 y = hacfl_2m500k_pc,
                                 color = TxType),
                   shape = 1) + 
        #threshold points, highlighted
        geom_point(data = res_r_p %>% 
                     #threshold
                     filter(jun_hacfl_2m500k_pc >= inv_threshold |
                              mar_hacfl_2m500k_pc >= inv_threshold) %>% 
                     filter(!is.na(rel_tx),
                            !rel_tx == "Pretreatment"),
                   mapping = aes(x = mar_hacfl_2m500k_pc, 
                                 y = jun_hacfl_2m500k_pc),
                   shape = 1,
                   color = inv_color) + 
        geom_hline(yintercept = 0) + 
        geom_vline(xintercept = 0) + 
        #add 5% 'threshold' line
        geom_hline(yintercept = inv_threshold, color=inv_color) +
        scale_color_brewer("Treatment", palette = "Dark2") + 
        labs(title = paste0(this_reg_label, " - ",
                            "Priority: ", this_priority),
             subtitle = "Facets by treatment type and relative year to treatment",
             caption = paste0("Positive values are INVERSIONS where 2m is worse than 500k.\nHighlighted points are above the HaCFL inversion threshold, ", inv_threshold, "%.\nCounts are of HaCFL inversions above threshold ONLY."),
             y = "JUNE HaCFL: (2m-500k)/500k*100",
             x = "MARCHj HaCFL: (2m-500k)/500k*100") + 
        facet_wrap(~rel_tx + TxType, dir="v") + 
        geom_label(data=t_r,
                   mapping=aes(label=count, x=max_x, y=max_y), 
                   size=3, hjust="inward", alpha=0.5,
                   color=inv_text_color) +
        geom_label(data=b_r,
                   mapping=aes(label=count, x=max_x, y=min_y), 
                   size=3, hjust="inward", alpha=0.5,
                   color=inv_text_color) +
        geom_label(data=b_l,
                   mapping=aes(label=count, x=min_x, y=min_y), 
                   size=3, hjust="inward", alpha=0.5,
                   color=inv_text_color) +
        geom_label(data=t_l,
                   mapping=aes(label=count, x=min_x, y=max_y), 
                   size=3, hjust="inward", alpha=0.5,
                   color=inv_text_color) +
        theme_bw() + 
        theme(aspect.ratio = 1)
      
      fn_thresh <- paste0(this_reg, "_", this_priority, 
                          "_HaCFL_inversions_marjun", 
                          "_above_", inv_threshold, 
                          ".jpg")
       
      ggsave(plot = p_inv_thresh,
             filename = file.path(plot_folder, fn_thresh),
             width = 8, height = 9, units = 'in')
      
    }
    
    
  } # end priority
} # end region




