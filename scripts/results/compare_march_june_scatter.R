# Compare March and June runs
#   - Nonburn fix, CBH adjustment, new weather 
# Scatter plots
# Loop for Region, Priority, Tx Type
# facet on rel_trt_yr (Pre, At, 5, 10)
# color with Intensity

# flag to filter on nonburn issue HUCs only


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  viridis, 
  gridExtra)

### User settings ---------------------------------------------

# Flag for nonburn issue HUCs only
only_nonburn <- FALSE

# Also save HaCFL as single larger jpgs
save_hacfl <- TRUE

folder_out <- file.path("plots", "scatter_mar_jun")
dir.create(folder_out)


### Results import -------------------------------------------

#Latest March run (does not include NC)
mar <- read_csv(file.path("results",
                          "202403_runs",
                          "datacube", 
                          "datacube_interim_sc_cc_sn_bl_bw_20240513.csv")) %>% 
  mutate(HUC12 = as.character(HUC12))

#June run (TBD)
jun <- read_csv(file.path("results",
                          "202403_runs",
                          "datacube", 
                          "datacube_interim_sc_cc_sn_bl_bw_20240513.csv")) %>% 
  mutate(HUC12 = as.character(HUC12)) %>% 
  #pretend strip out baseline
  filter(!Priority == "baseline", 
         !Priority == "baseweather")


if (only_nonburn){nb_hucs <- readRDS("data/nonburnable_rerun_list.RDS")}


### Data prep -----------------------------------------------------------

# Do not have to worry about NC, baselines, etc. as
# will do inner join, so anything not present in the other datasets will disappear

if (only_nonburn){
  mar <- mar %>% filter(HUC12 %in% nb_hucs$huc12)
  jun <- jun %>% filter(HUC12 %in% nb_hucs$huc12)
}

# comparing HaCFL, expFlame, HaCBP, expBurn, active, expPcActive

mar <- mar %>% 
  mutate(timing_group = case_when(
    Priority == "Fire" ~ timeFire, 
    Priority == "WUI" ~ timeWui,
    Priority == "Hybrid" ~ timeHybrid)) %>% 
  dplyr::select(HUC12, Region, Priority, TxIntensity, TxType, Year, timing_group,
                HaCFL, expFlame, HaCBP, expBurn, active_crown, expPcActive) %>% 
  rename_with(~ paste0(.x, "_mar", recycle0=TRUE),
              c(HaCFL, expFlame, HaCBP, expBurn, active_crown, expPcActive))

jun <- jun %>% 
  mutate(timing_group = case_when(
    Priority == "Fire" ~ timeFire, 
    Priority == "WUI" ~ timeWui,
    Priority == "Hybrid" ~ timeHybrid)) %>% 
  dplyr::select(HUC12, Region, Priority, TxIntensity, TxType, Year, timing_group,
                HaCFL, expFlame, HaCBP, expBurn, active_crown, expPcActive) %>% 
  rename_with(~ paste0(.x, "_jun", recycle0=TRUE),
              c(HaCFL, expFlame, HaCBP, expBurn, active_crown, expPcActive))


# INNER JOIN to get just regions, scenarios in both
#  e.g. NC region will be missing in mar and baselines from all regions in jun
res <- mar %>% 
  inner_join(jun,
             by = join_by(HUC12, Region, 
                          Priority, TxIntensity, TxType, 
                          Year, timing_group)) %>% 
  #factor Intensity for proper ordering on graph
  mutate(TxIntensity = as.factor(TxIntensity),
         TxIntensity = forcats::fct_relevel(TxIntensity,
                                            "500k", "1m", "2m")) %>% 
  #set up relative to treatment year info
  mutate(trt_yr = case_when(
    timing_group == "2024_yr1to5" ~ 2024,
    timing_group == "2024_2039_yr1to5_16to20" ~ 2024,
    timing_group == "2029_yr6to10" ~ 2029,
    timing_group == "2034_yr11to15" ~ 2034,
    timing_group == "2039_yr16to20" ~ 2039,
    timing_group == "Not treated" ~ 9999),
    rel_trt_yr = Year - trt_yr,
    rel_tx = case_when(
      rel_trt_yr < 0 ~ "Before/untreated",
      rel_trt_yr == 0 ~ "At treatment",
      rel_trt_yr == 5 ~ "5 yrs post",
      rel_trt_yr >= 10 ~ "10+ yrs post",
      #rel_trt_yr == 15 ~ "Fifteen years post",
      .default = "Unmatched"), 
    rel_tx = as.factor(rel_tx),
    rel_tx = forcats::fct_relevel(rel_tx, 
                                  "Before/untreated", "At treatment", 
                                  "5 yrs post", "10+ yrs post"))

#graph prep
i_colors <- c("500k" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[2],
              "1m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[3],
              "2m" = plasma(n=4, begin=0.1, end = 0.9, direction = 1)[4])


### Loop and graph -----------------------------------------------------------
#page graph collector
#p_collector <- list()

regions <- res %>% pull(Region) %>% unique() %>% sort()

#REGION (outer loop)
for (r in seq_along(regions)){
  
  p_collector_region <- list()
  
  this_reg <- regions[[r]]
  
  this_reg_label <- case_when(
    this_reg == "CC" ~ "Central Coast",
    this_reg == "NC" ~ "North Coast",
    this_reg == "SC" ~ "South Coast",
    this_reg == "SN" ~ "Sierra Nevada",
  )
  
  res_r <- res %>% 
    filter(Region == this_reg)
  
  #region folder if saving out individual HaCFLs
  if (save_hacfl){
    folder_out_reg <- file.path(folder_out,
                                this_reg)
    dir.create(folder_out_reg)
  }
  
  
  #priorities in this region (all same)
  priorities <- res_r %>% pull(Priority) %>% unique() %>% sort()
  
  #PRIORITY (first inner loop)
  for (p in seq_along(priorities)){
    
    this_priority <- priorities[[p]]
    
    res_r_p <- res_r %>% 
      filter(Priority == this_priority)
    
    #get treatments in this region-priority (CAN VARY)
    trts <- res_r_p %>% pull(TxType) %>% unique() %>% sort()
    
    
    #TREATMENT TYPE (second inner loop)
    for (t in seq_along(trts)){
      
      this_trt <- trts[[t]]
      
      #get dataset for this trt
      res_r_p_t <- res_r_p %>% 
        filter(TxType == this_trt)
      
      #hacfl
      p_hacfl <- ggplot() + 
        geom_point(data=res_r_p_t,
                   mapping=aes(x=HaCFL_mar,
                               y=HaCFL_jun,
                               color=TxIntensity),
                   shape=1) + 
        geom_abline(slope=1) + 
        scale_color_manual("Intensity", values = i_colors) + 
        labs(x = "March HaCFL",
             y = "June HaCFL") + 
        theme_bw() + 
        theme(aspect.ratio = 1) + 
        facet_wrap(~rel_tx)
      
      #hacbp
      p_hacbp <- ggplot() + 
        geom_point(data=res_r_p_t,
                   mapping=aes(x=HaCBP_mar,
                               y=HaCBP_jun,
                               color=TxIntensity),
                   shape=1) + 
        geom_abline(slope=1) + 
        scale_color_manual("Intensity", values = i_colors) + 
        labs(x = "March HaCBP",
             y = "June HaCBP") + 
        theme_bw() + 
        theme(aspect.ratio = 1) + 
        facet_wrap(~rel_tx)
      
      #active_crown
      p_ac <- ggplot() + 
        geom_point(data=res_r_p_t,
                   mapping=aes(x=active_crown_mar,
                               y=active_crown_jun,
                               color=TxIntensity),
                   shape=1) + 
        geom_abline(slope=1) + 
        scale_color_manual("Intensity", values = i_colors) + 
        labs(x = "March active crown",
             y = "June active crown") + 
        theme_bw() + 
        theme(aspect.ratio = 1) + 
        facet_wrap(~rel_tx)
      
      #expFlame
      p_expflame <- ggplot() + 
        geom_point(data=res_r_p_t,
                   mapping=aes(x=expFlame_mar,
                               y=expFlame_jun,
                               color=TxIntensity),
                   shape=1) + 
        geom_abline(slope=1) + 
        scale_color_manual("Intensity", values = i_colors) + 
        labs(x = "March expFlame",
             y = "June expFlame") + 
        theme_bw() + 
        theme(aspect.ratio = 1) + 
        facet_wrap(~rel_tx)
      
      #expBurn
      p_expburn <- ggplot() + 
        geom_point(data=res_r_p_t,
                   mapping=aes(x=expBurn_mar,
                               y=expBurn_jun,
                               color=TxIntensity),
                   shape=1) + 
        geom_abline(slope=1) + 
        scale_color_manual("Intensity", values = i_colors) + 
        labs(x = "March expBurn",
             y = "June expBurn") + 
        theme_bw() + 
        theme(aspect.ratio = 1) + 
        facet_wrap(~rel_tx)
      
      #expPcActive
      p_exppcactive <- ggplot() + 
        geom_point(data=res_r_p_t,
                   mapping=aes(x=expPcActive_mar,
                               y=expPcActive_jun,
                               color=TxIntensity),
                   shape=1) + 
        geom_abline(slope=1) + 
        scale_color_manual("Intensity", values = i_colors) + 
        labs(x = "March expPcActive",
             y = "June expPcActive") + 
        theme_bw() + 
        theme(aspect.ratio = 1) + 
        facet_wrap(~rel_tx)
      

      
      # INDIVIDUAL 
      if (save_hacfl){
        #add titles
        p_hacfl_ind <- p_hacfl + 
          labs(title = paste0(this_reg_label, " - ",
                              this_priority,
                              " ", this_trt),
               subtitle = "Comparing March and June runs",
               caption = "Above the line indicates June run had a higher value")
        
        # file name change for nonburn only or not
        if (only_nonburn){
          fn_ind <- paste0(this_reg, "_hacfl_", 
                           this_priority, "_", 
                           this_trt, "_mar_jun_NB.jpg")
        } else {
          fn_ind <- paste0(this_reg, "_hacfl_", 
                           this_priority, "_", 
                           this_trt, "_mar_jun.jpg")
        }
        
        ggsave(plot=p_hacfl_ind, 
               file.path(folder_out_reg, 
                         fn_ind),
               height = 6, width=6, units = "in")
        
        
      } #end save_hacfl
      
      
      
      # FULL PAGE SAVES
      
      all_title <- paste0(this_reg_label, " - ",
                           "Priority: ", this_priority,
                           ", Treatment: ", this_trt, 
                          " - Comparing March and June runs") 
      all_caption <- "Above the diagonal line indicates June run had a higher value"
      
      #all together on one page
      p_page <- arrangeGrob(p_hacfl, p_expflame,
                            p_hacbp, p_expburn, 
                            p_ac, p_exppcactive,
                            ncol = 2, 
                            top = all_title,
                            bottom = all_caption)
      
      # #maybe should have flattened loop instead of this fun index
      # i <- ((r-1)*length(trts)*length(priorities)) + ((p-1)*length(trts)) + t
      # p_collector[[i]] <- p_page
      
      j <- ((p-1)*length(trts)) + t
      p_collector_region[[j]] <- p_page
      
      
      
    } # end trt
  } # end priority 
  
  #save file per region
  plots_to_pdf_region <- marrangeGrob(p_collector_region, 
                                      nrow = 1, ncol = 1,
                                      top = NULL)
  
  if (only_nonburn){
    fn_reg <- paste0(this_reg, "_run_comparisons_march_june_nonburn_only.pdf")
  } else {
    fn_reg <- paste0(this_reg, "_run_comparisons_march_june.pdf")
  }
  
  ggsave(plots_to_pdf_region, 
         filename = file.path(folder_out, fn_reg),
         height = 12, width = 10, units = c("in"))
  
  
} # end region

# plots_to_pdf <- marrangeGrob(p_collector, 
#                              nrow = 1, ncol = 1,
#                              top = NULL)
# 
# if (only_nonburn){
#   fn <- "run_comparisons_march_june_nonburn_only.pdf"
# } else {
#   fn <- "run_comparisons_march_june.pdf"
# }
# 
# ggsave(plots_to_pdf, 
#        filename = file.path(folder_out, fn),
#        height = 12, width = 10, units = c("in"))
