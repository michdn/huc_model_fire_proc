# 500k vs 2m scatter plots per scenario
# Designed to run per region (or by regions)

# Separated by region, priority
# Facetted by trt type

# For final year 2039


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### Base Data import -------------------------------------------

output_folder <- file.path('plots', 'scatter')
#dir.create(output_folder, recursive = TRUE) #create with region in loop


res <- read_csv(file.path("results",
                               "datacube", 
                               "datacube_interim_sc_cc_sn_20240403.csv")) %>% 
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
                hacfl_500k, hacfl_1m, hacfl_2m)

res2039_trim


### Loop through available regions -----------------------------------

regions <- res2039_trim %>% pull(Region) %>% unique()

#REGION (outer loop)
for (r in seq_along(regions)){
  
  this_reg <- regions[[r]]
  
  res_r <- res2039_trim %>% 
    filter(Region == this_reg)
  
  
  #output 
  plot_folder <- file.path(output_folder, this_reg)
  dir.create(plot_folder, recursive = TRUE)
  
  
  #priorities in this region (all same)
  priorities <- res_r %>% pull(Priority) %>% unique()
  
  #PRIORITY (inner loop)
  for (p in seq_along(priorities)){
    
    this_priority <- priorities[[p]]
    
    res_r_p <- res_r %>% 
      filter(Priority == this_priority)
    
    #plots will facet by trt type
    
    #expBurn
    plot_yr20_expBurn_p <- ggplot() + 
      geom_point(data=res_r_p, 
                 aes(x=expBurn_500k, y=expBurn_2m),
                 shape=1, color="blue") +
      theme(aspect.ratio = 1) + 
      geom_abline(intercept=0, slope=1) + 
      stat_smooth(data=res_r_p,
                  aes(x=expBurn_500k, y=expBurn_2m),
                  method=lm,
                  geom="smooth",
                  formula = 'y ~ x',
                  fullrange = TRUE) + 
      labs(title=paste("Year 2039 ExpBurn:", this_reg, this_priority), 
           x="500k Acres Treated",
           y="2.3 Million Acres Treated") +
      facet_wrap(~TxType)
    #plot_yr20_expBurn_p
    
    fn_expburn <- paste0("year2039_expBurn_500kvs2m_scatter_", 
                 this_priority, 
                 ".jpg")
    
    ggsave(plot = plot_yr20_expBurn_p,
           filename = fn_expburn,
           path = plot_folder,
           width = 6, height = 3, units = "in")
    
    #expFlame
    plot_yr20_expFlame_p <- ggplot() + 
      geom_point(data=res_r_p, 
                 aes(x=expFlame_500k, y=expFlame_2m),
                 shape=1, color="blue") +
      theme(aspect.ratio = 1) + 
      geom_abline(intercept=0, slope=1) + 
      stat_smooth(data=res_r_p,
                  aes(x=expFlame_500k, y=expFlame_2m),
                  method=lm,
                  geom="smooth",
                  formula = 'y ~ x',
                  fullrange = TRUE) + 
      labs(title=paste("Year 2039 ExpFlame:", this_reg, this_priority), 
           x="500k Acres Treated",
           y="2.3 Million Acres Treated") +
      facet_wrap(~TxType)
    #plot_yr20_expFlame_p
    
    fn_expFlame <- paste0("year2039_expFlame_500kvs2m_scatter_", 
                 this_priority, 
                 ".jpg")
    
    ggsave(plot = plot_yr20_expFlame_p,
           filename = fn_expFlame,
           path = plot_folder,
           width = 6, height = 3, units = "in")
    
    
    # log scale HaCFL
    plot_yr20_hacfl_p_log <- ggplot() + 
      geom_point(data=res_r_p, 
                 aes(x=log10(hacfl_500k), y=log10(hacfl_2m)),
                 shape=1, color="blue") +
      theme(aspect.ratio = 1) + 
      geom_abline(intercept=0, slope=1) + 
      # stat_smooth(data=res_r_p,
      #             aes(x=log10(hacfl_500k), y=log10(hacfl_2m)),
      #             method=lm,
      #             geom="smooth",
      #             formula = 'y ~ x',
      #             fullrange = TRUE) + 
      labs(title=paste("Year 2039:", this_reg, this_priority)) +
      facet_wrap(~TxType)

    fn_hacfl_log <- paste0("year2039_hacfl_500kvs2m_scatter_", 
                 this_priority, "_log10",
                 ".jpg")
    
    ggsave(plot = plot_yr20_hacfl_p_log,
           filename = fn_hacfl_log,
           path = plot_folder,
           width = 6, height = 3, units = "in")
    
    # log scale HaCBP
    plot_yr20_hacbp_p_log <- ggplot() + 
      geom_point(data=res_r_p, 
                 aes(x=log10(hacbp_500k), y=log10(hacbp_2m)),
                 shape=1, color="blue") +
      theme(aspect.ratio = 1) + 
      geom_abline(intercept=0, slope=1) + 
      # stat_smooth(data=res_r_p,
      #             aes(x=log10(hacbp_500k), y=log10(hacbp_2m)),
      #             method=lm,
      #             geom="smooth",
      #             formula = 'y ~ x',
      #             fullrange = TRUE) + 
      labs(title=paste("Year 2039:", this_reg, this_priority)) +
      facet_wrap(~TxType)

    fn_hacbp_log <- paste0("year2039_hacbp_500kvs2m_scatter_", 
                     this_priority, "_log10",
                     ".jpg")
    
    ggsave(plot = plot_yr20_hacbp_p_log,
           filename = fn_hacbp_log,
           path = plot_folder,
           width = 6, height = 3, units = "in")
    
    # HaCFL
    plot_yr20_hacfl_p <- ggplot() + 
      geom_point(data=res_r_p, 
                 aes(x=hacfl_500k, y=hacfl_2m),
                 shape=1, color="blue") +
      theme(aspect.ratio = 1) + 
      geom_abline(intercept=0, slope=1) + 
      # stat_smooth(data=res_r_p,
      #             aes(x=hacfl_500k, y=hacfl_2m),
      #             method=lm,
      #             geom="smooth",
      #             formula = 'y ~ x',
      #             fullrange = TRUE) + 
      labs(title=paste("Year 2039:", this_reg, this_priority)) +
      facet_wrap(~TxType)
    
    fn_hacfl <- paste0("year2039_hacfl_500kvs2m_scatter_", 
                           this_priority, 
                           ".jpg")
    
    ggsave(plot = plot_yr20_hacfl_p,
           filename = fn_hacfl,
           path = plot_folder,
           width = 6, height = 3, units = "in")
    
    # HaCBP
    plot_yr20_hacbp_p <- ggplot() + 
      geom_point(data=res_r_p, 
                 aes(x=hacbp_500k, y=hacbp_2m),
                 shape=1, color="blue") +
      theme(aspect.ratio = 1) + 
      geom_abline(intercept=0, slope=1) + 
      # stat_smooth(data=res_r_p,
      #             aes(x=hacbp_500k, y=hacbp_2m),
      #             method=lm,
      #             geom="smooth",
      #             formula = 'y ~ x',
      #             fullrange = TRUE) + 
      labs(title=paste("Year 2039:", this_reg, this_priority)) +
      facet_wrap(~TxType)
    
    fn_hacbp <- paste0("year2039_hacbp_500kvs2m_scatter_", 
                           this_priority, 
                           ".jpg")
    
    ggsave(plot = plot_yr20_hacbp_p,
           filename = fn_hacbp,
           path = plot_folder,
           width = 6, height = 3, units = "in")
    
    
    # # HaCFL / HaCBP
    # plot_yr20_hacflhacbp_p <- ggplot() + 
    #   geom_point(data=res_r_p, 
    #              aes(x=(hacfl_500k/hacbp_500k), y=(hacfl_2m/hacbp_2m)),
    #              shape=1, color="blue") +
    #   theme(aspect.ratio = 1) + 
    #   geom_abline(intercept=0, slope=1) + 
    #   labs(title=paste("Year 2039:", this_reg, this_priority)) +
    #   facet_wrap(~TxType)
    # 
    # fn_hacflhacbp <- paste0("year2039_hacflhacbp_500kvs2m_scatter_", 
    #                    this_priority, 
    #                    ".jpg")
    # 
    # ggsave(plot = plot_yr20_hacflhacbp_p,
    #        filename = fn_hacflhacbp,
    #        path = plot_folder,
    #        width = 6, height = 3, units = "in")
    
    
  } # end p priority
} # end r region
