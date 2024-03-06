# takes result for fuels_grps_ts.R and makes timeseries charts

# NOTE! 
# There are DIFFERENT treatment blocks for 500k, 1m, 2m, 
#  so there are not comparable exactly like HUC summaries


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)
  

## Data ------------------------------------------------------  

sc <- readRDS(file = file.path('qa','fuel_timing',
                               'blocks_sc_fml.RDS'))
nc <- readRDS(file = file.path('qa','fuel_timing',
                               'blocks_nc_fml.RDS'))
sn <- readRDS(file = file.path('qa','fuel_timing',
                               'blocks_sn_fml.RDS'))
cc <- readRDS(file = file.path('qa','fuel_timing',
                               'blocks_cc_fml.RDS'))

reg_block <- nc


top3 <- reg_block %>% 
  #relevant fields
  dplyr::select(scenario, 
                tx_code, tx_type, tx_yr_lyr, 
                year, 
                mode, val_count) %>% 
  #arrange to view and confirm
  arrange(scenario, 
          tx_code, tx_type, tx_yr_lyr, 
          year, 
          desc(val_count)) %>%
  #want top 3 by these groups
  group_by(scenario, year,
           tx_code, tx_type, tx_yr_lyr) %>% 
  arrange(desc(val_count)) %>% 
  slice(1:3) 

top3

top3 %>% 
  #add a field that will become a field name when pivoted
  mutate(mode_order = c('mode1', 'mode2', 'mode3')) %>% 
  #pivot wide
  pivot_wider(id_cols = c(scenario, tx_code, tx_type, tx_yr_lyr, year),
              names_from = mode_order,
              names_prefix = 'mode',
              values_from = mode) %>% 
  #arrange by timing group
  arrange(scenario, 
          tx_code, tx_type, tx_yr_lyr,
          year) %>% View()






# priorities <- reg_block %>% pull(priority) %>% unique()
# trts <- reg_block %>% pull(trt) %>% unique()
# 
# ## loop on priorities / trt -------------------------------
# 
# for (i in seq_along(priorities)){
#   
#   this_priority <- priorities[[i]]
#   
#   this_rb_p <- reg_block %>% 
#     filter(priority == this_priority)
#   
#   for (j in seq_along(trts)){
#     
#     this_trt <- trts[[j]]
#     
#     this_rb_p_t <- this_rb_p %>% 
#       filter(trt == this_trt)
#     
#     this_region <- this_rb_p_t %>% 
#       pull(region) %>% 
#       unique() # only one
#     
#     name_r_p_t <- paste0(this_region, " ", 
#                        this_priority, " ", 
#                        this_trt)
#     
#     this_plot <- ggplot() + 
#       geom_line(
#         data = this_rb_p_t %>% 
#           filter(layer == "cc"),
#         mapping = aes(x = as.numeric(year), y = summary,
#                       color = intensity)) +
#       scale_x_continuous(breaks = year_breaks) + 
#       facet_wrap(~paste0('Block grp', tx_code, "-", tx_yr_lyr)) +
#       labs(title = paste0(name_r_p_t, ": ", "Canopy Cover (cc)"),
#            subtitle = "Treatment blocks summary",
#            x = "Year",
#            y = "Sum of CC",
#            caption = "Note: Different intensities have different treatment blocks.")
#     
#     ggsave(filename = file.path('qa','fuel_timing', 
#                                 'autoplots',
#                                 paste0(name_r_p_t, '_cc.jpg')),
#            plot = this_plot, 
#            width = 6.5, height = 4, units = "in")
#     
#   } # end j trt
# } # end i priority
# 
# 
# 
