# takes result for fuels_grps_ts.R and makes timeseries charts


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)
  

## Data ------------------------------------------------------  

sc <- readRDS(file = file.path('qa','fuel_timing',
                               'sc_canopy.RDS'))
reg_block <- sc


year_breaks <- c(2024, 2029, 2034, 2044)


priorities <- reg_block %>% pull(priority) %>% unique()
trts <- reg_block %>% pull(trt) %>% unique()


## loop on priorities / trt -------------------------------

for (i in seq_along(priorities)){
  
  this_priority <- priorities[[i]]
  
  this_rb_p <- reg_block %>% 
    filter(priority == this_priority)
  
  for (j in seq_along(trts)){
    
    this_trt <- trts[[j]]
    
    this_rb_p_t <- this_rb_p %>% 
      filter(trt == this_trt)
    
    this_region <- this_rb_p_t %>% 
      pull(region) %>% 
      unique() # only one
    
    name_r_p_t <- paste0(this_region, " ", 
                       this_priority, " ", 
                       this_trt)
    
    this_plot <- ggplot() + 
      geom_line(
        data = this_rb_p_t %>% 
          filter(layer == "cc"),
        mapping = aes(x = as.numeric(year), y = summary,
                      color = intensity)) +
      scale_x_continuous(breaks = year_breaks) + 
      facet_wrap(~paste0('Block grp', tx_code, "-", tx_yr_lyr)) +
      labs(title = paste0(name_r_p_t, ": ", "Canopy Cover (cc)"),
           subtitle = "Treatment blocks summary",
           x = "Year",
           y = "Sum of CC")
    
    ggsave(filename = file.path('qa','fuel_timing', 
                                'autoplots',
                                paste0(name_r_p_t, '_cc.jpg')),
           plot = this_plot, 
           width = 6, height = 4, units = "in")
    
  }
}



## individual priority-trt ----------------------------------


scf6 <- readRDS(file = file.path('qa','fuel_timing',
                                 'sc_fire_trt6_attempt1.RDS'))
scf4 <- readRDS(file = file.path('qa','fuel_timing',
                                 'sc_fire_trt4_attempt1.RDS'))


year_breaks <- c(2024, 2029, 2034, 2044)

## TS plots -------------------------------------------------

scf6

#cbd, cbh, cc, cht

scf6_plot <- ggplot(data = scf6 %>%
         filter(layer == "cc"),
       mapping = aes(x = as.numeric(year), y = summary_blks,
                     color = intensity)) +
  geom_line() +
  scale_x_continuous(breaks = year_breaks) +
  facet_wrap(~paste0('block grp', tx_yr_code, "-", tx_yr_lyr)) +
  labs(title = "SC Fire trt 6: ",
       subtitle = "Treatment blocks summary")

ggsave(filename = file.path('qa','fuel_timing', 
                            'sc_fire_trt6_cc_attempt1.jpg'),
       plot = scf6_plot, 
       width = 6, height = 4, units = "in")

ggplot(data = scf6 %>%
         filter(layer == "cc"),
       mapping = aes(x = as.numeric(year), y = summary_blks,
                     color = intensity)) +
  geom_line() +
  scale_x_continuous(breaks = year_breaks) +
  facet_wrap(~paste0('block grp', tx_yr_code, "-", tx_yr_lyr)) +
  labs(title = "SC Fire trt 4: ",
       subtitle = "Treatment blocks summary")


