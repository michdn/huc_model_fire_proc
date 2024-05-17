# exploring gridfire weather sampling

# comparing gridfire and FVS 'static' weather



### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  gridExtra)


### User settings ------------------------------------------------

reg_group <- "SN" 


### Data -----------------------------------------------------------

# Static weather from Dave
static <- tibble(
  "static_relative_humidity" = 32.6318,
  "static_temperature" = 75.8051,
  "static_wind_from_direction" = 332.6178,
  "static_wind_speed_20ft" = 4.1907,
  "static_foliar_moisture" = 82.2)


scen <- readRDS(file.path("results", "misc", 
                           paste0(reg_group, "_fire_results_weather.RDS"))) %>% 
  as_tibble() %>% 
  rename(foliar_moisture = foliar_moisutre)

bl <- readRDS(file.path("results", "misc",
                        paste0(reg_group, "bl_fire_results_weather.RDS"))) %>% 
  as_tibble() %>% 
  rename(foliar_moisture = foliar_moisutre)

bw <- readRDS(file.path("results", "misc",
                        paste0(reg_group, "bw_fire_results_weather.RDS"))) %>% 
  as_tibble() %>% 
  rename(foliar_moisture = foliar_moisutre)

all <- bind_rows(scen, bl, bw) 


hucs_sample <- read_csv(file.path('qa', 'samplehucs.csv')) 
hucs_sample_new <- read_csv(file.path('qa', 'sampledhucs_topthreegroups.csv')) 

sample_hucs <- hucs_sample_new %>% 
  filter(HUC12 %in% hucs_sample$HUC12) %>% 
  filter(Region == reg_group) %>% 
  filter(grouping == "Fire")

### Exploring -----------------------------------------------------------

scen %>%
  dplyr::select(HUC12, fire_i, array_index) %>%
  group_by(fire_i, array_index) %>%
  summarize(count = n())
#same index used for each fire
# same index irrespective of scenario, huc, year
# so when weather pool is the same, weather is same
# values vary across (some) years, across HUCs
# otherwise weather is the same per fire (in a HUC) across scenarios

scen %>%
  filter(fire_i == 0) %>%
  dplyr::select(HUC12, fire_i, mas_scenario, Year, array_index, temperature) %>%
  group_by(HUC12, fire_i, mas_scenario, array_index, temperature) %>%
  summarize(count = n())
# 2 years same data 2024, 2029.


bl %>% 
  dplyr::select(HUC12, fire_i, array_index) %>%
  group_by(fire_i, array_index) %>%
  summarize(count = n())

bw %>% 
  dplyr::select(HUC12, fire_i, array_index) %>%
  group_by(fire_i, array_index) %>%
  summarize(count = n())


all %>% 
  dplyr::select(HUC12, fire_i, array_index) %>%
  group_by(fire_i, array_index) %>%
  summarize(count = n())

index_count <- all %>% 
  group_by(fire_i, array_index) %>%
  summarize(count = n(), .groups = "drop") %>% 
  group_by(array_index) %>% 
  summarize(count = n()) %>% 
  arrange(array_index)

all %>% select(foliar_moisture) %>% group_by(foliar_moisture) %>% summarize(count = n())
# # A tibble: 1 x 2
# foliar_moisture    count
# <dbl>    <int>
#   1            82.2 26633600

### Example HUCs -----------------------------------------------------------

subset <- bl %>% 
  filter(HUC12 %in% sample_hucs$HUC12) %>% 
  dplyr::select(HUC12, Year, fire_i, array_index, 
                relative_humidity, temperature, wind_from_direction, 
                wind_speed_20ft, foliar_moisture) %>% 
  bind_cols(static)

subset %>% 
  group_by(HUC12, Year) %>% 
  summarize(ave_rel_hum = mean(relative_humidity),
            ave_temp = mean(temperature),
            ave_wind_speed = mean(wind_speed_20ft),
            ave_moisture = mean(foliar_moisture))


#loop over HUCs

for (i in 1:nrow(sample_hucs)){
  
  this_huc <- sample_hucs[i,] %>% pull(HUC12)
  
  this_data <- subset %>% 
    filter(HUC12 == this_huc)
  
  p1 <- ggplot() +
    geom_boxplot(data = this_data,
                 mapping = aes(x=Year,
                               y=relative_humidity)) +
    geom_point(data = this_data,
               mapping = aes(x = Year, y = static_relative_humidity),
               color = "red3", size = 2) + 
    labs(title = "Relative humidity")
    
  p2 <- ggplot() +
    geom_boxplot(data = this_data,
                 mapping = aes(x=Year,
                               y=temperature)) +
    geom_point(data = this_data,
               mapping = aes(x = Year, y = static_temperature),
               color = "red3", size = 2) + 
    labs(title = "Temperature")
  
  p3 <- ggplot() +
    geom_boxplot(data = this_data,
                 mapping = aes(x=Year,
                               y=wind_speed_20ft)) +
    geom_point(data = this_data,
               mapping = aes(x = Year, y = static_wind_speed_20ft),
               color = "red3", size = 2) + 
    labs(title = "Wind speed 20ft")
  
  p4 <- ggplot() +
    geom_boxplot(data = this_data,
                 mapping = aes(x=Year,
                               y=wind_from_direction)) +
    geom_point(data = this_data,
               mapping = aes(x = Year, y = static_wind_from_direction),
               color = "red3", size = 2) + 
    labs(title = "Wind from direction")
  
  p5 <- arrangeGrob(p1, p2, p3, p4, nrow = 2,
               top = paste0("HUC ", this_huc))
  
  ggsave(plot = p5, 
         filename = file.path("plots", 
                              "weather",
                              paste0(reg_group, "_", this_huc,
                                     "_weather_static_comparison.jpg")),
         width = 6,
         height = 6,
         units = "in")
  
}



  
