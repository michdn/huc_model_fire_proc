
### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### Data ---------------------------------------------------------

zc <- readRDS(file.path("qa",
                        "SN_fuel_adjective_comparison_threshold1.01.RDS"))

hucs_sample <- read_csv(file.path('qa', 'samplehucs.csv')) 
hucs_sample_new <- read_csv(file.path('qa', 'sampledhucs_topthreegroups.csv')) 

sample_hucs <- hucs_sample_new %>% 
  filter(HUC12 %in% hucs_sample$HUC12) %>% 
  filter(Region == "SN") %>% 
  filter(grouping == "Fire")


### Explore -----------------------------------------------------------

zc %>% filter(HUC12 == "180201250901", Priority == "Fire")

zc %>% filter(HUC12 == "180201210802", Priority == "Fire", TxType == "trt1")

zc %>% filter(HUC12 == "180200030405", Priority == "Fire", Year == 2034)


ggplot() + 
  geom_histogram(data = zc,
                 mapping = aes(x=mean_diff)) + 
  geom_vline(xintercept = 0, color = "red") +
  labs(title = paste0("Mean difference in adjective value for ", 
                      nrow(zc), 
                      " inversions")) 

### Filter and figure out what to graph ------------------------------

fire_samples <- sample_hucs %>% pull(HUC12)

zcs <- zc %>% 
  filter(HUC12 %in% fire_samples,
         Priority == "Fire") %>% 
  arrange(HUC12, TxType, TxIntensity)

zcs


zcs_trim <- zcs %>% 
  mutate(expFlame_comp = case_when(
    TxIntensity == "2m" ~ expFlame_2m,
    TxIntensity == "1m" ~ expFlame_1m,
    TxIntensity == "500k" ~ expFlame_500k
  )) %>% 
  dplyr::select(HUC12, trt_yr, Priority, TxType, TxIntensity, Year,
                mean_diff, VL_diff, L_diff, M_diff, H_diff, VH_diff, X_diff,
                expFlame_base, expFlame_comp)

write_csv(zcs_trim, file.path("qa", "adjective_comp_examples.csv"))




