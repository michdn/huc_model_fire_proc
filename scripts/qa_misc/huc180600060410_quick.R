if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

res <- read_csv(file.path("results",
                               "datacube", 
                               "datacube_interim_sc_cc_20240328.csv")) %>% 
  mutate(HUC12 = as.character(HUC12))


res %>% filter(HUC12 == "180600060410") %>% View()
#res %>% filter(HUC12 == "180600060410") %>% write_csv("huc180600060410_datacube.csv")


res %>% 
  filter(HUC12 == "180600060410") %>%
  filter(Year == 2039) %>% View()
