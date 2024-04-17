
### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)


zc <- readRDS(file.path("qa",
                        "SN_fuel_adjective_comparison_threshold1.01.RDS"))


zc %>% filter(HUC12 == "180201250901", Priority == "Fire")

zc %>% filter(HUC12 == "180201210802", Priority == "Fire", TxType == "trt1")

zc %>% filter(HUC12 == "180200030405", Priority == "Fire", Year == 2034)

