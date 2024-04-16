
### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)


zc <- readRDS(file.path("qaqc",
                        "sn_intensity_inversion",
                        "SN_fuel_adjective_comparison_threshold1.5.RDS"))


zc %>% filter(HUC12 == "180201250901", Priority == "Fire")

zc %>% filter(HUC12 == "180201210802", Priority == "Fire", TxType == "trt1")
