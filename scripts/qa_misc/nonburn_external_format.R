# formatting script to save nonburn/coastal issue list for external


if (!require("pacman")) install.packages("pacman")
 pacman::p_load(
   tidyverse,
   terra)

nb_hucs <- readRDS("data/nonburnable_rerun_list.RDS")

nb_hucs %>% group_by(region) %>% summarize(count=n())

nb_formatted <- nb_hucs %>% 
  rename(HUC12 = huc12,
         Region = region) %>% 
  dplyr::select(Region, HUC12, nonburn_perc, hucAc_orig, hucAc, coastal) %>% 
  replace_na(list(coastal=FALSE)) %>% 
  arrange(Region, desc(nonburn_perc))

write_csv(nb_formatted, 
          file=file.path("data", "huc12_nonburn_corrections.csv"))

