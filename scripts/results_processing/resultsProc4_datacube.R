# Short script to bind all region absolute metrics together for final datacube




### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)


### User settings ---------------------------------------------

input_folder <- file.path("results", "absolute") 

output_folder <- file.path("results", "datacube")
dir.create(output_folder, recursive = TRUE) 


#region results
sn <- read_csv(file.path(input_folder, "SN_absolute_202405xx.csv")) %>% 
  mutate(HUC12 = as.character(HUC12))

sc <- read_csv(file.path(input_folder, "SC_absolute_202406xx.csv")) %>% 
  mutate(HUC12 = as.character(HUC12))

cc <- read_csv(file.path(input_folder, "CC_absolute_202406xx.csv")) %>% 
mutate(HUC12 = as.character(HUC12))

nc <- read_csv(file.path(input_folder, "NC_absolute_202406xx.csv")) %>% 
  mutate(HUC12 = as.character(HUC12))


### Bind and save -------------------------------------------

cube <- bind_rows(sn, sc, cc, nc)

stamp <- format(Sys.time(), "%Y%m%d")

write_csv(cube,
          file.path(output_folder,
                    paste0('datacube_', "interim_", stamp, '.csv')))
