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
sc <- read_csv(file.path(input_folder, "SC_absolute_20240328.csv")) %>% 
  mutate(HUC12 = as.character(HUC12))

cc <- read_csv(file.path(input_folder, "CC_absolute_20240328.csv")) %>% 
mutate(HUC12 = as.character(HUC12))

#nc

#sn

### Bind and save -------------------------------------------

cube <- bind_rows(sc, cc) #, nc, sn)

stamp <- format(Sys.time(), "%Y%m%d")

write_csv(cube,
          file.path(output_folder,
                    paste0('datacube_', "interim_sc_cc_", stamp, '.csv')))
