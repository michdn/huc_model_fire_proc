# Short script to bind all region absolute metrics together for final datacube




### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)


### User settings ---------------------------------------------

input_folder <- file.path('results', 'absolute') 

output_folder <- file.path('results')

#region results
sc <- read_csv(file.path(input_folder, 'SC_absolute_TEST20240307.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

#cc

#nc

#sn

### Bind and save -------------------------------------------

cube <- bind_rows(sc, cc, nc, sn)

stamp <- format(Sys.time(), "%Y%m%d")

write_csv(cube,
          file.path(output_folder,
                    paste0('datacube_', stamp, '.csv')))
