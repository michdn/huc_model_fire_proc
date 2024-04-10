# Short script to bind all region absolute metrics together for final datacube




### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)


### User settings ---------------------------------------------

input_folder <- file.path("results", "absolute") 

output_folder <- file.path("results", "datacube")
dir.create(output_folder, recursive = TRUE) 


#region results, PREnonburn fix
sc <- read_csv(file.path(input_folder, "SC_absolute_20240410.csv")) %>% 
  mutate(HUC12 = as.character(HUC12))

cc <- read_csv(file.path(input_folder, "CC_absolute_20240410.csv")) %>% 
mutate(HUC12 = as.character(HUC12))

sn <- read_csv(file.path(input_folder, "SN_absolute_20240410.csv")) %>% 
  mutate(HUC12 = as.character(HUC12))


#region results, POSTnonburn fix
#nc

#region results, POSTnonburn fix, ONLY nonburn issue HUCs
# will need to supercede what is in sc, cc, and sn
#scnb
#ccnb
#snnb

#baseline and baseweather, POSTnonburn fix

snbl <- read_csv(file.path(input_folder, "SNbl_absolute_20240410.csv")) %>% 
  mutate(HUC12 = as.character(HUC12))
#snbw

### nonburn correction -------------------------------------

#take what's in *nb over the previous region results

# nbs <- bind_rows(scnb, ccnb, snnb)
# nb_hucs <- nbs %>% pull(HUC12) %>% unique()
# 
# with_bad <- bind_rows(sc, cc, sn)
# only_good <- with_bad %>% 
#   filter(!HUC12 %in% nb_hucs)
# 
# corrected <- bind_rows(only_good, nbs)


### Bind and save -------------------------------------------

cube <- bind_rows(sc, cc, sn, snbl) #, nc) 

stamp <- format(Sys.time(), "%Y%m%d")

write_csv(cube,
          file.path(output_folder,
                    paste0('datacube_', "interim_sc_cc_sn_snbl", stamp, '.csv')))
