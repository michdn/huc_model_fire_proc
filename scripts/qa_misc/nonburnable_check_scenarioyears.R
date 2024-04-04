# check if nonburnable changes in any scenario-year

# Abandoned as we didn't need it. No changes affected unburnable to/from, so good to use base fm40


if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  raster,
  terra)


### User settings -------------------------------------

region_to_run <- "SN" #"CC", "SN", "NC"

in_folder <- file.path("E:", "MAS", "blended_rasters", region_to_run)

### get fml -------------------------------------------

fml_files <- list.files(in_folder,
                        full.names = TRUE,
                        recursive = TRUE,
                        pattern = "RunID.+_fml_.+tif$")

### raster -------------------------------------------

# make stack? 
# count number of times value 98 is in that pixel
# see if same value everywhere? 
# check 91, 92, 93, 98, 99

#stack equivalent
#terra::rast(list(x1,x2,x3))