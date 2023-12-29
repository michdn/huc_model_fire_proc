# looking at example huc
# 180701060203

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  stars,
  terra)

#install.packages("rgdal", repos = "http://R-Forge.R-project.org")

### User settings ---------------------------------------------

base_folder <- "hucs_val"

huc <- "180701060203"

run <- "RunID19_SC_WUI_500k_trt1"
year <- "2024"

### reading in ------------------------------------------------

huc_folder <- file.path(base_folder, paste0("huc_", huc))
inputs_folder <- file.path(huc_folder, "inputs")
topo_folder <- file.path(inputs_folder, "topography")
fuels_folder <- file.path(huc_folder, "fuels")

indicator <- terra::rast(file.path(inputs_folder,
                                   "huc_indicator.tif"))

aspect <- terra::rast(file.path(topo_folder, 
                                "aspect.tif"))
elev <- terra::rast(file.path(topo_folder, 
                                "elevation.tif"))
slope <- terra::rast(file.path(topo_folder, 
                                "slope.tif"))

cc <- terra::rast(file.path(fuels_folder, run, year, "cc.tif"))
cht <- terra::rast(file.path(fuels_folder, run, year, "cht.tif"))
cbh <- terra::rast(file.path(fuels_folder, run, year, "cbh.tif"))
cbd <- terra::rast(file.path(fuels_folder, run, year, "cbd.tif"))
fml <- terra::rast(file.path(fuels_folder, run, year, "fml.tif"))

### checking ----------------------------------------------------

indicator
aspect
elev
slope
cc
cht
cbh
cbd
fml

unique(fml)
