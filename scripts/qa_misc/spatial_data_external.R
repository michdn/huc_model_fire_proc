#create basic shp/geopackage of HUC12 spatial data

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf)

### Base Data import -------------------------------------------
#spatial
hucs_orig <- st_read("data/data_huc/TxHucsTimingGroups.shp")

### Trimming ---------------------------------------------------

#add two letter region code 
xwalk <- hucs_orig %>% 
  dplyr::select(reginNm) %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  arrange(reginNm) %>% 
  #manually add code that is used in fuel file names
  #alpha sorted Central Coast, North Coast, Sierra Nevada, South Coast
  add_column(regCode = c("CC", "NC", "SN", "SC"))

#create trimmed 
hucs <- hucs_orig %>% 
  #join to get region code
  left_join(xwalk, by = "reginNm") %>% 
  #subset to basic fields
  dplyr::select(huc12, 
                reginNm, regCode, name, states) %>% 
  rename(state = states,
         #going to break my scripts, but matches exactly to cube
         HUC12 = huc12,
         #for consistency with regCode
         regName = reginNm) %>% 
  arrange(regCode, HUC12)


### Saving ---------------------------------------------------------

head(hucs)

st_write(hucs,
         dsn="results/spatial/huc12_watersheds.gpkg",
         layer="huc12")

st_write(hucs,
         dsn="results/spatial/huc12_watersheds.shp")


