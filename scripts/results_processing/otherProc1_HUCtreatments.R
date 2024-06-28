# Treated area per HUC
# extracting treatment footprints from Kyle's MAS_FCATtreatments
#  per HUC (to add as 'treated area' information)

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  future.apply)

options(future.globals.onReference = "warning")

### Data  --------------------------------------------------

#huc shapefile, only needed
hucs_all <- sf::st_read(file.path("data", 
                                  "data_huc", 
                                  "TxHucsTimingGroups.shp"))

# treatment shapefile folder
folder_tx <- file.path("data",
                       "MAS_FCATtreatments")

files_tx <- list.files(folder_tx,
                       full.names = TRUE,
                       pattern = "shp$")

### Prep --------------------------------------------------

# Will need to match timing groups between HUC-priority and treatment zone.
# Need matching fields. 
# Crosswalk huc value to tx value

# hucs timeWui: "yr1to5_16to20" "yr6to10"       "yr11to15"      "notTreated"
# tx WUI: "year_int_1-5_16-20"  "year_int_6-10"      "year_int_11-15"     

# hucs timeFir: "yr1to5"   "yr6to10"  "yr11to15" "yr16to20"
# tx Fire: "year_int_1-5"   "year_int_6-10"  "year_int_11-15" "year_int_16-20"

# hucs timHybrd: "yr1to5"   "yr6to10"  "yr11to15" "yr16to20"
# tx RFFC: "year_int_1-5"   "year_int_6-10"  "year_int_11-15" "year_int_16-20"

hucs_all <- hucs_all %>% 
  mutate(
    wuit = case_when(
      timeWui == "yr1to5_16to20" ~ "year_int_1-5_16-20",
      timeWui == "yr6to10" ~ "year_int_6-10",
      timeWui == "yr11to15" ~ "year_int_11-15",
      #note will not match tx because simply not there, but want to identify later
      timeWui == "notTreated" ~ timeWui,
      .default = "WUI_UNMATCHED"),
    firet = case_when(
      timeFir == "yr1to5" ~ "year_int_1-5",
      timeFir == "yr6to10" ~ "year_int_6-10",
      timeFir == "yr11to15" ~ "year_int_11-15",
      timeFir == "yr16to20" ~ "year_int_16-20",
      .default = "FIRE_UNMATCHED"),
    rffct = case_when(
      tmHybrd == "yr1to5" ~ "year_int_1-5",
      tmHybrd == "yr6to10" ~ "year_int_6-10",
      tmHybrd == "yr11to15" ~ "year_int_11-15",
      tmHybrd == "yr16to20" ~ "year_int_16-20",
      .default = "RFFC_UNMATCHED"))


### Loop -------------------------------------------

# only 36, just loop 
tx_collector <- vector("list", length = length(files_tx))
rounding_collector <- vector("list", length = length(files_tx))

for (i in seq_along(files_tx)){
  # per tx file, intersect with hucs
  #  calculate area
  # summarize per HUC, sum of area
  # tx_type has what can be converted into year of treatment! 
  
  this_tx_file <- files_tx[i]
  this_tx <- sf::st_read(this_tx_file)
  
  #extract name pieces
  this_tx_name <- tools::file_path_sans_ext(basename(this_tx_file))
  this_region <- str_split(this_tx_name, "_")[[1]][3]
  this_priority <- str_split(this_tx_name, "_")[[1]][4]
  this_intensity <- str_split(this_tx_name, "_")[[1]][5]
  this_runid <- str_split(this_tx_name, "_")[[1]][1]
  
  #sf::st_crs(this_tx)
  #sf::st_crs(hucs_all)                    
  
  # get this region hucs with this priority's timing
  this_hucs <- hucs_all %>% 
    filter(region == this_region) %>% 
    mutate(this_timing = case_when(
      this_priority == "Fire" ~ firet,
      this_priority == "WUI" ~ wuit,
      this_priority == "RFFC" ~ rffct,
      .default = "UNMATCHED"
    ))
  
  # tx to hucs projection (NAD83 California Albers EPSG:3310)
  this_tx_proj <- sf::st_transform(this_tx, sf::st_crs(hucs_all))
  
  # intersection
  hucstx <- sf::st_intersection(this_hucs %>% 
                                  dplyr::select(region, huc12, this_timing, hucAc, hucSqm, TxArea),
                                this_tx_proj %>%
                                  dplyr::select(tx_code, tx_type))
  
  
  # Because of overlap/edge effects between treatment zones and 
  #  HUC boundaries, there are tiny pieces of wrong timestep treatment zones
  #  in with the correct HUC treatment zones. 
  # Use what timestep the HUC should be in to filter. This takes a bit of work. 
  
  # first calc area and summarize
  hucstx$tx_area_m2 <- sf::st_area(hucstx)
  
  hucstx_sum <- hucstx %>% 
    group_by(region, huc12, this_timing, hucAc, hucSqm, TxArea, tx_code, tx_type) %>% 
    summarize(tx_area_m2 = sum(tx_area_m2),
              .groups = "drop") %>% 
    arrange(huc12) %>% 
    # no need to keep geometry now
    sf::st_drop_geometry() %>% 
    # can drop units as well
    mutate(tx_area_m2 = units::drop_units(tx_area_m2))
    
  #collect things we are about to filter out
  rounding_collector[[i]] <- hucstx_sum %>% 
    filter(!this_timing == tx_type)
  
  hucstx_sum <- hucstx_sum %>% 
    # filter to only tx in this timing group 
    filter(this_timing == tx_type)
  
  # for Fire, Hybrid priorities this will be all HUCs in the region. 
  # for WUI however, this is missing the fourth WUI group (untreated), 
  # which we will conditionally add back in
  if (this_priority == "WUI"){
    
    hucstx_sum <- hucstx_sum %>% 
      bind_rows(this_hucs %>% 
                  sf::st_drop_geometry() %>% 
                  filter(this_timing == "notTreated") %>% 
                  dplyr::select(region, huc12, this_timing, hucAc, hucSqm, TxArea) %>% 
                  #add other columns to match
                  mutate(tx_code = 0,
                         tx_type = "not_treated",
                         tx_area_m2 = 0))
    
  }
  
  #nrow(hucstx_sum) == nrow(this_hucs)
  
  hucstx_final <- hucstx_sum %>% 
    mutate(Priority = this_priority,
           TxIntensity = this_intensity,
           runID = this_runid) %>% 
    rename(HUC12 = huc12) %>% 
    dplyr::select(region, HUC12, Priority, TxIntensity, runID, 
                  tx_code, tx_type, tx_area_m2)
  #hucAc, hucSqm, TxArea)
  
  #hucstx_final %>% filter(tx_area_m2 > hucSqm)
  #hucstx_final %>% filter(tx_area_m2 > TxArea)
  
  #collect
  tx_collector[[i]] <- hucstx_final
  
} # end loop

#combine
tx_collected <- do.call(bind_rows, tx_collector)

saveRDS(tx_collected, 
        file = file.path("data", 
                         "treatment_areas",
                         "main_scenario_areas.RDS"))

rounding_collected <- do.call(bind_rows, rounding_collector)
saveRDS(rounding_collected,
        file = file.path("data",
                         "treatment_areas",
                         "main_scenario_filtered_out.RDS"))

### PARKING LOT ----------------------------------------------

# st_write(hucstx,
#          dsn="qa/treatments/hucstx_test1.gpkg",
#          layer=tools::file_path_sans_ext(basename(this_tx_file)),
#          append=FALSE)

# get_intersect_type <- function(hucs_shp, tx_shp, which_type){
#   
#   this_txtype <- tx_shp %>% 
#     filter(tx_type == which_type)
#   
#   # intersect tx and hucs
#   hucstx <- sf::st_intersection(hucs_shp, 
#                                 this_txtype %>% 
#                                   dplyr::select(tx_code, tx_type))
#   
#   hucstx
#   
# }


