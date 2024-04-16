# extract weather pointer information per fire from sqlite

#INSERT INTO simulated_ignitions (fire_i, ign_i, ign_j, weather_sample_index) VALUES (?, ?, ?, ?)

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
if (!require("RSQLite")) install.packages("RSQLite") #needs to be installed, but not library
pacman::p_load(
  tidyverse,
  DBI)


#local function
#matching Anna"s field names
# NOTE! str_split only works as expected b/c it"s inside a function here
# otherwise use tidyr::separate or str_split %>% map_chr(., n)
split_rename_run <- function(df){
  df %>% 
    mutate(HUC12 = str_split(huc_id, "_")[[1]][2]) %>% 
    rename(Year = time_horizon) %>% 
    mutate(run = str_split(mas_scenario, "_")[[1]][1],
           Region = str_split(mas_scenario, "_")[[1]][2],
           Priority = str_split(mas_scenario, "_")[[1]][3],
           TxIntensity = str_split(mas_scenario, "_")[[1]][4],
           TxType = str_split(mas_scenario, "_")[[1]][5]) %>% 
    select(HUC12, Region, Priority, TxIntensity, TxType, run, Year, everything()) %>% 
    select(-huc_id)
}


### User settings ---------------------------------------------

# where to find the sql output files
# NC, SC, SN, CC
reg_group <- "SN" 
results_folder <- file.path("R:",
                            "rem",
                            "MAS_gridfire_outputs",
                            reg_group)

# where to write csv summary files
output_folder <- file.path("results", "misc")

dir.create(output_folder, recursive = TRUE) 

#this will perhaps make dbconnections quicker? 
options(connectionObserver = NULL)
#time SN without: 2000 to 4000 :  1m37s
# time SN with: 2000 to 4000 : 1m26s

### Base Data -------------------------------------------

(time_start <- Sys.time())


#Read in list of all sql files in results folder
sql_files <- list.files(results_folder, 
                        full.names=TRUE,
                        recursive=TRUE,
                        pattern="*.sqlite$")
num_sql <- length(sql_files)


### SQLite extract -------------------------------------------

#set up collectors
ignitions_collect <- list()

#LOOP per output file to scrape all data together
for (i in seq_along(sql_files)){ 
  
  this_sql_file <- sql_files[i]
  
  #start connection
  this_db <- dbConnect(RSQLite::SQLite(), this_sql_file)
  
  tbl_meta <- dbReadTable(this_db, "mas_metadata")
  tbl_ig <- dbReadTable(this_db, "simulated_ignitions")

  
  #add metadata to series
  tbl_meta_split <- split_rename_run(tbl_meta)
  tbl_ig_meta <- dplyr::bind_cols(tbl_meta_split, tbl_ig)

  
  #add data to collector lists
  ignitions_collect[[i]] <- tbl_ig_meta

  
  #disconnect from database
  dbDisconnect(this_db)
  
  #print progress every 2000 files
  if (i%%2000 == 0){
    print(paste0(i, " of ", num_sql, " at ", Sys.time()))
  }
  
}


#row bind list items together, respectively
all_ignitions <- do.call(bind_rows, ignitions_collect)
#save out collected data
saveRDS(all_ignitions,
        file.path(output_folder,
                  paste0(reg_group, "_ignitions_from_sql.RDS")))


#end times
(time_end <- Sys.time())
(time_elapsed <- time_end - time_start)


