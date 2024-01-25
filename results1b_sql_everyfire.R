#results 1b

#full fire data extraction
# so can do joint data bootstrap 


## abandoned, not giving client per fire results


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
if (!require("RSQLite")) install.packages("RSQLite") #needs to be installed, but not library
pacman::p_load(
  tidyverse,
  sf,
  DBI,
  boot)

#local function
#matching Anna's field names
run_split_rename <- function(df){
  df %>% 
    mutate(huc12 = str_split(huc_id, "_")[[1]][2]) %>% 
    #left_join(hr, by = "huc12") %>% 
    #select(-RRK_Rgn) %>% 
    rename(Year = time_horizon,
           HUC12 = huc12) %>% 
    mutate(run = str_split(mas_scenario, "_")[[1]][1],
           RRK = str_split(mas_scenario, "_")[[1]][2],
           Priority = str_split(mas_scenario, "_")[[1]][3],
           TxIntensity = str_split(mas_scenario, "_")[[1]][4],
           TxType = str_split(mas_scenario, "_")[[1]][5]) %>% 
    select(HUC12, RRK, Priority, TxIntensity, TxType, run, Year, everything()) %>% 
    select(-huc_id, -mas_scenario)
}


### User settings ---------------------------------------------

# where to find the sql output files
# NC, SC, 
# SN_CC_thru180201220206, SN_CC_thru180300100704, SN_CC_thru180500040805, SN_CC_thru180902060702
reg_group <- "SC" 
results_folder <- file.path("results", reg_group) 

# where to write csv summary files
output_folder <- file.path('results_csv', 'every_fire') #use file.path() if multiple folder levels
dir.create(output_folder, recursive = TRUE) 

# ***
#NOTE: writing files WILL overwrite existing files, so make sure output folder is correct

### Base Data import -------------------------------------------

(time_start <- Sys.time())


#Read in list of all sql files in results folder
sql_files <- list.files(results_folder, 
                        full.names=TRUE,
                        recursive=TRUE,
                        pattern="*.sqlite$")
num_sql <- length(sql_files)


### SQLite import -------------------------------------------

#when we have multiple files / real results
# Create a loop per file
#  extract info from sql file
#  add to lists of data
#  then bind_rows at the end
#  (this is much more R efficient than binding rows per loop)

#set up collectors
cbp_collect <- list()
cfl_collect <- list()


#LOOP per output file to scrape all data together
for (i in seq_along(sql_files)){ 
  
  this_sql_file <- sql_files[i]
  
  #start connection
  this_db <- dbConnect(RSQLite::SQLite(), this_sql_file)
  
  tbl_meta <- dbReadTable(this_db, "mas_metadata")
  tbl_hacbp <- dbReadTable(this_db, "ha_cbp_series")
  tbl_hacfl <- dbReadTable(this_db, "ha_cfl_series")
  
  #add metadata to series
  tbl_hacbp_meta <- dplyr::bind_cols(tbl_meta, tbl_hacbp)
  tbl_hacfl_meta <- dplyr::bind_cols(tbl_meta, tbl_hacfl)
  
  #add data to collector lists
  cbp_collect[[i]] <- tbl_hacbp_meta
  cfl_collect[[i]] <- tbl_hacfl_meta
  
  #disconnect from database
  dbDisconnect(this_db)
  
  #print progress every 2000 files
  if (i%%2000 == 0){
    print(paste0(i, " of ", num_sql, " at ", Sys.time()))
  }
  
}

#row bind list items together, respectively
all_main_data <- do.call(bind_rows, main_collector)
all_cft_data <- do.call(bind_rows, cft_collector)
all_cfl_data <- do.call(bind_rows, cfl_collector)

  