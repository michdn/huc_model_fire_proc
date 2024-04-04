#Full fire data extraction from sqlite
# found to be faster to extract everything from sqlite
#  and then later process, than trying to do sql queries in main pull

#script "1" no longer needed, as it was the copy off of pyregence script
# This script can run on bluejay reading from the share

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
#file.path("run_202401_badblend", "results", reg_group) 
# file.path("results", "raw_sqlite", reg_group) 

# where to write csv summary files
output_folder <- file.path("results", "extracts")

# file.path("run_202401_badblend", "results_raw_extraction_test") 
#  #file.path("results", "csv_extraction")

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
cft_hist_collect <- list()
cfl_hist_collect <- list()


#LOOP per output file to scrape all data together
for (i in seq_along(sql_files)){ 
  
  this_sql_file <- sql_files[i]
  
  #start connection
  this_db <- dbConnect(RSQLite::SQLite(), this_sql_file)
  
  tbl_meta <- dbReadTable(this_db, "mas_metadata")
  tbl_hacbp <- dbReadTable(this_db, "ha_cbp_series")
  tbl_hacfl <- dbReadTable(this_db, "ha_cfl_series")
  tbl_cft_hist <- dbReadTable(this_db, "ha_cft_hist_series")
  tbl_cfl_hist <- dbReadTable(this_db, "ha_cfl_hist_series")
  
  
  #add metadata to series
  tbl_meta_split <- split_rename_run(tbl_meta)
  tbl_hacbp_meta <- dplyr::bind_cols(tbl_meta_split, tbl_hacbp)
  tbl_hacfl_meta <- dplyr::bind_cols(tbl_meta_split, tbl_hacfl)
  tbl_cft_hist_meta <- dplyr::bind_cols(tbl_meta_split, tbl_cft_hist)
  tbl_cfl_hist_meta <- dplyr::bind_cols(tbl_meta_split, tbl_cfl_hist)
  
  
  #add data to collector lists
  cbp_collect[[i]] <- tbl_hacbp_meta
  cfl_collect[[i]] <- tbl_hacfl_meta
  cft_hist_collect[[i]] <- tbl_cft_hist_meta
  cfl_hist_collect[[i]] <- tbl_cfl_hist_meta
  
  
  #disconnect from database
  dbDisconnect(this_db)
  
  #print progress every 2000 files
  if (i%%2000 == 0){
    print(paste0(i, " of ", num_sql, " at ", Sys.time()))
  }
  
}


# Running out of memory, doing one at a time. 

#CBP
#row bind list items together, respectively
all_cbp_fires <- do.call(bind_rows, cbp_collect)
#save out collected data
write_csv(all_cbp_fires,
          file.path(output_folder,
                    paste0(reg_group, "_cbp_all_fires_from_sql.csv")))
#as RDS too
saveRDS(all_cbp_fires,
        file.path(output_folder,
                  paste0(reg_group, "_cbp_all_fires_from_sql.RDS")))
#delete
rm(all_cbp_fires)
rm(cbp_collect)
gc()


#CFL
all_cfl_fires <- do.call(bind_rows, cfl_collect)
write_csv(all_cfl_fires,
          file.path(output_folder,
                    paste0(reg_group, "_cfl_all_fires_from_sql.csv")))
saveRDS(all_cfl_fires,
        file.path(output_folder,
                  paste0(reg_group, "_cfl_all_fires_from_sql.RDS")))
rm(all_cfl_fires)
rm(cfl_collect)
gc()

#CFT HIST
all_cft_hist <- do.call(bind_rows, cft_hist_collect)
write_csv(all_cft_hist,
          file.path(output_folder,
                    paste0(reg_group,"_cft_hist_from_sql.csv")))
saveRDS(all_cft_hist,
        file.path(output_folder,
                  paste0(reg_group,"_cft_hist_from_sql.RDS")))
rm(all_cft_hist)
rm(cft_hist_collect)
gc()

#CFL HIST
all_cfl_hist <- do.call(bind_rows, cfl_hist_collect)
write_csv(all_cfl_hist,
          file.path(output_folder,
                    paste0(reg_group,"_cfl_hist_from_sql.csv")))
saveRDS(all_cfl_hist,
          file.path(output_folder,
                    paste0(reg_group,"_cfl_hist_from_sql.RDS")))

#end times
(time_end <- Sys.time())
(time_elapsed <- time_end - time_start)


# saveRDS(cfl_hist_collect,
#         file.path(output_folder,
#                   paste0(reg_group,"_cfl_hist_collect.RDS")))

