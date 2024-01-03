# Loading results from sqlite files
# Calculating some stats
# Saving for future analyses

##################################
# NOTE: How to get data from tahoe
# 
# 0. Submit a ticket 1) to get access to bluejay, 2) '/mnt/tahoe/mas_sim_outputs/' mapped to bluejay
#
# 1. Ask Val or Jeff if they could tar up the region folder, e.g. tahoe\mas_sim_outputs\fires200\SC
#    Use 7zip to untar - https://www.7-zip.org/a/7z2301-x64.exe
#
# 2. Log into bluejay (10.1.30.113) via RDC and make sure that you have enabled drive sharing
#    Open Remote Desktop Connection, click 'Show Options'. 
#    Click on the 'Local Resources' tab
#    Under 'Local devices and resources' click on More...
#    Click on the check box next to 'Drives'. 
#    Now you will be able to access your laptop's hard drive from the remote computer
#
# 3. On bluejay, you'll need to finish mapping tahoe to bluejay, follow the directions IT gives you. 
#
# 4. In one file window explorer, navigate to 'tahoe\mas_sim_outputs\fires200\'. In other explore, navigate to the folder you want it on your computer. 
#    Drag and drop the appropriate tar file to copy it down. 
#
# Note: SC was approx 2.6 GB; NC 4.1 GB; SN_CC 9 GB
# Note: SC took about 4.2 hours for this script
#       NC took about 8.2 hours
#       SN_CC took about x hours
##################################


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

#fxs for bootstrap CI
# following example from 
#  https://www.geeksforgeeks.org/bootstrap-confidence-interval-with-r-programming/
bt_hacbp_ave_fun <- function(df, idx){
  this_df <- df[idx, ]
  ave(this_df[["huc_burned_frac"]])
}
bt_hacfl_ave_fun <- function(df, idx){
  this_df <- df[idx, ]
  ave(this_df[["huc_avg_fl"]])
}


### User settings ---------------------------------------------

# where to find the sql output files
reg_group <- "SN_CC_part4"
results_folder <- file.path("results", reg_group) 

# where to write csv summary files
output_folder <- 'results_csv' #use file.path() if multiple folder levels
dir.create(output_folder, recursive = TRUE) 

# ***
#NOTE: writing files WILL overwrite existing files, so make sure output folder is correct

### Base Data import -------------------------------------------

(time_start <- Sys.time())

# NOT NEEDED. Pulling from long runid information
# #for crosswalk as metadata does not have region
# hucs_shp <- st_read("data/data_huc/TxPrctRankRrkWipRffc.shp")
# hr <- hucs_shp %>% 
#   #get just the region names
#   select(RRK_Rgn) %>% 
#   st_drop_geometry() %>% 
#   distinct() %>% 
#   arrange(RRK_Rgn) %>% 
#   #manually add code that is used in fuel file names
#   #alpha sorted Central Coast, North Coast, Sierra Nevada, South Coast
#   #NOTE: this is corrected version using c() instead of list()
#   # still works, but now just has a value instead of list item
#   add_column(reg_code = c("CC", "NC", "SN", "SC")) %>% 
#   #join back to get all fields
#   left_join(hucs_shp, by = "RRK_Rgn") %>% 
#   #create crosswalk from reg_code to huc12s
#   select(RRK_Rgn, reg_code, huc12)


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
main_collector <- list()
cft_collector <- list()
cfl_collector <- list()


#LOOP per output file to scrape all data together
for (i in seq_along(sql_files)){ #seq_along(sql_files) #33847:51840
  
  this_sql_file <- sql_files[i]
  
  #start connection
  this_db <- dbConnect(RSQLite::SQLite(), this_sql_file)
  
  #get base raw row data
  #dbGetQuery(this_db, 'SELECT AVG(t2.huc_burned_frac) AS HaCBP, STDEV(t2.huc_burned_frac) AS HaCBP_stdev FROM ha_cbp_series AS t2')
  
  this_row_raw <- dbGetQuery(this_db, 'SELECT t1.huc_id, t1.time_horizon, t1.mas_scenario, AVG(t2.huc_burned_frac) AS HaCBP, AVG(t3.huc_avg_fl) AS HaCFL FROM mas_metadata AS t1, ha_cbp_series AS t2, ha_cfl_series AS t3')
  
  this_cft_raw <- dbGetQuery(this_db, 'SELECT t1.huc_id, t1.time_horizon, t1.mas_scenario, t4.fire_type, AVG(t4.huc_burned_frac_in_ftype_bin) AS burn_frac_ave FROM mas_metadata AS t1, ha_cft_hist_series AS t4 GROUP BY fire_type')
  
  this_cfl_raw <- dbGetQuery(this_db, 'SELECT t1.huc_id, t1.time_horizon, t1.mas_scenario, t5.fl_bin_lower, t5.fl_bin_upper, AVG(t5.huc_burned_frac_in_fl_bin) AS burned_frac_ave FROM mas_metadata AS t1, ha_cfl_hist_series AS t5 GROUP BY fl_bin_lower, fl_bin_upper')
  
  #at least one huc fails to have any data, including metadata
  #  need to fill from filename
  if (is.na(this_row_raw[["huc_id"]])){
    this_scenario <- basename(dirname(this_sql_file))
    this_year <- basename(dirname(dirname(this_sql_file)))
    this_huc_id <- basename(dirname(dirname(dirname(this_sql_file))))
    
    this_row_raw["huc_id"] <- this_huc_id
    this_row_raw["time_horizon"] <- this_year
    this_row_raw["mas_scenario"] <- this_scenario
  } 
  
  #add region, rename fields
  this_row <- run_split_rename(this_row_raw)
  
  
  #at least once in SC, cft hist series was empty and caused an error
  # this prevents an error and will write a line with NA data values so we can 
  # figure out what hucs-scenario-year afterwards had issues. 
  if (nrow(this_cft_raw) > 0){
    this_cft <- run_split_rename(this_cft_raw)
  } else {
    this_cft <- this_row[,1:7]
  }
  
  if (nrow(this_cfl_raw) > 0){
    this_cfl <- run_split_rename(this_cfl_raw)
  } else {
    this_cfl <- this_row[,1:7]
  }

  #calculate and add bootstrap CIs for HaCBP and HaCFL
  #hacbp
  tbl_hacbp <- dbReadTable(this_db, "ha_cbp_series")
  
  if (nrow(tbl_hacbp) > 0){
    bt_hacbp_res <- boot(data = tbl_hacbp, 
                         statistic = bt_hacbp_ave_fun, 
                         R = 1000)
    bt_hacbp_ci <- boot.ci(boot.out = bt_hacbp_res, 
                           type = c("norm"))
    #column append results
    this_row_ci <- this_row %>% 
      add_column("hacbp_ci_low" = bt_hacbp_ci$normal[[2]],
                 "hacbp_ci_high" = bt_hacbp_ci$normal[[3]])
  } else {
    #no rows, HUC had some kind of model failure running
    # set to NA
    this_row_ci <- this_row %>% 
      add_column("hacbp_ci_low" = NA_real_,
                 "hacbp_ci_high" = NA_real_)
  }
  
  
  #hacfl
  tbl_hacfl <- dbReadTable(this_db, "ha_cfl_series")
  
  if (nrow(tbl_hacfl) > 0){
    bt_hacfl_res <- boot(data = tbl_hacfl, 
                         statistic = bt_hacfl_ave_fun, 
                         R = 1000)
    bt_hacfl_ci <- boot.ci(boot.out = bt_hacfl_res, 
                           type = c("norm"))
    #column append results
    this_row_ci <- this_row_ci %>% 
      add_column("hacfl_ci_low" = bt_hacfl_ci$normal[[2]],
                 "hacfl_ci_high" = bt_hacfl_ci$normal[[3]])
  } else {
    #no rows, HUC had some kind of model failure running
    # set to NA
    this_row_ci <- this_row_ci %>% 
      add_column("hacfl_ci_low" = NA_real_,
                 "hacfl_ci_high" = NA_real_)
    
  }

  
  #add data to collector lists
  main_collector[[i]] <- this_row_ci
  cft_collector[[i]] <- this_cft
  cfl_collector[[i]] <- this_cfl
  
  #disconnect from database
  dbDisconnect(this_db)
  
  #print progress every 2000 files
  if (i%%2000 == 0){
    print(paste0(i, " of ", num_sql, " at ", Sys.time()))
  }
  
} #end loop

#row bind list items together, respectively
all_main_data <- do.call(bind_rows, main_collector)
all_cft_data <- do.call(bind_rows, cft_collector)
all_cfl_data <- do.call(bind_rows, cfl_collector)

#save out collected data
write_csv(all_main_data,
          file.path(output_folder,
                    paste0('main_results_from_sql_', reg_group, '.csv')))
write_csv(all_cft_data,
          file.path(output_folder,
                    paste0('cft_results_from_sql_', reg_group, '.csv')))
write_csv(all_cfl_data,
          file.path(output_folder,
                    paste0('cfl_results_from_sql_', reg_group, '.csv')))

#end times
(time_end <- Sys.time())
(time_elapsed <- time_end - time_start)


## EXPLORE/TESTING -------------------------------------------------------------------

# #row bind list items together, respectively
# par_main_data <- do.call(bind_rows, main_collector)
# par_cft_data <- do.call(bind_rows, cft_collector)
# par_cfl_data <- do.call(bind_rows, cfl_collector)
# 
# #save out collected data
# write_csv(par_main_data, file.path(output_folder, 'main_results_from_sql_PARTIAL33846.csv'))
# write_csv(par_cft_data, file.path(output_folder, 'cft_results_from_sql_PARTIAL33846.csv'))
# write_csv(par_cfl_data, file.path(output_folder, 'cfl_results_from_sql_PARTIAL33846.csv'))

# rm(par_main_data)
# rm(par_cfl_data)
# rm(par_cft_data)


#tbl_cft_hist <- dbReadTable(this_db, "ha_cft_hist_series")


# this_sql_file <- sql_files[1]
# 
# #start connection
# this_db <- dbConnect(RSQLite::SQLite(), this_sql_file)
# 
# # #test connection & explore
# dbListTables(this_db)
# # #[1] "ha_cbp_series"       "ha_cfl_hist_series"  "ha_cfl_series"       "ha_cft_hist_series"
# # #[5] "mas_metadata"        "simulated_ignitions"
# 
# dbListFields(this_db, "ha_cbp_series")
# #"fire_i"          "huc_burned_frac"
# dbGetQuery(this_db, 'SELECT * FROM ha_cbp_series LIMIT 5')
# tbl_cbp <- dbReadTable(this_db, "ha_cbp_series")
# head(tbl_cbp)
# 
# dbListFields(this_db, "ha_cfl_hist_series")
# # #"fire_i"                    "fl_bin_lower"              "fl_bin_upper"        "huc_burned_frac_in_fl_bin"
# dbListFields(this_db, "ha_cft_hist_series")
# # #"fire_i"                       "fire_type"                    "huc_burned_frac_in_ftype_bin"
# 
# dbGetQuery(this_db, 'SELECT * FROM ha_cft_hist_series LIMIT 6')
# #CI 
# tbl_hacbp <- dbReadTable(this_db, "ha_cbp_series")
# tbl_hacbp
# 
# bt_hacbp_res <- boot(data = tbl_hacbp, 
#                      statistic = bt_hacbp_ave_fun, 
#                      R = 1000)
# plot(bt_hacbp_res)
# 
# bt_hacbp_ci <- boot.ci(boot.out = bt_hacbp_res, 
#                        type = c("norm"))
# bt_hacbp_ci$normal[[2]]
# bt_hacbp_ci$normal[[3]]
# 
# this_row_raw %>% add_column("hacbp_ci_low" = bt_hacbp_ci$normal[[2]],
#                             "hacbp_ci_high" = bt_hacbp_ci$normal[[3]])
# 
# 
# tbl_hacfl <- dbReadTable(this_db, "ha_cfl_series")
# 
# bt_hacfl_res <- boot(data = tbl_hacfl, 
#                      statistic = bt_hacfl_ave_fun, 
#                      R = 1000)
# plot(bt_hacfl_res)
# 
# bt_hacfl_ci <- boot.ci(boot.out = bt_hacfl_res, 
#                        type = c("norm"))
# bt_hacfl_ci$normal[[2]]
# bt_hacfl_ci$normal[[3]]
# 
# this_row_raw %>% add_column("hacfl_ci_low" = bt_hacfl_ci$normal[[2]],
#                             "hacfl_ci_high" = bt_hacfl_ci$normal[[3]])


# dbDisconnect(this_db)
