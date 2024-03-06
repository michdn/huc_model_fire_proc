


### Libraries -------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse) 

### User settings ---------------------------------------------

#Mounted shared: R:\
shared <- file.path("R:", "rem")
#test <- file.path("R:", "rem", "RunID19_SC_WUI_500k_trt1")

#local
outfolder <- file.path(
  #backslashes escaped
  "\\\\tsclient", 
  "C", "Users", "nekodawn", "code_local", 
  "huc_model_fire", "data", "fuels_merged_bluejay")

# run ids, include _, important for single digit runids
selected_runs <- "RunID10_|RunID16_|RunID19_|RunID25_|RunID28_|RunID34_"

#"RunID9_|RunID12_|RunID18_|RunID21_|RunID27_|RunID30_|RunID36_"

(time_start <- Sys.time())


### select files -----------------------------------------------------

# No pattern parameter in list.dirs unlike list.files
# Plan is to get all directories, then filter to correct directories
# Then copy files over

all_dirs <- list.dirs(shared)
#RunIDs
run_dirs <- grep(selected_runs, all_dirs, value=TRUE)
#just FVS_fuels
fvsfuels_dirs <- grep("FVS_fuels", run_dirs, value=TRUE)
#just blended fuels
blended_dirs <- grep("outputs", fvsfuels_dirs, value=TRUE)


files_to_get <- list.files(blended_dirs,
                           full.names = TRUE,
                           recursive = TRUE,
                           pattern = "RunID.+_fml_merged.tif$")


### copy files --------------------------------------------------------

#file names do NOT have trt in it. Need to loop and add to file name. 

for (i in seq_along(files_to_get)){
  
  this_file <- files_to_get[[i]]
  
  this_scenario_name <- this_file %>% 
    dirname() %>% 
    dirname() %>% 
    dirname() %>% 
    basename()
  
  this_trt <- str_split(this_scenario_name, "_")[[1]][5]
  
  new_file_name <- paste0(this_file %>% 
                            tools::file_path_sans_ext() %>% 
                            basename(),
                          "_",
                          this_trt, 
                          ".tif")
  
  file.copy(this_file, file.path(outfolder, new_file_name))
  
}


#file.copy(files_to_get, outfolder)

(time_end <- Sys.time())
(time_end - time_start) 
#55 minutes for 540 SC files (27 scenarios)
