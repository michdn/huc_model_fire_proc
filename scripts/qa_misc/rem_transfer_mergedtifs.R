
## CANNOT USE FROM LAPTOP
# Takes too long for list dir to work 
# (Killed at 3hours)



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
  "C", "Users", "nekodawn", "code_local", 
  "huc_model_fire", "data", "unblended_fuels")

# run ids, include _, important for single digit runids
selected_runs <- "RunID21_|RunID27_|RunID24"

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


files_to_get <- list.files(shared,
                        full.names = TRUE,
                        recursive = TRUE,
                        pattern = "RunID*2024_fml_merged.tif$")


### copy files --------------------------------------------------------

#file names do NOT have trt in it. Need to loop and add to file name. 

for (i in seq_along(files_to_get)){
  
  this_file <- files_to_get[[i]]
  
  this_scenario_folder <- this_files %>% 
    tools::file_path_sans_ext() %>% 
    tools::file_path_sans_ext() %>% 
    tools::file_path_sans_ext() %>% 
    basename()
  
  this_trt <- str_split(this_scenario_folder, "_")[[1]][5]
  
}


#file.copy(files_to_get, outfolder)

(time_end <- Sys.time())
(time_end - time_start) 
#55 minutes for 540 SC files (27 scenarios)
