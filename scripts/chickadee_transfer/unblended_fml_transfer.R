


### Libraries -------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse) 

### User settings ---------------------------------------------

#local
outfolder <- file.path(
  #backslashes escaped
  "\\\\tsclient", 
  "C", "Users", "nekodawn", "code_local", 
  "huc_model_fire", "qaqc_fvs", "nc_qaqc") #sc_qaqc, cc_qaqc, nc_qaqc, sn_qaqc

# run ids, include _, important for single digit runids
selected_list <- 
  c("RunID10_NC_WUI_500k_trt7", "RunID15_NC_Fire_1m_trt7") 
selected_runs <- paste(selected_list, collapse="|")

#"RunID10_|RunID16_|RunID19_|RunID25_|RunID28_|RunID34_"
#"RunID9_|RunID12_|RunID18_|RunID21_|RunID27_|RunID30_|RunID36_"


#Mounted shared: R:\
shared <- file.path("R:", "rem")


(time_start <- Sys.time())


### select files -----------------------------------------------------

#original fvs
fvs_names_orig <- read_csv(file.path("data", "FvsNames.csv"))

#create NC trt7 runids. 
fvs_nc7 <- fvs_names_orig %>% 
  filter(grepl('NC.+trt4', name)) %>% 
  mutate(name = str_replace(name, 'trt4', 'trt7'))
#remove NC trt4, add in trt7
fvs_names_all <- fvs_names_orig %>% 
  #! is not
  filter(!grepl('NC.+trt4', name)) %>% 
  bind_rows(fvs_nc7) 

fvs_names_all <- fvs_names_all %>% 
  mutate(merged_folders = file.path('R:',
                                    'rem',
                                    name,
                                    "FVS_fuels",
                                    "outputs"))

selected_rem <- grep(pattern=selected_runs, 
                     fvs_names_all %>% pull(merged_folders),
                     value=TRUE)


# files_to_get <- list.files(selected_rem,
#                            full.names = TRUE,
#                            recursive = TRUE,
#                            pattern = "RunID.+_fml_merged.tif$")

files_to_get <- list.files(selected_rem,
                           full.names = TRUE,
                           recursive = TRUE,
                           pattern = "RunID.+_cc_merged.tif$")


# No pattern parameter in list.dirs unlike list.files
# Plan is to get all directories, then filter to correct directories
# Then copy files over

# all_dirs <- list.dirs(shared)
# #RunIDs
# run_dirs <- grep(selected_runs, all_dirs, value=TRUE)
# #just FVS_fuels
# fvsfuels_dirs <- grep("FVS_fuels", run_dirs, value=TRUE)
# #just blended fuels
# blended_dirs <- grep("outputs", fvsfuels_dirs, value=TRUE)
# 
# 
# files_to_get <- list.files(blended_dirs,
#                            full.names = TRUE,
#                            recursive = TRUE,
#                            pattern = "RunID.+_fml_merged.tif$")


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
