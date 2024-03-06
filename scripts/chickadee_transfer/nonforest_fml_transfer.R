


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
  "huc_model_fire", "qaqc_fvs", "fuels_nonforest_bluejay")

# run ids, include _, important for single digit runids
selected_runs <- 
  "RunID10_|RunID16_|RunID19_|RunID25_|RunID28_|RunID34_|RunID9_|RunID12_|RunID18_|RunID21_|RunID27_|RunID30_|RunID36_"

(time_start <- Sys.time())


### select files -----------------------------------------------------

# No pattern parameter in list.dirs unlike list.files
# Plan is to get all directories, then filter to correct directories
# Then copy files over

#[scenario]/FVS_fuels/clipped_reproj_inputs/[...]fml_[CRS].tif (not the ones that end in temp.tif).

all_dirs <- list.dirs(shared)
#RunIDs
run_dirs <- grep(selected_runs, all_dirs, value=TRUE)
#subset
subset_dirs <- grep("FVS_fuels", run_dirs, value=TRUE)
#just blended fuels
subsubset_dirs <- grep("clipped_reproj_inputs", subset_dirs, value=TRUE)

# #remove '_orig'
# orig_removed <- grep(pattern='_orig', subsubset_dirs, invert=TRUE, value=TRUE)
# 

files_to_get <- list.files(orig_removed,
                           full.names = TRUE,
                           recursive = TRUE,
                           pattern = "RunID.+_fml_.+tif$")

#need to filters out temp.tif
files_to_get <- grep(files_to_get, 
                     pattern='temp.tif$',
                     invert=TRUE,  #NOT these
                     value=TRUE)


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

