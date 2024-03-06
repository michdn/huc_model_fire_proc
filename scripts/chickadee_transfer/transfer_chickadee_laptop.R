### Libraries -------------------------------------------------

#if (!require("pacman")) install.packages("pacman")
#pacman::p_load(tidyverse) 

### User settings ---------------------------------------------

#Mounted shared: R:\rem
shared <- file.path("R:", "rem")
#test <- file.path("R:", "rem", "RunID19_SC_WUI_500k_trt1")
# update 2024-03 new blended rasters
# New blended FM40 rasters (hopefully correct): /mnt/share/rem/RunID21_SC_Fire_500k_trt6/FVS_fuels/blended_outputs/*fml*tif
# Original blended FM40 rasters (incorrect): /mnt/share/rem/RunID21_SC_Fire_500k_trt6_orig/FVS_fuels/blended_outputs/*fml*tif

#My drive & folder over RDC: 
#\\tsclient\C\Users\nekodawn\code_local\huc_val_gridfire\data\data_fuels_bluejay
outfolder <- file.path(
  #backslashes escaped
  "\\\\tsclient", 
  "C", "Users", "nekodawn", "code_local", 
  "huc_model_fire", "qaqc_fvs", "data_fuels_test_RunID21_SC_Fire_500k_trt6_v2")


### select files -----------------------------------------------------
# No pattern parameter in list.dirs unlike list.files
# Plan is to get all directories, then filter to correct directories
# Then copy files over

all_dirs <- list.dirs(shared) 


#Run per region   
region_dirs <- grep(pattern="SC", all_dirs, value=TRUE)
#just FVS_fuels
fvsfuels_dirs <- grep(pattern="FVS_fuels", region_dirs, value=TRUE)
#just blended fuels
blended_dirs <- grep(pattern="blended_outputs", fvsfuels_dirs, value=TRUE)


#test example (to be removed later for real runs)
test_folders <- grep(pattern='RunID21_SC_Fire_500k_trt6', blended_dirs, value=TRUE)
#need to get rid of '_orig' folders
orig_removed <- grep(pattern='_orig', test_folders, invert=TRUE, value=TRUE)
#need to get rid of '_wrong' folders
wrong_removed <- grep(pattern='_wrong', orig_removed, invert=TRUE, value=TRUE)



tif_files <- list.files(wrong_removed,
                        full.names = TRUE,
                        #no longer need recursive here, since getting exact
                        # folders from above
                        #recursive = TRUE,
                        #probably only needed "tif$" but extra pattern shouldn't hurt
                        pattern = "RunID.*tif$")



### copy files --------------------------------------------------------
(time_start <- Sys.time())

file.copy(from=tif_files, to=outfolder)

(time_end <- Sys.time())
(time_end - time_start) 
#55 minutes for 540 SC files (27 scenarios)
