### Libraries -------------------------------------------------

#if (!require("pacman")) install.packages("pacman")
#pacman::p_load(tidyverse) 

### User settings ---------------------------------------------

#Mounted shared: R:\rem
shared <- file.path("R:", "rem")
#test <- file.path("R:", "rem", "RunID19_SC_WUI_500k_trt1")

#My drive & folder over RDC: 
#\\tsclient\C\Users\nekodawn\code_local\huc_val_gridfire\data\data_fuels_bluejay
outfolder <- file.path(
  #backslashes escaped
  "\\\\tsclient", 
  "C", "Users", "nekodawn", "code_local", 
  "huc_val_gridfire", "data", "data_fuels_bluejay")

(time_start <- Sys.time())


### select files -----------------------------------------------------
# No pattern parameter in list.dirs unlike list.files
# Plan is to get all directories, then filter to correct directories
# Then copy files over

all_dirs <- list.dirs(shared) 
#Run per region as finished. SC first. 
region_dirs <- grep("SC", all_dirs, value=TRUE)
#just FVS_fuels
fvsfuels_dirs <- grep("FVS_fuels", region_dirs, value=TRUE)
#just blended fuels
blended_dirs <- grep("blended_outputs", fvsfuels_dirs, value=TRUE)

tif_files <- list.files(blended_dirs,
                        full.names = TRUE,
                        #no longer need recursive here, since getting exact
                        # folders from above
                        #recursive = TRUE,
                        #probably only needed "tif$" but extra pattern shouldn't hurt
                        pattern = "RunID.*tif$")

#testing
#fuels_files <- grep(all_files, pattern='fuels', value=TRUE)
#no_temp <- grep(fuels_files, pattern="temp", invert=TRUE, value=TRUE)


### copy files --------------------------------------------------------

file.copy(tif_files, outfolder)

(time_end <- Sys.time())
(time_end - time_start) 
#55 minutes for 540 SC files (27 scenarios)
