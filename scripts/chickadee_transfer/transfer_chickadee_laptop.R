# for transferring blended fuels from chickadee to local laptop for qaqc
# (used to be for production, but now leaving on bluejay with processing on bluejay)

# Mostly looking at fml or canopy cover cc. 


# ***********
# USE V2 NOW! 
# **********

# Too many folders now, with gridfire in same shared directory,
# initial dir scan was taking ~hour




### User settings ---------------------------------------------

#Mounted shared: R:\rem
shared <- file.path("R:", "rem")

# run ids, include _ if runid only, important for single digit runids
selected_runs <- 
  'RunID30_.+trt6|RunID32_.+trt6|RunID33_.+trt1|RunID34_.+trt1|RunID35_.+trt4|RunID36_.+trt4' #CC
  #'RunID21_.+trt1|RunID25_.+trt1|RunID23_.+trt1' # trt4 # part of SC
  #'RunID25_|RunID20_ #trt1 #part of SC
  #"RunID20_|RunID21_|RunID23_|RunID24_|RunID26_|RunID27_" # part of SC


#My drive & folder over RDC: 
outfolder <- file.path(
  #backslashes escaped
  "\\\\tsclient", 
  "C", "Users", "nekodawn", "code_local", 
  "huc_model_fire", "qaqc_fvs", "cc_qaqc") #sc_qaqc, cc_qaqc, nc_qaqc, sn_qaqc


### select files -----------------------------------------------------
# No pattern parameter in list.dirs unlike list.files
# Plan is to get all directories, then filter to correct directories
# Then copy files over

#This will take a while
all_dirs <- list.dirs(shared) 


# #Run per region   (not really needed now with RunID pattern, but doesn't really hurt)
# region_dirs <- grep(pattern="SC", all_dirs, value=TRUE)

#test scenarios
test_folders <- grep(pattern=selected_runs, all_dirs, value=TRUE)

#just FVS_fuels and blended
fuels_dirs <- grep(pattern="/FVS_fuels/blended_outputs$", test_folders, value=TRUE)
fuels_dirs

#need to get rid of various folders (may not need, no harm to include)
do_not_find <- '_orig|_wrong|_re-run|MAS_delete|MAS_orig|_moved'
dnf_removed <- grep(pattern=do_not_find, fuels_dirs, invert=TRUE, value=TRUE)

dnf_removed

fml_files <- list.files(dnf_removed,
                        full.names = TRUE,
                        pattern = "RunID.+_fml_.+tif$") 

cc_files <- list.files(dnf_removed,
                        full.names = TRUE,
                        pattern = "RunID.+_cc_.+tif$") 

fml_files
cc_files

# Need to REMOVE 2044
fml_files <- grep(fml_files, pattern="_2044_", invert=TRUE, value=TRUE)
cc_files <- grep(cc_files, pattern="_2044_", invert=TRUE, value=TRUE)

### copy files --------------------------------------------------------
(time_start <- Sys.time())

file.copy(from=fml_files, to=outfolder)
file.copy(from=cc_files, to=outfolder)


(time_end <- Sys.time())
(time_end - time_start) 

