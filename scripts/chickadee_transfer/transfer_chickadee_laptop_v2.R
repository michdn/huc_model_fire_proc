# for transferring blended fuels from chickadee to local laptop for qaqc
# (used to be for production, but now leaving on bluejay with processing on bluejay)

# Mostly looking at fml or canopy cover cc. 

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse) 


### User settings ---------------------------------------------

#Mounted shared: R:\rem
shared <- file.path("R:", "rem")

# run ids, include _ if runid only, important for single digit runids
selected_list <- 
  c("RunID1_SN_WUI_500k_trt4", "RunID1_SN_WUI_500k_trt6", 
    "RunID3_SN_Fire_500k_trt6",
    "RunID5_SN_RFFC_1m_trt4", "RunID5_SN_RFFC_1m_trt6", 
    "RunID6_SN_Fire_1m_trt1", 
    "RunID7_SN_WUI_2m_trt6", 
    "RunID9_SN_Fire_2m_trt6") #SN
  #'CC.+trt6'
  #'RunID30_.+trt6|RunID32_.+trt6|RunID33_.+trt1|RunID34_.+trt1|RunID35_.+trt4|RunID36_.+trt4' #CC
  #'RunID21_.+trt1|RunID25_.+trt1|RunID23_.+trt1' # trt4 # part of SC
  #'RunID25_|RunID20_ #trt1 #part of SC
  #"RunID20_|RunID21_|RunID23_|RunID24_|RunID26_|RunID27_" # part of SC
selected_runs <- paste(selected_list, collapse="|")

#My drive & folder over RDC: 
outfolder <- file.path(
  #backslashes escaped
  "\\\\tsclient", 
  "C", "Users", "nekodawn", "code_local", 
  "huc_model_fire", "qaqc_fvs", "sn_qaqc") #sc_qaqc, cc_qaqc, nc_qaqc, sn_qaqc

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

#for SN, all 2039 and new trt6 are under FVS_Fuels2
# and all new reblended under FVS_fuels (but some old as well that we will need to filter out)
# and as rest of SN is still archived, we will pull the remaining from an archive source below
fvs_names_all <- fvs_names_all %>% 
  #add normal fuel folder
  mutate(fuel = "FVS_fuels") 

# Need to REMOVE affected trt6 from FVS_fuels (NOT FVS_fuels2)
sn6 <- c("RunID2_SN_RFFC_500k_trt6", "RunID3_SN_Fire_500k_trt6", "RunID5_SN_RFFC_1m_trt6",
         "RunID6_SN_Fire_1m_trt6", "RunID8_SN_RFFC_2m_trt6", "RunID9_SN_Fire_2m_trt6") 
sn6_pattern <- paste(sn6, collapse="|")

fvs_names_snreduced <- fvs_names_all %>% 
  filter(!grepl(sn6_pattern, name))
  
#new data until FVS_Fuels2
fvs_names <- fvs_names_snreduced %>% 
  #add in special (extra rows for SN)
  bind_rows(fvs_names_all %>% 
              filter(grepl('_SN_', name)) %>% 
              mutate(fuel="FVS_Fuels2"))

fvs_names <- fvs_names %>% 
  #temporary just for nice sorting
  mutate(run = str_split(name, "_") %>% map_chr(.,1),
         id = str_remove(run, "\\D+") %>% as.numeric()) %>%
  arrange(id) %>% 
  select(-run, -id)


### select files -----------------------------------------------------

#create folders that look like R:/rem/RunID36_CC_Fire_2m_trt4/FVS_fuels/blended_outputs/

fvs_names <- fvs_names %>% 
  mutate(runid_folders = file.path('R:',
                                   'rem',
                                   name,
                                   fuel,
                                   'blended_outputs'))

selected_rem <- grep(pattern=selected_runs, 
                     fvs_names %>% pull(runid_folders),
                     value=TRUE)


fml_files_rem <- list.files(selected_rem,
                        full.names = TRUE,
                        pattern = "RunID.+_fml_.+tif$") 

cc_files_rem <- list.files(selected_rem,
                       full.names = TRUE,
                       pattern = "RunID.+_cc_.+tif$") 

# Need to REMOVE 2044 if present
fml_files_rem <- grep(fml_files_rem, pattern="_2044_", invert=TRUE, value=TRUE)
cc_files_rem <- grep(cc_files_rem, pattern="_2044_", invert=TRUE, value=TRUE)

# NOTE: NOT dealing with this, as not selected SN runs
# Need to REMOVE any unarchived canopy layers from FVS_fuels (NOT FVS_Fuels2)
#  as pulling those from bluejay source
# Run9 trt1 and trt4


#special SN bluejay local folder
all_orig_sn <- list.files(file.path('data', 'data_fuels_sn'),
                          full.names = TRUE,
                          pattern="RunID.+tif$")
#get selected
selected_orig_sn <- grep(pattern=selected_runs,
                         all_orig_sn,
                         value=TRUE)
#We need to remove all fml files (new will be in R:/rem/)
selected_sn <- grep(pattern="fml_.+tif$", 
                    selected_orig_sn,
                    invert=TRUE,
                    value=TRUE)
#we also need to remove the old trt6 FVS_fuels, but only for affected
# remove e.g.
# RunID2_SN_RFFC_500k_trt6_2024_fml_32611_FF_FVS.tif"
# #RunID2, RunID3, RunID5, RunID7, RunID8, RunID9
selected_sn2 <- grep(pattern=sn6_pattern, #set above 
                    selected_sn,
                    invert=TRUE,
                    value=TRUE)

#get fml and cc (note: no fml will be pulled from blue)
# fml_files_blue <- grep(selected_sn2, 
#                        pattern="RunID.+_fml_.+tif$",
#                        value=TRUE)
cc_files_blue <- grep(selected_sn2, 
                      pattern="RunID.+_cc_.+tif$", 
                      value=TRUE)


# Also need to REMOVE 2044
#fml_files_blue <- grep(fml_files_blue, pattern="_2044_", invert=TRUE, value=TRUE)
cc_files_blue <- grep(cc_files_blue, pattern="_2044_", invert=TRUE, value=TRUE)


#combine sources
fml_files <- fml_files_rem #c(fml_files_rem, fml_files_blue)
cc_files <- c(cc_files_rem, cc_files_blue)


# No pattern parameter in list.dirs unlike list.files
# Plan is to get all directories, then filter to correct directories
# Then copy files over
#This will take a while -- TOO MANY FOLDERS NOW
# all_dirs <- list.dirs(shared) 
# #Run per region   (not really needed now with RunID pattern, but doesn't really hurt)
# region_dirs <- grep(pattern="SC", all_dirs, value=TRUE)
# #test scenarios
# test_folders <- grep(pattern=selected_runs, all_dirs, value=TRUE)
# #just FVS_fuels and blended
# fuels_dirs <- grep(pattern="/FVS_fuels/blended_outputs$", test_folders, value=TRUE)
# fuels_dirs
# #need to get rid of various folders (may not need, no harm to include)
# do_not_find <- '_orig|_wrong|_re-run|MAS_delete|MAS_orig|_moved'
# dnf_removed <- grep(pattern=do_not_find, fuels_dirs, invert=TRUE, value=TRUE)
# dnf_removed

### copy files --------------------------------------------------------
(time_start <- Sys.time())

file.copy(from=fml_files, to=outfolder)
file.copy(from=cc_files, to=outfolder)


(time_end <- Sys.time())
(time_end - time_start) 

