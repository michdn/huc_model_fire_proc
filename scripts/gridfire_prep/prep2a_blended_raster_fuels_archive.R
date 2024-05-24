# FVS blended raster fuels archiving

# and for use in unburnable issue

# Basically the first section of the fuels addition code,
#  but then save onto bluejay
#  (for further analysis or compressing to archive in google drive)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

#options(future.globals.onReference = "error")

### User settings ---------------------------------------------

region_to_run <- "NC" #"CC", "SN", "NC"

out_folder <- file.path("E:", "MAS", "blended_rasters", region_to_run)
dir.create(out_folder, recursive = TRUE) 


shared <- file.path("R:", "rem")

#original fvs
fvs_names_orig <- read_csv(file.path("data", "FvsNames.csv"))

### Data finding: fuels ---------------------------------------

#create folders that look like R:/rem/RunID36_CC_Fire_2m_trt4/FVS_fuels/blended_outputs/

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
#for NC, all new, incl trt7, reblended under FVS_fuels, but other canopy levels to be pulled from archive
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
  #NOTE pulling from fvs_names_all here
  bind_rows(fvs_names_all %>% 
              filter(grepl('_SN_', name)) %>% 
              mutate(fuel="FVS_Fuels2"))

#108 - 6 + 27 = 129 for SN, 108 for all others

fvs_names <- fvs_names %>% 
  #temporary just for nice sorting to check
  mutate(run = str_split(name, "_") %>% map_chr(.,1),
         id = str_remove(run, "\\D+") %>% as.numeric()) %>%
  arrange(id) %>% 
  select(-run, -id)


#create folders
fvs_names <- fvs_names %>% 
  mutate(runid_folders = file.path('R:',
                                   'rem',
                                   name,
                                   fuel,
                                   'blended_outputs'))


selected_rem <- grep(pattern=region_to_run, 
                     fvs_names %>% pull(runid_folders),
                     value=TRUE)
#should be 27 for a region, except for SN (27 + (27-6) = 48)


fuel_files_rem <- list.files(selected_rem,
                             full.names = TRUE,
                             #probably only needed "tif$" but extra pattern shouldn't hurt
                             pattern = "RunID.*tif$")

# Need to REMOVE 2044 if present
fuel_files_rem <- grep(fuel_files_rem, pattern="_2044_", invert=TRUE, value=TRUE)

# Need to REMOVE any unarchived canopy layers from FVS_fuels (NOT FVS_Fuels2)
#  as pulling those from bluejay source
# Run9 trt1 and trt4
# (Technically won't hurt if overwriting, but no.)
# cbd, cbh, cc, cht
# Flipped around, only fml layers from FVS_fuels
# This is a mess, sorry. Would have been better to split if else entirely
if (region_to_run == "SN"){
  fuel_files_temp <- c(grep(fuel_files_rem, pattern="FVS_Fuels2", value=TRUE), 
                       (grep(fuel_files_rem, pattern="FVS_fuels.+fml", value=TRUE)))
  fuel_files_rem <- fuel_files_temp
}

#should be 540 for a region (SN different, just the new ones)
length(fuel_files_rem)
# SN. 
# FVS_Fuels2: 
# 27 scenarios with 5 fuels for 2039 [135] plus
#  6 scenarios with 3 years of 5 fuels [90]. 135 + 90 = 225
# FVS_fuels:
#     21 scenarios with 3 years of reblended fml [63]
# Total 225 + 63 = 288

#NC: 396


if (region_to_run == "SN"){
  # SN correct old non-fml rasters
  #special SN bluejay local folder
  all_orig_sn <- list.files(file.path('data', 'data_fuels_sn_badblend'),
                            full.names = TRUE,
                            pattern="RunID.+tif$")
  #get region (none if not SN)
  selected_orig_sn <- grep(pattern=region_to_run, 
                           all_orig_sn,
                           value=TRUE)
  
  #We need to remove all fml files (new will be in R:/rem/)
  selected_sn <- grep(selected_orig_sn,
                      pattern="fml_.+tif$",
                      invert=TRUE,
                      value=TRUE)
  #27 * 4 years * 4 fuel files = 432
  #we also need to remove the old trt6 FVS_fuels, but only for affected
  fuel_files_blue <- grep(selected_sn,
                          pattern=sn6_pattern, #set above 
                          invert=TRUE,
                          value=TRUE)
  # 432 - (6 * 4 fuels * 4 years) = 336
  # Also need to REMOVE 2044
  fuel_files_blue <- grep(fuel_files_blue, 
                          pattern="_2044_", 
                          invert=TRUE, 
                          value=TRUE)
  # 336 - (21 remaining scenarios * 4 fuels) = 252
  
  #combine
  fuel_files <- c(fuel_files_rem, fuel_files_blue)
  
} else if (region_to_run == "NC"){
  
  nc6 <- c("RunID11_NC_RFFC_500k_trt6", "RunID12_NC_Fire_500k_trt6", "RunID14_NC_RFFC_1m_trt6",
           "RunID15_NC_Fire_1m_trt6", "RunID17_NC_RFFC_2m_trt6", "RunID18_NC_Fire_2m_trt6") 
  nc6_pattern <- paste(nc6, collapse="|")
  
  # SN correct old non-fml rasters
  #special SN bluejay local folder
  all_orig_nc <- list.files(file.path('data', 'data_fuels_nc_badblend'),
                            full.names = TRUE,
                            pattern="RunID.+tif$")
  #remove trt4
  selected_orig_nc <- grep(all_orig_nc,
                           pattern="trt4",
                           invert=TRUE,
                           value=TRUE)
  
  #We need to remove all fml files (new will be in R:/rem/)
  selected_nc <- grep(selected_orig_nc,
                      pattern="fml_.+tif$",
                      invert=TRUE,
                      value=TRUE)

  #we also need to remove the old trt6 FVS_fuels, but only for affected
  fuel_files_blue <- grep(selected_nc,
                          pattern=nc6_pattern, #set above 
                          invert=TRUE,
                          value=TRUE)

    # Also need to REMOVE 2044
  fuel_files_blue <- grep(fuel_files_blue, 
                          pattern="_2044_", 
                          invert=TRUE, 
                          value=TRUE)
  # 144
  
  
  #combine
  fuel_files <- c(fuel_files_rem, fuel_files_blue)
  #540
  
} else {
    fuel_files <- fuel_files_rem
}

length(fuel_files)
#540
all.equal(length(fuel_files), 540)

### Save to temporary data storage location -------------------

file.copy(from=fuel_files, to=out_folder)
