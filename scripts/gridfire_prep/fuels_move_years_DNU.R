
###
#
# ABANDONED
#
###


# Script to copy later years from hucs_val (which will become only 2024, 2029)
#  to another folder set with only the later years (2034, 2044)
# Due to processing time and wanting to get started as early as possible on 
#  running Year 0 and Year 5. Otherwise it'll take much longer to create a full 
#  dataset. 


### Libraries -------------------------------------------------
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(
#   tidyverse) 

### User settings ---------------------------------------------

year_to_move <- "2034" #2034 #2044

from_folder <- "hucs_val"
to_folder <- "hucs_val_2034_2044"

(time_start <- Sys.time())

### Folder and file selection ---------------------------------

fuel_files <- list.files(from_folder,
                        full.names = TRUE,
                        #don't need recursive here
                        #recursive = TRUE,
                        #this gets all fuel files
                        pattern = "RunID.*tif$")

#grep for year to move
move_files <- grep(fuel_files, pattern = year_to_move, value = TRUE)

#loop

for (i in seq_along(move_files)){
  
  this_file <- move_files[i]
  
  this_dir_path <- dirname(this_file)
  
  #last subfolder : basename(this_dir_path). can chain...??
  # might need tools::file_path_sans_ext()
  
  #name pieces, important for saving later
  # this_huc <- 
  # this_runid_str <- 
  # this_year <- 
  
}


