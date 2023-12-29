See: https://docs.google.com/document/d/1Ii-no0iXaHaqOs7gHi4Q1WtiJ0zt-rKjwcct1W6qVMw/edit?usp=sharing

Log of data acquisition and processing: fuels_weather.xlsx
Recap of scripts and process: 

Data Acquisition:
On Windows VM Bluejay (10.1.30.113), there’s an R project for transferring data from Chickadee to my laptop (from bluejay I can connect to both at the same time). Script transfer_bluejay_laptop.R - edit the region it is grepping for and run to copy it into  nekodawn/code_local/huc_val_gridfire/data/data_fuels_bluejay/. 

Data Processing:
In nekodawn/code_local/huc_val_gridfire there are several important scripts. 
folder_building.R: This creates the folder structure, HUC indicator (with the 1200m buffer extent), and the topographical data (clip/projected/aligned with HUC indicator). 
The zipped folder ‘hucs_val_foldersetup.zip’ is this before any other data processing (fuels, weather). Takes roughly an hour to generate. 
fuels_addition.R: This adds in fuel data, clipped/projected/aligned with HUC indicator. Depending on how many fuel files are being processed at a time this can take several to dozens of hours. Try to keep it to shorter (~5 hours) runs except overnight runs, b/c I feel like R has a memory leak and longer processes take longer than it should. 
Track runs here: fuels_weather.xlsx
General flow: 1) From data/data_fuels_holding_{REGION} folders, move some number of fuels to data/data_fuels_in. 2) Run the script that does the processing of these files. 3) After it has finished, move files from data/data_fuels_in to data_fuels_done_{REGION}. 4) Once a region has been fully processed, zip the fuels_done folder to conserve space. 
This writes to a rudimentary log file log.csv. DO NOT OPEN THIS FILE WHILE PROCESSING. If you must see inside during a run, copy the file and open the copy. There is no error handling and you will error out the processing if you alter the log file. It has the time of completion per fuel layer processed. 
weather_addition.R: This copies the weather files (jsons from Anna) into the appropriate HUC & year folders. This is fast because it is just a file copy with no spatial processing. 

One folder for all regions ended up being far, far too large. Created region_extraction.R script that will MOVE a particular region files out of the folder and into another one. Move was used as I did not have space for copy (out of disk space errors). So workflow will be the same as below for fuel and weather additions, but when a particular region fuel is finished, then you extract the FINISHED region. (Do not extract before adding weather and all of the region’s fuel data.) 

After the region folder is created, it is zipped (if possible), and uploaded to SIG’s FTP server via FileZilla under ‘MAS_Data’ folder that Jeff created for this. Let him know and then he will move it to mnt/tahoe. 
