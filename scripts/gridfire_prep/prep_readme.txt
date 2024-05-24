Notes for the 2024 May (202405) reruns. New weather, adjustment factor for CBH, (fuel moisture?). No baselines/baseweathers.

Goal: Create per HUC data for input into gridfire.

Data sources:
 Topographical/HUC: Using LF topo data, Anna's shapefile for HUC12s. 
 Fuel: Using region-wide fuel rasters created by Dave, blending FVS results and nonforest fuel layers (LF, with nonforest treatment blocks created by Kyle). 
 Weather: Created by Anna. 

Data processing scripts in huc_model_fire/scripts/prep:

 prep1_folder_building_region.R: 
	This creates the basic folder structure of region/huc{ID} with topography, weather, and fuels subdirectories. (No scenario subfolders, just the base the fuels folder.) This adds the topographical data (aspect, elevation, slope) and the HUC indicator (uncorrected, see prep1b). 

 prep1b_nonburnable_huc_indicator.R: 
	This corrects the HUC indicators for the nonburn/coastal issue identified HUCs. For HUCs with >= 50% area as nonburnable (FMFB40 fuels) plus one additional coastal waters HUC (48%), it corrected the HUC indicator to only includes burnable area. This is so that gridfire as it is putting in ignitions, there will be fires that can burn (and therefore the stats aren't dependent on just a few fires). This problem was discovered with coastal waters HUCs (HUCs on the coast that include a lot of the coastal waters in the HUC polygon). This script can be done in any order with the rest. 
	
 prep2a_blended_raster_fuels_archive.R: 
	This collects all the blended fuels region-wide rasters from the shared drive in the many subfolders and collates them into a bluejay local 'archive' folder. Used to then 1) zip and save intermediate files in Drive, and 2) the fuels addition script will pull from here, which will clean up that script a lot. This script has lots of exceptions by each region (NC trt7 replacing trt4, various trt6 reruns from different folders, SN not being fully FVS-unarchived from Jan run to combine with March FVS data, etc.)

 prep2b_fuels_addition_parallel_fromarchive.R: 
	This takes the region-wide fuel rasters and chops them up per HUC and puts them in the right folders. This pulls from the archive created in prep2a. This is parallelized! Takes about 6 - 12 hours depending on region. This should be run on bluejay (access to archive and all the CPUs/memory). 


 prep3_weather_addition.R:
	This takes Anna's weather JSONs and renames and copies them into the right HUC folders. This only takes a few minutes. 





-----------------------------------------------------------------------------
Notes from 2024 March rerun, with good fuel blends, running part on bluejay. 

Goal: Create per HUC data for input into gridfire. 

Data acquisition: Fuels no longer have to be copied from Dave's linux VM share to my laptop via Bluejay. Instead, we will run that fuel addition step on Bluejay itself. 

Data Processing:
In nekodawn/code_local/huc_model_fire/scripts/prep there are several important scripts. 

newprep1_folder_building_region.R: 
This creates the folder structure, HUC indicator (with the 1200m buffer extent), and the topographical data (clip/projected/aligned with HUC indicator). It can be run for a single region or list of regions. This is now written to be parallelized and only takes a few minutes now. 
The zipped folder ‘hucs_gf_foldersetup.zip’ is this for all four regions before any other data processing (fuels, weather). 
(This was run on laptop first, then moved up to Google drive to download to Bluejay. Future runs, if needed, could be moved to bluejay, but we'd have to get the topo data and such as well.) 

newprep2_weather_addition.R:
This can be run after newprep1 immediately, or after newprep3_fuels_addition. This will add Anna's created weather JSON files to the HUC folder structure for the appropriate year. 

newprep2b_weather_addition_baselineweather2024.R:
A variant of newprep2 that is for the baseline weather scenario - when we hold 2024 weather for ALL years (2024, 2029, 2034, 2039). 

newprep3_fuels_addition_parallel.R:
This script adds the fuel layers (blended results from FVS and nonforest fm40 layers) to the HUC folders (cutting them up into HUC-sized pieces, same extent as the topographical data, as required by gridfire). 
We are pulling the fuel layers directly from the FVS output folders. The script creates target folders from the name of the RunIDs, plus a bunch of grepping / pattern searching. SN is particularly complicated as we were pulling fuels from three seperate sources (1. brand new data, 2. correctly blended data, 3. my copy of old data as this was still archived on the rem share.) 





---------------------------------------------------------------------
BELOW are notes from the original 2024 January run (bad fuel blends) and unparallelized looping scripts running on local laptop:

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
