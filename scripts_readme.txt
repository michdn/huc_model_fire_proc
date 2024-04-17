Brief synopsis of scripts and folders. 

scripts/gridfire_prep: Scripts to prep data for gridfire runs. See the readme in there. 

scripts/results_processing: Scripts to process gridfire results. 
-offshoot1 & 2: probably should be under qa_misc, just off-ones to calculate/pull various per fire data.
-<results1>: results1 ended up not being needed, it was to transfer from bluejay to local laptop, but we ended up running SQL extraction straight from bluejay this time. (It was not possible when gridfire had been run on pyregence.)
-results2_sql_extractevery.R: To be run on bluejay, for a specific region. This extracts the PER FIRE data from the sqlite databases (per HUC per scenario per time step) and collates them into a large regional dataset (rds and csv). 
-results3_regional_conditional.R: Can be run on bluejay, or local laptop (only if extraction files copied down first), for a specific region. This summarizes the per fire results into single rows for the HUC-scenario-year also creating the 'conditional' metrics. It joins with the HUC-summarized FVS data (created by Dave, modified by Anna) to add that information in. There is a variant script "NOFVS" that skips the FVS join step, if needed. 
-results4_region_absolutemetrics_baselineruns.R: For a specific region, this script uses the annual burn probability raster layer to transform the conditional metrics into absolute metrics. This produces a regional mini datacube. This version incorporates 'baseline' runs for that region and uses that for absolute metric conversion. May need modification for including 'baseweather' runs. 
-results5_datacube.R: Simply appends the four regional absolute metric mini datacubes into one datacube. Adds any finishing touches. Will need editing for incorporating nonburn-fix and other reruns. 

scripts/results: Script to graph, plot, and explore the results. 
-expt*: experimental/exploratory scripts
-fire_metrics_case_use.Rmd: RMD that is the backbone of the Case Use document. 
-map_prep_bestscenario.R: Creates the best scenario (by flame index) for each HUC and saves out the dataset for use in QGIS. Used for the map for the 2024-03-2x presentation. 
-scatter_500k2m_2039_fulldatacube.R and scatter_500k2m_2039_region.R: Scatter plots for QA, Comparison document, and general exploration into results between 500k and 2m intensities at a high level. 
-timeseries_priority_trtintensity.R & timeseries_trt_priorityintensity.R: Different ways to display as much comparison data as possible on one timeseries graph. The first, for each priority, average the metric and show different treatments (color) and intensities (line type). The second, for each treatment, average the metric and show different priorities (color) and intensities (line type). May obscure HUC variation, but gives an overall comparison between the different scenarios on few graphs. 
-timeseries_treatmentgroups.R: Superceded. Boxplot timeseries, the ones David Saah originally asked for on one of the early calls. Facetted - each row-set is an intensity (500k/1m/2m), and each column-set is a timing group (e.g. treated in 2024, 2029, 3034, 2039). The x-axis are years, and y-axis is the metric (multiple metrics). Script loops for all regions, all priorities, all treatment types. Each plot is one region-priority-trt over time. Superceded by timeseries_treatmentgroups_baselines.R
-timerseries_treatmentgroups_baselines.R: Boxplot timeseries, the ones David Saah originally asked for on one of the early calls. Facetted - each row-set is an intensity (500k/1m/2m), and each column-set is a timing group (e.g. treated in 2024, 2029, 3034, 2039). The x-axis are years, and y-axis is the metric (multiple metrics). Script loops for all regions, all priorities, all treatment types. Each plot is one region-priority-trt over time. This version incorporates 'baseline' as an intensity for all priorities-treatment-types. May need modification for 'baseweather' inclusion. 
-timeseries_treatmentgroups_FVS.R: Timeseries boxplots for some of the FVS data to compare to. 
-timeslice_treatmentgroups.R: Superceded. Boxplot of fire metric (multiple) by treatment intensity (x-axis) for a particular year/epoch. Facetted by treatment type (row-sets) and by treatment timing (column-sets). Superceded by timeslice_treatmentgroups_baselines.R
-timeslice_treatmentgroups_baselines.R: Boxplot of fire metric (multiple) by treatment intensity (x-axis) for a particular year/epoch. Facetted by treatment type (row-sets) and by treatment timing (column-sets). This version incorporates 'baseline' as an intensity for all priorities-treatment-types. May need modification for 'baseweather' inclusion.


scripts/qa_misc: Miscellaneous one-off scripts, or qa checks. 



scripts/chickadee_transfer: Scripts to transfer data from rem share to local laptop, mostly used now for QAQC of FVS blended results. 



-----------------------------------------------------------
OLD NOTES

scripts/results_preweighting/results1_sql.R : This extracts results from the sqlite databases. It takes a hours to run per set. Note: SN+CC was split into four sets. It produces csv files per set. 

scripts/results_preweighting/results2_datacube_preweighted.R : This collates all the csv extraction and turns it into one row per HUC-scenario-year (309,396 rows). This is only the CONDITIONAL metrics, the absolute metrics have not yet been calculated. 

scripts/weighting/weighting1_absolutemetrics.R : This calculates the weighted/absolute expected metrics from the conditional metrics and the UCSB Park et al 2021 annual burn probability map. 

scripts/weighting/fire_metrics_drilldown.Rmd : rmd for the use case document