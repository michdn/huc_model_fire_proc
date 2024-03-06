Important scripts

scripts/results_preweighting/results1_sql.R : This extracts results from the sqlite databases. It takes a hours to run per set. Note: SN+CC was split into four sets. It produces csv files per set. 

scripts/results_preweighting/results2_datacube_preweighted.R : This collates all the csv extraction and turns it into one row per HUC-scenario-year (309,396 rows). This is only the CONDITIONAL metrics, the absolute metrics have not yet been calculated. 

scripts/weighting/weighting1_absolutemetrics.R : This calculates the weighted/absolute expected metrics from the conditional metrics and the UCSB Park et al 2021 annual burn probability map. 

scripts/weighting/fire_metrics_drilldown.Rmd : rmd for the use case document