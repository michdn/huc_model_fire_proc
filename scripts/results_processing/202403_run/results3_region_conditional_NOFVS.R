

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf)
  

### User settings ---------------------------------------------

#whether or not to update the HUC area (hucAc) in HUCs with
# the nonburnable/coastal issue. 
# MAKE SURE TO ALIGN WITH RUN! 
update_hucac_nb <- FALSE #TRUE

#region or folder name for variants/reruns
reg_code <- "extreme_wind_1k"

#reg_code <- "WindTempRH98-100_1k"
#reg_code <- "top200Wind" #"top200RH" #"temp87to100percentile" #"temp74to87percentile" #"temp61to74percentile"
#reg_code <- "top200Wind_1k"
#reg_code <- "top200RH_1k"
#reg_code <- "temp87to100percentile_baseline_1k"
#reg_code <- "temp74to87percentile_baseline_1k"
#reg_code <- "temp61to74percentile_baseline_1k"



input_folder <- file.path("qa", "qa_weather")
output_folder <- file.path("qa", "qa_weather")

#input_folder <- file.path('results', 'extracts')
# file.path('run_202401_badblend', 'results_raw_extraction_test') 
 #file.path('results', 'csv_extraction')

#output_folder <- file.path('results', 'conditional')
#dir.create(output_folder, recursive = TRUE) 


### HUC & FVS data ----------------------------------------------------

#to get area for priority groups
hucs_shp <- st_read("data/data_huc/TxHucsTimingGroups.shp")
  #st_read("data/data_huc/TxPrctRankRrkWipRffc.shp")

# #Anna's FVS results
# fvs <- read_csv(file.path('results',
#                           'FVSprocessedOutputsHucs_v2.csv')) %>%
#   mutate(HUC12 = as.character(HUC12)) %>% 
#   rename(Region = RRK)


### SQL extraction, every fire results -------------------------

cbp <- read_csv(file.path(input_folder, 
                          paste0(reg_code, '_cbp_all_fires_from_sql.csv'))) %>% 
  mutate(HUC12 = as.character(HUC12))
cfl <- read_csv(file.path(input_folder, 
                          paste0(reg_code, '_cfl_all_fires_from_sql.csv'))) %>% 
  mutate(HUC12 = as.character(HUC12))

cft_hist <- read_csv(file.path(input_folder, 
                               paste0(reg_code, '_cft_hist_from_sql.csv'))) %>% 
  mutate(HUC12 = as.character(HUC12))
#cfl_hist <- read_csv(file.path(input_folder, paste0(reg_code, '_cfl_hist_from_sql.csv')))


### Summarize and combine --------------------------------------------------

hacbp <- cbp %>% 
  group_by(HUC12, Region, Priority, TxIntensity, TxType, run, Year, mas_scenario) %>% 
  summarize(HaCBP = mean(huc_burned_frac), .groups='drop')

hacfl <- cfl %>% 
  group_by(HUC12, Region, Priority, TxIntensity, TxType, run, Year, mas_scenario) %>% 
  summarize(HaCFL = mean(huc_avg_fl), 
            hacfl_avesq = mean(huc_avg_fl^2),
            .groups='drop')

hacft_hist <- cft_hist %>% 
  group_by(HUC12, Region, Priority, TxIntensity, TxType, run, Year, mas_scenario, fire_type) %>% 
  summarize(burn_frac_ave = mean(huc_burned_frac_in_ftype_bin), .groups='drop')

#pivot fire type results from longer to wider
ft <- hacft_hist %>%
  #1 for surface fire, 2 for passive crown fire, 3 for active
  mutate(type = case_when(
    fire_type == 1 ~ "surface",
    fire_type == 2 ~ "passive_crown",
    fire_type == 3 ~ "active_crown",
    TRUE ~ "unknown"
  )) %>%
  pivot_wider(
    id_cols = c(HUC12, Region, Priority, TxIntensity, TxType, run, Year, mas_scenario),
    names_from = type,
    values_from = burn_frac_ave)

ft

#combined metrics
combined <- hacbp %>% 
  left_join(hacfl, 
            by = join_by(HUC12, Region, Priority, TxIntensity, TxType, 
                         run, Year, mas_scenario)) %>% 
  left_join(ft,
            by = join_by(HUC12, Region, Priority, TxIntensity, TxType, 
                         run, Year, mas_scenario))

### Missing check -------------------------------------------------

# EXPLICIT
combined %>% filter(is.na(HaCBP))

# IMPLICIT

combined_expand <- combined %>% 
  #NOT runID b/c can't associate it to huc/region/priority/intensity/type without losing some combinations
  # will add back in later
  expand(nesting(HUC12, Region), Priority, TxIntensity, TxType, Year)

#Region: : same number, no implicit missing. 

res <- combined

#if was:
# combined_full <- combined_expand %>% 
#   left_join(main, by = c("HUC12", "Region", "Priority", "TxIntensity", "TxType", "Year")) %>% 
#   mutate(missing_flag = ifelse(is.na(run), TRUE, FALSE))
# 
# 
# # Make crosswalk that contains run, so can add run back in here
# run_xwalk <- combined %>% 
#   dplyr::select(HUC12, Region, Priority, TxIntensity, run, mas_scenario) %>% 
#   distinct() %>% 
#   rename(run_update = run,
#          mas_update = mas_scenario)
# 
# #add run back in for NA (rows that had been missing)
# combined_full <- combined_full %>% 
#   left_join(run_xwalk, by = join_by(HUC12, Region, Priority, TxIntensity)) %>% 
#   mutate(run = ifelse(missing_flag, run_update, run),
#          mas_scenario = ifelse(missing_flag, mas_update, mas_scenario))
# 
# 
# #remove missing_flag, select(-run_update)
# combined_full <- combined_full %>% 
#   dplyr::select(-missing_flag, -run_update)

# res <- combined_full

### Add FVS -------------------------------------------------------------------

#FVS has Hybrid labeled as Hybrid (not older RFFC)
#rename Priority RFFC to Hybrid
res <- res %>%
  #change name to Hybrid
  mutate(Priority = ifelse(Priority == 'RFFC', 'Hybrid', Priority))


# #join FVS results with fire results
# res_all <- fvs %>% 
#   inner_join(res,
#              by = c("HUC12", "RRK", "Priority", "TxIntensity", "TxType", "run", "Year"))
# nrow(res_all) # confirm still 306,396

res_all <- res

### Rename RFFC, Add in area and percentile groups ----------------------------

# TxBpPrct == for fire priority
# TxRffcP == for RFFC (aka hybrid) priority
# TxWPrct == for WUI priority
res_all <- res_all %>%
  left_join(hucs_shp %>%
              st_drop_geometry() %>%
              select(huc12,  hucAc,
                     fireGrp, hybrdGr, wuiGrop,
                     timeFir, tmHybrd, timeWui) %>%
              rename(fireGroup = fireGrp,
                     hybridGroup = hybrdGr,
                     wuiGroup = wuiGrop,
                     timeFire = timeFir,
                     timeHybrid = tmHybrd) %>% 
              #better sorting with yr_epoch format
              mutate(across(c(timeFire, timeHybrid, timeWui),
                            ~ case_when(
                              . == "yr1to5" ~ paste0("2024_", .),
                              . == "yr6to10" ~ paste0("2029_", .),
                              . == "yr11to15" ~ paste0("2034_", .),
                              . == "yr16to20" ~ paste0("2039_", .),
                              . == "yr1to5_16to20" ~ paste0("2024_2039_", .),
                              . == "notTreated" ~ "Not treated"))),
            by = join_by("HUC12" == "huc12"))


if (update_hucac_nb){
  res_all <- res_all %>% 
    left_join(nb_hucs %>% 
                dplyr::select(huc12, hucAc) %>% 
                rename(hucAc_new = hucAc),
              by = join_by("HUC12" == "huc12")) %>% 
    mutate(hucAc = if_else(!is.na(hucAc_new), hucAc_new, hucAc)) %>% 
    #dplyr::select(HUC12, hucAc, hucAc_new, hucAc2) %>% View()
    dplyr::select(-hucAc_new)
}


#reorder nicely
res_all <- res_all %>%
  select(HUC12, Region,
         Priority, TxIntensity, TxType,
         run, Year, mas_scenario, 
         hucAc,
         timeFire, timeHybrid, timeWui,
         fireGroup, hybridGroup, wuiGroup,
         everything())


### Save out region conditional ---------------------------------------

stamp <- format(Sys.time(), "%Y%m%d")

write_csv(res_all, 
          file.path(output_folder, 
                    paste0(reg_code, '_conditional_NOFVS_', stamp, '.csv')))



