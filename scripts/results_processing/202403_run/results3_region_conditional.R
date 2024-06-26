

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf)
  

### User settings ---------------------------------------------

#whether or not to update the HUC area (hucAc) in HUCs with
# the nonburnable/coastal issue. 
# MAKE SURE TO ALIGN WITH RUN! ONLY UPDATE WITH RUNS WITH NBFIXES IN PLACE! 
#  Fixes: All 'bl'; all 'bw'; the 'nbonly' reruns (SC, CC, SN); 
#   and NC (but after 'gridfire_prep/nonburnable_huc_indicator.R', gridfire run, etc.)
update_hucac_nb <- TRUE

#region or folder name for variants/reruns #"SC", "SCbl", "SCbw", etc. 
reg_code <- "SNbl"

input_folder <- file.path('results', 'extracts')

output_folder <- file.path('results', 'conditional')
dir.create(output_folder, recursive = TRUE)


### HUC & FVS data ----------------------------------------------------

#to get area for priority groups
hucs_shp <- st_read("data/data_huc/TxHucsTimingGroups.shp")
#hucs_shp <- st_read("data/data_huc/TxPrctRankRrkWipRffc.shp")

#Anna's FVS results
fvs_orig <- read_csv(file.path('data',
                          'data_fvs',
                          'FVSprocessedOutputsHucsScCcSnBase.csv'))
fvs <- fvs_orig %>% 
  mutate(huc12 = as.character(huc12)) %>% 
  rename(Region = region,
         HUC12 = huc12,
         TxIntensity = Intensity) %>% 
  #do not need these duplicated/will be duplicated fields
  dplyr::select(-c(regionName,hucAc,
                   wuiGroup,fireGroup,hybridGroup,
                   timeWui,timeFire,timeHybrid)) 

# both baseline and baseweather use the FVS baseline values
# > fvs %>% filter(run == "Baseline") %>% select(Priority, TxIntensity, TxType, run) %>% distinct()
# # A tibble: 1 x 4
# Priority TxIntensity TxType   run     
# <chr>    <chr>       <chr>    <chr>   
#   1 baseline baseline    baseline Baseline
#
# cbp %>% select(Priority, TxIntensity, TxType, run) %>% distinct()
# Priority TxIntensity      TxType    run
# 1 baseweather baseweather baseweather RunIDx

#need to duplicate FVS baseline values for (GF) baseweather results
fvs_bw <- fvs %>% 
  filter(run == "Baseline") %>% 
  mutate(Priority = "baseweather",
         TxIntensity = "baseweather",
         TxType = "baseweather") 

fvs <- fvs %>% 
  bind_rows(fvs_bw) %>% 
  #need to match up baseline/baseweather 'run' values. 
  mutate(run = if_else(run == "Baseline", "RunIDx", run))
  

if (update_hucac_nb){
  nb_hucs <- readRDS("data/nonburnable_rerun_list.RDS")
}

### SQL extraction, every fire results -------------------------

cbp <- readRDS(file.path(input_folder, 
                          paste0(reg_code, '_cbp_all_fires_from_sql.RDS'))) 
cfl <- readRDS(file.path(input_folder, 
                          paste0(reg_code, '_cfl_all_fires_from_sql.RDS'))) 

cft_hist <- readRDS(file.path(input_folder, 
                               paste0(reg_code, '_cft_hist_from_sql.RDS')))

# cbp <- read_csv(file.path(input_folder, 
#                           paste0(reg_code, '_cbp_all_fires_from_sql.csv'))) %>% 
#   mutate(HUC12 = as.character(HUC12))
# cfl <- read_csv(file.path(input_folder, 
#                           paste0(reg_code, '_cfl_all_fires_from_sql.csv'))) %>% 
#   mutate(HUC12 = as.character(HUC12))
# 
# cft_hist <- read_csv(file.path(input_folder, 
#                                paste0(reg_code, '_cft_hist_from_sql.csv'))) %>% 
#   mutate(HUC12 = as.character(HUC12))
##cfl_hist <- read_csv(file.path(input_folder, paste0(reg_code, '_cfl_hist_from_sql.csv')))


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

#If combined_expand has the same number as combined, then there are no implicit missing. 
nrow(combined)/4
all.equal(nrow(combined), nrow(combined_expand))

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


#join FVS results with fire results
res_all <- fvs %>% 
  inner_join(res %>% 
               mutate(Year = as.numeric(Year)),
             by = c("HUC12", "Region", "Priority", "TxIntensity", "TxType", "run", "Year"))
nrow(res_all) 

# IMPORTANT!!  Confirm still same number as combined
all.equal(nrow(res_all), nrow(combined))


# missing <- res %>% 
#   dplyr::select(HUC12, Region, Priority, TxIntensity, TxType, run, Year) %>% 
#   left_join(res_all, 
#             by = c("HUC12", "Region", "Priority", "TxIntensity", "TxType", "Year")) %>% 
#   mutate(missing = ifelse(is.na(run.y), TRUE, FALSE)) %>% 
#   filter(missing)


### Add in area and timing groups ----------------------------
#old fields
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
    rename(hucAc_old = hucAc) %>% 
    mutate(hucAc = if_else(!is.na(hucAc_new), hucAc_new, hucAc_old)) 
    
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
                    paste0(reg_code, '_conditional_', stamp, '.csv')))



