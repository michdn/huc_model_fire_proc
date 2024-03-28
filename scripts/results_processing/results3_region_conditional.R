

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf)
  

### User settings ---------------------------------------------

reg_code <- "SC"

input_folder <- file.path('results', 'extracts')
# file.path('run_202401_badblend', 'results_raw_extraction_test') 
 #file.path('results', 'csv_extraction')

output_folder <- file.path('results', 'conditional')
dir.create(output_folder, recursive = TRUE) 


### HUC & FVS data ----------------------------------------------------

#to get area for priority groups
hucs_shp <- st_read("data/data_huc/TxHucsTimingGroups.shp")
#hucs_shp <- st_read("data/data_huc/TxPrctRankRrkWipRffc.shp")

#Anna's FVS results
fvs <- read_csv(file.path('data',
                          'data_fvs',
                          'FVSprocessedOutputsHucsScCcSn.csv')) %>%
  mutate(huc12 = as.character(huc12)) %>% 
  rename(Region = region,
         HUC12 = huc12,
         TxIntensity = Intensity)


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
  summarize(HaCFL = mean(huc_avg_fl), .groups='drop')

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

#join FVS results with fire results
res_all <- fvs %>% 
  inner_join(res,
             by = c("HUC12", "Region", "Priority", "TxIntensity", "TxType", "run", "Year"))
nrow(res_all) # confirm still 306,396


### Rename RFFC, Add in area and percentile groups ----------------------------

#rename Priority RFFC to Hybrid
res_all <- res_all %>%
  #change name to Hybrid
  mutate(Priority = ifelse(Priority == 'RFFC', 'Hybrid', Priority))


# TxBpPrct == for fire priority
# TxRffcP == for RFFC (aka hybrid) priority
# TxWPrct == for WUI priority
res_all <- res_all %>%
  left_join(hucs_shp %>%
              st_drop_geometry() %>%
              select(huc12,  hucAc,
                     TxBpPrc, TxRffcP, TxWPrct,
                     timeFire, timeHybrid, timeWUI) %>%
              rename(fireGroup = TxBpPrc,
                     hybridGroup = TxRffcP,
                     wuiGroup = TxWPrct),
            by = join_by("HUC12" == "huc12"))


#reorder nicely
res_all <- res_all %>%
  select(HUC12, Region,
         Priority, TxIntensity, TxType,
         run, Year, mas_scenario, 
         hucAc,
         timeFire, timeHybrid, timeWUI,
         fireGroup, hybridGroup, wuiGroup,
         everything())


### Save out region conditional ---------------------------------------

stamp <- format(Sys.time(), "%Y%m%d")

write_csv(res_all, 
          file.path(output_folder, 
                    paste0(reg_code, '_conditional_', stamp, '.csv')))



