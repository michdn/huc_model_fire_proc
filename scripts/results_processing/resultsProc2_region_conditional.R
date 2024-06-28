

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf)
  

### User settings ---------------------------------------------

#region or folder name for variants/reruns #"SC", "SCbl", "SCbw", etc. 
# for baseline/baseweather, make sure to align with settings below
reg_code <- "NC"

#Include FVS or not?
# interim datasets may be created before FVS summary data available 
incl_FVS <- FALSE
# running for BASEWEATHER results 
#  (only relevant when adding FVS results as baseline gets used for baseweather too)
run_baseweather <- FALSE


#whether or not to update the HUC area (hucAc) in HUCs with
# the nonburnable/coastal issue. 
# Make sure to align with run. Most everything past 202405 is TRUE
update_hucac_nb <- TRUE


# folders
input_folder <- file.path('results', 'extracts')

output_folder <- file.path('results', 'conditional')
dir.create(output_folder, recursive = TRUE)


### HUC & FVS data ----------------------------------------------------

#to get area for priority groups
hucs_shp <- st_read("data/data_huc/TxHucsTimingGroups.shp")
#hucs_shp <- st_read("data/data_huc/TxPrctRankRrkWipRffc.shp")

#Anna's FVS results
if (incl_FVS) {
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
  
  
  #if both FVS and baseweather
  if (run_baseweather){
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
    
  } # end run_baseweather
  
} # end incl_FVS


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

if (nrow(combined %>% filter(is.na(HaCBP))) > 0){
  stop("Explicit missing HaCBP")
}

if (nrow(combined %>% filter(is.na(HaCFL))) > 0){
  stop("Explicit missing HaCFL")
}


# IMPLICIT

combined_expand <- combined %>% 
  #NOT runID b/c can't associate it to huc/region/priority/intensity/type without losing some combinations
  # will add back in later
  expand(nesting(HUC12, Region), Priority, TxIntensity, TxType, Year)

#If combined_expand has the same number as combined, then there are no implicit missing. 
nrow(combined)
if (!all.equal(nrow(combined), nrow(combined_expand))){
  stop("Implicit missing, check and repair as necessary.")
}

res <- combined

#if implicit missin:
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


if (incl_FVS){
  #join FVS results with fire results
  res_all <- fvs %>% 
    inner_join(res %>% 
                 mutate(Year = as.numeric(Year)),
               by = c("HUC12", "Region", "Priority", "TxIntensity", "TxType", "run", "Year"))
  nrow(res_all) 
  
  # IMPORTANT!!  Confirm still same number as combined
  if (!all.equal(nrow(res_all), nrow(combined))){
    stop("Join failure with FVS results")
  }
 
  # missing <- res %>% 
  #   dplyr::select(HUC12, Region, Priority, TxIntensity, TxType, run, Year) %>% 
  #   left_join(res_all, 
  #             by = c("HUC12", "Region", "Priority", "TxIntensity", "TxType", "Year")) %>% 
  #   mutate(missing = ifelse(is.na(run.y), TRUE, FALSE)) %>% 
  #   filter(missing)
  
} else {
  res_all <- res
}#end incl_FVS


### Add in area and timing groups ----------------------------

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
} # end update_hucac_nb

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

if (incl_FVS){
  file_nm <- paste0(reg_code, '_conditional_', stamp, '.csv')
} else {
  file_nm <- paste0(reg_code, '_conditional_NOFVS_', stamp, '.csv')
}

write_csv(res_all, 
          file.path(output_folder, 
                    file_nm))


### error checking --------------
# cbp %>% group_by(HUC12, Year) %>% summarize(count = n()) %>% head()
# #each HUC should have 21600 rows (with data). 5400 per year. 
# 
# cbp_nc <- cbp %>% 
#   as_tibble() %>% 
#   filter(is.na(huc_burned_frac))
# #NC 42,768 rows with NA
# 
# cbp_nc %>% group_by(HUC12) %>% summarize(count = n())
# # HUC12        count
# # 180101070302 21492
# # 180101070401 21276
# 
# cbp_nc %>% group_by(HUC12, Priority) %>% summarize(count = n())
# cbp_nc %>% group_by(HUC12, TxType) %>% summarize(count = n())
# cbp_nc %>% group_by(HUC12, TxIntensity) %>% summarize(count = n())
# cbp_nc %>% group_by(HUC12, Year) %>% summarize(count = n())
# 
# cbp_nc %>% 
#   group_by(HUC12, Priority, TxType, TxIntensity, Year) %>% 
#   summarize(count = n()) %>% View()
# 
# # 180101070302 always has 199 fires NA
# # 180101070401 always has 197 fires NA
# 
# cfl %>% as_tibble() %>% filter(is.na(huc_avg_fl)) %>% 
#   group_by(HUC12) %>% summarize(count = n())
# 
# nb_hucs %>%  
#   filter(huc12 %in% c("180101070302","180101070401"))
# nb_hucs %>% arrange(desc(nonburn_perc))
# 
# igni <- readRDS("results/extracts/NC_errors/NC_ignitions_from_sql.RDS")
# igni %>% 
#   as_tibble() %>% 
#   filter(HUC12 %in% c("180101070302","180101070401")) %>% 
#   group_by(HUC12, Priority, TxIntensity, TxType, Year) %>% 
#   summarize(count = n()) %>% 
#   filter(count < 200)
# 
# cbp %>% 
#   as_tibble() %>% 
#   filter(HUC12 %in% c("180101070302","180101070401")) %>% 
#   #filter(!is.na(huc_burned_frac)) %>% filter(huc_burned_frac > 0)
#   group_by(HUC12, Priority, TxIntensity, TxType, Year, huc_burned_frac) %>% 
#   summarize(count = n()) %>% View()
# 
# cbp %>% 
#   as_tibble() %>% 
#   filter(HUC12 %in% c("180101070302","180101070401")) %>% 
#   filter(huc_burned_frac == 0) %>% 
#   group_by(HUC12, fire_i) %>% 
#   summarize(count = n())
# 
# 
# igni %>% 
#   as_tibble() #%>% View()
#   filter(HUC12 == "180101070302") # %>% View()
#   #group_by(fbfm40_number) %>% summarize(count = n())
