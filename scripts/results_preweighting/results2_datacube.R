# Script to collate data, also for data cube

#Fire modeling outputs: burn probability, flame length, 
#  % active crown fire, % passive crown fire, % surface fire
# calculate fire type percentages
# add/calc fire size areas
# add FVS data

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf)

### User settings ---------------------------------------------

input_folder <- 'results_csv'
output_folder <- 'results_csv'

### HUC data ----------------------------------------------------

#to get area for fire size calculations
# and priority groups
hucs_shp <- st_read("data/data_huc/TxPrctRankRrkWipRffc.shp")


### Main Data import & prep -------------------------------------------

#main data
main_sc <- read_csv(file.path(input_folder, 
                              'main_results_from_sql_SC.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

main_nc <- read_csv(file.path(input_folder,
                              'main_results_from_sql_NC.csv')) %>%
  mutate(HUC12 = as.character(HUC12))

main_sn_cc1 <- read_csv(file.path(input_folder, 
                                  'main_results_from_sql_SN_CC_thru180201220206.csv')) %>%
  mutate(HUC12 = as.character(HUC12))
main_sn_cc2 <- read_csv(file.path(input_folder, 
                                  'main_results_from_sql_SN_CC_thru180300100704.csv')) %>%
  mutate(HUC12 = as.character(HUC12))
main_sn_cc3 <- read_csv(file.path(input_folder, 
                                  'main_results_from_sql_SN_CC_thru180500040805.csv')) %>%
  mutate(HUC12 = as.character(HUC12))
main_sn_cc4 <- read_csv(file.path(input_folder, 
                                'main_results_from_sql_SN_CC_thru180902060702.csv')) %>%
  mutate(HUC12 = as.character(HUC12))

#bind
main <- bind_rows(main_sc, main_nc,
                 main_sn_cc1, main_sn_cc2, main_sn_cc3, main_sn_cc4) %>% 
  #sort nicely
  arrange(RRK, HUC12, Priority, TxIntensity, TxType, Year)

# expecting 2837
main %>% pull(HUC12) %>% unique() %>% length()  

# nominally 306396, missing are implicitly missing
nrow(main)
#306355, or 41 implicit missing

#explicit missing
main %>% filter(is.na(HaCBP))
# zero


## Expand to fill all implicit missing as explicit missing

main_expand <- main %>% 
  #NOT runID b/c can't associate it to huc/region/priority/intensity/type without losing some combinations
  # will add back in later
  expand(nesting(HUC12, RRK), Priority, TxIntensity, TxType, Year)

main_full <- main_expand %>% 
  left_join(main, by = c("HUC12", "RRK", "Priority", "TxIntensity", "TxType", "Year")) %>% 
  mutate(missing_flag = ifelse(is.na(run), TRUE, FALSE))


# Make crosswalk that contains run, so can add run back in here
run_xwalk <- main %>% 
  dplyr::select(HUC12, RRK, Priority, TxIntensity, run) %>% 
  distinct() %>% 
  rename(run_update = run)

#add run back in for NA (rows that had been missing)
main_full <- main_full %>% 
  left_join(run_xwalk, by = join_by(HUC12, RRK, Priority, TxIntensity)) %>% 
  mutate(run = ifelse(missing_flag, run_update, run))
  

#remove missing_flag, select(-run_update)
main_full <- main_full %>% 
  dplyr::select(-missing_flag, -run_update)
  
main_full %>% filter(is.na(HaCBP)) # %>% View()
#41, all SN Hybrid 1m trt 6, some HUCs missing all years. e.g. 180201210201
main_full %>% filter(is.na(HaCBP)) %>% pull(HUC12) %>% unique()
#15 HUCs affected


### Fire type Data import & prep -------------------------------------------

#fire type data
ft_sc <- read_csv(file.path(input_folder,
                            'cft_results_from_sql_SC.csv')) %>%
  mutate(HUC12 = as.character(HUC12))
ft_nc <- read_csv(file.path(input_folder,
                            'cft_results_from_sql_NC.csv')) %>%
  mutate(HUC12 = as.character(HUC12))

ft_sn_cc1 <- read_csv(file.path(input_folder, 
                            'cft_results_from_sql_SN_CC_thru180201220206.csv')) %>%
  mutate(HUC12 = as.character(HUC12))
ft_sn_cc2 <- read_csv(file.path(input_folder, 
                            'cft_results_from_sql_SN_CC_thru180300100704.csv')) %>%
  mutate(HUC12 = as.character(HUC12))
ft_sn_cc3 <- read_csv(file.path(input_folder, 
                            'cft_results_from_sql_SN_CC_thru180500040805.csv')) %>%
  mutate(HUC12 = as.character(HUC12))
ft_sn_cc4 <- read_csv(file.path(input_folder, 
                            'cft_results_from_sql_SN_CC_thru180902060702.csv')) %>%
  mutate(HUC12 = as.character(HUC12))

#bind
ft <- bind_rows(ft_sc, ft_nc, 
                ft_sn_cc1, ft_sn_cc2, ft_sn_cc3, ft_sn_cc4) %>% 
  #sort nicely
  arrange(RRK, HUC12, Priority, TxIntensity, TxType, Year)


ft %>% filter(is.na(burn_frac_ave))
#0 explicit missing 
nrow(ft) / 3
#306355, same as main with implicit missing


### Flame length import -----------------------------------------------------
#not used, but collated saved out if later needed

fl_sc <- read_csv(file.path(input_folder,
                            'cfl_results_from_sql_SC.csv')) %>%
  mutate(HUC12 = as.character(HUC12))
fl_nc <- read_csv(file.path(input_folder,
                            'cfl_results_from_sql_NC.csv')) %>%
  mutate(HUC12 = as.character(HUC12))

fl_sn_cc1 <- read_csv(file.path(input_folder, 
                                'cfl_results_from_sql_SN_CC_thru180201220206.csv')) %>%
  mutate(HUC12 = as.character(HUC12))
fl_sn_cc2 <- read_csv(file.path(input_folder, 
                                'cfl_results_from_sql_SN_CC_thru180300100704.csv')) %>%
  mutate(HUC12 = as.character(HUC12))
fl_sn_cc3 <- read_csv(file.path(input_folder, 
                                'cfl_results_from_sql_SN_CC_thru180500040805.csv')) %>%
  mutate(HUC12 = as.character(HUC12))
fl_sn_cc4 <- read_csv(file.path(input_folder, 
                                'cfl_results_from_sql_SN_CC_thru180902060702.csv')) %>%
  mutate(HUC12 = as.character(HUC12))

#bind
fl <- bind_rows(fl_sc, fl_nc, 
                fl_sn_cc1, fl_sn_cc2, fl_sn_cc3, fl_sn_cc4) %>% 
  #sort nicely
  arrange(RRK, HUC12, Priority, TxIntensity, TxType, Year)



## FVS results --------------------------------------------------------------------


#Anna's FVS results
fvs <- read_csv(file.path(input_folder,
                          'FVSprocessedOutputsHucs_v2.csv')) %>%
  mutate(HUC12 = as.character(HUC12))

nrow(fvs)
#fixed in v2 [1] 321624 ??
#306396

# fvs_dupls <- fvs %>% group_by(HUC12, Priority, TxIntensity, TxType, Year) %>% filter(n()>1) %>% summarize(n=n()) %>% ungroup()
#  
# fvs_dupls %>% 
#    left_join(fvs, by = c("HUC12", "Priority", "TxIntensity", "TxType", "Year")) #%>% View()
#  
# fvs_dupls %>% 
#    left_join(fvs, by = c("HUC12", "Priority", "TxIntensity", "TxType", "Year")) %>% 
#    select(HUC12, RRK) %>% 
#    distinct()
# 
# #duplicates will be dropped when I join fire data, will only keep HUCs in the regions as fire modeled


### Data processing and joining ------------------------------------------


#pivot fire type results from longer to wider
ft_wide <- ft %>% 
  #1 for surface fire, 2 for passive crown fire, 3 for active
  mutate(type = case_when(
    fire_type == 1 ~ "surface",
    fire_type == 2 ~ "passive_crown",
    fire_type == 3 ~ "active_crown",
    TRUE ~ "unknown"
  )) %>% 
  pivot_wider(
    id_cols = c(HUC12, RRK, Priority, TxIntensity, TxType, run, Year),
    names_from = type,
    values_from = burn_frac_ave) 

ft_wide


#calculate actual percentages
ft_wide_perc <- ft_wide %>% 
  mutate(total_fire_type = surface + passive_crown + active_crown,
         surfacePc = surface / total_fire_type * 100,
         pCrownPc = passive_crown / total_fire_type * 100,
         aCrownPc = active_crown / total_fire_type * 100) #%>% 
  #select(-c(surface, passive_crown, active_crown, total_fire))


#join with main results
res_fire <- main_full %>% 
  left_join(ft_wide_perc, 
            by = c("HUC12", "RRK", "Priority", "TxIntensity", "TxType", "run", "Year"))


#join FVS results with fire results
res_all <- fvs %>% 
  inner_join(res_fire,
             by = c("HUC12", "RRK", "Priority", "TxIntensity", "TxType", "run", "Year"))
nrow(res_all) # confirm still 306,396
  

#rename Priority RFFC to Hybrid
res_all <- res_all %>% 
  #change name to Hybrid
  mutate(Priority = ifelse(Priority == 'RFFC', 'Hybrid', Priority))


# not including this extra conditional metric
#Add in fire area. HaCBP is average of burned fraction of HUC,
# so area of HUC * burned fraction is fire size
# res_all <- res_all %>% 
#   left_join(hucs_shp %>%
#               st_drop_geometry() %>%
#               dplyr::select(huc12, hucAc), #acres
#             by = join_by("HUC12" == "huc12")) %>% 
#   mutate(fireSize = HaCBP * hucAc)

res_all

### Add in area and percentile groups ------------------------------------------------

# TxBpPrct == for fire priority
# TxRffcP == for RFFC (aka hybrid) priority
#  RENAME to TxHybPr
# TxWPrct == for WUI priority
res_all <- res_all %>%
  left_join(hucs_shp %>%
              st_drop_geometry() %>%
              select(huc12,  hucAc,
                     TxBpPrc, TxRffcP, TxWPrct) %>% 
              rename(fireGroup = TxBpPrc,
                     hybridGroup = TxRffcP,
                     wuiGroup = TxWPrct),
            by = join_by("HUC12" == "huc12"))


#reorder nicely
res_all <- res_all %>% 
  select(HUC12, RRK, 
         Priority, TxIntensity, TxType,
         run, Year,
         hucAc,
         fireGroup, hybridGroup, wuiGroup,
         everything())


### Save out ------------------------------------------

# #main results, collated
# write_csv(main, 
#           file.path(output_folder, 
#                     paste0('main_results_from_sql_all.csv')))
# 
# #fire type results, collated
# write_csv(ft,
#           file.path(output_folder,
#                     paste0('cft_results_from_sql_all_20240124.csv')))
# 
# #flame length results, collated
# write_csv(fl,
#           file.path(output_folder,
#                     paste0('cfl_results_from_sql_all_20240124.csv')))


#data cube
write_csv(res_all %>% 
            #no CIs but has CFT intermediate values
            dplyr::select(-hacbp_ci_low, -hacbp_ci_high, -hacfl_ci_low, -hacfl_ci_high), 
          file.path(output_folder, 
                    paste0('datacube_preweight_20240205.csv')))


#actual datacube created after weighting
# write_csv(res_all %>%
#             #no CIs
#             select(-hacbp_ci_low, -hacbp_ci_high, -hacfl_ci_low, -hacfl_ci_high),
#           file.path(output_folder,
#                     paste0('datacube_2024012.csv')))



### Parking lot -----------------------------------------

# > main %>% pull(HUC12) %>% unique() %>% length()
# [1] 2837 
# matches hucs shp

#known explicitly missing: 
# main %>% filter(is.na(HaCBP))
# # A tibble: 5 × 13
# HUC12        RRK   Priority TxIntensity TxType run     Year HaCBP HaCFL hacbp_ci_low hacbp_ci_high hacfl_ci_low hacfl_ci_high
# <chr>        <chr> <chr>    <chr>       <chr>  <chr>  <dbl> <dbl> <dbl>        <dbl>         <dbl>        <dbl>         <dbl>
#   1 160501020107 SN    Fire     2m          trt1   RunID9  2024    NA    NA           NA            NA           NA            NA
# 2 160501020107 SN    Fire     2m          trt6   RunID9  2024    NA    NA           NA            NA           NA            NA
# 3 180102040102 SN    Fire     1m          trt4   RunID6  2029    NA    NA           NA            NA           NA            NA
# 4 180102040102 SN    WUI      2m          trt6   RunID7  2029    NA    NA           NA            NA           NA            NA
# 5 180200021403 SN    Fire     2m          trt4   RunID9  2029    NA    NA           NA            NA           NA            NA

# > nrow(main)
# [1] 299765

#unknown implicitly missing???
# > 2837*108
# [1] 306396
# > 2837*108-nrow(main)
# [1] 6631

# > main_full %>% filter(is.na(run)) %>% pull(RRK) %>% unique()
# [1] "NC" "SN" #6,631
# Checked a couple - one was missing a whole year folder, another missing just that scenario-year folder

# main_full %>% filter(is.na(run)) %>% 
#   select(HUC12, RRK, Priority, TxIntensity, TxType, Year) %>% 
#   write_csv(file.path(output_folder, 'missing_main_20230101.csv')) 

#with full scenario run ID
# for val, offshoot
# main_full %>% 
#   filter(missing_flag) %>%
#   mutate(mas_scenario = paste(run, RRK, Priority, TxIntensity, TxType, sep="_")) %>% 
#   select(HUC12, mas_scenario, run, Year, RRK, Priority, TxIntensity, TxType) %>% 
#   arrange(RRK, HUC12, mas_scenario) %>% 
#   write_csv(file.path(output_folder, 'missing_main_20230103.csv'))


# > ft %>% filter(is.na(fire_type))
# # A tibble: 17 × 9
# HUC12        RRK   Priority TxIntensity TxType run      Year fire_type burn_frac_ave
# <chr>        <chr> <chr>    <chr>       <chr>  <chr>   <dbl>     <dbl>         <dbl>
#   1 180500060302 CC    RFFC     2m          trt4   RunID35  2034        NA            NA
# 2 180500060302 CC    WUI      2m          trt4   RunID34  2034        NA            NA
# 3 180600020503 CC    Fire     1m          trt4   RunID33  2029        NA            NA
# 4 180703020405 SC    Fire     1m          trt1   RunID24  2029        NA            NA
# 5 160501020107 SN    Fire     2m          trt1   RunID9   2024        NA            NA
# 6 160501020107 SN    Fire     2m          trt6   RunID9   2024        NA            NA
# 7 180102040102 SN    Fire     1m          trt4   RunID6   2029        NA            NA
# 8 180102040102 SN    WUI      2m          trt6   RunID7   2029        NA            NA
# 9 180200021101 SN    WUI      2m          trt6   RunID7   2024        NA            NA
# 10 180200021403 SN    Fire     2m          trt4   RunID9   2029        NA            NA
# 11 180200030801 SN    Fire     2m          trt4   RunID9   2024        NA            NA
# 12 180200030801 SN    WUI      1m          trt1   RunID4   2024        NA            NA
# 13 180300030103 SN    Fire     1m          trt1   RunID6   2024        NA            NA
# 14 180300030103 SN    Fire     1m          trt6   RunID6   2024        NA            NA
# 15 180400090202 SN    RFFC     1m          trt1   RunID5   2034        NA            NA
# 16 180400090202 SN    RFFC     2m          trt1   RunID8   2034        NA            NA
# 17 180800030302 SN    RFFC     500k        trt4   RunID2   2024        NA            NA
