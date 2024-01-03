# Script to collate data, also for data cube

#"Fire modeling outputs: burn probability, flame length, 
#  % active crown fire, % passive crown fire, % surface fire"
# plus CIs

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### User settings ---------------------------------------------

input_folder <- 'results_csv'
output_folder <- 'results_csv'

### Main Data import & prep -------------------------------------------

#main data
main_sc <- read_csv(file.path(input_folder, 
                              'main_results_from_sql_SC.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

main_nc <- read_csv(file.path(input_folder,
                              'main_results_from_sql_NC.csv')) %>%
  mutate(HUC12 = as.character(HUC12))

main_sn_cc1 <- read_csv(file.path(input_folder, 
                                  'main_results_from_sql_SN_CC_part1.csv')) %>%
  mutate(HUC12 = as.character(HUC12))
main_sn_cc2 <- read_csv(file.path(input_folder, 
                                  'main_results_from_sql_SN_CC_part2.csv')) %>%
  mutate(HUC12 = as.character(HUC12))
main_sn_cc3 <- read_csv(file.path(input_folder, 
                                  'main_results_from_sql_SN_CC_part3.csv')) %>%
  mutate(HUC12 = as.character(HUC12))
main_sn_cc4 <- read_csv(file.path(input_folder, 
                                'main_results_from_sql_SN_CC_part4.csv')) %>%
  mutate(HUC12 = as.character(HUC12))

#bind
main <- bind_rows(main_sc, main_nc,
                 main_sn_cc1, main_sn_cc2, main_sn_cc3, main_sn_cc4) %>% 
  #sort nicely
  arrange(RRK, HUC12, Priority, TxIntensity, TxType, Year)

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

main_expand <- main %>% 
  #NOT runID b/c can't associate it to huc/region/priority/intensity/type without losing some combinations
  expand(nesting(HUC12, RRK), Priority, TxIntensity, TxType, Year)

main_full <- main_expand %>% 
  left_join(main, by = c("HUC12", "RRK", "Priority", "TxIntensity", "TxType", "Year"))

# > main_full %>% filter(is.na(run)) %>% pull(RRK) %>% unique()
# [1] "NC" "SN" #6,631
# Checked a couple - one was missing a whole year folder, another missing just that scenario-year folder

# main_full %>% filter(is.na(run)) %>% 
#   select(HUC12, RRK, Priority, TxIntensity, TxType, Year) %>% 
#   write_csv(file.path(output_folder, 'missing_main_20230101.csv')) 

### Fire type Data import & prep -------------------------------------------

#fire type data
ft_sc <- read_csv(file.path(input_folder,
                            'cft_results_from_sql_SC.csv')) %>%
  mutate(HUC12 = as.character(HUC12))
ft_nc <- read_csv(file.path(input_folder,
                            'cft_results_from_sql_NC.csv')) %>%
  mutate(HUC12 = as.character(HUC12))

ft_sn_cc1 <- read_csv(file.path(input_folder, 
                            'cft_results_from_sql_SN_CC_part1.csv')) %>%
  mutate(HUC12 = as.character(HUC12))
ft_sn_cc2 <- read_csv(file.path(input_folder, 
                            'cft_results_from_sql_SN_CC_part2.csv')) %>%
  mutate(HUC12 = as.character(HUC12))
ft_sn_cc3 <- read_csv(file.path(input_folder, 
                            'cft_results_from_sql_SN_CC_part3.csv')) %>%
  mutate(HUC12 = as.character(HUC12))
ft_sn_cc4 <- read_csv(file.path(input_folder, 
                            'cft_results_from_sql_SN_CC_part4.csv')) %>%
  mutate(HUC12 = as.character(HUC12))

#bind
ft <- bind_rows(ft_sc, ft_nc, 
                ft_sn_cc1, ft_sn_cc2, ft_sn_cc3, ft_sn_cc4) %>% 
  #sort nicely
  arrange(RRK, HUC12, Priority, TxIntensity, TxType, Year)

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


#Anna's FVS results
fvs <- read_csv(file.path(input_folder,
                          'FVSprocessedOutputsHucs.csv')) %>%
  mutate(HUC12 = as.character(HUC12))

# > nrow(fvs)
# [1] 321624 ??

fvs_dupls <- fvs %>% group_by(HUC12, Priority, TxIntensity, TxType, Year) %>% filter(n()>1) %>% summarize(n=n()) %>% ungroup()

fvs_dupls %>% 
  left_join(fvs, by = c("HUC12", "Priority", "TxIntensity", "TxType", "Year")) #%>% View()

fvs_dupls %>% 
  left_join(fvs, by = c("HUC12", "Priority", "TxIntensity", "TxType", "Year")) %>% 
  select(HUC12, RRK) %>% 
  distinct()


### Data processing ------------------------------------------

#pivot fire type results from longer to 
ft_wide <- ft %>% 
  #1 for surface fire, 2 for passive crown fire, 3 for active
  mutate(type = case_when(
    fire_type == 1 ~ "surface_fire_perc",
    fire_type == 2 ~ "passive_crown_fire_perc",
    fire_type == 3 ~ "active_crown_fire_perc",
    TRUE ~ "unknown"
  )) %>% 
  pivot_wider(
    id_cols = c(HUC12, RRK, Priority, TxIntensity, TxType, run, Year),
    names_from = type,
    values_from = burn_frac_ave) %>% 
  select(-unknown) #none were unknown, all NA

ft_wide

#join with main results
res_fire <- main_full %>% 
  #drop extra run field that we can't join on (b/c didn't exist in fire data)
  select(-run) %>% 
  left_join(ft_wide, 
            by = c("HUC12", "RRK", "Priority", "TxIntensity", "TxType", "Year"))


#join FVS results with fire results
res_all <- fvs %>% 
  inner_join(res_fire %>% 
               #drop extra run field that we can't join on (b/c didn't exist in fire data)
               select(-run),
            by = c("HUC12", "RRK", "Priority", "TxIntensity", "TxType", "Year"))
  


### Save out ------------------------------------------

# #main results, collated
# write_csv(main, 
#           file.path(output_folder, 
#                     paste0('main_results_from_sql_all.csv')))
# 
# #fire tire results, collated
# write_csv(ft, 
#           file.path(output_folder, 
#                     paste0('cft_results_from_sql_all.csv')))

#data cube
write_csv(res_all, 
          file.path(output_folder, 
                    paste0('datacube_20230102a.csv')))
