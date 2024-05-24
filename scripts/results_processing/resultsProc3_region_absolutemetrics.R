# Script for weighting conditional gridfire results to absolute metrics 
#  using an Annual Burn Probability map

# Alternative script for combining region and baseline/weather runs

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  raster,
  exactextractr,
  sf) 

### User settings ---------------------------------------------

#which region to run right now
reg_code <- "SN"

#their conditional metric file names, to be selected based on reg_code
reg_files <- c(
  "SN" = "SN_conditional_20240529.csv",
  "SC" = "SC_conditional_20240529.csv",
  "CC" = "CC_conditional_20240529.csv", 
  "NC" = "NC_conditional_20240529.csv")
this_reg_file <- reg_files[[reg_code]]


# for optional baseline/baseweather inclusion
use_bl <- FALSE
if (use_bl){
  bl_files <- c(
    "SN" = "SNbl_conditional_202406xx.csv",
    "SC" = "SCbl_conditional_202406xx.csv",
    "CC" = "CCbl_conditional_202406xx.csv", 
    "NC" = "NCbl_conditional_202406xx.csv")
  this_bl_file <- reg_files[[reg_code]]
}
#optional baseweather inclusion
use_bw <- FALSE
if (use_bw){
  bw_files <- c(
    "SN" = "SNbw_conditional_202406xx.csv",
    "SC" = "SCbw_conditional_202406xx.csv",
    "CC" = "CCbw_conditional_202406xx.csv", 
    "NC" = "NCbw_conditional_202406xx.csv")
  this_bw_file <- reg_files[[reg_code]]
}

#folders
input_folder <- file.path('results', 'conditional')

output_folder <- file.path('results', 'absolute')
dir.create(output_folder, recursive = TRUE) 


### Data ----------------------------------------------------

#read in scenario conditional data
reg <- read_csv(file.path(input_folder, this_reg_file)) %>% 
  mutate(HUC12 = as.character(HUC12))

#optional bl/bw inclusion
if (use_bl & use_bw) {
  bl <- read_csv(file.path(input_folder, this_bl_file)) %>% 
    mutate(HUC12 = as.character(HUC12))
  
  bw <- read_csv(file.path(input_folder, this_bw_file)) %>%
    mutate(HUC12 = as.character(HUC12))
  
  #combine region and baseline/weather results
  res <- bind_rows(reg, bl, bw)
  
} else if (use_bl){
  bl <- read_csv(file.path(input_folder, this_bl_file)) %>% 
    mutate(HUC12 = as.character(HUC12))
  
  #combine 
  res <- bind_rows(reg, bl)
  
} else if (use_bw){
  bw <- read_csv(file.path(input_folder, this_bw_file)) %>%
    mutate(HUC12 = as.character(HUC12))
  
  #combine 
  res <- bind_rows(reg, bw)
  
} else {
  
  #just scenario, as it will be until baseline is fixes
  res <- reg
}


# ANNUAL BURN PROBABILITY RASTER 
#Update cell size when changing abp rasters
abp <- raster("other_datasets/ucsb_calfire/ucsb_burn_severity.tif")
# abp is 1000m x 1000m = 247.105381 ac
cell_acres = 247.105381

#Park IW, Mann ML, Flint LE, Flint AL, Moritz M (2021) Relationships of climate, human activity, and fire history to spatiotemporal variation in annual fire probability across California. PLoS ONE 16(11): e0254723. https://doi.org/10.1371/journal.pone.0254723
#from EE "projects/pyregence-ee/assets/mas/Park_BP_2010to2016_mean"


# HUC SPATIAL DATA
hucs_shp <- st_read("data/data_huc/TxHucsTimingGroups.shp")

#hucs to the same crs as abp 
hucs <- st_transform(hucs_shp, st_crs(abp))


### zonal summary -----------------------------------------------------

#extract the sum of the abp per huc
# (with partial pixel coverage support using exact_extract)
# it's fast, not going to worry about filtering on region
# IF switch to a finer resolution ABP, then worry about filtering hucs first
sum_abp <- exact_extract(abp, 
                         hucs,
                         fun = 'sum', 
                         append_cols = c('huc12', 'hucAc'),
                         progress = TRUE)

#add in cell size and multiply
hucs_abp <- sum_abp %>% 
  rename(abp_sum = sum) %>% 
  mutate(cell_acres = cell_acres,
         abp_burned = abp_sum * cell_acres) #acres in year

head(hucs_abp)

# write_csv(hucs_abp,
#           file.path('results',
#                     'hucs_abp_sn_20240403.csv'))


### base HaCBP, calc weights -------------------------------------------

# 'Baseline HaCBP' definition depends on data availability
# without baseline/baseweather, it is the mean of the 500k scenarios in 2024
# otherwise, it is the baseline/baseweather in 2024
# (if both baseline and baseweather, will use baseline, but it doesn't matter
#  as they should be the same for 2024)

if (use_bl){
  #BASELINE
  base <- res %>% 
    filter(Year == 2024,
           #priority, txintensity, txtype all 'baseline'. Run is RunIDx 
           Priority == "baseline") %>%
    #rename to match when calculated
    mutate(HaCBP_mean = HaCBP) %>% 
    dplyr::select(HUC12, HaCBP_mean)
  
} else if (use_bw){
  #BASEWEATHER
  base <- res %>% 
    filter(Year == 2024,
           #priority, txintensity, txtype all 'baseweather'. Run is RunIDx (but RunIDx is also baseline)
           Priority == "baseweather") %>%
    #rename to match when calculated
    mutate(HaCBP_mean = HaCBP) %>% 
    dplyr::select(HUC12, HaCBP_mean)
  
} else {
  #SCENARIO ONLY
  #if no baseline/baseweather, as in 202405 runs
  # average of 500k scenario results in 2024
  base <- res %>% 
    filter(Year == 2024) %>%
    group_by(HUC12) %>% 
    summarize(HaCBP_mean = mean(HaCBP)) %>% 
    dplyr::select(HUC12, HaCBP_mean)
}


#create weighting/conversion factors
weighting <- base %>% 
  left_join(hucs_abp, by = join_by("HUC12" == "huc12")) %>% 
  mutate(sim_burned = HaCBP_mean * 200 * hucAc, #acres
         sim_yrs = sim_burned / abp_burned)

#histogram of sim yrs
ggplot() +
  geom_histogram(data = weighting,
                 mapping = aes(x=sim_yrs),
                 binwidth = 50)


### Reweight ------------------------------------------------------

res_adj <- res %>% 
  left_join(weighting %>% 
              dplyr::select(HUC12, 
                            abp_sum, 
                            cell_acres, 
                            abp_burned,
                            sim_burned, 
                            sim_yrs),
            by = join_by(HUC12)) %>% 
  mutate(modifier = hucAc * (200/sim_yrs),
         expBurn = HaCBP * modifier,
         expFlame = HaCFL * modifier,
         expSurface = surface * modifier,
         expPassive = passive_crown * modifier,
         expActive = active_crown * modifier) %>% 
  #change firetype to percentages
  mutate(exp_all_firetype = expSurface + expPassive + expActive,
         expPcSurface = expSurface / exp_all_firetype * 100,
         expPcPassive = expPassive / exp_all_firetype * 100,
         expPcActive = expActive / exp_all_firetype * 100)


## Final adjustments ----------------------------------------------

res_adj_trim <- res_adj %>% 
  #using any_of b/c fields may or may not exist depending on when run, hucac update, etc.
  dplyr::select(-any_of(c("run", 
                          "abp_sum", "cell_acres",
                          "exp_all_firetype",
                          "fireGroup", "wuiGroup", "hybridGroup",
                          "hacfl_avesq",
                          "hucAc_old", "hucAc_new")))

stamp <- format(Sys.time(), "%Y%m%d")

#filename based on which data used
if (use_bl & use_bw) {
  file_nm <- paste0(reg_code, "_absolute_", stamp, "_blbw")
  
} else if (use_bl){
  file_nm <- paste0(reg_code, "_absolute_", stamp, "_bl")
  
} else if (use_bw){
  file_nm <- paste0(reg_code, "_absolute_", stamp, "_bw")
  
} else {
  file_nm <- paste0(reg_code, "_absolute_", stamp)
}

#SAVE OUT
write_csv(res_adj,
          file.path(output_folder, paste0(file_nm, "_expanded.csv")))

write_csv(res_adj_trim,
          file.path(output_folder, paste0(file_nm, ".csv")))

