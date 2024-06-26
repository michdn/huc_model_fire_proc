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

# Must have region, baseweather (used for conversion), should have baseline too.

reg_code <- "SC"
reg_file <- paste0(reg_code, '_conditional_20240410.csv')

bl_code <- "SCbl"
bl_file <- paste0(bl_code, '_conditional_20240416.csv')

bw_code <- "SCbw"
bw_file <- paste0(bw_code, "_conditional_20240423.csv")


input_folder <- file.path('results', 'conditional')

output_folder <- file.path('results', 'absolute')
dir.create(output_folder, recursive = TRUE) 


### Data ----------------------------------------------------

reg <- read_csv(file.path(input_folder, 
                          reg_file)) %>% 
  mutate(HUC12 = as.character(HUC12))

bl <- read_csv(file.path(input_folder,
                         bl_file)) %>% 
  mutate(HUC12 = as.character(HUC12))

bw <- read_csv(file.path(input_folder,
                         bw_file)) %>%
  mutate(HUC12 = as.character(HUC12))


#combine region and baseline/weather results
res <- bind_rows(reg, bl, bw)
#res <- bind_rows(reg, bl)


hucs_shp <- st_read("data/data_huc/TxHucsTimingGroups.shp")
#hucs_shp <- st_read("data/data_huc/TxPrctRankRrkWipRffc.shp")

#Update cell size when changing abp rasters
abp <- raster("other_datasets/ucsb_calfire/ucsb_burn_severity.tif")
# abp is 1000m x 1000m = 247.105381 ac
cell_acres = 247.105381

#origin
#Park IW, Mann ML, Flint LE, Flint AL, Moritz M (2021) Relationships of climate, human activity, and fire history to spatiotemporal variation in annual fire probability across California. PLoS ONE 16(11): e0254723. https://doi.org/10.1371/journal.pone.0254723
#from
# EE "projects/pyregence-ee/assets/mas/Park_BP_2010to2016_mean"


### CRS prep -----------------------------------------------------

#crs(abp)
#st_crs(abp)
#st_crs(hucs_shp)

#hucs to the same crs as abp 
hucs <- st_transform(hucs_shp, st_crs(abp))


### zonal summary -----------------------------------------------------

#extract the sum of the abp per huc
# (with partial pixel coverage support using exact_extract)
# it's fast, not going to worry about filtering on region
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


### baseline HaCBP, calc weights -------------------------------------------

# Baseweather is 'baseline' runs. (either baseline or baseweather, same in 2024)

base <- res %>% 
  filter(Year == 2024,
         #priority, txintensity, txtype all 'baseweather'. Run is RunIDx (but RunIDx is also baseline)
         Priority == "baseweather") %>%
  #group_by(HUC12) %>% 
  #summarize(HaCBP_mean = mean(HaCBP))
  #with baseweather, single value now, so summary needed
  dplyr::select(HUC12, HaCBP)


weighting <- base %>% 
  left_join(hucs_abp, by = join_by("HUC12" == "huc12")) %>% 
  mutate(sim_burned = HaCBP * 200 * hucAc, #acres
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

write_csv(res_adj,
          file.path(output_folder,
                    paste0(reg_code, "_", bl_code, "_", bw_code,
                           "_absolute_expanded_", stamp, ".csv")))

write_csv(res_adj_trim,
          file.path(output_folder,
                    paste0(reg_code, "_", bl_code, "_", bw_code,
                           "_absolute_", stamp, ".csv")))

