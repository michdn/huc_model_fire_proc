# match up weather and ignitions



### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)


### User settings --------------------------------------------

reg_group <- "SNbw" 


igs <- readRDS(file.path("results",
                         "misc",
                         paste0(reg_group, "_ignitions_from_sql.RDS")))

weather <- readRDS(file.path("results",
                             "misc",
                             paste0(reg_group, "_all_weather.RDS")))


cbp <- readRDS(file.path('results', 'extracts',
                         paste0(reg_group, '_cbp_all_fires_from_sql.RDS')))
cfl <- readRDS(file.path('results', 'extracts',
                         paste0(reg_group, '_cfl_all_fires_from_sql.RDS')))


### ----------------------------------------------------------------

igw <- igs %>%
  rename(array_index = weather_sample_index) %>%
  left_join(weather %>%
              rename(Year = year),
            by = join_by("HUC12", "Year", "array_index"))


#flame_burn_ratio
fire <- cbp %>%
  left_join(cfl,
            by = join_by(HUC12, Region,
                         Priority, TxIntensity, TxType,
                         run, Year, mas_scenario,
                         fire_i)) %>%
  mutate(flame_burn_ratio = huc_avg_fl / huc_burned_frac)


joint <- igw %>%
  left_join(fire,
            by = join_by(HUC12, Region,
                         Priority, TxIntensity, TxType,
                         run, Year, mas_scenario,
                         fire_i))

saveRDS(joint,
        file.path("results", "misc",
                  paste0(reg_group, "_fire_results_weather.RDS")))


#write_csv(joint, file.path("results", "misc", paste0(reg_group, "_fire_results_weather.csv")))

# joint <- readRDS(file.path("results", "misc", 
#                            paste0(reg_group, "_fire_results_weather.RDS")))
#
