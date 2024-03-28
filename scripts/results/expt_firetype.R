# explore histogram or similar of fire types


### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

### Results import -------------------------------------------

res <- read_csv(file.path('results',
                               'absolute',
                               'SC_absolute_expanded_NOFVS_20240319.csv')) %>% 
  mutate(HUC12 = as.character(HUC12))

### ----------------------------------------------------

res %>% 
  arrange(desc(expPcActive)) %>% 
  dplyr::select(HUC12, Region, Priority, TxIntensity, TxType, Year, 
                expPcActive, expPcPassive, expPcSurface) %>% 
  head()

# df <- read_csv("https://raw.githubusercontent.com/GDS-ODSSS/unhcr-dataviz-platform/master/data/part_to_a_whole/column_stacked_100perc.csv")
# head(df)


ggplot() +
  geom_histogram(data=res,
                 mapping=aes(x=expPcActive))

ggplot() +
  geom_histogram(data=res,
                 mapping=aes(x=expPcSurface))

ggplot() + 
  geom_histogram(data=res,
                  mapping=aes(x=active_crown))

ggplot() +
  geom_histogram(data=res,
                   mapping=aes(x=surface))


### stacked --------------------------------------------

#need long
#example https://dataviz.unhcr.org/tools/r/r_column_stacked_100perc_chart.html
