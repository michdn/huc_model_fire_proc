# dataset comparison doc

# best scenarios

# conditional vs absolute

### Libraries -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  ggspatial)

### Base Data import -------------------------------------------

#data comparison doc check. Should be same. 

res <- read_csv(file.path("results",
                          "datacube", 
                          "datacube_interim_sc_cc_20240328.csv")) %>% 
  mutate(HUC12 = as.character(HUC12))

### compare conditional versus absolute metrics --------------------------

res_comp <- res %>% 
  # note, NOT by year, averaging over year
  group_by(HUC12, Region, Priority, TxIntensity, TxType) %>% 
  summarize(mean_expFlame = mean(expFlame),
            mean_expBurn = mean(expBurn),
            mean_hacfl = mean(HaCFL),
            mean_hacbp = mean(HaCBP),
            count_check = n(),
            .groups = 'drop')


low_expFlame <- res_comp %>% 
  group_by(HUC12) %>% 
  #lowest value first
  arrange(mean_expFlame) %>% 
  slice(1)

low_expBurn <- res_comp %>% 
  group_by(HUC12) %>% 
  #lowest value first
  arrange(mean_expBurn) %>% 
  slice(1)

low_hacfl <- res_comp %>% 
  group_by(HUC12) %>% 
  #lowest value first
  arrange(mean_hacfl) %>% 
  slice(1)

low_hacbp <- res_comp %>% 
  group_by(HUC12) %>% 
  #lowest value first
  arrange(mean_hacbp) %>% 
  slice(1)



#Compare best HaCBP vs best expBurn 

lb <- low_expBurn %>% 
  group_by(Region, Priority, TxIntensity, TxType) %>% 
  summarize(count_lb = n(),
            .groups = "drop") %>% 
  arrange(Region, desc(count_lb))

lh <- low_hacbp %>% 
  group_by(Region, Priority, TxIntensity, TxType) %>% 
  summarize(count_lh = n(),
            .groups = "drop") %>% 
  arrange(Region, desc(count_lh))

lh %>% 
  left_join(lb, by = c("Region", "Priority", "TxIntensity", "TxType")) %>% 
  mutate(diff = count_lh - count_lb) %>% 
  filter(!diff == 0)

#one that switched categories because of zero abp, so zero absolute metric values
# (and just by order flipped)
# found last time in CC, known. 

# Region Priority TxIntensity TxType count_lh count_lb  diff
# <chr>  <chr>    <chr>       <chr>     <int>    <int> <int>
#   1 CC     WUI      2m          trt1         58       57     1
# 2 CC     Fire     1m          trt1          9       10    -1


#Compare best HaCFL vs best expFlame

lf <- low_expFlame %>% 
  group_by(Region, Priority, TxIntensity, TxType) %>% 
  summarize(count_lf = n(),
            .groups = "drop") %>% 
  arrange(Region, desc(count_lf))

ll <- low_hacfl %>% 
  group_by(Region, Priority, TxIntensity, TxType) %>% 
  summarize(count_ll = n(),
            .groups = "drop") %>% 
  arrange(Region, desc(count_ll))

lf %>% 
  left_join(ll, by = c("Region", "Priority", "TxIntensity", "TxType")) %>% 
  mutate(diff = count_lf - count_ll) %>% 
  filter(!diff == 0)

#two flips??
# Region Priority TxIntensity TxType count_lf count_ll  diff
# <chr>  <chr>    <chr>       <chr>     <int>    <int> <int>
#   1 CC     WUI      2m          trt4         13       14    -1
# 2 CC     Fire     1m          trt1          2        1     1
# 3 SC     WUI      2m          trt1         75       76    -1
# 4 SC     Hybrid   2m          trt1         40       39     1

low_hacfl %>% 
  anti_join(low_expFlame, 
            by = join_by(HUC12, Region, Priority, TxIntensity, TxType))

# HUC12        Region Priority TxIntensity TxType mean_expFlame mean_expBurn mean_hacfl mean_hacbp
# <chr>        <chr>  <chr>    <chr>       <chr>          <dbl>        <dbl>      <dbl>      <dbl>
#   1 180500040804 CC     WUI      2m          trt4            0            0    0.00000250 0.00000396
# 2 180600140103 SC     WUI      2m          trt1            2.21         1.15 0.00000230 0.00000120

# 180500040804 is the zero abp, fine. 

switchedhuc <- 180600140103

res_comp %>% 
  filter(HUC12 == switchedhuc,
         Priority == "WUI",
         TxType == "trt1") %>% View()

# values are EQUAL. so rest of ordering is which one gets sliced
# (Could have other of these, equal values, that happened to align with same scenario)
# not a concern
