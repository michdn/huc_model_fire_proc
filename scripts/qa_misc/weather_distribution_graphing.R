#graphing weather distributions

### library -----------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  moments)

### data  --------------------------------------------

reg_grp <- "CC"

w <- readRDS(file.path("qa",
                       "qa_weather0520",
                       paste0(reg_grp, "_weather0520.RDS")))

w2024 <- w %>% 
  filter(Year == 2024)

### 2024 all histogram ----------------------

ws24_mean <- w2024 %>%  pull(wind_speed_20ft) %>% mean()
rh24_mean <- w2024 %>%  pull(relative_humidity) %>% mean()
t24_mean <- w2024 %>%  pull(temperature) %>% mean()


p_ws <- ggplot() + 
  geom_histogram(data=w2024,
                 mapping=aes(x=wind_speed_20ft),
                 binwidth=0.5) + 
  geom_vline(aes(xintercept=ws24_mean)) + 
  annotate(x=ws24_mean, y=+Inf, vjust=1,
           label=paste0("Mean: ", round(ws24_mean, 1)), 
           geom="label") + 
  theme_bw() + 
  labs(title=paste0(reg_grp, " 2024: Wind speed 20ft (mph) histogram"),
       y="Count")
p_ws

p_rh <- ggplot() +
  geom_histogram(data=w2024,
                 mapping=aes(x=relative_humidity),
                 binwidth=0.2) + 
  geom_vline(aes(xintercept=rh24_mean)) + 
  annotate(x=rh24_mean, y=+Inf, vjust=1,
           label=paste0("Mean: ", round(rh24_mean, 1)), 
           geom="label") + 
  theme_bw() + 
  labs(title=paste0(reg_grp, " 2024: Relative humidity histogram"),
       y="Count")
p_rh

p_t <- ggplot() +
  geom_histogram(data=w2024,
                 mapping=aes(x=temperature),
                 binwidth=1) + 
  geom_vline(aes(xintercept = t24_mean)) + 
  annotate(x=t24_mean, y=+Inf, vjust=1,
           label=paste0("Mean: ", round(t24_mean, 1)), 
           geom="label") + 
  theme_bw() + 
  labs(title=paste0(reg_grp, " 2024: Temperature (F) histogram"),
       y="Count")
p_t


ggsave(plot=p_ws, 
       filename=file.path("qa", "qa_weather0520",
                          paste0("histogram_ws20ft_", reg_grp, "_2024.jpg")),
       height=6, width=8, units="in")

ggsave(plot=p_rh, 
       filename=file.path("qa", "qa_weather0520",
                          paste0("histogram_relhum_", reg_grp, "_2024.jpg")),
       height=6, width=8, units="in")

ggsave(plot=p_t, 
       filename=file.path("qa", "qa_weather0520",
                          paste0("histogram_temperature_", reg_grp, "_2024.jpg")),
       height=6, width=8, units="in")


## skewness / kurtosis calc -----------------------------------


skewness(w2024 %>% 
           pull(wind_speed_20ft))
#SN 1.2 (0 is no skew) positive skew, majority of data values less than the mean.  
mean(w2024 %>% pull(wind_speed_20ft)) #SN 17.2

kurtosis(w2024 %>% 
           pull(wind_speed_20ft))
#SN 5.9 (3 is normal). Distribution has more values in tails compared to normal

w_ws2024_stats <- w2024 %>% 
  group_by(HUC12) %>% 
  summarize(ws_mean = mean(wind_speed_20ft),
            ws_min = min(wind_speed_20ft),
            ws_max = max(wind_speed_20ft),
            ws_skewness = skewness(wind_speed_20ft),
            ws_kurtosis = kurtosis(wind_speed_20ft))

p_ws24_skew <- ggplot()+
  geom_histogram(data=w_ws2024_stats,
                 mapping=aes(x=ws_skewness),
                 binwidth = 0.1) + 
  theme_bw() + 
  labs(title=paste0(reg_grp, " 2024 Wind Speed 20ft Skewness per HUC Histogram"))

p_ws24_kurt <- ggplot()+
  geom_histogram(data=w_ws2024_stats,
                 mapping=aes(x=ws_kurtosis),
                 binwidth = 0.5) + 
  geom_vline(mapping=aes(xintercept=3)) +
  theme_bw() + 
  labs(title=paste0(reg_grp, " 2024 Wind Speed 20ft Kurtosis per HUC Histogram"))

ggsave(plot=p_ws24_skew, 
       filename=file.path("qa", "qa_weather0520",
                          paste0("histogram_ws_", reg_grp, "_2024_skewness.jpg")),
       height=6, width=8, units="in")

ggsave(plot=p_ws24_kurt, 
       filename=file.path("qa", "qa_weather0520",
                          paste0("histogram_ws_", reg_grp, "_2024_kurtosis.jpg")),
       height=6, width=8, units="in")
