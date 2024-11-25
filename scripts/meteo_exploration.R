# setup -------------------------------------------------------------------

library(tidyverse)
library(janitor)

## load data ---------------------------------------------------------------

meteo_2023 <- read.csv("data/Meteorology_tower/DE-Hai_Meteo_all_D_20230101_20231107_gapfilled.csv", header = T)
meteo_2023 <- meteo_2023 |> 
  clean_names() |> 
  mutate(timestamp = as.Date(timestamp, format = "%Y-%m-%d")) 


fluxes_2023 <- read.csv("data/EddyCovarianceFluxes_tower/DE-Hai_HS_LI7200_fluxes_daily_20230101_20231001.csv", 
                        header = T,
                        skip = 1)
colnames(fluxes_2023) <- read.csv("data/EddyCovarianceFluxes_tower/DE-Hai_HS_LI7200_fluxes_daily_20230101_20231001.csv", 
                                  header = F,
                                  nrows = 1)
fluxes_2023 <- fluxes_2023 |> 
  clean_names() |> 
  mutate(timestamp = as.Date(timestamp, format = "%Y-%m-%d")) 



# SPEI  -------------------------------------------------------------------

library(SPEI)


## tutorial ---------------------------------------------------------------

# Load data
data(wichita)

# Compute potential evapotranspiration (PET) and climatic water
# balance (BAL).
wichita$PET <- thornthwaite(wichita$TMED, 37.6475)
wichita$BAL <- wichita$PRCP - wichita$PET

# Convert to a ts (time series) for convenience
wichita <- ts(wichita[, -c(1, 2)], end = c(2011, 10), frequency = 12)
plot(wichita)

# One and twelve-months SPEI
spei1 <- spei(wichita[, "BAL"], 1)
spei12 <- spei(wichita[, "BAL"], 12)
class(spei1)
plot(spei1)
plot(spei12)

# Extract information from `spei` object: summary, call function,
# fitted values, and coefficients
summary(spei1)
names(spei1)
spei1$call
spei1$fitted
spei1$coefficients

# Plot `spei` object
par(mfrow = c(2, 1))
plot(spei1, main = "Wichita, SPEI-1")
plot(spei12, main = "Wichita, SPEI-12")

# One and tvelwe-months SPI
spi_1 <- spi(wichita[, "PRCP"], 1)
spi_12 <- spi(wichita[, "PRCP"], 12)

par(mfrow = c(2, 1))
plot(spi_1, "Wichita, SPI-1")
plot(spi_12, "Wichita, SPI-12")

# Time series not starting in January
plot(spei(ts(wichita[, "BAL"], freq = 12, start = c(1980, 6)), 12))

# Using a particular reference period (1980-2000) for computing the
# parameters. This may result in unexpected values (Inf, NaN) if data
# outside the reference period are way higher or lower than those within
# the reference period.
plot(spei(ts(wichita[, "BAL"], freq = 12, start = c(1980, 6)), 12,
          ref.start = c(1980, 1), ref.end = c(2000, 1)
))

# Using different kernels
spei24 <- spei(wichita[, "BAL"], 24)
spei24_gau <- spei(wichita[, "BAL"], 24,
                   kernel = list(type = "gaussian", shift = 0)
)
par(mfrow = c(2, 1))
plot(spei24)
plot(spei24_gau)
dev.off()

# Using different methods (distributions)
spi_gam <- spi(wichita[, "PRCP"], 12, distribution = "Gamma")
spi_pe3 <- spi(wichita[, "PRCP"], 12, distribution = "PearsonIII")
plot(spi_gam$fitted, spi_pe3$fitted)
grid()

# Using custom (user provided) parameters
coe <- spei1$coefficients
dim(coe)
spei(wichita[, "BAL"], 1, params = coe)

# Matrix input (computing data from several stations at one)
# Dataset `balance` contains time series of the climatic water balance at
# 12 locations. Note that input must be provided as matrix.
data(balance)
head(balance)
bal_spei12 <- spei(as.matrix(balance), 12)
plot(bal_spei12)

# 3-d array input (computing data from a gridded spatio-temporal dataset)
# Dataset cruts4 contains monthly time series of the climatic water balance
# at six locations, in a gridded format (3-d array).
data(cruts4)
dim(cruts4)
spei_12 <- spei(cruts4, 12)
dim(spei_12$fitted)

# Modding the plot
# Since plot.spei() returns a ggplot object, it is possible to add or tweak
# parts of the plot.
require(ggplot2)
plot(spei(wichita[, "BAL"], 12)) +
  ggtitle("SPEI1 at Wichita") +
  scale_fill_manual(values = c("blue", "red")) + # classic SPEI look
  scale_color_manual(values = c("blue", "red")) + # classic SPEI look
  theme_classic() +
  theme(legend.position = "bottom")


## try with hainich data ---------------------------------------------------

hainich_2023 <- meteo_2023 |> 
  select(timestamp, ta_2m_deg_c, ta_44m_deg_c, p_mm) |> 
  left_join(fluxes_2023, by = c("timestamp")) |> 
  mutate(timestamp = as.Date(timestamp, format = "%Y-%m-%d"))

hainich_2023 <- hainich_2023 |> 
  mutate(le_mjm2 = le_f,
         lambda_2m = 2.501 - (2.361 * 10^(-3)) * ta_2m_deg_c,
         lambda_44m = 2.501 - (2.361 * 10**-3) * ta_44m_deg_c,
         ET_2m = le_mjm2 / 2.45,
         ET_44m = le_mjm2 / lambda_44m)

hainich_2023$ET_2m
hainich_2023$ET_44m

hainich_2023 |>
  ggplot() +
  geom_line(aes (x = timestamp, y = ET_44m), color = "red") +
  geom_line(aes(x = timestamp, y = p_mm), color = "blue") +
  theme_bw()

sum(hainich_2023$ET_44m, na.rm = TRUE)
sum(hainich_2023$ET_2m, na.rm = TRUE)

sum(hainich_2023$p_mm, na.rm = TRUE)

hainich_2023_summ <- hainich_2023 |> 
  mutate(month = month(timestamp)) |> 
  group_by(month) |> 
  summarise(sum_ET_mm = sum(ET_44m, na.rm = TRUE),
            mean_Ta_44m = mean(ta_44m_deg_c, na.rm = TRUE),
            sum_p_mm = sum(p_mm, na.rm = TRUE)) |>
  mutate(PET_thornthwaite = thornthwaite(mean_Ta_44m, lat = 51.1),
         BAL = sum_p_mm - PET_thornthwaite)

hainich_2023_summ |> 
  ggplot() +
  geom_line(aes(x = month, y = sum_ET_mm), color = "red") +
  geom_line(aes(x = month, y = sum_p_mm), color = "blue") +
  geom_line(aes(x = month, y = PET_thornthwaite), color = "green") +
  theme_bw()

hainich_2023_ts <- ts(hainich_2023_summ, frequency = 12, start = c(2023, 1))

spei(hainich_2023_ts[, "BAL"], scale = 1)
spei(wichita[1:24, "BAL"], scale = 1)


## try with DWD - Eisenach data --------------------------------------------

dwd_eisenach_tair <- read.table("data/dwd/t_air/produkt_tu_stunde_20071101_20231231_07368.txt", header = TRUE, sep = ";") |> 
  mutate(timestamp = as.POSIXct(MESS_DATUM |> as.character(), format = "%Y%m%d%H", tz = "UTZ"),
         ta_degc = TT_TU) |> 
  select(timestamp, ta_degc)

dwd_eisenach_prec <- read.table("data/dwd/prec/produkt_rr_stunde_20071101_20231231_07368.txt", header = TRUE, sep = ";") |> 
  mutate(timestamp = as.POSIXct(MESS_DATUM |> as.character(), format = "%Y%m%d%H", tz = "UTZ"),
         prec_mm = R1) |>
  select(timestamp, prec_mm)

climate_eisenach_2007_24 <- dwd_eisenach_tair |> 
  left_join(dwd_eisenach_prec, by = c("timestamp")) |> 
  mutate(timestamp = as.Date(timestamp)) |> 
  filter(ta_degc > -999 & prec_mm > -999)

climate_eisenach_2007_24 |> 
  ggplot() +
  geom_line(aes(x = timestamp, y = ta_degc), color = "red") +
  geom_line(aes(x = timestamp, y = prec_mm), color = "blue") +
  theme_bw()

climate_eisenach_month <- climate_eisenach_2007_24 |> 
  mutate(year = year(timestamp),
         month = month(timestamp)) |> 
  group_by(year, month) |> 
  summarise(sum_prec_mm = sum(prec_mm),
            mean_ta_degc = mean(ta_degc))

climate_eisenach_month <- climate_eisenach_month |> 
  filter(year != 2007, year != 2024) |> 
  mutate(pet_mm = thornthwaite(mean_ta_degc, 51.1),
         bal_mm = sum_prec_mm - pet_mm,
         timestamp = paste(year, month, "01") |> as.POSIXct(format = "%Y %m %d"),
         year = as.integer(year),
         month = as.integer(month))

climate_eisenach_month |>
  ggplot() +
  geom_line(aes(x = timestamp, y = pet_mm), color = "red") +
  geom_line(aes(x = timestamp, y = sum_prec_mm), color = "blue") +
  theme_bw()

climate_eisenach_month <- ts(climate_eisenach_month, start = c(2008, 1), frequency = 12)
climate_eisenach_spei_3mon = spei(climate_eisenach_month[, "bal_mm"], scale = 3)
climate_eisenach_spei_1mon = spei(climate_eisenach_month[, "bal_mm"], scale = 1)

plot(climate_eisenach_spei)

df_climate_eisenach_month <- climate_eisenach_spei_3mon$fitted |> 
  unlist() |> 
  as.data.frame() |> 
  cbind(climate_eisenach_month |> as.data.frame()) |> 
  mutate(spei_3mon = x,
         timestamp = paste(year, month, "01") |> as.POSIXct(format = "%Y %m %d")) |> 
  
  
  df_climate_eisenach_month |> 
  ggplot() +
  geom_line(aes(x = timestamp, y = spei_3mon, col = spei_3mon)) +
  scale_color_gradient2(low = "red", high = "blue", mid = "white", midpoint  = 0) +
  theme_bw()


# relative extractable water ----------------------------------------------

df_swc <- meteo_2023 |> 
  select(c(timestamp, swc_1_8cm, swc_1_16cm, swc_1_32cm)) |>
  pivot_longer(cols = -timestamp, names_to = "depth", values_to = "swc") 


df_swc |>
  ggplot() +
  geom_line(aes(x = timestamp, y = swc, col = depth)) 

min_swc <- df_swc |> 
  group_by(depth) |> 
  summarise(min_swc = min(swc, na.rm = TRUE))

max_swc <- df_swc |>
  group_by(depth) |>
  summarise(max_swc = max(swc, na.rm = TRUE))

f.rew <- function(swc, swc_min, swc_max) {
  (swc - swc_min) / (swc_max - swc_min)
}

df_swc <- df_swc |> 
  left_join(min_swc, by = "depth") |> 
  left_join(max_swc, by = "depth") |> 
  mutate(rew = f.rew(swc, min_swc, max_swc),
         rew_per = rew * 100)

df_swc |>
  ggplot() +
  geom_line(aes(x = timestamp, y = rew_per, col = depth)) +
  theme_bw()

# exploration -------------------------------------------------------------


fluxes_2023 <- fluxes_2023 |> 
  mutate(Timestamp = as.Date(TIMESTAMP, format = "%Y-%m-%d"))


ggplot(fluxes_2023) +
  geom_line(aes(x = Timestamp, y = NEE_f)) +
  geom_area(aes(x = Timestamp, y = NEE_f), fill = "blue", alpha = 0.3) +
  labs(title = "Net Ecosystem Exchange (NEE) 2023",
       x = "Date",
       y = "NEE (umol/m2/s)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

rollmean <- function(data, window = 14) {
  data |> 
    mutate(rolling_mean = zoo::rollmean(NEE_f, window, fill = NA))
}

fluxes_2023 <- fluxes_2023 |> 
  rollmean()

ggplot(fluxes_2023) +
  geom_line(aes(x = Timestamp, y = rolling_mean), color = "red") +
  geom_area(aes(x = Timestamp, y = rolling_mean), fill = "red", alpha = 0.3) +
  labs(title = "Net Ecosystem Exchange (NEE) 2023",
       x = "Date",
       y = "NEE (umol/m2/s)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(fluxes_2023) +
  geom_line(aes(x = Timestamp, y = GPP_f), color = "red") +
  geom_area(aes(x = Timestamp, y = GPP_f), fill = "red", alpha = 0.3) +
  labs(title = "Gross Primary Product (GPP) 2023",
       x = "Date",
       y = "GPP (umol/m2/s)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(fluxes_2023) +
  geom_line(aes(x = Timestamp, y = ET_f), color = "red") +
  geom_area(aes(x = Timestamp, y = ET_f), fill = "red", alpha = 0.3) +
  labs(title = "Evapotranspiration 2023",
       x = "Date",
       y = "ET (mm/d)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(meteo_2023) +
  geom_line(aes(x = Timestamp, y = TA_2m_degC)) +
  labs(title = "Air Temperature 2023",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(meteo_2023) +
  geom_line(aes(x = Timestamp, y = SW_IN_MJm.2)) +
  labs(title = "Incoming Shortwave Radiation 2023",
       x = "Date",
       y = "Radiation (W/m2)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

SWC_2023 <- meteo_2023 |> 
  select(Timestamp, SWC_1_8cm_., SWC_1_16cm_., SWC_1_32cm_.) |> 
  pivot_longer(cols = -Timestamp, names_to = "Depth", values_to = "SWC")

ggplot(SWC_2023) +
  geom_line(aes(x = Timestamp, y = SWC, col = Depth)) +
  labs(title = "Soil Water Content 2023",
       x = "Date",
       y = "Water Content (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))




# 2023 --------------------------------------------------------------------


df_gmin_2023 = df_gmin_rwc7090 |> 
  filter(year == "2023") |> 
  mutate(date = date |> as.character() |> str_replace_all("2000", "2023") |> as.Date("%Y-%m-%d"))

ggplot() +
  geom_line(data = meteo_2023, aes(x = timestamp, y = p_mm), color = "blue") +
  theme_bw()

meteo_2023_month <- meteo_2023 |> 
  mutate(month = month(timestamp),
         week = week(timestamp)) |> 
  group_by(month) |>
  summarise(sum_p_mm = sum(p_mm),
            mean_ta_2m_degC = mean(ta_44m_deg_c))
meteo_2023_week <- meteo_2023 |> 
  mutate(month = month(timestamp),
         week = week(timestamp)) |> 
  group_by(week) |>
  summarise(sum_p_mm = sum(p_mm),
            mean_ta_2m_degC = mean(ta_44m_deg_c))

## extract sum of p_mm and mean_ta for 1, 2, 4 weeks before date in df_gmin_2023
df_gmin_date_window = df_gmin_2023 |> 
  mutate(week1 = date - weeks(1),
         week2 = date - weeks(2),
         week4 = date - weeks(4)) |> 
  pivot_longer(cols = c(week1, week2, week4), names_to = "window", values_to = "date_start") |>
  ungroup()

calculate_meteo_summary <- function(start_date, end_date) {
  meteo_2023 |> 
    filter(timestamp >= start_date, timestamp <= end_date) |> 
    summarise(
      sum_p_mm = sum(p_mm, na.rm = TRUE),
      mean_ta = mean(ta_44m_deg_c, na.rm = TRUE)
    )
}

df_gmin_date_window_long <- df_gmin_date_window |> 
  rowwise() |> 
  mutate(
    sum_p_mm = calculate_meteo_summary(date_start, date)$sum_p_mm,
    mean_ta = calculate_meteo_summary(date_start, date)$mean_ta,
  ) |> 
  ungroup()

df_gmin_date_window_long |> 
  ggplot() +
  geom_point(aes(x = sum_p_mm, y = mean_gmin, group = species, col = species)) +
  facet_wrap(~window, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom")

df_gmin_date_window_long |> 
  ggplot() +
  geom_point(aes(x = mean_ta, y = mean_gmin, group = species, col = species)) +
  facet_wrap(~window, scales = "free")


## take the sum of the precipitation 1month prior to each campaign


df_water_balance_2023 <- data.frame(
  date = campaign_dates_2023,
  prec_month = 1:4 |> lapply(function(x) {
    meteo_2023 |>  filter(TIMESTAMP >= campaign_dates_2023[x] - months(1) &
                            TIMESTAMP <= campaign_dates_2023[x]) |>
      summarise(Prec = sum(P_mm))
  }) |>    unlist() |>    as.vector(),
  prec_14_days = 1:4 |> lapply(function(x) {
    meteo_2023 |>  filter(TIMESTAMP >= campaign_dates_2023[x] - days(14) &
                            TIMESTAMP <= campaign_dates_2023[x]) |>
      summarise(Prec = sum(P_mm))
  }) |>    unlist() |>    as.vector(),
  precip_7_days = 1:4 |> lapply(function(x) {
    meteo_2023 |>  filter(TIMESTAMP >= campaign_dates_2023[x] - days(7) &
                            TIMESTAMP <= campaign_dates_2023[x]) |>
      summarise(Prec = sum(P_mm))
  }) |>    unlist() |>    as.vector()
)

gmin_2023 <- gmin_2023 |> 
  mutate(date = rep(campaign_dates_2023, 2)) |> 
  right_join(df_water_balance_2023, by = "date")

gmin_2023 |> 
  ggplot() +
  geom_line(aes(x = date, y = gmin, col = species)) +
  geom_point(aes(x = date, y = gmin, col = species)) +
  labs(title = "Minimum stomatal conductance 2023",
       x = "Date",
       y = "gmin (mol/m2/s)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

gmin_2023 |>
  ggplot() +
  geom_point(aes(x = prec_month, y = gmin, col = species)) +
  labs(title = "Minimum stomatal conductance 2023",
       x = "Precipitation (mm)",
       y = "gmin (mol/m2/s)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

gmin_2023 |>
  ggplot() +
  geom_point(aes(x = prec_14_days, y = gmin, col = species)) +
  labs(title = "Minimum stomatal conductance 2023",
       x = "Precipitation (mm)",
       y = "gmin (mol/m2/s)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

gmin_2023 |>
  ggplot() +
  geom_point(aes(x = precip_7_days, y = gmin, col = species)) +
  labs(title = "Minimum stomatal conductance 2023",
       x = "Precipitation (mm)",
       y = "gmin (mol/m2/s)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



# radial growth -----------------------------------------------------------

radial_growth_2023 <- read.csv("data/dendrometers/circ_change_beech_hour.csv", header = T)
radial_growth_2023_long <- radial_growth_2023 |> 
  pivot_longer(cols = -time, names_to = "Tree", values_to = "Circumference_change_mm")


radial_growth_2023_long |> 
  ggplot() +
  geom_line(aes(x = time, y = Circumference_change_mm, group = Tree, col = Tree)) +
  labs(title = "Radial growth 2023",
       x = "Date",
       y = "Radial growth (mm)") +
  # facet_wrap(~Tree) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

meteo_2023 |> 
  ggplot() +
  geom_line(aes(x = Timestamp, y = VPD_hPa)) +
  labs(title = "Vapour Pressure Deficit 2023",
       x = "Date",
       y = "VPD (hPa)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


radial_growth_2023_day_sum <- radial_growth_2023_long |> 
  mutate(TIMESTAMP = lubridate::date(time) |> as.character()) |> 
  group_by(TIMESTAMP, Tree) |> 
  summarise(sum_day = max(Circumference_change_mm) - min(Circumference_change_mm))

meteo_2023_radial_growth <- radial_growth_2023_day_sum |> 
  right_join(meteo_2023, by = c("TIMESTAMP"))

meteo_2023_radial_growth <- meteo_2023_radial_growth |> 
  group_by(Tree) |> 
  mutate(norm_growth = sum_day/max(sum_day)) |>
  ungroup()

meteo_2023_radial_growth |> 
  ggplot() +
  geom_point(aes(x = VPD_hPa, y = norm_growth, col = Tree)) +
  geom_smooth(aes(x = VPD_hPa, y = norm_growth, col = Tree), method = "lm") +
  facet_wrap(~Tree)

