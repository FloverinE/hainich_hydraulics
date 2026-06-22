# setup -------------------------------------------------------------------

library(tidyverse)
library(janitor)


## use gapfilled data
df.meteo_23 = read_csv("data/microclimate_new_2026/Meteorology_tower/DE-Hai_Meteo_tower_10min_20230101_20240101_gapfilled.csv") |> 
  janitor::clean_names()
df.meteo_24 = read_csv("data/microclimate_new_2026/Meteorology_tower/DE-Hai_Meteo_tower_10min_20240101_20250101_gapfilled.csv") |> 
  janitor::clean_names() 


## select relevant columns
## timestamp, prec, tair 44m, rh 44m, SWC 1-4 8cm, 1 16cm, 1 32cm
## omit second row with units
df.meteo_23 = df.meteo_23[2:nrow(df.meteo_23), ] |> 
  select(timestamp_start, precipitation_a_1m, air_temperature_a_44_5m, rel_humidity_a_44_5m, swc_t_1_8cm:swc_t_4_8cm) 

df.meteo_24 = df.meteo_24[2:nrow(df.meteo_24), ] |> 
  select(timestamp_start, precipitation_a_1m, air_temperature_a_44_5m, rel_humidity_a_44_5m, swc_t_1_8cm:swc_t_4_8cm)

## join and fix data type
df.meteo_23_24 = rbind.data.frame(df.meteo_23, df.meteo_24) |> 
  mutate(timestamp_start = as.POSIXct(timestamp_start),
         across(precipitation_a_1m:swc_t_4_8cm, ~ as.numeric(.x)))


## calculate VPD

## es: saturation vapor pressure [kPa]
f.es_kPa <- function(T_air) {
  0.6108 * exp(17.27 * T_air / (T_air + 237.3)) # sat vapor pressure
}

## ea: actual vapor pressure [kPa]
f.ea_kPa <- function(es, RH) {
  RH / 100 * (es) # act vapor pressure
}

df.meteo_23_24 = df.meteo_23_24 |> 
  mutate(
         es_kPa = f.es_kPa(air_temperature_a_44_5m),
         ea_kPa = f.ea_kPa(es_kPa, rel_humidity_a_44_5m),
         vpd_kPa = es_kPa - ea_kPa
  )

df.swc_longterm = data.table::fread(
  "data/microclimate_old/FLX_DE-Hai_FLUXNET2015_FULLSET_HH_2000-2023_1-3.csv"
) |>
  clean_names() |>
  mutate(
    timestamp_start = timestamp_start |>
      as.character() |>
      as.POSIXct(format = "%Y%m%d%H%M"),
    day = day(timestamp_start),
    month = month(timestamp_start),
    year = year(timestamp_start)
  ) |> 
  select(c(day, month, year, swc_f_mds_1, swc_f_mds_2, swc_f_mds_3)) |>
  filter(swc_f_mds_1 >= 0, swc_f_mds_2 >= 0, swc_f_mds_3 >= 0) |>
  group_by(year, month, day) |>
  summarise(
    swc_0_32_avg_d = mean(
      c(swc_f_mds_1, swc_f_mds_2, swc_f_mds_3),
      na.rm = TRUE
    )
  ) 

## get permanent wilting point as 2% and field capacity as 98% quantile
quantile(df.swc_longterm$swc_0_32_avg_d, probs = c(0.02, 0.98))

swc_min <- quantile(df.swc_longterm$swc_0_32_avg_d, probs = c(0.02)) |>
  as.numeric()
swc_max <- quantile(df.swc_longterm$swc_0_32_avg_d, probs = c(0.98)) |>
  as.numeric()

f.rew <- function(swc, swc_min, swc_max) {
  (swc - swc_min) / (swc_max - swc_min)
}

## calculate REW
df.meteo_23_24 = df.meteo_23_24 |> 
  mutate(swc_all_8cm = (swc_t_1_8cm + swc_t_2_8cm + swc_t_3_8cm + swc_t_4_8cm) / 4,
         swc_16_cm = swc_t_1_16cm,
         swc_32_cm = swc_t_1_32cm,
         swc_all_perc = (swc_all_8cm * 12 + ## from 0 - 12 cm (+ 8 - 4cm)
           swc_16_cm * 12 + ## from 12 - 24 cm (+- 6cm)
           swc_32_cm * 16) / 40  ## from 24 - 40 cm (+- 8 cm)
         ,
         rew_perc = f.rew(swc_all_perc, swc_min, swc_max))


df.meteo_23_24 |> 
  ggplot() +
  geom_line(aes(x = timestamp_start, y = REW_perc)) +
  lims(y = c(0, 1))

df.meteo_23_24 |> 
  ggplot() +
  geom_line(aes(x = timestamp_start, y = vpd_kPa))

## intermediate save
write_csv(df.meteo_23_24, "data/microclimate_new_2026/df_meteo_23_24.csv")


## summarise to weekly data
df.meteo_23_24_summ = df.meteo_23_24 |> 
  mutate(year = year(timestamp_start),
         week = isoweek(timestamp_start),
         month = month(timestamp_start),
         day = yday(timestamp_start)
         ) |>  
  group_by(year, week) |> 
  summarise(timestamp = as.Date(first(timestamp_start)),
            prec_mm = sum(precipitation_a_1m),
            t_air_degC_mean = mean(air_temperature_a_44_5m),
            t_air_degC_min = min(air_temperature_a_44_5m),
            t_air_degC_max = max(air_temperature_a_44_5m),
            rh_perc = mean(rel_humidity_a_44_5m),
            swc_all_perc = mean(swc_all_perc),
            rew_perc = mean(rew_perc),
            vpd_kPa_mean = mean(vpd_kPa),
            vpd_kPa_min = min(vpd_kPa),
            vpd_kPa_max = max(vpd_kPa)
            )

write_csv(df.meteo_23_24_summ, "data/microclimate_new_2026/df_meteo_23_24_summ.csv")
