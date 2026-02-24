# setup -------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(ggExtra)

source("scripts/ggplot_themes.R")


# load data ---------------------------------------------------------------

df_meteo_2023_2024 <- data.table::fread("data/microclimate/meteo_fluxes_2023_2024_30min.csv", header = TRUE) |> 
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"))

df_meteo_H <- data.table::fread("data/microclimate/meteo_2023_2024_H.csv", header = TRUE) |> 
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"))

df_meteo_30min <- data.table::fread("data/microclimate/meteo_2023_2024_30min.csv", header = TRUE) |> 
  mutate(timestamp = timestamp |> as.POSIXct(format = "%Y-%m-%d %H:%M:%S"))

df_fluxes_30min <- data.table::fread("data/microclimate/fluxes_2023_2024_30min.csv", header = TRUE) |> 
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"))

df_meteo_D <- data.table::fread("data/Meteorology_tower/meteo_2023_2024_D.csv", header = T) |> 
  mutate(timestamp = as.Date(timestamp, format = "%Y-%m-%d"))
df_fluxes_D <- data.table::fread("data/EddyCovarianceFluxes_tower/fluxes_2023_2024_D.csv", header = T) |> 
  mutate(timestamp = as.Date(timestamp, format = "%Y-%m-%d"))

df_meteo_2000_2023 <- data.table::fread("data/microclimate/FLX_DE-Hai_FLUXNET2015_FULLSET_HH_2000-2023_1-3.csv") |> 
  clean_names() |> 
  mutate(timestamp_start = timestamp_start |> as.character() |> as.POSIXct(format = "%Y%m%d%H%M"),
         day = day(timestamp_start),
         month = month(timestamp_start),
         year = year(timestamp_start))


# clean data ---------------------------------------------------------------

## join longterm and 2024 data

df_meteo_2000_2023_join <- df_meteo_2000_2023 |> 
  select(-c(timestamp_end, year, month, day)) |> 
  mutate(timestamp = timestamp_start) |> 
  select(c(timestamp, ta_f_mds, p_f, rh, sw_in_f_mds, sw_out, lw_in_f_mds, lw_out, ws_f, le_f_mds, h_f_mds)) |> 
  rename(
    ta_44m = ta_f_mds,
    p_1m = p_f,
    rh_44m = rh,
    sw_in_44m = sw_in_f_mds,
    sw_out_44m = sw_out,
    lw_in_44m = lw_in_f_mds,
    lw_out_44m = lw_out,
    ws_44m = ws_f,
    le_f = le_f_mds,
    h_f = h_f_mds
  )

df_meteo_2000_2023 |> names()
df_meteo_2023_2024 |> names()


ggplot() +
  geom_point(data = df_meteo_2000_2023_join |> filter(timestamp > "2023-01-01"),
             aes(x = timestamp, y = ta_f_mds)) +
  geom_point(data = df_meteo_2023_2024 |> filter(timestamp > "2023-01-01"),
             aes(x = timestamp, y = ta_44m), col = "red")

## fao penman
df_meteo_2000_2023_join <- df_meteo_2000_2023_join |> 
  mutate(delta = f.delta(T_air = ta_44m),
         es = f.es(T_air = ta_44m),
         ea = f.ea(es, RH = rh_44m),
         Rn_Wm2 = f.Rn_Wm2(sw_in_44m, sw_out_44m, lw_in_44m, lw_out_44m),
         Rn_MJm2 = f.wm2_to_mjm2(Rn_Wm2, 1800),
         L_Wm2 = le_f,
         L_MJm2 = f.wm2_to_mjm2(L_Wm2, 1800), # latent heat flux density
         H_Wm2 = h_f,
         H_MJm2 = f.wm2_to_mjm2(H_Wm2, 1800), # sensible heat flux density
         G_MJm2 = f.G_mjm2(Rn_MJm2, L_MJm2, H_MJm2), # soil heat flux density
         u2 = f.u2(Wspd = ws_44m),
         pet_mm = f.PET_penman(delta, Rn_MJm2, G_MJm2, gamma, T_air = ta_44m, u2, es, ea)) 
  
df_meteo_2000_2024 <- rbind(df_meteo_2000_2023_join, df_meteo_2023_2024 |> filter(timestamp > "2023-12-20"),
                            fill = T)

df_meteo_2000_2024$pet_mm 

df_meteo_2000_2024 |> 
  ggplot(aes(x = timestamp, y = pet_mm)) +
  geom_line()

df_meteo_2000_2024 |> 
  ggplot(aes(x = pet_mm)) +
  geom_histogram()

sum(df_meteo_2000_2024$pet_mm > 10, na.rm = TRUE)

df_meteo_2023_2024 |> 
  ggplot(aes(x = timestamp, y = ta_44m)) 

df_meteo_2023_2024$pet_mm |> sum(na.rm = TRUE)
## meteo -------------------------------------------------------------------

### 30 mins ----
meteo_2023_30min <- read.csv("data/microclimate/DE-Hai_Meteo_tower_30min_20230101_20231231_gapfilled.csv", 
                             header = F, skip = 2)

meteo_2024_30min <- read.csv("data/microclimate/DE-Hai_Meteo_tower_30min_20240101_20241031_gapfilled.csv", 
                             header = F, skip = 2)

colnames(meteo_2023_30min) <- read.csv("data/microclimate/DE-Hai_Meteo_tower_30min_20230101_20231231_gapfilled.csv", 
                                       header = F, nrows = 1)
colnames(meteo_2024_30min) <- read.csv("data/microclimate/DE-Hai_Meteo_tower_30min_20240101_20241031_gapfilled.csv", 
                                       header = F, nrows = 1)

meteo_2023_30min <- meteo_2023_30min |>
  clean_names() |>
  mutate(timestamp = as.POSIXct(timestamp_end, format = "%Y-%m-%d %H:%M:%S")) 

meteo_2024_30min <- meteo_2024_30min |>
  clean_names() |>
  mutate(timestamp = as.POSIXct(timestamp_end, format = "%Y-%m-%d %H:%M:%S"))

meteo_2023_30min <- meteo_2023_30min[, colnames(meteo_2023_30min) %in% colnames(meteo_2024_30min)]
meteo_2024_30min <- meteo_2024_30min[, colnames(meteo_2024_30min) %in% colnames(meteo_2023_30min)]

meteo_2023_2024_30min <- rbind(meteo_2023_30min, meteo_2024_30min)

write.csv(meteo_2023_2024_30min, 
          "data/microclimate/meteo_2023_2024_30min.csv", 
          row.names = FALSE)



### 10 mins ----

meteo_2023_10min <- read.csv("data/microclimate/DE-Hai_Meteo_tower_10min_20230101_20231231_gapfilled.csv", 
                             header = F, skip = 2)
meteo_2024_10min <- read.csv("data/microclimate/DE-Hai_Meteo_tower_10min_20240101_20241031_gapfilled.csv", 
                             header = F, skip = 2)



### hourly ----
meteo_2023_H <- read.csv("data/microclimate/DE-Hai_Meteo_tower_H_20230101_20231231_gapfilled.csv", 
                         header = T)

meteo_2023_H <- meteo_2023_H |>
  clean_names() |>
  mutate(timestamp = as.POSIXct(timestamp_end, format = "%Y-%m-%d %H:%M:%S")) 

meteo_2024_H <- read.csv("data/microclimate/DE-Hai_Meteo_tower_H_20240101_20241031_gapfilled.csv", 
                         header = F, skip = 2)

colnames(meteo_2024_H) <- read.csv("data/microclimate/DE-Hai_Meteo_tower_30min_20240101_20241031_gapfilled.csv", 
                                   header = F, nrows = 1)

meteo_2024_H <- meteo_2024_H |>
  clean_names() |>
  mutate(timestamp = as.POSIXct(timestamp_end, format = "%Y-%m-%d %H:%M:%S")) 

colnames(meteo_2023_H) = 
  c("timestamp_end", "timestamp_start" , "ta_2m", "ta_10m", "ta_20m", "ta_32m", "ta_35m", "ta_40m", "ta_44m", "pa_44m",
    "rh_44m", "absolute_humidity_kgm_3", "vpd_44m", "wd_44m", "ws_44m", "sw_in_44m",
    "sw_out_44m", "lw_in_44m", "lw_out_44m", "netrad_m_jm_2", "ppfd_in_umolm_2s_1",
    "ppfd_out_umolm_2s_1", "sw_in_spn1_m_jm_2", "sw_dif_in_spn1_m_jm_2", "p_1m",
    "ts_2cm_1", "ts_5cm_1", "ts_15cm_1", "ts_30cm_1", "ts_50cm_1", 
    "ts_2cm_2", "ts_5cm_2", "ts_15cm_2", "ts_30cm_2", "ts_50cm_2", 
    "swc_8cm_1", "swc_16cm_1", "swc_32cm_1", "swc_8cm_2", "swc_8cm_3", "swc_8cm_4",
    "g_1_5cm_m_jm_2", "g_2_5cm_m_jm_2", "g_3_5cm_m_jm_2", "g_4_5cm_m_jm_2", "g_5_5cm_m_jm_2", "timestamp"
  )

colnames(meteo_2023_H)[which(!colnames(meteo_2023_H) %in% colnames(meteo_2024_H))]
## ta_2m and ts_50cm_1 missing in 2024
meteo_2023_H <- meteo_2023_H[, colnames(meteo_2023_H) %in% colnames(meteo_2024_H)]
meteo_2024_H <- meteo_2024_H[, colnames(meteo_2024_H) %in% colnames(meteo_2023_H)]

df_meteo_H <- rbind(meteo_2023_H, meteo_2024_H)
write.csv(df_meteo_H, "data/microclimate/meteo_2023_2024_H.csv", row.names = FALSE)

meteo_2023 <- read.csv("data/Meteorology_tower/DE-Hai_Meteo_all_D_20230101_20231107_gapfilled.csv", 
                       header = T)
meteo_2023 <- meteo_2023 |> 
  clean_names() |> 
  mutate(timestamp = as.Date(timestamp, format = "%Y-%m-%d %H-%M-%S")) 

meteo_2024 <- read.csv("data/Meteorology_tower/DE-Hai_Meteo_tower_D_20240101_20241031_gapfilled.csv", 
                       header = F, skip = 2)
colnames(meteo_2024) <- read.csv("data/Meteorology_tower/DE-Hai_Meteo_tower_D_20240101_20241031_gapfilled.csv", 
                                 header = F, nrows = 1)

meteo_2024 <- meteo_2024 |> 
  clean_names() |> 
  mutate(timestamp = as.Date(timestamp, format = "%Y-%m-%d")) 

colnames(meteo_2023) = 
  c("timestamp", "ta_2m", "ta_10m", "ta_20m", "ta_32m", "ta_35m", "ta_40m", "ta_44m", "pa_44m",
    "rh_44m", "absolute_humidity_kgm_3", "vpd_44m", "wd_44m", "ws_44m", "sw_in_44m",
    "sw_out_44m", "lw_in_44m", "lw_out_44m", "netrad_m_jm_2", "ppfd_in_umolm_2s_1",
    "ppfd_out_umolm_2s_1", "sw_in_spn1_m_jm_2", "sw_dif_in_spn1_m_jm_2", "p_1m",
    "ts_2cm_1", "ts_5cm_1", "ts_15cm_1", "ts_30cm_1", "ts_50cm_1", 
    "ts_2cm_2", "ts_5cm_2", "ts_15cm_2", "ts_30cm_2", "ts_50cm_2", 
    "swc_8cm_1", "swc_16cm_1", "swc_32cm_1", "swc_8cm_2", "swc_8cm_3", "swc_8cm_4",
    "g_1_5cm_m_jm_2", "g_2_5cm_m_jm_2", "g_3_5cm_m_jm_2", "g_4_5cm_m_jm_2", "g_5_5cm_m_jm_2"
  )

meteo_2023 <- meteo_2023[, colnames(meteo_2023) %in% colnames(meteo_2024)]
meteo_2024 <- meteo_2024[, colnames(meteo_2024) %in% colnames(meteo_2023)]

df_meteo <- rbind(meteo_2023, meteo_2024)

# units_2024 <- read.csv("data/Meteorology_tower/DE-Hai_Meteo_tower_D_20240101_20241031_gapfilled.csv", 
#                        header = F, skip = 1, nrows = 2) |> 
#   select(V1, V2) |> 
#   rename(col = V1, unit = V2)

write.csv(df_meteo, "data/microclimate/meteo_2023_2024_D.csv", row.names = FALSE)


## meteo long term ---------------------------------------------------------

df_meteo_2000_2023 <- data.table::fread("data/Meteorology_tower/FLX_DE-Hai_FLUXNET2015_FULLSET_HH_2000-2023_1-3.csv") |> 
  clean_names() |> 
  mutate(timestamp_start = timestamp_start |> as.character() |> as.POSIXct(format = "%Y%m%d%H%M"),
         day = day(timestamp_start),
         month = month(timestamp_start),
         year = year(timestamp_start))

write.csv(df_meteo_2000_2023, "data/Meteorology_Tower/meteo_2000_2023.csv", row.names = FALSE)

## fluxes ------------------------------------------------------------------

### 30 mins ----
fluxes_2023_H <- read.csv("data/microclimate/DE-Hai_HS_LI7200_fluxes_30min_20230101_20231001.csv", 
                          header = T,
                          skip = 2) 
colnames(fluxes_2023_H) <- read.csv("data/microclimate/DE-Hai_HS_LI7200_fluxes_30min_20230101_20231001.csv",
                                    header = F,
                                    nrows = 1)
fluxes_2023_H <- fluxes_2023_H |> 
  clean_names() |> 
  mutate(timestamp = as.POSIXct(timestamp_end, format = "%Y-%m-%d %H:%M")) 

fluxes_2024_H <- read.csv("data/microclimate/DE-Hai_HS_LI7200_fluxes_30min_20240101_20241001.csv", 
                          header = T,
                          skip = 2) 
colnames(fluxes_2024_H) <- read.csv("data/microclimate/DE-Hai_HS_LI7200_fluxes_30min_20240101_20241001.csv",
                                    header = F,
                                    nrows = 1)
fluxes_2024_H <- fluxes_2024_H |> 
  clean_names() |> 
  mutate(timestamp = as.POSIXct(timestamp_end, format = "%Y-%m-%d %H:%M")) 

fluxes_2023_H <- fluxes_2023_H[, colnames(fluxes_2023_H) %in% colnames(fluxes_2024_H)]
fluxes_2024_H <- fluxes_2024_H[, colnames(fluxes_2024_H) %in% colnames(fluxes_2023_H)]

df_fluxes_H <- rbind.data.frame(fluxes_2023_H, fluxes_2024_H)

write.csv(df_fluxes_H, "data/microclimate/fluxes_2023_2024_30min.csv", row.names = FALSE)

### daily ----

fluxes_2023 <- read.csv("data/EddyCovarianceFluxes_tower/DE-Hai_HS_LI7200_fluxes_daily_20230101_20231001.csv", 
                        header = T,
                        skip = 1)
colnames(fluxes_2023) <- read.csv("data/EddyCovarianceFluxes_tower/DE-Hai_HS_LI7200_fluxes_daily_20230101_20231001.csv", 
                                  header = F,
                                  nrows = 1)
fluxes_2023 <- fluxes_2023 |> 
  clean_names() |> 
  mutate(timestamp = as.Date(timestamp, format = "%Y-%m-%d")) 

fluxes_2024 <- read.csv("data/EddyCovarianceFluxes_tower/DE-Hai_HS_LI7200_fluxes_daily_20240101_20241001.csv", 
                        header = T,
                        skip = 1)
colnames(fluxes_2024) <- read.csv("data/EddyCovarianceFluxes_tower/DE-Hai_HS_LI7200_fluxes_daily_20240101_20241001.csv", 
                                  header = F,
                                  nrows = 1)

fluxes_2024 <- fluxes_2024 |>
  clean_names() |>
  mutate(timestamp = as.Date(timestamp, format = "%Y-%m-%d"))

fluxes_2023 <- fluxes_2023[, colnames(fluxes_2023) %in% colnames(fluxes_2024)]
fluxes_2024 <- fluxes_2024[, colnames(fluxes_2024) %in% colnames(fluxes_2023)]

df_fluxes_2023_2024_D <- rbind(fluxes_2023, fluxes_2024)

write.csv(df_fluxes_2023_2024_D, "data/fluxes_2023_2024_D.csv", row.names = FALSE)

# PET calculation ---------------------------------------------------------

## FAO hourly --------------------------------------------------------------

zsl <- 438 # m above sea level
P <-  101.3*((293-0.0065*zsl)/293)^5.26 # air pressure [KPa] at z m above sea level
gamma <-  0.665*10^(-3)*P # saturation slope vapour pressure curve

## delta: slope of vapor pressure curve [kPa °C-1]
f.delta = function(T_air) {
  delta <- 4098 * (0.6108 * exp((17.27 * T_air) / (T + 237.3))) / (T_air + 237.3) ^ 2
}

## es: saturation vapor pressure [kPa]
f.es <- function(T_air) {
  0.6108 * exp(17.27*T_air/(T_air+237.3)) # sat vapor pressure
}

## ea: acutal vapor pressure [kPa]
f.ea <- function(es, RH) {
  RH/100*(es) # act vapor pressure
}

## convert W/m2 to MJ/m2
f.wm2_to_mjm2 <- function(Wm2, timestep_sec) {
  Wm2 / (1000000 / timestep_sec)
}

## Rn: net radiation
f.Rn_Wm2 <- function(sw_in, sw_out, lw_in, lw_out) {
  (sw_in - sw_out) + (lw_in - lw_out)
}

## Rn = L + H + G net rad = sum of latent, sensible and ground heat flux
## G  = RN - L - H
f.G_mjm2 <- function(Rn_MJm2, L_MJm2, H_MJm2) {
  Rn_MJm2 - L_MJm2 - H_MJm2
}

## u2: reference wind speed at 2m above ground [m/s]
f.u2 <- function(Wspd) {
  Wspd * 4.87/log(67.8 * 10 - 5.42) # Wind speed [m/s] at 2m above ground
}


f.PET_penman <- function(delta, Rn_MJm2, G_MJm2, gamma, T_air, u2, es, ea) {
  PET = (0.408 * delta * (Rn_MJm2 - G_MJm2) + gamma * (37 / (T_air + 273)) * u2 * (es - ea))/
    (delta + gamma*(1 + 0.34 * u2))
}

df_meteo_flux_30min <- left_join(
  df_meteo_30min |> select(-c(timestamp_start, timestamp_end)) |>
    filter(!is.na(timestamp)),
  df_fluxes_30min |>  select(-c(timestamp_end)) |> filter(!is.na(timestamp)),
  by = "timestamp"
)

df_meteo_flux_30min <- df_meteo_flux_30min |> 
  mutate(delta = f.delta(T_air = ta_44m),
         es = f.es(T_air = ta_44m),
         ea = f.ea(es, RH = rh_44m),
         Rn_Wm2 = f.Rn_Wm2(sw_in_44m, sw_out_44m, lw_in_44m, lw_out_44m),
         Rn_MJm2 = f.wm2_to_mjm2(Rn_Wm2, 1800),
         L_Wm2 = le_f,
         L_MJm2 = f.wm2_to_mjm2(L_Wm2, 1800), # latent heat flux density
         H_Wm2 = h_f,
         H_MJm2 = f.wm2_to_mjm2(H_Wm2, 1800), # sensible heat flux density
         G_MJm2 = f.G_mjm2(Rn_MJm2, L_MJm2, H_MJm2), # soil heat flux density
         u2 = f.u2(Wspd = ws_44m),
         pet_mm = f.PET_penman(delta, Rn_MJm2, G_MJm2, gamma, T_air = ta_44m, u2, es, ea)) 

ggplot(df_meteo_flux_30min, aes(x = timestamp, y = pet_mm)) +
  geom_line() +
  # scale_x_datetime(date_labels = "%Y", date_breaks = "1 year", 
  #                  limits = as.POSIXct(c("2023-06-01 00:00", "2023-06-30 23:59"))) +
  theme_bw()

sum(df_meteo_flux_30min$pet_mm, na.rm = TRUE)

plot(y = df_meteo_flux_30min$ta_44m, x = df_meteo_flux_30min$timestamp, type = "l")
plot(y = df_fluxes_30min$h_f, x = df_fluxes_30min$timestamp, type = "l")

# ET ----------------------------------------------------------------------

df_meteo_flux_30min <- df_meteo_flux_30min |> 
  mutate(lambda_44m = 2.501 - (2.361 * 10^(-3)) * ta_44m,
         et_mm = L_MJm2 / lambda_44m)

ggplot(df_meteo_flux_30min, aes(x = timestamp, y = et_mm)) +
  geom_line() +
  theme_bw()

sum(df_meteo_flux_30min$p_1m, na.rm = TRUE)

ggplot(df_meteo_flux_30min, aes(x = timestamp, y = lambda_44m)) +
  geom_line() +
  theme_bw()

ggplot(df_meteo_flux_30min) +
  geom_line(aes(x = timestamp, y = ta_44m)) +
  geom_line(aes(x = timestamp, y = L_MJm2), col = "darkred") +
  theme_bw()

# ET - PET balance --------------------------------------------------------

df_meteo_flux_30min <- df_meteo_flux_30min |> 
  mutate(et_pet_mm = et_mm - pet_mm,
         p_pet_mm = p_1m - pet_mm,
         p_et_mm = p_1m - et_mm)




# relative extractable water ----------------------------------------------

df_swc <- df_meteo_flux_30min |> 
  select(c(timestamp, swc_8cm_1, swc_8cm_2, swc_8cm_3, swc_8cm_4, swc_16cm_1, swc_32cm_1)) |>
  group_by(timestamp) |>
  summarise(swc_8cm = mean(c(swc_8cm_1, swc_8cm_2, swc_8cm_3, swc_8cm_4), na.rm = TRUE),
            swc_0_32_avg = mean(c(swc_8cm, swc_16cm_1, swc_32cm_1), na.rm = TRUE)) |> 
  ungroup()

df_swc |>
  ggplot() +
  geom_line(aes(x = timestamp, y = swc_0_32_avg)) 

df_swc_longterm <- df_meteo_2000_2023 |> 
  select(c(day, month, year, swc_f_mds_1, swc_f_mds_2, swc_f_mds_3)) |> 
  filter(swc_f_mds_1 >= 0, swc_f_mds_2 >= 0, swc_f_mds_3 >= 0) |> 
  group_by(year, month, day) |>
  summarise(swc_0_32_avg_d = mean(c(swc_f_mds_1, swc_f_mds_2, swc_f_mds_3), na.rm = TRUE)) |> 
  mutate(timestamp = paste(year, month, day, sep = "-") |> as.Date(format = "%Y-%m-%d"))

df_swc_longterm |> 
  ggplot() +
  geom_line(aes(x = timestamp, y = swc_0_32_avg_d))

## get permanent wilting point as 2% and field capacity as 98% quantile
quantile(df_swc_longterm$swc_0_32_avg_d, probs = c(0.02, 0.98))

min_swc <- quantile(df_swc_longterm$swc_0_32_avg_d, probs = c(0.02)) |> as.numeric()
max_swc <- quantile(df_swc_longterm$swc_0_32_avg_d, probs = c(0.98)) |> as.numeric()

f.rew <- function(swc, swc_min, swc_max) {
  (swc - swc_min) / (swc_max - swc_min)
}

df_swc <- df_swc |> 
  mutate(rew = f.rew(swc_0_32_avg, min_swc, max_swc),
         rew_per = rew * 100,
         timestamp = as.POSIXct(timestamp))

df_meteo_flux_30min <- df_meteo_flux_30min |> 
  left_join(df_swc, by = "timestamp")



# export ------------------------------------------------------------------

write.csv(df_meteo_flux_30min, "data/microclimate/meteo_fluxes_2023_2024_30min.csv", row.names = FALSE)



# Plots for climatic variables --------------------------------------------

## REW ####

df_midday <- read.csv("data/midday_water_potential/df_midday.csv", header = TRUE) |> 
  mutate(timestamp = date |> as.POSIXct())

p <- df_swc |>
  ggplot(aes(x = timestamp, y = rew_per)) +
  geom_line() +
  geom_point(alpha = 0) +
  geom_vline(data = campaign_dates |> as.data.frame(), aes(xintercept = campaign_dates, color = ""), linetype = "dashed") +
  scale_x_datetime(date_breaks = "2 months", date_labels = "%b\n%Y") + # Adjust breaks to every 2 months
  labs(y = "Relative extractable water [%]",
       x = "Date",
       title = "Relative extractable soil water 2023 and 2024",
       subtitle = "Calculated from 2nd and 98th percentile of longterm SWC (2000-2023)",
       col = "Field campaign") +
  presentation_theme +
  theme(aspect.ratio = 0.4)
rew_2023_2024.png <- p |> 
  ggMarginal(type = "density", margins = "y", size = 6)

rew_2023_2024.png
ggsave("figures/rew_2023_2024.png", rew_2023_2024.png, width = 11, height = 6)

## set locale to US
Sys.setlocale("LC_TIME", "en_US.UTF-8")

p <- df_swc |>
  ggplot(aes(x = timestamp, y = rew_per)) +
  geom_line() +
  geom_point(alpha = 0)
p |> ggMarginal(type = "density", margins = "y")


## VPD ####
campaign_dates

p <- df_meteo_flux |>
  select(c(timestamp, vpd_44m)) |>
  mutate(day = as.POSIXct(as.Date(timestamp))) |>
  group_by(day) |>
  summarise(vpd_44m = mean(vpd_44m, na.rm = TRUE)) |>
  ggplot(aes(x = day, y = vpd_44m)) +
  geom_line() +
  geom_point(alpha = 0) +
  geom_vline(data = campaign_dates |> as.data.frame(), aes(xintercept = campaign_dates, color = ""), linetype = "dashed") +
  scale_x_datetime(date_breaks = "2 months", date_labels = "%b\n%Y") + # Adjust breaks to every 2 months
  labs(y = "VPD [kPa]",
       x = "Date",
       title = "Vapor Pressure Deficit 2023 and 2024",
       subtitle = "Calculated from relative humidity and air temperature at 44m",
       color = "Field campaign") +
  # guides(col = guide_legend("Field campaign")) +
  presentation_theme 

vpd_2023_2024.png <- p |> 
  ggMarginal(type = "density", margins = "y", size = 6) 
vpd_2023_2024.png

ggsave("figures/vpd_2023_2024.png", vpd_2023_2024.png, width = 10, height = 5)

## Max TA ####
campaign_dates

p <- df_meteo_flux |>
  select(c(timestamp, ta_44m)) |>
  mutate(day = as.POSIXct(as.Date(timestamp))) |>
  group_by(day) |>
  summarise(max_ta = max(ta_44m, na.rm = TRUE)) |>
  ggplot(aes(x = day, y = max_ta)) +
  geom_line() +
  geom_point(alpha = 0) +
  geom_vline(data = campaign_dates |> as.data.frame(), aes(xintercept = campaign_dates, color = ""), linetype = "dashed") +
  scale_x_datetime(date_breaks = "2 months", date_labels = "%b\n%Y") + # Adjust breaks to every 2 months
  labs(y = "Air temperature [°C]",
       x = "Date",
       title = "Maximum daily air temperatures 2023 and 2024",
       subtitle = "Measured at 44m",
       color = "Field campaign") +
  # guides(col = guide_legend("Field campaign")) +
  presentation_theme 

maxta_2023_2024.png <- p |> 
  ggMarginal(type = "density", margins = "y", size = 6) 
maxta_2023_2024.png

ggsave("figures/maxta_2023_2024.png", maxta_2023_2024.png, width = 11, height = 6)

## ET-PET ####
campaign_dates

p <- df_meteo_flux |>
  select(c(timestamp, et_pet_mm)) |>
  mutate(day = as.POSIXct(as.Date(timestamp))) |>
  group_by(day) |>
  summarise(et_pet_mm = sum(et_pet_mm, na.rm = TRUE)) |>
  ggplot(aes(x = day, y = et_pet_mm)) +
  geom_line() +
  geom_point(alpha = 0) +
  geom_vline(data = campaign_dates |> as.data.frame(), aes(xintercept = campaign_dates, color = ""), linetype = "dashed") +
  scale_x_datetime(date_breaks = "2 months", date_labels = "%b\n%Y") + # Adjust breaks to every 2 months
  labs(y = "Transpiration deficit [mm]",
       x = "Date",
       title = "Transpiration deficit 2023 and 2024",
       subtitle = "TD = Evapotranspiration - Potential evapotranspiration",
       color = "Field campaign") +
  # guides(col = guide_legend("Field campaign")) +
  presentation_theme 

etpet_2023_2024.png <- p |> 
  ggMarginal(type = "density", margins = "y", size = 6) 
etpet_2023_2024.png

ggsave("figures/etpet_2023_2024.png", etpet_2023_2024.png, width = 11, height = 6)



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

df_swc <- df_meteo_H |> 
  select(c(timestamp, swc_8cm_1, swc_8cm_2, swc_8cm_3, swc_8cm_4, swc_16cm_1, swc_32cm_1)) |>
  group_by(timestamp) |>
  summarise(swc_8cm = mean(c(swc_8cm_1, swc_8cm_2, swc_8cm_3, swc_8cm_4), na.rm = TRUE),
            swc_0_32_avg = mean(c(swc_8cm, swc_16cm_1, swc_32cm_1), na.rm = TRUE)) |> 
  ungroup()

df_swc |>
  ggplot() +
  geom_line(aes(x = timestamp, y = swc_0_32_avg)) 

df_swc_longterm <- df_meteo_2000_2023 |> 
  select(c(day, month, year, swc_f_mds_1, swc_f_mds_2, swc_f_mds_3)) |> 
  filter(swc_f_mds_1 >= 0, swc_f_mds_2 >= 0, swc_f_mds_3 >= 0) |> 
  group_by(year, month, day) |>
  summarise(swc_0_32_avg_d = mean(c(swc_f_mds_1, swc_f_mds_2, swc_f_mds_3), na.rm = TRUE)) |> 
  mutate(timestamp = paste(year, month, day, sep = "-") |> as.Date(format = "%Y-%m-%d"))

df_swc_longterm |> 
  ggplot() +
  geom_line(aes(x = timestamp, y = swc_0_32_avg_d))

## get permanent wilting point as 2% and field capacity as 98% quantile
quantile(df_swc_longterm$swc_0_32_avg_d, probs = c(0.02, 0.98))

min_swc <- quantile(df_swc_longterm$swc_0_32_avg_d, probs = c(0.02)) |> as.numeric()
max_swc <- quantile(df_swc_longterm$swc_0_32_avg_d, probs = c(0.98)) |> as.numeric()

f.rew <- function(swc, swc_min, swc_max) {
  (swc - swc_min) / (swc_max - swc_min)
}

df_swc <- df_swc |> 
  mutate(rew = f.rew(swc_0_32_avg, min_swc, max_swc),
         rew_per = rew * 100,
         timestamp = as.POSIXct(timestamp))

df_meteo_H_rew <- df_meteo_H |> 
  left_join(df_swc, by = "timestamp")

write.csv(df_meteo_H_rew, "data/microclimate/df_meteo_H_rew.csv", row.names = FALSE)






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

## meteo ----
ggplot(df_meteo) +
  geom_line(aes(x = timestamp, y = ta_44m)) +
  labs(title = "Air Temperature",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df_meteo) +
  geom_line(aes(x = timestamp, y = vpd_44m)) 




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


calculate_meteo_summary <- function(data, start_date, end_date) {
  data |> 
    filter(timestamp >= start_date, timestamp <= end_date) |> 
    summarise(
      sum_p_mm = sum(p_mm, na.rm = TRUE),
      mean_ta = mean(ta_44m, na.rm = TRUE)
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

