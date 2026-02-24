# setup -------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(ggExtra)

source("scripts/ggplot_themes.R")

# load data ---------------------------------------------------------------

df_meteo_2000_2024 <- data.table::fread("data/microclimate/meteo_flux_2000_2024.csv", header = TRUE)

df_meteo_2023_2024 <- data.table::fread("data/microclimate/meteo_2023_2024_30min.csv", header = TRUE) |>
  mutate(timestamp = as.POSIXct(timestamp_start, format = "%Y-%m-%d %H:%M:%S"))

df_fluxes_2023_2024 <- data.table::fread("data/microclimate/fluxes_2023_2024_30min.csv", header = TRUE) |>
  mutate(timestamp = as.POSIXct(timestamp_end, format = "%Y-%m-%d %H:%M:%S"))

df_meteo_fluxes_2023_2024 <- df_meteo_2023_2024 |> 
  left_join(df_fluxes_2023_2024, by = "timestamp")

df_meteo_2000_2023 <- data.table::fread("data/microclimate/FLX_DE-Hai_FLUXNET2015_FULLSET_HH_2000-2023_1-3.csv") |> 
  clean_names() |> 
  mutate(timestamp_start = timestamp_start |> as.character() |> as.POSIXct(format = "%Y%m%d%H%M"),
         day = day(timestamp_start),
         month = month(timestamp_start),
         year = year(timestamp_start))

# df_meteo_H <- data.table::fread("data/microclimate/meteo_2023_2024_H.csv", header = TRUE) |> 
#   mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"))
# 
# df_meteo_30min <- data.table::fread("data/microclimate/meteo_2023_2024_30min.csv", header = TRUE) |> 
#   mutate(timestamp = timestamp |> as.POSIXct(format = "%Y-%m-%d %H:%M:%S"))
# 
# df_fluxes_30min <- data.table::fread("data/microclimate/fluxes_2023_2024_30min.csv", header = TRUE) |> 
#   mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"))
# 
# df_meteo_D <- data.table::fread("data/Meteorology_tower/meteo_2023_2024_D.csv", header = T) |> 
#   mutate(timestamp = as.Date(timestamp, format = "%Y-%m-%d"))
# df_fluxes_D <- data.table::fread("data/EddyCovarianceFluxes_tower/fluxes_2023_2024_D.csv", header = T) |> 
#   mutate(timestamp = as.Date(timestamp, format = "%Y-%m-%d"))

# clean data ---------------------------------------------------------------

## new data 09.03.
df_meteo_30min_2023 <- data.table::fread("data/microclimate/DE-Hai_Meteo_tower_30min_20230101_20240101_gapfilled.csv", skip = 2)
names(df_meteo_30min_2023) = data.table::fread("data/microclimate/DE-Hai_Meteo_tower_30min_20230101_20240101_gapfilled.csv", header = F)[1,] |> unlist()
df_meteo_30min_2023 <- df_meteo_30min_2023 |> clean_names()

df_meteo_30min_2024 <- data.table::fread("data/microclimate/DE-Hai_Meteo_tower_30min_20240101_20250101_gapfilled.csv", skip = 2)
names(df_meteo_30min_2024) = data.table::fread("data/microclimate/DE-Hai_Meteo_tower_30min_20240101_20250101_gapfilled.csv", header = F)[1,] |> unlist()
df_meteo_30min_2024 <- df_meteo_30min_2024 |> clean_names()

df_fluxes_30min_2023 <- data.table::fread("data/microclimate/DE-Hai_HS_LI7200_fluxes_30min_20230101_20240101.csv", skip = 2)
names(df_fluxes_30min_2023) = data.table::fread("data/microclimate/DE-Hai_HS_LI7200_fluxes_30min_20230101_20240101.csv", header = F)[1,] |> unlist()
df_fluxes_30min_2023 <- df_fluxes_30min_2023 |> clean_names()

df_fluxes_30min_2024 <- data.table::fread("data/microclimate/DE-Hai_HS_LI7200_fluxes_30min_20240101_20250101.csv", skip = 2)
names(df_fluxes_30min_2024) = data.table::fread("data/microclimate/DE-Hai_HS_LI7200_fluxes_30min_20240101_20250101.csv", header = F)[1,] |> unlist()
df_fluxes_30min_2024 <- df_fluxes_30min_2024 |> clean_names()

df_meteo_2023_2024 <- bind_rows(df_meteo_30min_2023, df_meteo_30min_2024)
write.csv(df_meteo_2023_2024, "data/microclimate/meteo_2023_2024_30min.csv", row.names = FALSE)

df_fluxes_2023_2024 <- bind_rows(df_fluxes_30min_2023, df_fluxes_30min_2024)
write.csv(df_fluxes_2023_2024, "data/microclimate/fluxes_2023_2024_30min.csv", row.names = FALSE)

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
  ) |> 
  mutate(lw_out_44m = case_when(lw_out_44m < -1000 ~ NA, TRUE ~ lw_out_44m),
         sw_out_44m = case_when(sw_out_44m < -1000 ~ NA, TRUE ~ sw_out_44m),
         ta_44m = case_when(ta_44m < -1000 ~ NA, TRUE ~ ta_44m),
         rh_44m = case_when(rh_44m < -1000 ~ NA, TRUE ~ rh_44m),
         )

## fao penman
df_meteo_2000_2023_join <- df_meteo_2000_2023_join |> 
  mutate(delta = f.delta(T_air = ta_44m),
         es = f.es(T_air = ta_44m),
         ea = f.ea(es, RH = rh_44m),
         Rn_Wm2 = f.Rn_Wm2(sw_in_44m, sw_out_44m, lw_in_44m, lw_out_44m),
         Rn_MJm2 = f.wm2_to_mjm2(Rn_Wm2, 1800),
         L_Wm2 = le_f,
         L_MJm2 = f.wm2_to_mjm2(L_Wm2, 1800), # latent heat flux 
         H_Wm2 = h_f,
         H_MJm2 = f.wm2_to_mjm2(H_Wm2, 1800), # sensible heat flux 
         G_MJm2 = f.G_mjm2(Rn_MJm2, L_MJm2, H_MJm2), # soil heat flux 
         u2 = f.u2(Wspd = ws_44m),
         pet_mm = f.PET_penman(delta, Rn_MJm2, G_MJm2, gamma, T_air = ta_44m, u2, es, ea),
         lambda_44m = 2.501 - (2.361 * 10^(-3)) * ta_44m,
         et_mm = L_MJm2 / lambda_44m)
         
df_meteo_2000_2024 <- rbind(df_meteo_2000_2023_join, df_meteo_2023_2024 |> filter(timestamp > "2023-12-20"),
                            fill = T) |> 
  mutate(pet_mm = case_when(pet_mm < 0 ~ 0, TRUE ~ pet_mm),
         et_mm = case_when(et_mm < 0 ~ 0, TRUE ~ et_mm))

df_meteo_2000_2024


df_meteo_2000_2024 |> 
  write.csv("data/microclimate/meteo_flux_2000_2024.csv", row.names = F)


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

# df_meteo_flux_30min <- left_join(
#   df_meteo_30min |> select(-c(timestamp_start, timestamp_end)) |>
#     filter(!is.na(timestamp)),
#   df_fluxes_30min |>  select(-c(timestamp_end)) |> filter(!is.na(timestamp)),
#   by = "timestamp"
# )

df_meteo_fluxes_2023_2024 <- df_meteo_fluxes_2023_2024 |> 
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

ggplot(df_meteo_fluxes_2023_2024, aes(x = timestamp, y = pet_mm)) +
  geom_line() +
  theme_bw()

sum(df_meteo_flux_30min$pet_mm, na.rm = TRUE)

plot(y = df_meteo_flux_30min$ta_44m, x = df_meteo_flux_30min$timestamp, type = "l")
plot(y = df_fluxes_30min$h_f, x = df_fluxes_30min$timestamp, type = "l")

# ET ----------------------------------------------------------------------

df_meteo_fluxes_2023_2024 <- df_meteo_fluxes_2023_2024 |> 
  mutate(lambda_44m = 2.501 - (2.361 * 10^(-3)) * ta_44m,
         et_mm = L_MJm2 / lambda_44m)

ggplot(df_meteo_fluxes_2023_2024, aes(x = timestamp, y = et_mm)) +
  geom_line() +
  theme_bw()

sum(df_meteo_flux_30min$p_1m, na.rm = TRUE)


# ET - PET balance --------------------------------------------------------

df_meteo_fluxes_2023_2024 <- df_meteo_fluxes_2023_2024 |> 
  mutate(et_pet_mm = et_mm - pet_mm,
         p_pet_mm = p_1m - pet_mm,
         p_et_mm = p_1m - et_mm)

# REW relative extractable water ----------------------------------------------

df_swc <- df_meteo_fluxes_2023_2024 |> 
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

df_meteo_fluxes_2023_2024 <- df_meteo_fluxes_2023_2024 |> 
  left_join(df_swc, by = "timestamp")



# export ------------------------------------------------------------------

write.csv(df_meteo_fluxes_2023_2024, "data/microclimate/meteo_fluxes_2023_2024_30min.csv", row.names = FALSE)




# SPEI  -------------------------------------------------------------------

library(SPEI)

## Hainich data ---------------------------------------------------

df_hainich_meteo_summ <- df_meteo_2000_2024 |> 
  mutate(year = year(timestamp),
         month = month(timestamp)) |>
  group_by(year, month) |>
  summarise(sum_prec_mm = sum(p_1m, na.rm = T),
            pet_fao_mm =  sum(pet_mm, na.rm = T),
            mean_ta_degc = mean(ta_44m, na.rm = T)) |> 
  ungroup()

df_hainich_meteo_summ <- df_hainich_meteo_summ |> 
  mutate(pet_tw_mm = thornthwaite(mean_ta_degc, 51.1),
         bal_tw_mm = sum_prec_mm - pet_tw_mm,
         bal_fao_mm = sum_prec_mm - pet_fao_mm,
         timestamp = paste(year, month, "01") |> as.POSIXct(format = "%Y %m %d"),
         year = as.integer(year),
         month = as.integer(month))

climate_hainich_month <- ts(df_hainich_meteo_summ, start = c(2000, 1), frequency = 12)

spei_hainich_1mon <- spei(climate_hainich_month[, "bal_fao_mm"], scale = 1)
spei_hainich_3mon <- spei(climate_hainich_month[, "bal_fao_mm"], scale = 3)
spei_hainich_6mon <- spei(climate_hainich_month[, "bal_fao_mm"], scale = 6)
spei_hainich_12mon <- spei(climate_hainich_month[, "bal_fao_mm"], scale = 12)

plot(spei_hainich_1mon)
plot(spei_hainich_3mon)
plot(spei_hainich_6mon)
plot(spei_hainich_12mon)

df_hainich_meteo_summ <- df_hainich_meteo_summ |> 
  mutate(spei_3mon = spei_hainich_3mon$fitted |> unlist(),
         spei_6mon = spei_hainich_6mon$fitted |> unlist(),
         spei_12mon = spei_hainich_12mon$fitted |> unlist())


write.csv(df_hainich_meteo_summ, "data/microclimate/df_hainich_meteo_summ.csv", row.names = F)

## plot --------------------------------------------------------------------

df_hainich_meteo_summ <- read.csv("data/microclimate/df_hainich_meteo_summ.csv") 

library(scales)

plot.spei3month.png <- df_hainich_meteo_summ |> 
  mutate(date_plot = as.Date(timestamp)) |> 
  ggplot(aes(x = date_plot)) +
  geom_hline(yintercept = c(-100, 0, 100), linetype = "dashed", col = "grey") +
  geom_line(aes(y = bal_fao_mm, col = "Precipitation balance"), show.legend = F) +
  geom_line(aes(y = spei_3mon * 100, col = "SPEI"), show.legend = F) +
  scale_y_continuous(
    name = "P - PET [mm]", 
    sec.axis = sec_axis(~ . / 100, name = "3-month SPEI")
  ) +
  scale_x_date(
    limits = c(as.Date("2000-01-01"), as.Date("2024-12-31")),
    breaks = seq(as.Date("2000-01-01"), as.Date("2024-12-31"), by = "1 year"),
    labels = date_format("%Y")
  ) +
  scale_color_manual(values = c(
    "Precipitation balance" = "darkgrey",
    "SPEI" = "black"
  )) +
  guides(linetype = guide_legend(title = NULL)) +
  labs(x = "Year", color = "Variable", tag = "a)") +
  thesis_theme +
  theme(
    
    axis.text.x = element_text(angle = 45, hjust = 1)  # Adjust color if needed
  )
plot.spei3month.png
# ggsave("figures/meteo/plot.spei3month.png", plot.spei3month.png, width = 16, height = 8, units = "cm")

plot.spei12month.png <- df_hainich_meteo_summ |> 
  mutate(date_plot = as.Date(timestamp)) |> 
  ggplot(aes(x = date_plot)) +
  geom_hline(yintercept = c(-100, 0, 100), linetype = "dashed", col = "grey") +
  geom_line(aes(y = bal_fao_mm, col = "Precipitation balance"), show.legend = F) +
  geom_line(aes(y = spei_12mon * 100, col = "SPEI"), show.legend = T) +
  scale_y_continuous(
    name = "P - PET [mm]", 
    sec.axis = sec_axis(~ . / 100, name = "12-month SPEI")
  ) +
  scale_x_date(
    limits = c(as.Date("2000-01-01"), as.Date("2024-12-31")),
    breaks = seq(as.Date("2000-01-01"), as.Date("2024-12-31"), by = "1 year"),
    labels = date_format("%Y")
  ) +
  scale_color_manual(values = c(
    "Precipitation balance" = "darkgrey",
    "SPEI" = "black"
  )) +
  guides(linetype = guide_legend(title = NULL)) +
  labs(x = "Year", color = "Variable", tag = "b)") +
  thesis_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Adjust color if needed
  )
plot.spei12month.png
# ggsave("figures/meteo/plot.spei12month.png", plot.spei3month.png, width = 16, height = 8, units = "cm")

plot.spei3_12month.png <- plot.spei3month.png + plot.spei12month.png +
  plot_layout(heights = c(6,6))
plot.spei3_12month.png

ggsave("figures/meteo/plot.spei3_12month.png", plot.spei3_12month.png, width = 16, height = 12, units = "cm")  


### tutorial ---------------------------------------------------------------

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



### try with DWD - Eisenach data --------------------------------------------

dwd_eisenach_tair_hist <- read.table("data/dwd/t_air/produkt_tu_stunde_20071101_20231231_07368.txt", header = TRUE, sep = ";") |> 
  mutate(timestamp = as.POSIXct(MESS_DATUM |> as.character(), format = "%Y%m%d%H", tz = "UTZ"),
         ta_degc = TT_TU) |> 
  select(timestamp, ta_degc)

dwd_eisenach_tair_rec <- read.table("data/dwd/t_air/produkt_tu_stunde_20230828_20250227_07368.txt", header = TRUE, sep = ";") |> 
  mutate(timestamp = as.POSIXct(MESS_DATUM |> as.character(), format = "%Y%m%d%H", tz = "UTZ"),
         ta_degc = TT_TU) |> 
  select(timestamp, ta_degc) |> 
  filter(timestamp >= as.POSIXct("2023-12-31", tz = "UTZ"))

dwd_eisenach_tair <- rbind(dwd_eisenach_tair_hist, dwd_eisenach_tair_rec)

dwd_eisenach_prec_hist <- read.table("data/dwd/prec/produkt_rr_stunde_20071101_20231231_07368.txt", header = TRUE, sep = ";") |> 
  mutate(timestamp = as.POSIXct(MESS_DATUM |> as.character(), format = "%Y%m%d%H", tz = "UTZ"),
         prec_mm = R1) |>
  select(timestamp, prec_mm)

dwd_eisenach_prec_rec <- read.table("data/dwd/prec/produkt_rr_stunde_20230828_20250227_07368.txt", header = TRUE, sep = ";") |> 
  mutate(timestamp = as.POSIXct(MESS_DATUM |> as.character(), format = "%Y%m%d%H", tz = "UTZ"),
         prec_mm = R1) |>
  select(timestamp, prec_mm) |> 
  filter(timestamp >= as.POSIXct("2023-12-31", tz = "UTZ"))

dwd_eisenach_prec <- rbind(dwd_eisenach_prec_hist, dwd_eisenach_prec_rec)

climate_eisenach_2007_24 <- dwd_eisenach_tair |> 
  full_join(dwd_eisenach_prec, by = c("timestamp")) |> 
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
  filter(year != 2007) |> 
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
climate_eisenach_spei_1mon = spei(climate_eisenach_month[, "bal_mm"], scale = 1)
climate_eisenach_spei_3mon = spei(climate_eisenach_month[, "bal_mm"], scale = 3)
climate_eisenach_spei_6mon = spei(climate_eisenach_month[, "bal_mm"], scale = 6)
climate_eisenach_spei_12mon = spei(climate_eisenach_month[, "bal_mm"], scale = 12)

plot(climate_eisenach_spei_3mon)

df_climate_eisenach_month <- climate_eisenach_spei_3mon$fitted |> 
  unlist() |> 
  as.data.frame() |> 
  cbind(climate_eisenach_month |> as.data.frame()) |> 
  mutate(spei_3mon = x,
         timestamp = paste(year, month, "01") |> as.POSIXct(format = "%Y %m %d")) 
  

df_climate_eisenach_month |>
  ggplot() +
  geom_line(aes(x = timestamp, y = spei_3mon, col = spei_3mon)) +
  scale_color_discrete(values = c("red", "white", "blue")) +
  theme_bw()


