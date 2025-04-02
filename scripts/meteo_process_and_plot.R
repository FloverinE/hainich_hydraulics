# setup -------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(ggExtra)

source("scripts/ggplot_themes.R")

# 0. load data --------------------------------------------------------------------

df_meteo_2023_2024 <- data.table::fread("data/microclimate/meteo_fluxes_2023_2024_30min.csv", header = TRUE) |> 
  mutate(
    timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"),
    pet_mm = case_when(pet_mm < 0 ~ 0, TRUE ~ pet_mm),
    et_mm = case_when(et_mm < 0 ~ 0, TRUE ~ et_mm) 
  )

df_meteo_ref <- data.table::fread("data/microclimate/FLX_DE-Hai_FLUXNET2015_FULLSET_HH_2000-2023_1-3.csv") |> 
  clean_names() |> 
  mutate(timestamp_start = timestamp_start |> as.character() |> as.POSIXct(format = "%Y%m%d%H%M"),
         day = day(timestamp_start),
         month = month(timestamp_start),
         year = year(timestamp_start))

df_meteo_ref_summ <- df_meteo_ref |> 
  filter(year > 1999, year < 2023) |> 
  group_by(year, month) |> 
  summarise(sum_prec = sum(p_f,na.rm = TRUE),
            mean_ta = mean(ta_f_mds, na.rm = TRUE),
            min_ta = min(ta_f_mds, na.rm = TRUE),
            max_ta = max(ta_f_mds, na.rm = TRUE),
            mean_vpd = mean(vpd_f_mds, na.rm = TRUE)
  ) |> 
  group_by(month) |>
  summarise(mean_ta = mean(mean_ta, na.rm = TRUE),
            min_ta = min(min_ta, na.rm = TRUE),
            max_ta = max(max_ta, na.rm = TRUE),
            sum_prec = mean(sum_prec,na.rm = TRUE),
            mean_vpd = mean(mean_vpd, na.rm = TRUE)
  )

df_meteo_ref_summ_2y <- rbind(df_meteo_ref_summ, df_meteo_ref_summ) |> 
  mutate(year = c(rep(2023, 12), rep(2024, 12)),
         timestamp = as.Date(paste(year, month, "01", sep = "-")))

# 1.  descriptive sums / averages ---------------------------------------------

## growing season
df_meteo_2023_2024 |> 
  mutate(grow_seas_2023 = ifelse(timestamp >= "2023-05-01 00:00:00" & timestamp <= "2023-10-31 23:59:59", 1, 0),
         grow_seas_2024 = ifelse(timestamp >= "2024-05-01 00:00:00" & timestamp <= "2024-10-31 23:59:59", 1, 0)) |> 
  group_by(grow_seas_2023, grow_seas_2024) |>
  summarise(sum_prec = sum(p_1m, na.rm = TRUE),
            mean_temp = mean(ta_44m, na.rm = TRUE))


## whole year
df_meteo_2023_2024 |> 
  group_by(year(timestamp)) |> 
  summarise(sum_prec = sum(p_1m),
            mean_temp = mean(ta_44m, na.rm = TRUE))

df_meteo_2023_2024 |> 
  mutate(grow_seas_2023 = ifelse(timestamp >= "2023-05-01 00:00:00" & timestamp <= "2023-10-31 23:59:59", 1, 0),
         grow_seas_2024 = ifelse(timestamp >= "2024-05-01 00:00:00" & timestamp <= "2024-10-31 23:59:59", 1, 0)) |> 
  group_by(grow_seas_2023, grow_seas_2024) |>
  summarise(sum_prec = sum(p_1m),
            sum_et = sum(et_mm , na.rm = TRUE),
            sum_pet = sum(pet_mm, na.rm = TRUE),
            et_pet_perc = sum(pet_mm, na.rm = TRUE) / sum(et_mm, na.rm = TRUE),
            p_et_mm = sum_prec - sum_et
  )

# 2.  plot for Results 3.1 ------------------------------------------------------------
## set locale to english
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Precipitation
# REW
# Mean hourly / maximum daily temperatures
# VPD
# ET, PET
# PPFD / other radiation

date_breaks <- c("2023-01-01 00:00:00", "2023-03-01 00:00:00", "2023-05-01 00:00:00",
                 "2023-07-01 00:00:00", "2023-09-01 00:00:00",
                 "2023-11-01 00:00:00", "2024-01-01 00:00:00",
                 "2024-03-01 00:00:00", "2024-05-01 00:00:00",
                 "2024-07-01 00:00:00", "2024-09-01 00:00:00",
                 "2024-11-01 00:00:00", "2025-01-01 00:00:00") |>
  as.Date() 

campaign_dates <- read.csv("data/calculated_parameters/df_pv_params.csv") |> 
  dplyr::select(date) |> unique()

## Mean & max hourly temperature -----------------------------------------------------

plot.weekly_temp_2324 <- df_meteo_2023_2024 |> 
  mutate(year = year(timestamp), week = lubridate::isoweek(timestamp)) |> 
  group_by(year, week) |> 
  summarise(mean_ta = mean(ta_44m, na.rm = TRUE),
            min_ta = min(ta_44m, na.rm = TRUE),
            max_ta = max(ta_44m, na.rm = TRUE),
            timestamp_new = as.Date(first(timestamp))) |>
  ggplot() +
  geom_vline(data = campaign_dates, aes(xintercept = as.Date(date)), linetype = "dashed") +
  geom_hline(yintercept = 0, col = "grey") +
  # geom_line(aes(x = timestamp_new, y = max_ta, col = "maximum daily Temperature")) +
  geom_ribbon(aes(x = timestamp_new, ymin = min_ta, ymax = max_ta, col = "weekly maximum to minimum"), fill = "grey", alpha = 0.5) +
  geom_line(aes(x = timestamp_new, y = mean_ta, col = "weekly mean")) +
  geom_line(data = df_meteo_ref_summ_2y, aes(x = timestamp, y = mean_ta, col = "monthly reference 2000 - 2022"), linetype = "dotted") +
  scale_x_date(
    breaks = date_breaks,
    labels = scales::date_format("%b\n%Y"),
    limits = as.Date(c("2023-01-01", "2025-01-01")),
  ) +
  scale_color_manual(values = c("weekly maximum to minimum" = "grey",
                                "weekly mean" = "black",
                                "monthly reference 2000 - 2022" = "black"
  )) +
  # see::scale_color_oi(palette = "black_first", order= c(1)) +
  guides(col = guide_legend(title = NULL)) +
  lims(y = c(-15, 32)) +
  labs(y = "Air temperature [°C]", x = "Date", tag = "a)") +
  plot_theme +
  theme(legend.position = c(0.98, 0.02),  # Move legend snugly into the bottom right
        legend.justification = c(1, 0),   # Align legend box to the bottom right
        legend.background = element_rect(fill = "white", color = "black"))  # Optional: border around legend

plot.weekly_temp_2324
# ggsave("figures/meteo/temperature_2324.png", width = 15.5, height = 6, units = "cm", dpi = 300)

## Precipitation ----------------------------------------------------------------

plot.daily_precipitation_2324 <- df_meteo_2023_2024 |>
  group_by(as.Date(timestamp)) |> 
  summarise(p_1m = sum(p_1m)) |>
  mutate(timestamp = as.Date(`as.Date(timestamp)`)) |>
  filter(timestamp >= as.POSIXct("2023-01-01 00:30:00") & 
           timestamp <= as.POSIXct("2024-12-31 23:59:59")) |> 
  ggplot() +
  geom_vline(data = campaign_dates, aes(xintercept = as.Date(date), col = "campaign"), linetype = "dashed") +
  geom_line(aes(x = timestamp, y = p_1m, col = "Precipitation")) + 
  scale_x_date(
    breaks = date_breaks,
    labels = scales::date_format("%b\n%Y"),
    limits = as.Date(c("2023-01-01", "2024-11-01")),
  ) +
  see::scale_color_oi(palette = "black_first", order= c(1, 3)) +
  lims(y = c(0, 30)) +
  labs(y = "Precipitation [mm]", x = "Date", tag = "b)") +
  guides(col = guide_legend(title = NULL)) +
  plot_theme +
  theme(legend.position = c(0.98, 0.98), 
        legend.justification = c(1, 1),  
        legend.background = element_rect(fill = "white", color = "black"))  

plot.daily_precipitation_2324
# ggsave("figures/meteo/precipitation_2324.png", width = 16, height = 8, units = "cm")

plot.weekly_precipitation_2324 <- df_meteo_2023_2024 |>
  mutate(year = year(timestamp), month = week (timestamp)) |> 
  group_by(year, month) |> 
  summarise(sum_p = sum(p_1m, na.rm = TRUE),
            timestamp_new = as.Date(first(timestamp))) |>
  ggplot() +
  geom_col(aes(x = timestamp_new, y = sum_p), fill = "lightblue", width = 5) + 
  geom_vline(data = campaign_dates, aes(xintercept = as.Date(date)), linetype = "dashed", col = "black") +
  scale_x_date(
    breaks = date_breaks,
    labels = scales::date_format("%b\n%Y"),
  ) +
  # see::scale_color_oi(palette = "black_first", order= c(1, 3)) +
  # lims(y = c(0, 30)) +
  labs(y = "Precipitation [mm]", x = "Date", tag = "b)") +
  guides(col = guide_legend(title = NULL)) +
  plot_theme +
  theme(legend.position = c(0.98, 0.98), 
        legend.justification = c(1, 1),  
        legend.background = element_rect(fill = "white", color = "black"))  
plot.weekly_precipitation_2324

plot.monthly_precipitation_2324 <- df_meteo_2023_2024 |>
  mutate(year = year(timestamp), month = month(timestamp)) |> 
  group_by(year, month) |> 
  summarise(sum_p = sum(p_1m, na.rm = TRUE),
            timestamp_new = as.Date(first(timestamp))) |>
  ggplot() +
  geom_col(aes(x = timestamp_new, y = sum_p, fill = "monthly sum"), 
           position = position_nudge(x = -5), width = 10, col = "black") +
  geom_col(data = df_meteo_ref_summ_2y, aes(x = timestamp, y = sum_prec, fill = "monthly reference 2000 - 2022"), col = "black",
           position = position_nudge(x = 5), width = 10) +
  geom_vline(data = campaign_dates, aes(xintercept = as.Date(date)), linetype = "dashed", col = "black") +
  scale_fill_manual(values = c("monthly sum"  = "black",
                               "monthly reference 2000 - 2022" = "white")) +
  scale_x_date(
    breaks = date_breaks,
    labels = scales::date_format("%b\n%Y"),
  ) +
  # see::scale_color_oi(palette = "black_first", order= c(1, 3)) +
  # lims(y = c(0, 30)) +
  labs(y = "Precipitation [mm]", x = "Date", tag = "b)") +
  guides(fill = guide_legend(title = NULL)) +
  plot_theme +
  theme(legend.position = c(0.98, 0.98), 
        legend.justification = c(1, 1),  
        legend.background = element_rect(fill = "white", color = "black"))  
plot.monthly_precipitation_2324

## VPD ----

plot.daily_mean_max_vpd_2324 <- df_meteo_2023_2024 |> 
  group_by(as.Date(timestamp)) |> 
  summarise(mean_vpd = mean(vpd_44m, na.rm = TRUE),
            max_vpd = max(vpd_44m, na.rm = TRUE)) |>
  mutate(timestamp = as.Date(`as.Date(timestamp)`)) |>
  ggplot() +
  geom_vline(data = campaign_dates, aes(xintercept = as.Date(date), col = "campaign"), linetype = "dashed") +
  geom_line(aes(x = timestamp, y = mean_vpd, col = "mean daily VPD")) +
  geom_line(aes(x = timestamp, y = max_vpd, col = "maximum daily VPD")) +
  scale_x_date(
    breaks = date_breaks,
    labels = scales::date_format("%b\n%Y"),
    # limits = as.Date(c("2023-01-01", "2024-11-01")),
  ) +
  see::scale_color_oi(palette = "black_first", order= c(1, 8, 2)) +
  guides(colour = guide_legend(title = NULL)) +
  lims(y = c(0, 30)) +
  labs(y = "Vapor Pressure Deficit [hPa]", x = "Date", tag = "c)") +
  plot_theme +
  theme(legend.position = c(0.98, 0.98),  # Move legend snugly into the bottom right
        legend.justification = c(1, 1),   # Align legend box to the bottom right
        legend.background = element_rect(fill = "white", color = "black"))  # Optional: border around legend

plot.daily_mean_max_vpd_2324
# ggsave("figures/meteo/vpd_2324.png", width = 15.5, height = 8, units = "cm", dpi = 300)

plot.weekly_vpd_2324 <- df_meteo_2023_2024 |> 
  mutate(year = year(timestamp), week = lubridate::isoweek(timestamp)) |> 
  group_by(year, week) |> 
  summarise(mean_vpd = mean(vpd_44m, na.rm = TRUE),
            min_vpd = min(vpd_44m, na.rm = TRUE),
            max_vpd = max(vpd_44m, na.rm = TRUE),
            timestamp_new = as.Date(first(timestamp))) |>
  ggplot() +
  geom_vline(data = campaign_dates, aes(xintercept = as.Date(date)), linetype = "dashed", col = "black") +
  geom_ribbon(aes(x = timestamp_new, ymin = min_vpd, ymax = max_vpd, col = "weekly maximum to minimum"), fill = "grey", alpha = 0.5) +
  geom_line(aes(x = timestamp_new, y = mean_vpd, col = "weekly mean")) +
  geom_line(data = df_meteo_ref_summ_2y, aes(x = timestamp, y = mean_vpd, col = "monthly reference 2000 - 2022"), linetype = "dotted") +
  scale_x_date(
    breaks = date_breaks,
    labels = scales::date_format("%b\n%Y"),
    # limits = as.Date(c("2023-01-01", "2024-11-01")),
  ) +
  scale_color_manual(values = c("weekly maximum to minimum" = "grey",
                                "weekly mean" = "black",
                                "monthly reference 2000 - 2022" = "black"
  )) +
  guides(colour = guide_legend(title = NULL)) +
  # lims(y = c(0, 30)) +
  labs(y = "Vapor Pressure Deficit [hPa]", x = "Date", tag = "c)") +
  plot_theme +
  theme(legend.position = c(0.98, 0.98),  # Move legend snugly into the bottom right
        legend.justification = c(1, 1),   # Align legend box to the bottom right
        legend.background = element_rect(fill = "white", color = "black"))  # Optional: border around legend

plot.weekly_vpd_2324

## REW ---------------------------------------------------------------------

plot.daily_rew_2324 <- df_meteo_2023_2024 |> 
  group_by(as.Date(timestamp)) |> 
  summarise(mean_rew = mean(rew_per, na.rm = TRUE)) |>
  mutate(timestamp = as.Date(`as.Date(timestamp)`)) |>
  ggplot() +
  geom_vline(data = campaign_dates, aes(xintercept = as.Date(date)), col = "black", linetype = "dashed") +
  geom_line(aes(x = timestamp, y = mean_rew), col = "black") +
  scale_x_date(
    breaks = date_breaks,
    labels = scales::date_format("%b\n%Y"),
    # limits = as.Date(c("2023-01-01", "2024-11-01")),
  ) +
  lims(y = c(0, 100)) +
  labs(y = "Relative extractable water [%]", x = "Date", tag = "a)") +
  # see::scale_color_oi(palette = "black_first", order= c(1, 3)) +
  guides(colour = guide_legend(title = NULL)) +
  plot_theme +
  theme(legend.position = c(0.02, 0.02),  # Move legend to the bottom left
        legend.justification = c(0, 0),   # Align legend box to the bottom left
        legend.background = element_rect(fill = "white", color = "black"))  # Optional: border around legend

plot.daily_rew_2324
# ggsave("figures/meteo/rew_2324.png", width = 15.5, height = 8, units = "cm")


## Radiation ---------------------------------------------------------------

plot.daily_ppfd_2324 <- df_meteo_2023_2024 |>
  group_by(as.Date(timestamp)) |>
  summarise(sum_rad = sum(ppfd_in_44m / 1000, na.rm = TRUE)) |>
  mutate(timestamp = as.Date(`as.Date(timestamp)`)) |>
  ggplot() +
  geom_vline(data = campaign_dates, aes(xintercept = as.Date(date)), col = "black",  linetype = "dashed") +
  geom_line(aes(x = timestamp, y = sum_rad), col = "black") +
  scale_x_date(
    breaks = date_breaks,
    labels = scales::date_format("%b\n%Y"),
    # limits = as.Date(c("2023-01-01", "2024-11-01")),
  ) +
  lims(y = c(0, 40)) +
  labs(
    y = expression(paste("PPFD ", 
                         "[mmol " * m^{-2} * d^{-1} * "]")),
    x = "Date",
    tag = "b)"
  ) +
  see::scale_color_oi(palette = "black_first", order= c(1, 2)) +
  guides(colour = guide_legend(title = NULL)) +
  plot_theme +
  theme(legend.position = c(0.02, 0.98),  # Move legend snugly into the bottom right
        legend.justification = c(0, 1),   # Align legend box to the bottom right
        legend.background = element_rect(fill = "white", color = "black"))  # Optional: border around legend

plot.daily_ppfd_2324
# ggsave("figures/meteo/ppfd_2324.png", width = 15.5, height = 8, units = "cm", dpi = 300)

## ET, PET ------------------------------------------------------------------------

plot.daily_etpet_2324 <- df_meteo_2023_2024 |> 
  mutate(et_mm = case_when(et_mm < 0 ~ 0, TRUE ~ et_mm),
         pet_mm = case_when(pet_mm < 0 ~ 0, TRUE ~ pet_mm),
         day = as.Date(timestamp)) |> 
  group_by(day) |>
  summarise(et_pet_mm = sum(et_pet_mm, na.rm = TRUE),
            et_mm = sum(et_mm, na.rm = TRUE),
            pet_mm = sum(pet_mm, na.rm = TRUE)) |> 
  # mutate(day = as.Date(`as.Date(timestamp)`)) |>
  ggplot() +
  geom_vline(data = campaign_dates, aes(xintercept = as.Date(date), col = "campaign"), linetype = "dashed") +
  geom_line(aes(x = day, y = et_mm, col = "Evapotranspiration")) +
  geom_line(aes(x = day, y = pet_mm, col = "Potential Evapotranspiration")) +
  scale_x_date(
    breaks = date_breaks,
    labels = scales::date_format("%b\n%Y"),
    limits = as.Date(c("2023-01-01", "2024-10-31")),
  ) +
  lims(y = c(0, 8)) +
  labs(y = "(potential) Evapotranspiration [mm]", x = "Date", tag = "c)") +
  see::scale_color_oi(palette = "black_first", order= c(1, 8, 2)) +
  guides(colour = guide_legend(title = NULL)) +
  plot_theme +
  theme(legend.position = c(0.02, 0.98),  # Move legend snugly into the bottom right
        legend.justification = c(0, 1),   # Align legend box to the bottom right
        legend.background = element_rect(fill = "white", color = "black"))  # Optional: border around legend

plot.daily_etpet_2324
# ggsave("figures/meteo/et_pet_2324.png", width = 15.5, height = 8, units = "cm")


plot.monthly_etpet_2324 <- df_meteo_2023_2024 |> 
  mutate(et_mm = case_when(et_mm < 0 ~ 0, TRUE ~ et_mm),
         pet_mm = case_when(pet_mm < 0 ~ 0, TRUE ~ pet_mm),
         month = month(timestamp),
         year = year(timestamp)) |> 
  group_by(year, month) |>
  summarise(et_mm = sum(et_mm, na.rm = TRUE),
            pet_mm = sum(pet_mm, na.rm = TRUE),
            timestamp = as.Date(first(timestamp)), .groups = "drop") |>  # Drop grouping warning
  ggplot() +
  geom_col(aes(x = timestamp, y = et_mm, fill = "Evapotranspiration"),
           position = position_nudge(x = -5), width = 10, col = "black") +
  geom_col(aes(x = timestamp, y = pet_mm, fill = "Potential Evapotranspiration"),
           position = position_nudge(x = 5), width = 10, col = "black") +
  geom_vline(data = campaign_dates, aes(xintercept = as.Date(date)), linetype = "dashed") +
  scale_x_date(
    breaks = date_breaks,
    labels = scales::date_format("%b\n%Y")
  ) +
  labs(y = "(Potential) Evapotranspiration [mm]", x = "Date", tag = "c)") +
  scale_fill_manual(values = c("Evapotranspiration" = "black", 
                               "Potential Evapotranspiration" = "white")) +  
  scale_color_manual(values = c("campaign" = "black")) +
  guides(fill = guide_legend(title = NULL),
         col = guide_legend(title = NULL)) +
  plot_theme +
  theme(legend.position = c(0.02, 0.98),  
        legend.justification = c(0, 1),  
        legend.background = element_rect(fill = "white", color = "black"))  

plot.monthly_etpet_2324


## save all ----------------------------------------------------------------

plot.meteo_2023_2024_1.png <- 
  plot.weekly_temp_2324 +
  plot.monthly_precipitation_2324 +
  plot.weekly_vpd_2324 +
  plot_layout(heights = c(5,5,5))
plot.meteo_2023_2024_1.png

ggsave(plot.meteo_2023_2024_1.png, file = "figures/meteo/plot.meteo_2023_2024_1.png", width = 16, height = 15)


plot.meteo_2023_2024_2.png <- 
  plot.daily_rew_2324 +
  plot.daily_ppfd_2324 +
  plot.monthly_etpet_2324 +
  plot_layout(heights = c(5,5,5))
plot.meteo_2023_2024_2.png

ggsave(plot.meteo_2023_2024_2.png, file = "figures/meteo/plot.meteo_2023_2024_2.png", width = 16, height = 15)



# data cleaning -----------------------------------------------------------



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

## clean data ---------------------------------------------------------------

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


