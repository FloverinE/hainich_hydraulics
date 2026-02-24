# setup -------------------------------------------------------------------

library(tidyverse)
library(broom)
library(broom.mixed)
library(modelr)
library(patchwork)

source("scripts/ggplot_themes.R")

# load data --------------------------------------------------------------------

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

# descriptive sums / averages ---------------------------------------------

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


# plot for Results 3.1 ------------------------------------------------------------
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

