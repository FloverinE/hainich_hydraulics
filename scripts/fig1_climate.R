# setup -------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(ggnewscale)

date_breaks <- c("2023-01-01 00:00:00", "2023-03-01 00:00:00", "2023-05-01 00:00:00",
                 "2023-07-01 00:00:00", "2023-09-01 00:00:00",
                 "2023-11-01 00:00:00", "2024-01-01 00:00:00",
                 "2024-03-01 00:00:00", "2024-05-01 00:00:00",
                 "2024-07-01 00:00:00", "2024-09-01 00:00:00",
                 "2024-11-01 00:00:00") |>
  as.Date() 

campaign_dates <-  read_csv("data/calculated_parameters/df_pv_params.csv") |>
  dplyr::select(date) |> 
  unique() |> 
  mutate(date = date %>% as.Date(format = "%d.%m.%Y"))

source("scripts/ggplot_themes.R")

Sys.setlocale("LC_ALL","English")

# 0. load data --------------------------------------------------------------------

df_meteo_ref_summ = read_csv("data/publication_figs/df_meteo_ref_summ.csv")

df_meteo_2023_2024 <- data.table::fread("data/microclimate_new_2026/df_meteo_23_24_summ.csv", 
                                        header = TRUE) |> 
  filter(timestamp <= "2024-12-31", timestamp >= "2023-01-01")

# # read_csv("data/publication_figs/df_meteo_ref_summ.csv")
# df_meteo_ref = 
#   data.table::fread("data/microclimate/FLX_DE-Hai_FLUXNET2015_FULLSET_HH_2000-2023_1-3.csv") |>
#   clean_names() |>
#   mutate(timestamp_start = timestamp_start |> as.character() |> as.POSIXct(format = "%Y%m%d%H%M"),
#          day = day(timestamp_start),
#          week = week(timestamp_start),
#          month = month(timestamp_start),
#          year = year(timestamp_start))
# 
# df_meteo_ref_summ = df_meteo_ref |>
#   filter(year >= 2000, year <= 2022) |>
#   group_by(year, month, week) |>
#   summarise(sum_prec = sum(p_f,na.rm = TRUE),
#             mean_ta = mean(ta_f_mds, na.rm = TRUE),
#             min_ta = min(ta_f_mds, na.rm = TRUE),
#             max_ta = max(ta_f_mds, na.rm = TRUE),
#             mean_vpd = mean(vpd_f_mds, na.rm = TRUE)
#   ) |>
#   group_by(month) |>
#   summarise(mean_ta = mean(mean_ta, na.rm = TRUE),
#             min_ta = min(min_ta, na.rm = TRUE),
#             max_ta = max(max_ta, na.rm = TRUE),
#             sum_prec = mean(sum_prec,na.rm = TRUE),
#             mean_vpd = mean(mean_vpd, na.rm = TRUE)
#   )
# 
# df_meteo_ref_summ = df_meteo_2023_2024 |> 
#   filter(timestamp >= "2023-01-01 01:00:00", timestamp <= "2024-12-31") |> 
#   select(timestamp) |>
#   mutate(year = year(timestamp), month = month(timestamp), week = week(timestamp)) |> 
#   group_by(year, month) |> 
#   summarise(timestamp = first(timestamp)) |> 
#   left_join(df_meteo_ref_summ, by = c("month")) |> 
#   as.data.frame()
# write_csv(df_meteo_ref_summ, "data/publication_figs/df_meteo_ref_summ.csv")


# 1. Mean & max hourly temperature -----------------------------------------------------

plot.weekly_temp_2324 = df_meteo_2023_2024 |> 
  filter(timestamp >= "2023-01-01 01:00:00", timestamp <= "2024-12-31") |> 
  ggplot() +
  geom_vline(data = campaign_dates, aes(xintercept = as.Date(date)), linetype = "dashed", linewidth = 0.75) +
  geom_hline(yintercept = 0, col = "grey") +
  geom_ribbon(aes(x = timestamp, ymin = t_air_degC_min , ymax = t_air_degC_max, 
                  col = "weekly range"), fill = "grey", alpha = 0.2) +
  geom_line(data = df_meteo_ref_summ, aes(x = timestamp, y = mean_ta, col = "reference mean"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = timestamp, y = t_air_degC_mean, color = "weekly mean"), linewidth = 1) +
  scale_x_date(
    breaks = date_breaks,
    labels = scales::date_format("%b\n%Y"),
    expand = c(0,0),
    limits = as.Date(c("2023-01-01", "2024-12-31")),
  ) +
  scale_y_continuous(
    name = "Air temperature [°C]",
    limits = c(-15, 32),
  ) +
  scale_color_manual(
    name = "Temperature",
    values = c("weekly mean" = "black", "reference mean" = "darkgrey", "weekly range" = "darkgrey")
  ) +
  labs(x = "Date") +
  plot_theme +
  theme(
    legend.background = element_rect(fill = "white", color = "black")
  )

plot.weekly_temp_2324
ggsave(filename = "publication_figures/plot_weekly_temp_2324.png",
       plot.weekly_temp_2324, width = 30, height = 12, units = "cm")


# 2. Precipitation and REW ------------------------------------------------

plot.prec_rew_2324 = df_meteo_2023_2024 |> 
  filter(timestamp >= "2023-01-01 01:00:00", timestamp <= "2024-12-31") |> 
  ggplot() +
  geom_vline(data = campaign_dates, aes(xintercept = as.Date(date)), linetype = "dashed", linewidth = 0.75) +
  geom_hline(yintercept = 0, col = "grey") +
  geom_col(aes(x = timestamp, y = prec_mm, fill = "weekly sum", col = "transparent"),
           width = 5, position = position_dodge()) +
  geom_line(aes(x = timestamp, y = rew_perc*100/1.2, color = "weekly mean"), linewidth = 1) +
  scale_x_date(
    breaks = date_breaks,
    labels = scales::date_format("%b\n%Y"),
    expand = c(0.001,0),
    limits = as.Date(c("2023-01-01", "2024-12-31")),
  ) +
  scale_y_continuous(
    name = "Precipitation [mm]",
    limits = c(0, 80),
    sec.axis = sec_axis(~ . *1.2, name = "Relative extractable water [%]", breaks = seq(0,100,20))
  ) +
  scale_color_manual(
    name = "Relative extractable water",
    values = c("weekly mean" = "black")
  ) +
  scale_fill_manual(
    name = "Precipitation",
    values = c("weekly sum" = "grey")
  ) +
  labs(x = "Date") +
  theme_bw() +
  plot_theme +
  theme(
    legend.background = element_rect(fill = "white", color = "black")
  ) 
  
plot.prec_rew_2324
ggsave(filename = "publication_figures/plot_prec_rew_2324.png", 
       plot.prec_rew_2324, width = 30, height = 12, units = "cm")
    
# 3. VPD ------------------------------------------------------------------

plot.weekly_vpd_2324 = df_meteo_2023_2024 |> 
  filter(timestamp >= "2023-01-01 01:00:00", timestamp <= "2024-12-31") |> 
  ggplot() +
  geom_vline(data = campaign_dates, aes(xintercept = as.Date(date)), linetype = "dashed", linewidth = 0.75) +
  geom_hline(yintercept = 0, col = "grey") +
  geom_ribbon(aes(x = timestamp, ymin = vpd_kPa_min, ymax = vpd_kPa_max, col = "weekly range"),
              fill = "grey", alpha = 0.2) +
  geom_line(data = df_meteo_ref_summ, aes(x = timestamp, y = mean_vpd / 10, col = "reference mean"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = timestamp, y = vpd_kPa_mean, color = "weekly mean"), linewidth = 1) +
  scale_x_date(
    breaks = date_breaks,
    labels = scales::date_format("%b\n%Y"),
    expand = c(0,0),
    limits = as.Date(c("2023-01-01", "2024-12-31")),
  ) +
  scale_y_continuous(
    name = "VPD [kPa]",
    limits = c(-0.1, 4),
  ) +
  scale_color_manual(
    name = "VPD",
    values = c("weekly mean" = "black", "reference mean" = "darkgrey", "weekly range" = "darkgrey")
  ) +
  labs(x = "Date") +
  plot_theme +
  theme(
    legend.background = element_rect(fill = "white", color = "black")
  )

plot.weekly_vpd_2324
ggsave(filename = "publication_figures/plot_weekly_vpd_2324.png", 
       plot.weekly_vpd_2324, width = 30, height = 12, units = "cm")


# mean values for text ----------------------------------------------------

df_meteo_2023_2024 |> 
  mutate(month = month(timestamp_start), year = year(timestamp_start)) |> 
  filter(month %in% c(4:10)) |> 
  group_by(year) |> 
  summarise(meanT = mean(ta_44m),
            sdT = sd(ta_44m),
            sumP = sum(p_1m),
            meanVPD_kPa = mean(vpd_44m),
            sdVPD_kPa = sd(vpd_44m),
            maxVPD_kPa = max(vpd_44m))

df_meteo_2023_2024 |> 
  mutate(month = month(timestamp_start), year = year(timestamp_start), hour = hour(timestamp_start)) |> 
  filter(month %in% c(4:10), hour %in% c(10:16)) |> 
  group_by(year) |> 
  summarise(meanT = mean(ta_44m),
            sdT = sd(ta_44m),
            sumP = sum(p_1m),
            meanVPD_kPa = mean(vpd_44m) / 10,
            sdVPD_kPa = sd(vpd_44m) / 10,
            maxVPD_kPa = max(vpd_44m) / 10)

df_meteo_2023_2024 |> 
  mutate(month = month(timestamp_start), 
         year = year(timestamp_start), 
         day = yday(timestamp_start),
         hour = hour(timestamp_start)) |> 
  filter(month %in% c(4:10), hour %in% c(10:16)) |> 
  group_by(year, day) |> 
  summarise(maxT = max(ta_44m)) |> 
  group_by(year) |> 
  summarise(gr30= sum(maxT > 30),
            gr28= sum(maxT > 28),
            gr25= sum(maxT > 25))
  