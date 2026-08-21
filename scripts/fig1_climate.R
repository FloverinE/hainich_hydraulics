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

df_meteo_ref_summ = read_csv("data/microclimate_new_2026/df_meteo_ref_summ.csv")
df_meteo_2023_2024 <- data.table::fread("data/microclimate_new_2026/df_meteo_23_24_summ.csv", 
                                        header = TRUE) |> 
  filter(timestamp <= "2024-12-31", timestamp >= "2023-01-01")
df.fluxes_summ = read_csv("data/microclimate_new_2026/df.fluxes_summ.csv")

# ## prepare GPP 
# 
# df.fluxes_2023 = data.table::fread("data/microclimate_new_2026/EddyCovarianceFluxes_tower/DE-Hai_HS-LI7200_fluxes_30min_20230101_20240101.csv", skip = 2, header = F)
# df.fluxes_2024 = data.table::fread("data/microclimate_new_2026/EddyCovarianceFluxes_tower/DE-Hai_HS-LI7200_fluxes_30min_20240101_20250101.csv", skip = 2, header = F)
# 
# colnames_fluxes = data.table::fread("data/microclimate_new_2026/EddyCovarianceFluxes_tower/DE-Hai_HS-LI7200_fluxes_30min_20230101_20240101.csv", nrows = 0)|> 
#   janitor::clean_names() |> colnames()
# 
# df.fluxes = rbind(df.fluxes_2023, df.fluxes_2024) |> 
#   setNames(colnames_fluxes)
# 
# per_sec2per_day = 60 * 60 * 24
# # * 60 * 30 / 3600 / 24
# per_sec2per_day_30min = 1800
#   # 60 * 30 / 3600 / 24
# # / 1e6 * 12 (mol weight) = 1.2e5
# umol2gC = -1e-6 * 12.011
# 
# umol2gC
# 
# df.fluxes_summ = df.fluxes |> 
#   select(timestamp_end, nee_f, gpp_f) |> 
#   rename(timestamp = timestamp_end) |> 
#   mutate(week = lubridate::week(timestamp), year = lubridate::year(timestamp), yday = lubridate::yday(timestamp)) |> 
#   group_by(year, yday) |> 
#   summarise(mean_nee = mean(nee_f) / per_sec2per_day, 
#          min_nee = min(nee_f) / per_sec2per_day, 
#          max_nee = max(nee_f) / per_sec2per_day,
#          mean_gpp = mean(gpp_f) / per_sec2per_day, 
#          min_gpp = min(gpp_f) / per_sec2per_day, 
#          max_gpp = max(gpp_f) * per_sec2per_day,
#          sum_gpp = sum(gpp_f),
#          sum_gpp_gC = sum(gpp_f) * umol2gC * per_sec2per_day_30min,
#          timestamp = first(timestamp))
# df.fluxes_summ
# 
# write_csv(df.fluxes_summ, "data/microclimate_new_2026/df.fluxes_summ.csv")

# read_csv("data/publication_figs/df_meteo_ref_summ.csv")
# df_meteo_ref =
#   data.table::fread("data/microclimate_old/FLX_DE-Hai_FLUXNET2015_FULLSET_HH_2000-2023_1-3.csv") |>
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
# write_csv(df_meteo_ref_summ, "data/microclimate_new_2026/df_meteo_ref_summ.csv")


# 1. Mean & max hourly temperature -----------------------------------------------------

plot.weekly_temp_2324 = df_meteo_2023_2024 |> 
  filter(timestamp >= "2023-01-01 01:00:00", timestamp <= "2024-12-31") |> 
  ggplot() +
  geom_vline(data = campaign_dates, aes(xintercept = as.Date(date)), linetype = "dashed", linewidth = 0.75) +
  geom_hline(yintercept = 0, col = "grey") +
  geom_ribbon(aes(x = timestamp, ymin = t_air_degC_min , ymax = t_air_degC_max, 
                  col = "weekly range"), fill = "grey", alpha = 0.2, show.legend = F) +
  # geom_line(data = df_meteo_ref_summ, aes(x = timestamp, y = mean_ta, col = "reference mean"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = timestamp, y = t_air_degC_mean, color = "weekly mean"), linewidth = 1, show.legend = F) +
  # geom_text(label = "A)", aes(x =as.Date("2023-01-15"), y = 32), size = 7.5) +
  scale_x_date(
    breaks = date_breaks,
    labels = scales::date_format("%b\n%Y"),
    expand = c(0,0),
    limits = as.Date(c("2023-01-01", "2024-12-31")),
  ) +
  scale_y_continuous(
    name = "Air temperature [°C]",
    limits = c(-15, 34),
  ) +
  scale_color_manual(
    name = "Temperature",
    values = c("weekly mean" = "black", "reference mean" = "darkgrey", "weekly range" = "darkgrey")
  ) +
  labs(x = NULL, tag = "A)") +
  plot_theme +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.background = element_rect(fill = "white", color = "black")
  )

plot.weekly_temp_2324
ggsave(filename = "figures/1_1_plot_weekly_temp_2324.png",
       plot.weekly_temp_2324, width = 30, height = 12, units = "cm")


# 2. Precipitation and REW ------------------------------------------------

plot.prec_rew_2324 = df_meteo_2023_2024 |> 
  filter(timestamp >= "2023-01-01 01:00:00", timestamp <= "2024-12-31") |> 
  ggplot() +
  geom_vline(data = campaign_dates, aes(xintercept = as.Date(date)), linetype = "dashed", linewidth = 0.75) +
  geom_hline(yintercept = 0, col = "grey") +
  geom_col(aes(x = timestamp, y = prec_mm, fill = "weekly sum", col = "transparent"),
           width = 5, position = position_dodge(), show.legend = F)  +
  geom_line(aes(x = timestamp, y = rew_perc*100/1.2, color = "weekly mean"), linewidth = 1, show.legend = F)  +
  # geom_text(label = "B)", data = campaign_dates, aes(x =as.Date("2023-01-15"), y = 80), size = 7.5) +
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
  labs(x = NULL, tag = "B)") +
  theme_bw() +
  plot_theme +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.background = element_rect(fill = "white", color = "black")
  )
  
plot.prec_rew_2324
ggsave(filename = "figures/1_2_plot_prec_rew_2324.png", 
       plot.prec_rew_2324, width = 30, height = 12, units = "cm")
    
# 3. VPD ------------------------------------------------------------------

plot.weekly_vpd_2324 = df_meteo_2023_2024 |> 
  filter(timestamp >= "2023-01-01 01:00:00", timestamp <= "2024-12-31") |> 
  ggplot() +
  geom_vline(data = campaign_dates, aes(xintercept = as.Date(date)), linetype = "dashed", linewidth = 0.75) +
  geom_hline(yintercept = 0, col = "grey") +
  geom_ribbon(aes(x = timestamp, ymin = vpd_kPa_min, ymax = vpd_kPa_max, col = "weekly range"),
              fill = "grey", alpha = 0.2, show.legend = F) +
  # geom_line(data = df_meteo_ref_summ, aes(x = timestamp, y = mean_vpd / 10, col = "reference mean"), 
  #           linetype = "solid", linewidth = 1, show.legend = F)  +
  geom_line(aes(x = timestamp, y = vpd_kPa_mean, color = "weekly mean"), linewidth = 1, show.legend = F)  +
  # geom_text(label = "C)", data = campaign_dates, aes(x =as.Date("2023-01-15"), y = 4), size = 7.5) +
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
  labs(x = "Date", tag = "C)") +
  plot_theme +
#   
#   theme(
#     legend.background = element_rect(fill = "white", color = "black")
#   )
# + 
  ## switch for including GPP in the figure
theme(
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(),
  legend.background = element_rect(fill = "white", color = "black")
)

plot.weekly_vpd_2324
ggsave(filename = "figures/1_3_plot_weekly_vpd_2324.png", 
       plot.weekly_vpd_2324, width = 30, height = 12, units = "cm")



# 4. GPP ------------------------------------------------------------------

plot.daily_gpp_2324 = df.fluxes_summ |> 
  filter(timestamp >= "2023-01-01 01:00:00", timestamp <= "2024-12-31") |> 
  ggplot() +
  geom_vline(data = campaign_dates, aes(xintercept = as.Date(date)), linetype = "dashed", linewidth = 0.75) +
  geom_hline(yintercept = 0, col = "grey") +
  # geom_ribbon(aes(x = timestamp, ymin = min_gpp, ymax = max_gpp, col = "weekly range"),
  #             fill = "grey", alpha = 0.2, show.legend = F) +
  # # geom_line(data = df_meteo_ref_summ, aes(x = timestamp, y = mean_vpd / 10, col = "reference mean"), 
  # #           linetype = "solid", linewidth = 1, show.legend = F)  +
  # geom_line(aes(x = timestamp, y = mean_gpp, color = "weekly mean"), linewidth = 1, show.legend = F)  +
  geom_line(aes(x = timestamp, y = sum_gpp_gC, color = "weekly mean"), linewidth = 1, show.legend = F)  +
  scale_x_date(
    breaks = date_breaks,
    labels = scales::date_format("%b\n%Y"),
    expand = c(0,0),
    limits = as.Date(c("2023-01-01", "2024-12-31")),
  ) +
  scale_y_continuous(
    # name = "GPP [umol m-2 d-1]",
    name = "GPP [gC m-2 d-1]",
    # limits = c(-0.1, 4),
  ) +
  scale_color_manual(
    name = "VPD",
    values = c("weekly mean" = "black", "reference mean" = "darkgrey", "weekly range" = "darkgrey")
  ) +
  labs(x = "Date", tag = "D)") +
  plot_theme +
  theme(
    legend.background = element_rect(fill = "white", color = "black")
  )
plot.daily_gpp_2324

# collate into single figure ----------------------------------------------

library(patchwork)

plot.temp_prec_vpd = 
  plot.weekly_temp_2324 / plot.prec_rew_2324 / plot.weekly_vpd_2324

plot.temp_prec_vpd

ggsave(filename = "figures/1_plot.temp_prec_vpd.png", 
       plot.temp_prec_vpd, width = 30, height = 24, units = "cm")

plot.temp_prec_vpd_gpp = 
  plot.weekly_temp_2324 / plot.prec_rew_2324 / plot.weekly_vpd_2324 / plot.daily_gpp_2324

plot.temp_prec_vpd_gpp

ggsave(filename = "figures/1_plot.temp_prec_vpd_gpp.png", 
       plot.temp_prec_vpd_gpp, width = 30, height = 32, units = "cm")

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
  