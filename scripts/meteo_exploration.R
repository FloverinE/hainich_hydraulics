library(tidyverse)


meteo_2023 <- read.csv("data/Meteorology_tower/DE-Hai_Meteo_all_D_20230101_20231107_gapfilled.csv", header = T)
meteo_2023 <- meteo_2023 |> 
  mutate(Timestamp = as.Date(TIMESTAMP, format = "%Y-%m-%d"))

fluxes_2023 <- read.csv("data/EddyCovarianceFluxes_tower/DE-Hai_HS_LI7200_fluxes_daily_20230101_20231001.csv", 
                        header = T,
                        skip = 1)
colnames(fluxes_2023) <- read.csv("data/EddyCovarianceFluxes_tower/DE-Hai_HS_LI7200_fluxes_daily_20230101_20231001.csv", 
                                  header = F,
                                  nrows = 1)

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

gmin_2023 <- data.frame(species = rep(c("FASY", "FREX"), each = 4),
                        gmin = c(2.274, 1.665, 3.024, 2.536, 0.969, 1.524, 1.770, 3.993))


campaign_dates_2023 <- c("2023-06-13", "2023-07-17", "2023-08-09", "2023-09-18") |> as.POSIXct()

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

