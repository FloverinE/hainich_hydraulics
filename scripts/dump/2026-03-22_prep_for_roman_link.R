
# setup -------------------------------------------------------------------

library(tidyverse)


# prepare trait and sugar / nutrient data for wide format export ----------


df.phenology = readxl::read_excel("data/microclimate_new_2026/Hai_TowerCamera_Seasons_Summary.xlsx", 
                                  sheet = "import_to_R")

df.hydr_traits = readxl::read_excel(
  "data/calculated_parameters/df_hydr_traits.xlsx",
  sheet = "df_hydr_traits_subsel"
) |>
  mutate(
    date_fac = as.factor(date),
    doy_d = yday(date),
    t5_degc = as.numeric(t5_degc),
    t50_degc = as.numeric(t50_degc),
    t95_degc = as.numeric(t95_degc)
  ) |>  
  left_join(df.phenology, by = c("year", "species")) |> 
  mutate(leaf_age_d = doy_d - sos50_doy,
         year = as.factor(year)) |> 
  dplyr::select(
    -c(
      sos50_doy,
      eos50_doy,
      campaign,
      vessel_order,
      pheno_start_doy,
      date,
      doy_d
    )) |> 
  distinct()

df.hydr_traits_wide = df.hydr_traits |> 
  pivot_wider(names_from = "nutrient_name", values_from = "nutrient_conc") |> 
  pivot_wider(names_from = "sugar_name", values_from = "sugar_conc_")

write_csv(df.hydr_traits_wide, "data/publication/df_traits_nutrients_sugars.csv")

## converted to excel file and annotated with description of variables


# climate data ------------------------------------------------------------

## air temperature (mean, max of 2 weeks up to sampling date)
## rh
## vpd
## REW

df.meteo_2023_2024 = data.table::fread("data/microclimate_new_2026/df_meteo_23_24.csv", 
                                        header = TRUE)


df.meteo_rel = df.meteo_2023_2024 |> 
  select(timestamp_start, precipitation_a_1m, air_temperature_a_44_5m, 
         rel_humidity_a_44_5m, vpd_kPa, swc_all_perc, rew_perc )
## get sampling dates from first df

df.sampling_dates = data.frame(sample_date = df.hydr_traits$date_fac |> unique() |> as.Date()) |> 
  mutate(start_period = sample_date - lubridate::days(14))

df.sampling_dates_meteo =df.sampling_dates |> 
  mutate(campaign = c(1:8)) |> 
  nest(data = -c(campaign)) |> 
  mutate(meteo_data = map(data, ~ filter(df.meteo_rel, 
                                   timestamp_start >= .x$start_period,
                                   timestamp_start <= .x$sample_date)))

df.sampling_dates_meteo = df.sampling_dates_meteo |> 
  mutate(meteo_summary = map(meteo_data,
                              ~ .x |> summarise(mean_Tair_degC = mean(air_temperature_a_44_5m),
                                                max_Tair_degC = max(air_temperature_a_44_5m),
                                                mean_RH_perc= mean(rel_humidity_a_44_5m),
                                                max_RH_perc = max(rel_humidity_a_44_5m),
                                                mean_VPD_kPa = mean(vpd_kPa),
                                                max_VPD_kPa = max(vpd_kPa),
                                                sum_prec_mm = sum(precipitation_a_1m),
                                                mean_REW_perc = mean(rew_perc),
                                                min_REW_perc = min(rew_perc)
                                                )))

df.meteo_export = df.sampling_dates_meteo |> 
  select(data, meteo_summary) |> 
  unnest(cols = c(data, meteo_summary)) |> 
  mutate(across(mean_Tair_degC:min_REW_perc,  ~ round(.x, 3)))

df.meteo_export |> 
  write_csv("data/publication/df_meteo_14day_summary.csv")
