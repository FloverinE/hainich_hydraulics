# gmin from mass ~ time slope ---------------------------------------------

df_gmin_slope_nest <- df_gmin_interpolated |>
  mutate(
    elapsed_time_sec = elapsed_time_min * 60,
    leaf_area_m2 = leaf_area_cm2 / 10000,
    leaf_water_molar_mass = leaf_mass_g * 1000 / 18,
    rwc_class = case_when(
      rwc >= 70 & rwc < 90 ~ "standard",
      rwc < 60 ~ "rwc60-",
      rwc >= 60 & rwc < 70 ~ "rwc70",
      rwc >= 70 & rwc < 80 ~ "rwc80",
      rwc >= 80 & rwc < 90 ~ "rwc90",
      rwc >= 90 ~ "rwc90+"
    ) |> as.factor()
  ) |> 
  nest(data = -c(year, campaign, sample_id, date, leaf_area_m2, rwc_class)) 

df_gmin_slope_nest <- df_gmin_slope_nest |> 
  mutate(mod_gmin = map(data, ~ lm(leaf_water_molar_mass ~ elapsed_time_sec, data = .x))) |>
  mutate(gmin_slope = map_dbl(mod_gmin, ~ coef(.)[2])) |> 
  mutate(gmin = - gmin_slope / leaf_area_m2 / mf_vpd) 

df_gmin_slope <- df_gmin_slope_nest |> 
  select(c(year, campaign, sample_id, date, rwc_class, gmin))

df_gmin_slope |> 
  mutate(species = str_sub(sample_id, 1, 4) |> as.factor()) |>
  filter(rwc_class == "standard") |>
  ggplot() +
  geom_line(aes(x = date, y = gmin, col = rwc_class, group = interaction(rwc_class, sample_id)), alpha = 1) +
  facet_wrap(species~year, ncol = 2, scales = "free_x") +
  theme_bw()
