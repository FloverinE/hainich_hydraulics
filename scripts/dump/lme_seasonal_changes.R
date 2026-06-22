# setup -------------------------------------------------------------------
{
  library(lme4)
  # library(MASS)
  library(tidyverse)
  # library(nlme)
  library(glmmTMB)
  library(emmeans)
  library(performance)
  library(flextable)
  library(see)
  # library(cowplot)
  # library(patchwork)
  # library(sjPlot)
  # library(modelr)
  library(ggtext)
}

source("scripts/ggplot_themes.R")

# set locale to US
Sys.setlocale("LC_TIME", "en_US.UTF-8")

plot_specs = list(
  "lme_width_cm" = 12,
  "lme_height_cm" = 6,
  "plot_position_offset" = 5,
  "plot_alpha" = 0.5
)

## reletter compact letter display from top left to bottom right of graps
f.reletter_cld = function(cld_column) {
  cld_column = cld_column |>
    str_remove_all("\\s")

  cld_letters = cld_column |>
    paste(collapse = "") |>
    strsplit("") |>
    unlist() |>
    unique()

  reletter_mat = data.frame(
    old = cld_letters,
    proxy = 1:length(cld_letters) |> as.character(),
    new = letters[1:length(cld_letters)]
  )
  reletter_mat

  for (i in 1:nrow(reletter_mat)) {
    cld_column = cld_column |>
      str_replace_all(reletter_mat$old[i], reletter_mat$proxy[i])
  }
  for (i in 1:nrow(reletter_mat)) {
    cld_column = cld_column |>
      str_replace_all(reletter_mat$proxy[i], reletter_mat$new[i])
  }
  cld_column |>
    map(
      ~ .x |>
        str_split("") |>
        unlist() |>
        sort() |>
        paste0(collapse = "")
    ) |>
    unlist()
}

# Seasonal plasticity - interspecific differences ------------------------

df.hydr_traits = readxl::read_excel(
  "data/calculated_parameters/df_hydr_traits.xlsx",
  sheet = "df_hydr_traits_subsel"
) |>
  mutate(
    year = as.factor(year),
    date_fac = as.factor(date),
    pheno_start_doy = case_when(
      species == "FASY" & year == 2023 ~ 121,
      species == "FASY" & year == 2024 ~ 104,
      species == "FREX" & year == 2023 ~ 140,
      species == "FREX" & year == 2024 ~ 138
    ),
    doy_d = yday(date),
    leaf_age_d = doy_d - pheno_start_doy,
    t5_degc = as.numeric(t5_degc),
    t50_degc = as.numeric(t50_degc),
    t95_degc = as.numeric(t95_degc)
  )


# 1. lme models per trait ---------------------------------------------------

df.hydr_traits_long = df.hydr_traits |>
  filter(vessel_order != "all") |>
  dplyr::select(
    -c(
      campaign,
      vessel_order,
      pheno_start_doy,
      date,
      doy_d,
      sugar_name:nutrient_name
    )
  ) |>
  pivot_longer(
    c(psi_midday_mpa:t95_degc),
    names_to = "trait",
    values_to = "trait_value"
  ) |>
  mutate(trait_value = as.numeric(trait_value))

## nest
df.hydr_traits_nest = df.hydr_traits_long |>
  nest(data = c(year, date_fac, species, sample_id, leaf_age_d, trait_value))

## lme
df.hydr_traits_nest = df.hydr_traits_nest |>
  mutate(
    data = map(data, ~ .x |> filter(!is.na(trait_value)) |> distinct())
  )

# df.hydr_traits_nest$data[[12]] |> View()

lmer(
  trait_value ~ species * date_fac * year + (1 | sample_id),
  data = df.hydr_traits_nest$data[[1]]
)

df.hydr_traits_nest_base = df.hydr_traits_nest |>
  filter(
    !trait %in%
      c("elast_tot_mpa", "gmin_mmol_m2_s", "t5_degc", "t50_degc", "t95_degc")
  ) |>
  mutate(
    lme = map(
      data,
      ~ glmmTMB(
        trait_value ~ species * date_fac * year + (1 | sample_id),
        data = .x,
        family = gaussian()
      )
    )
  )

df.hydr_traits_nest_glm = df.hydr_traits_nest |>
  filter(trait %in% c("elast_tot_mpa", "gmin_mmol_m2_s")) |>
  mutate(
    lme = map(
      data,
      ~ glmmTMB(
        trait_value ~ species * date_fac * year + (1 | sample_id),
        data = .x,
        family = Gamma(link = "log")
      )
    )
  )

df.hydr_traits_nest_tsm = df.hydr_traits_nest |>
  filter(trait %in% c("t5_degc", "t50_degc", "t95_degc")) |>
  mutate(
    lme = map(
      data,
      ~ glmmTMB(
        trait_value ~ species * date_fac + (1 | sample_id),
        data = .x,
        family = gaussian()
      )
    )
  )


# 2. emmeans -------------------------------------------------------------

df.hydr_traits_nest_base = df.hydr_traits_nest_base |>
  mutate(
    emm = map(
      lme,
      ~ emmeans(
        .x,
        ~ species + date_fac + year,
        adjust = "mvt",
        type = "response"
      )
    )
  )

df.hydr_traits_nest_base$data[[1]] |> View()

df.hydr_traits_nest_glm = df.hydr_traits_nest_glm |>
  mutate(
    emm = map(
      lme,
      ~ emmeans(
        .x,
        ~ species + date_fac + year,
        adjust = "mvt",
        type = "response"
      )
    )
  )

df.hydr_traits_nest_tsm = df.hydr_traits_nest_tsm |>
  mutate(
    emm = map(
      lme,
      ~ emmeans(
        .x,
        ~ species + date_fac,
        adjust = "mvt",
        type = "response"
      )
    )
  )

## bind
df.hydr_traits_nest_mods = rbind.data.frame(
  df.hydr_traits_nest_base,
  df.hydr_traits_nest_glm,
  df.hydr_traits_nest_tsm
) |>
  mutate(
    emm = map(
      emm,
      ~ .x |> multcomp::cld(Letters = letters) |> as.data.frame(),
      emm = map(
        emm,
        ~ .x |>
          mutate(
            Year = year,
            species = recode(
              species,
              "FASY" = "*Fagus sylvatica*",
              "FREX" = "*Fraxinus excelsior*"
            ),
            group = f.reletter_cld(.group)
          )
      )
    )
  )
df.hydr_traits_nest_mods$emm[[3]]

# 3. unnest and export contrast table ------------------------------------

df.hydr_traits_nest_mods = df.hydr_traits_nest_mods |>
  mutate(
    data_emm = map2(
      .x = data,
      .y = emm,
      ~ left_join(.x, .y, by = c("species", "date_fac"))
    )
  )

df.hydr_traits_mods = df.hydr_traits_nest_mods |>
  select(trait, data_emm) |>
  unnest(data_emm)

write_csv(
  df.hydr_traits_mods,
  "data/calculated_parameters/df_hydr_traits_emmeans.csv"
)


# 4. plot -------------------------------------------------------------------

df.plot = df.hydr_traits_mods |>
  mutate(
    trait_order = case_when(
      trait == "psi_midday_mpa" ~ "1_psi_midday_mpa",
      trait == "psi_ft_mpa" ~ "2_psi_ft_mpa",
      trait == "psi_tlp_mpa" ~ "3_psi_tlp_mpa",
      trait == "elast_tot_mpa" ~ "4_elast_tot_mpa",
      trait == "rwc_tlp_tot_perc" ~ "5_rwc_tlp_tot_perc",
      trait == "capacitance_ft_tot" ~ "6_capacitance_ft_tot",
      trait == "capacitance_tlp_tot" ~ "7_capacitance_tlp_tot",
      trait == "gmin_mmol_m2_s" ~ "8_gmin_mmol_m2_s",

      trait == "p12_mpa" ~ "9_p12_mpa",
      trait == "p50_mpa" ~ "10_p50_mpa",
      trait == "p88_mpa" ~ "11_p88_mpa",

      trait == "t5_degc" ~ "12_t5_degc",
      trait == "t50_degc" ~ "13_t50_degc",
      trait == "t95_degc" ~ "14_t95_degc",

      # trait == "gmin_mmol_m2_s" ~ "Minimum leaf conductance ",

      T ~ trait
    ),
    Year = year(date_fac) |> as.factor(),
    species = recode(
      species,
      "FASY" = "*Fagus sylvatica*",
      "FREX" = "*Fraxinus excelsior*"
    ),
    pred_val = case_when(
      is.na(response) ~ emmean,
      !is.na(response) ~ response,
      T ~ NA
    ),
    # lower_lim = case_when(
    #   is.na(asymp.LCL) ~ lower.CL,
    #   !is.na(asymp.LCL) ~ asymp.LCL,
    #   T ~ NA
    # ),
    # upper_lim = case_when(
    #   is.na(asymp.UCL) ~ upper.CL,
    #   !is.na(asymp.UCL) ~ asymp.UCL,
    #   T ~ NA
    # ),
    lower_lim = asymp.LCL,
    upper_lim= asymp.UCL,
    # group_y = case_when(year == "2023" ~ group_y + 0.2, T ~ group_y)
  )

trait_labels = c(
  "1_psi_midday_mpa" = "Ψ<sub>MD</sub> (MPa)",
  "2_psi_ft_mpa" = "Ψ<sub>FT</sub> (MPa)",
  "3_psi_tlp_mpa" = "Ψ<sub>TLP</sub> (MPa)",
  "4_elast_tot_mpa" = "ε (MPa)",
  "5_rwc_tlp_tot_perc" = "RWC<sub>TLP</sub> (%)",
  "6_capacitance_ft_tot" = "C<sub>FT</sub> (% MPa<sup>-1</sup>)",
  "7_capacitance_tlp_tot" = "C<sub>TLP</sub> (% MPa<sup>-1</sup>)",
  "8_gmin_mmol_m2_s" = "g<sub>min</sub> (mmol m<sup>-2</sup> s<sup>-1</sup>)",
  "9_p12_mpa" = "P<sub>12</sub> (MPa)",
  "10_p50_mpa" = "P<sub>50</sub> (MPa)",
  "11_p88_mpa" = "P<sub>88</sub> (MPa)",
  "12_t5_degc" = "T<sub>5</sub> (°C)",
  "13_t50_degc" = "T<sub>50</sub> (°C)",
  "14_t95_degc" = "T<sub>95</sub> (°C)"
)

df.plot$trait_order <- factor(
  df.plot$trait_order,
  levels = names(trait_labels)
)

df.plot_dummy_limits = df.plot |>
  group_by(trait_order) |>
  reframe(
    min_y = case_when(
      min(trait_value) > 0 & max(trait_value) < 0.5 ~ 0,
      T ~ min(trait_value / 2 |> trunc()) |> floor() * 2
    ),
    max_y = case_when(
      min(trait_value) > 0 & max(trait_value) < 0.5 ~ 0.5,
      T ~ max(trait_value / 2 |> trunc()) |> ceiling() * 2
    ),
  ) |>
  pivot_longer(cols = c("min_y":"max_y"), values_to = "y") |>
  mutate(
    x = rep(c(0, 175), length(unique(trait_order))),
    species = "*Fagus sylvatica*",
    Year = "2023"
  )

df.plot_dummy_limits$trait_order <- factor(
  df.plot_dummy_limits$trait_order,
  levels = names(trait_labels)
)

plot.all.png <-
  ggplot(
    df.plot,
    aes(
      x = leaf_age_d,
      col = species,
      linetype = Year
    )
  ) +
  geom_point(
    aes(y = trait_value),
    alpha = plot_specs$plot_alpha,
    show.legend = T
  ) +
  geom_line(
    aes(y = pred_val),
    alpha = plot_specs$plot_alpha,
    show.legend = F
  ) +
  geom_errorbar(
    aes(ymin = lower_lim, ymax = upper_lim),
    alpha = plot_specs$plot_alpha,
    width = 3,
    show.legend = T
  ) +
  geom_blank(
    data = df.plot_dummy_limits,
    aes(x = x, y = y),
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  # geom_text(
  #   aes(y = group_y, label = group),
  #   show.legend = F
  # ) +
  scale_color_oi(order = c(6, 2)) +
  scale_x_continuous(limits = c(0, 175), breaks = seq(0, 175, 25)) +
  facet_wrap(
    ~trait_order,
    scales = "free_y",
    strip.position = "top",
    labeller = labeller(trait_order = as_labeller(trait_labels))
  ) +
  labs(
    y = NULL,
    x = "Days since full leaf expansion (d)",
  ) +
  thesis_theme +
  theme(
    strip.text = ggtext::element_markdown(),
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.text = ggtext::element_markdown(),
  )

plot.all.png


# trait == "psi_midday_mpa" ~ "Midday leaf water potential [MPa]",

# trait == "psi_tlp" ~ "Osm. potential at turgor loss point [MPa]",
# trait == "psi_ft" ~ "Osm. potential at full turgor [MPa]",
# trait == "elast_tot" ~ "Total elasticity [MPa]",
# trait == "rwc_tlp_tot" ~ "Relative water content at turgor loss [%]",
# trait == "capacitance_tlp_tot" ~ "Capacitance at turgor loss [%/MPa]",
# trait == "capacitance_ft_tot" ~ "Capacitance at full turgor [%/MPa]",

# 1.1 Midday water potentials -------------------------------------------------

df.psi_midday = df.hydr_traits |>
  select(
    date_fac,
    date,
    year,
    species,
    sample_id,
    psi_midday_mpa,
    leaf_age_d
  ) |>
  distinct() |>
  filter(!is.na(psi_midday_mpa))

## 1.1.2 lme ------------------------------------------------

lme.psi_midday <- lmer(
  psi_midday_mpa ~ species * date_fac * year + (1 | sample_id),
  data = df.psi_midday
)

lme.psi_midday |> summary()
lme.psi_midday |> r2()

performance::check_model(lme.psi_midday)

df.psi_midday |>
  ggplot() +
  geom_point(aes(x = leaf_age_d, y = psi_midday_mpa, color = species)) +
  geom_line(aes(
    x = leaf_age_d,
    y = predict(lme.psi_midday),
    color = species,
    group = interaction(year, sample_id),
    linetype = as.factor(year)
  )) +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme


## 1.1.3 emmeans ------------------------------------------------

emm.psi_midday <- emmeans(
  lme.psi_midday,
  ~ species * date_fac * year,
  adjust = "bonferroni",
  type = "response"
)

emm.psi_midday

df.emm.psi_midday <- emm.psi_midday |>
  multcomp::cld(Letters = letters) |>
  as.data.frame()

df.emm.psi_midday

df.emm.psi_midday = emm.psi_midday |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  # select(-.group) |>
  left_join(
    df.hydr_traits |>
      dplyr::select(species, date_fac, year, psi_midday_mpa, leaf_age_d) |>
      distinct(),
    by = c("species", "date_fac", "year")
  ) |>
  mutate(
    group_y = case_when(species == "FASY" ~ 0, species == "FREX" ~ -4)
  ) |>
  pivot_longer(
    cols = c(psi_midday_mpa),
    names_to = "stat",
    values_to = "meas_value"
  ) |>
  group_by(stat) |>
  arrange(leaf_age_d) |>
  mutate(
    y_axis_label = "\u03a8 [MD]~(MPa)",
    Year = year,
    species = recode(
      species,
      "FASY" = "*Fagus sylvatica*",
      "FREX" = "*Fraxinus excelsior*"
    ),
    group = f.reletter_cld(.group)
  )


## plot ----
## comments Sharath 23.02.2026
# we could show both years in one facet.
# only one line for a year/species
# showing 95% CI's as shaded lines ## => we don't have observations there, so CI errorbars should be better
# separate line for year (then could use a different line type for years)

plot.psi_midday.png <- df.psi_midday_emmeans |>
  arrange(species, date_fac, year) |>
  mutate(
    Year = year,
    species = recode(
      species,
      "FASY" = "*Fagus sylvatica*",
      "FREX" = "*Fraxinus excelsior*"
    ),
    group = f.reletter_cld(.group)
  ) |>
  ggplot() +
  geom_point(
    aes(x = leaf_age_d, y = psi_midday_mpa, col = species),
    alpha = plot_specs$plot_alpha,
  ) +
  geom_line(
    aes(x = leaf_age_d, y = emmean, col = species, linetype = Year),
  ) +
  geom_errorbar(
    aes(
      x = leaf_age_d,
      ymin = lower.CL,
      ymax = upper.CL,
      color = species,
      linetype = Year
    ),
    width = 3,
  ) +
  geom_text(
    aes(x = leaf_age_d, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  scale_color_oi(order = c(6, 2)) +
  scale_x_continuous(limits = c(0, 175), breaks = seq(0, 175, 25)) +
  lims(y = c(-4, 0)) +
  labs(
    y = "Midday water potential [MPa]",
    x = "Days since full leaf expansion [d]"
  ) +
  guides(color = guide_legend(title = "Species")) +
  thesis_theme +
  theme(
    legend.text = ggtext::element_markdown(),
  )
plot.psi_midday.png

plot_size_lme = list("height_cm" = 6, "width_cm" = 12)

ggsave(
  "figures/Fig02/plot.psi_midday.png",
  plot.psi_midday.png,
  width = plot_size_lme$width_cm,
  height = plot_size_lme$height_cm,
  units = "cm",
  dpi = 150
)

df.psi_midday_emmeans |>
  arrange(psi_midday_mpa) |>
  dplyr::select(c(
    species,
    year,
    sample_id,
    campaign,
    emmean,
    psi_midday_mpa
  )) |>
  mutate(psi_midday_mpa = psi_midday_mpa |> round(1)) |>
  View()

df.psi_midday_emmeans |>
  group_by(species, year, date_fac) |>
  reframe(
    mean = mean(psi_midday_mpa) |> round(1),
    sd = sd(psi_midday_mpa) |> round(1),
    emmean = emmean
  ) |>
  View()


## 1.1.5 export ------------------------------------------------------------------

df.emm.psi_midday |>
  arrange(year, species, date_fac) |>
  mutate(.group = f.reletter_cld(.group)) |>
  write.table("clipboard", sep = "\t", row.names = F)

lme.psi_midday |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F)


# 1.2 Pressure-Volume ---------------------------------------------------------------

## 1.2.1 prepare ------------------------------------------------------------
df.pv_params = df.hydr_traits |>
  select(
    date_fac,
    year,
    species,
    sample_id,
    psi_ft,
    psi_tlp,
    elast_tot,
    rwc_tlp_tot,
    capacitance_tlp_tot,
    leaf_age_d
  ) |>
  distinct() |>
  filter(!is.na(psi_ft))

## 1.2.2 lme ---------------------------------------------------------------------

lme.psi_ft <- lmer(
  psi_ft ~ species * date_fac * year + (1 | sample_id),
  data = df.pv_params
)

lme.psi_ft |> summary()
lme.psi_ft |> r2()

ggplot(df.pv_params) +
  geom_point(aes(x = leaf_age_d, y = psi_ft, color = species)) +
  # geom_line(aes(x = doy, y = psi_ft, color = species, group = sample_id)) +
  geom_line(aes(
    x = leaf_age_d,
    y = predict(lme.psi_ft),
    color = species,
    group = sample_id
  )) +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme

lme.psi_tlp <- lmer(
  psi_tlp ~ species * date_fac * year + (1 | sample_id),
  data = df.pv_params
)

lme.psi_tlp |> summary()
lme.psi_tlp |> r2()

ggplot(df.pv_params) +
  geom_point(aes(x = leaf_age_d, y = psi_tlp, color = species)) +
  # geom_line(aes(x = doy, y = psi_ft, color = species, group = sample_id)) +
  geom_line(aes(
    x = leaf_age_d,
    y = predict(lme.psi_tlp),
    color = species,
    group = sample_id
  )) +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme

lme.elast_tot <- lmer(
  elast_tot ~ species * date_fac * year + (1 | sample_id),
  data = df.pv_params
)
lme.elast_tot |> summary()
lme.elast_tot |> r2()

df.pv_params |>
  add_predictions(lme.elast_tot, var = "elast_tot_pred") |>
  ggplot() +
  geom_point(aes(x = leaf_age_d, y = elast_tot, color = species)) +
  geom_line(aes(
    x = leaf_age_d,
    y = elast_tot_pred,
    color = species,
    group = sample_id
  )) +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme

lme.rwc_tlp_tot <- lmer(
  rwc_tlp_tot ~ species * date_fac * year + (1 | sample_id),
  data = df.pv_params
)
lme.rwc_tlp_tot |> summary()
lme.rwc_tlp_tot |> r2()

df.pv_params |>
  add_predictions(lme.rwc_tlp_tot, var = "rwc_tlp_tot_pred") |>
  ggplot() +
  geom_point(aes(x = leaf_age_d, y = rwc_tlp_tot, color = species)) +
  geom_line(aes(
    x = leaf_age_d,
    y = rwc_tlp_tot_pred,
    color = species,
    group = sample_id
  )) +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme

lme.capacitance_tlp_tot <- lmer(
  capacitance_tlp_tot ~ species * date_fac * year + (1 | sample_id),
  data = df.pv_params
)
lme.capacitance_tlp_tot |> summary()
lme.capacitance_tlp_tot |> r2()

df.pv_params |>
  add_predictions(lme.capacitance_tlp_tot, var = "capacitance_tlp_tot_pred") |>
  ggplot() +
  geom_point(aes(x = leaf_age_d, y = capacitance_tlp_tot, color = species)) +
  geom_line(aes(
    x = leaf_age_d,
    y = capacitance_tlp_tot_pred,
    color = species,
    group = sample_id
  )) +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme


## 1.2.3 emmeans ----------------------------------------------------------------

emm.psi_ft <- emmeans(
  lme.psi_ft,
  ~ species + date_fac + year,
  adjust = "mvt",
  trans = "response"
)

df.emm.psi_ft = emm.psi_ft |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  # select(-.group) |>
  left_join(
    df.pv_params |> dplyr::select(species, date_fac, year, psi_ft, leaf_age_d),
    by = c("species", "date_fac", "year")
  ) |>
  mutate(
    group_y = case_when(species == "FASY" ~ 0, species == "FREX" ~ -4)
  ) |>
  pivot_longer(
    cols = c(psi_ft),
    names_to = "stat",
    values_to = "meas_value"
  ) |>
  group_by(stat) |>
  arrange(leaf_age_d) |>
  mutate(
    y_axis_label = "\u03a8 [FT]~(MPa)",
    Year = year,
    species = recode(
      species,
      "FASY" = "*Fagus sylvatica*",
      "FREX" = "*Fraxinus excelsior*"
    ),
    group = f.reletter_cld(.group)
  )

emm.psi_tlp <- emmeans(
  lme.psi_tlp,
  ~ species + date_fac + year,
  adjust = "mvt",
  trans = "response"
)

df.emm.psi_tlp = emm.psi_tlp |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  left_join(
    df.pv_params |> dplyr::select(species, date_fac, year, psi_tlp, leaf_age_d),
    by = c("species", "date_fac", "year")
  ) |>
  mutate(
    group_y = case_when(species == "FASY" ~ 0, species == "FREX" ~ -4),
  ) |>
  pivot_longer(
    cols = c(psi_tlp),
    names_to = "stat",
    values_to = "meas_value"
  ) |>
  group_by(stat) |>
  arrange(leaf_age_d) |>
  mutate(
    y_axis_label = "\u03a8 [TLP]~(MPa)",
    Year = year,
    species = recode(
      species,
      "FASY" = "*Fagus sylvatica*",
      "FREX" = "*Fraxinus excelsior*"
    ),
    group = f.reletter_cld(.group)
  ) |>
  ungroup()

emm.elast_tot <- emmeans(
  lme.elast_tot,
  ~ species + date_fac + year,
  adjust = "mvt",
  type = "response"
)

df.emm.elast_tot = emm.elast_tot |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  # select(-.group) |>
  left_join(
    df.pv_params |>
      dplyr::select(species, date_fac, year, elast_tot, leaf_age_d),
    by = c("species", "date_fac", "year")
  ) |>
  mutate(
    group_y = case_when(species == "FASY" ~ 0, species == "FREX" ~ 60),
  ) |>
  pivot_longer(
    cols = c(elast_tot),
    names_to = "stat",
    values_to = "meas_value"
  ) |>
  group_by(stat) |>
  arrange(leaf_age_d) |>
  mutate(
    y_axis_label = "\u03b5~(MPa)",
    Year = year,
    species = recode(
      species,
      "FASY" = "*Fagus sylvatica*",
      "FREX" = "*Fraxinus excelsior*"
    ),
    group = f.reletter_cld(.group)
  ) |>
  ungroup()

emm.rwc_tlp_tot <- emmeans(
  lme.rwc_tlp_tot,
  ~ species + date_fac + year,
  adjust = "mvt",
  type = "response"
)

df.emm.rwc_tlp_tot = emm.rwc_tlp_tot |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  # select(-.group) |>
  left_join(
    df.pv_params |>
      dplyr::select(species, date_fac, year, rwc_tlp_tot, leaf_age_d),
    by = c("species", "date_fac", "year")
  ) |>
  mutate(
    group_y = case_when(species == "FASY" ~ 70, species == "FREX" ~ 100),
  ) |>
  pivot_longer(
    cols = c(rwc_tlp_tot),
    names_to = "stat",
    values_to = "meas_value"
  ) |>
  group_by(stat) |>
  arrange(leaf_age_d) |>
  mutate(
    y_axis_label = "RWC[TLP]~(%)",
    Year = year,
    species = recode(
      species,
      "FASY" = "*Fagus sylvatica*",
      "FREX" = "*Fraxinus excelsior*"
    ),
    group = f.reletter_cld(.group)
  ) |>
  ungroup()

emm.capacitance_tlp_tot <- emmeans(
  lme.capacitance_tlp_tot,
  ~ species + date_fac + year,
  adjust = "mvt",
  type = "response"
)

df.emm.capacitance_tlp_tot = emm.capacitance_tlp_tot |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  # select(-.group) |>
  left_join(
    df.pv_params |>
      dplyr::select(species, date_fac, year, capacitance_tlp_tot, leaf_age_d),
    by = c("species", "date_fac", "year")
  ) |>
  mutate(
    group_y = case_when(species == "FASY" ~ 0, species == "FREX" ~ 1),
  ) |>
  pivot_longer(
    cols = c(capacitance_tlp_tot),
    names_to = "stat",
    values_to = "meas_value"
  ) |>
  group_by(stat) |>
  arrange(leaf_age_d) |>
  mutate(
    y_axis_label = "RWC[TLP]~(%)",
    Year = year,
    species = recode(
      species,
      "FASY" = "*Fagus sylvatica*",
      "FREX" = "*Fraxinus excelsior*"
    ),
    group = f.reletter_cld(.group)
  ) |>
  ungroup()

## 1.2.4 plot --------------------------------------------------------------

df.plot = rbind.data.frame(
  df.emm.psi_midday,
  df.emm.psi_tlp,
  df.emm.psi_ft,
  df.emm.elast_tot,
  df.emm.rwc_tlp_tot,
  df.emm.capacitance_tlp_tot
) |>
  mutate(
    trait_title = case_when(
      stat == "psi_tlp" ~ "Osm. potential at turgor loss point [MPa]",
      stat == "psi_ft" ~ "Osm. potential at full turgor [MPa]",
      stat == "elast_tot" ~ "Total elasticity [MPa]",
      stat == "psi_midday_mpa" ~ "Midday leaf water potential [MPa]",
      stat == "rwc_tlp_tot" ~ "Relative water content at turgor loss [%]",
      stat == "capacitance_tlp_tot" ~ "Capacitance at turgor loss [%/MPa]",
      T ~ stat
    ),
    group_y = case_when(year == "2023" ~ group_y + 0.2, T ~ group_y)
  )


df.plot_dummy_limits = df.plot |>
  group_by(stat) |>
  summarise(
    min_y = min(meas_value / 2 |> trunc()) |> floor() * 2,
    max_y = max(meas_value / 2 |> trunc()) |> ceiling() * 2
  ) |>
  pivot_longer(cols = c("min_y":"max_y"), values_to = "y") |>
  mutate(
    x = rep(c(0, 175), length(unique(stat))),
    species = "*Fagus sylvatica*",
    Year = "2023"
  )

trait_labels = c(
  "psi_tlp" = "\u03a8 [TLP]~(MPa)",
  "psi_ft" = "\u03a8 [FT]~(MPa)",
  "elast_tot" = "\u03b5~(MPa)"
)

plot.psi_ft.png <-
  ggplot(
    df.plot,
    aes(
      x = leaf_age_d,
      col = species,
      linetype = Year
    )
  ) +
  geom_blank(data = df.plot_dummy_limits, aes(x = x, y = y), show.legend = F) +
  geom_point(
    aes(y = meas_value),
    alpha = plot_specs$plot_alpha,
    show.legend = T
  ) +
  geom_line(
    aes(y = emmean),
    alpha = plot_specs$plot_alpha,
    show.legend = F
  ) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    alpha = plot_specs$plot_alpha,
    width = 3,
    show.legend = T
  ) +
  # geom_text(
  #   aes(y = group_y, label = group),
  #   show.legend = F
  # ) +
  scale_color_oi(order = c(6, 2)) +
  scale_x_continuous(limits = c(0, 175), breaks = seq(0, 175, 25)) +
  facet_wrap(
    ~stat,
    scales = "free_y",
    strip.position = "left",
    labeller = labeller(stat = as_labeller(trait_labels, label_parsed))
  ) +
  labs(
    y = NULL,
    x = "Days since full leaf expansion (d)",
  ) +
  thesis_theme +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.text = ggtext::element_markdown(),
  )

plot.psi_ft.png

ggsave(
  "figures/pv/plot.psi_ft.png",
  plot.psi_ft.png,
  width = 8,
  height = 4,
  dpi = 150
)


## export contrast tables ----

df.emm.psi_ft |>
  arrange(psi_midday_mpa) |>
  select(c(species, year, sample_id, campaign, emmean, psi_midday_mpa)) |>
  mutate(psi_midday_mpa = psi_midday_mpa |> round(1)) |>
  View()

df.emm.psi_ft |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(psi_ft) |> round(1),
    sd = sd(psi_ft) |> round(1),
    emmean = emmean
  ) |>
  View()

#### psi_tlp ----

df.emm.psi_tlp <- df.emm.psi_tlp |>
  left_join(
    df.pv_params |> dplyr::select(species, campaign, year, date, psi_tlp),
    by = c("species", "campaign", "year")
  ) |>
  mutate(
    group_y = case_when(species == "FASY" ~ 0, species == "FREX" ~ -4),
    date_plot = date
  )

year(df.emm.psi_tlp$date_plot) = 2000

plot.psi_tlp.png <- df.emm.psi_tlp |>
  arrange(species, campaign, year) |>
  mutate(
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group = f.reletter_cld(.group)
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = psi_tlp, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 0.2,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = F
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  scale_color_oi(order = c(6, 2)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  lims(y = c(-4, 0)) +
  labs(y = expression(Psi[tlp] ~ "[MPa]"), x = "Date", tag = "b)") +
  guides(color = guide_legend(title = "Species")) +
  facet_wrap(~year) +
  thesis_theme +
  theme(strip.background = element_blank())
plot.psi_tlp.png

ggsave(
  "figures/pv/plot.psi_tlp.png",
  plot.psi_tlp.png,
  width = 8,
  height = 4,
  dpi = 150
)

df.emm.psi_tlp |>
  arrange(psi_tlp) |>
  select(c(species, year, sample_id, campaign, emmean, psi_tlp)) |>
  mutate(psi_tlp = psi_tlp |> round(1)) |>
  View()

df.emm.psi_tlp |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(psi_tlp) |> round(1),
    sd = sd(psi_tlp) |> round(1),
    emmean = emmean
  ) |>
  View()

#### elast ----

df.emm.elast <- df.emm.elast |>
  left_join(
    df.pv_params |>
      dplyr::select(species, campaign, year, date, elasticity_tot),
    by = c("species", "campaign", "year")
  ) |>
  mutate(date_plot = date)

year(df.emm.elast$date_plot) = 2000

plot.elast.png <- df.emm.elast |>
  arrange(species, campaign, year) |>
  mutate(
    group_y = case_when(species == "FASY" ~ 40, species == "FREX" ~ 0),
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group = f.reletter_cld(.group)
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = elasticity_tot, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 0.2,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = F
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  scale_color_oi(order = c(6, 2)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  lims(y = c(0, 40)) +
  labs(y = expression(epsilon ~ "[MPa]"), x = "Date", tag = "c)") +
  guides(color = guide_legend(title = "Species")) +
  facet_wrap(~year) +
  thesis_theme +
  theme(strip.background = element_blank())
plot.elast.png

ggsave(
  "figures/pv/plot.elast.png",
  plot.elast.png,
  width = 8,
  height = 4,
  dpi = 150
)

df.emm.elast |>
  arrange(elasticity_tot) |>
  select(c(species, year, campaign, emmean, elasticity_tot)) |>
  mutate(elasticity_tot = elasticity_tot |> round(1)) |>
  View()

df.emm.elast |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(elasticity_tot) |> round(1),
    sd = sd(elasticity_tot) |> round(1),
    emmean = emmean
  ) |>
  View()

#### rwc_tlp ----

df.emm.rwc_tlp <- df.emm.rwc_tlp |>
  left_join(
    df.pv_params |> dplyr::select(species, campaign, year, date, rwc_tlp_tot),
    by = c("species", "campaign", "year")
  ) |>
  mutate(date_plot = date)

year(df.emm.rwc_tlp$date_plot) = 2000

plot.rwc_tlp.png <- df.emm.rwc_tlp |>
  arrange(species, campaign, year) |>
  mutate(
    group_y = case_when(species == "FASY" ~ 100, species == "FREX" ~ 65),
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group = f.reletter_cld(.group)
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = rwc_tlp_tot, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset),
    show.legend = T
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 0.2,
    position = position_dodge(width = plot_position_offset),
    show.legend = T
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = T
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  scale_color_oi(order = c(6, 2)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  lims(y = c(65, 100)) +
  labs(y = expression(RWC[tlp] ~ "[%]"), x = "Date", tag = "d)") +
  guides(color = guide_legend(title = "Species")) +
  facet_wrap(~year) +
  thesis_theme +
  theme(
    strip.background = element_blank(),
    legend.text = element_text(face = "italic"),
    axis.title.y = element_text(size = 8),
    plot.tag.position = c(0.00, 0.95),
  )
plot.rwc_tlp.png

ggsave(
  "figures/pv/plot.rwc_tlp.png",
  plot.rwc_tlp.png,
  width = 8,
  height = 4,
  dpi = 150
)

df.emm.rwc_tlp |>
  arrange(rwc_tlp_tot) |>
  select(c(species, year, campaign, emmean, rwc_tlp_tot)) |>
  mutate(rwc_tlp_tot = rwc_tlp_tot |> round(1)) |>
  View()

df.emm.rwc_tlp |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(rwc_tlp_tot) |> round(1),
    sd = sd(rwc_tlp_tot) |> round(1),
    emmean = emmean
  ) |>
  View()

extra_theme_pv <- theme(
  axis.title.x = element_blank(),
  axis.title.y = element_text(size = 8),
  # strip.text = element_blank(),
  plot.tag.position = c(0.00, 0.95),
)

plot.all_PV.png <-
  plot.psi_ft.png +
  extra_theme_pv +
  plot.psi_tlp.png +
  extra_theme_pv +
  plot.elast.png +
  extra_theme_pv +
  plot.rwc_tlp.png +
  plot_layout(heights = c(5, 5, 5, 5))
plot.all_PV.png

ggsave(
  "figures/pv/plot.all_PV.png",
  plot.all_PV.png,
  width = 16,
  height = 23,
  dpi = 150,
  unit = "cm"
)

## 1.2.5 export ------------------------------------------------------------------
lme.psi_ft |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F)

df.emm.psi_ft |>
  arrange(year, species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  write.table("clipboard", sep = "\t", row.names = F)

lme.psi_ft |>
  r2() |>
  as.data.frame() |>
  write.table("clipboard", sep = "\t", row.names = F)

lme.psi_tlp |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F)

df.emm.psi_tlp |>
  arrange(year, species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  write.table("clipboard", sep = "\t", row.names = F)

lme.psi_tlp |>
  r2() |>
  as.data.frame() |>
  write.table("clipboard", sep = "\t", row.names = F)

lme.elast |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F)

df.emm.elast |>
  arrange(year, species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  write.table("clipboard", sep = "\t", row.names = F)

lme.elast |>
  r2() |>
  as.data.frame() |>
  write.table("clipboard", sep = "\t", row.names = F)

lme.rwc_tlp |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F)

df.emm.rwc_tlp |>
  arrange(year, species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  write.table("clipboard", sep = "\t", row.names = F)

lme.elast |>
  r2() |>
  as.data.frame() |>
  write.table("clipboard", sep = "\t", row.names = F)

# 1.3. Gmin -----------------------------------------------------------------

## 1.3.1 prepare -------------------------------------------------------------

df.gmin_params <- read.csv("data/calculated_parameters/df.gmin_params.csv") |>
  mutate(
    sample_id = as.factor(sample_id),
    species = sample_id |> str_sub(1, 4) |> as.factor(),
    campaign = as.factor(campaign),
    year = as.factor(year),
    gmin = gmin_mean,
    date = date |>
      as.Date(format = "%d.%m.%Y")
  ) |>
  mutate(
    date = case_when(
      date == "2023-07-19" ~ "2023-07-17" |> as.Date(),
      date == "2023-08-11" ~ "2023-08-09" |> as.Date(),
      T ~ date
    )
  )

unique(df.gmin_params$date)

## 1.3.2 lme -----------------------------------------------------------------

glme.gmin <- glmmTMB::glmmTMB(
  gmin ~ species * campaign * year + (1 | sample_id),
  data = df.gmin_params |> filter(rwc_interval == "standard"),
  family = Gamma(link = "log")
)
# glm_test |> summary()
emm.glm <- emmeans(
  glme.gmin,
  ~ species * campaign * year,
  adjust = "mvt",
  type = "response"
)
emm.glm

df.gmin_params |>
  filter(rwc_interval == "standard") |>
  ggplot() +
  geom_point(aes(x = date, y = gmin, color = species)) +
  geom_line(aes(
    x = date,
    y = predict(glm_test, type = "response"),
    color = species,
    group = sample_id
  )) +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme


# lme.gmin_standard <- lmer(gmin ~ species * campaign * year + (1 | sample_id),
#                           data = df.gmin_params |> filter(rwc_interval == "standard"))
# lme.gmin_standard |> summary()
# lme.gmin_standard |> r2()

# lme.gmin_tlp <- lmer(gmin ~ species * campaign * year + (1 | sample_id),
#                      data = df.gmin_params |> filter(rwc_interval == "rwc_tlp_90"))
# lme.gmin_tlp |> summary()
# lme.gmin_tlp |> r2()
#
# df.gmin_params |> filter(rwc_interval == "rwc_tlp_90") |>
#   ggplot() +
#   geom_point(aes(x = date, y = gmin, color = species)) +
#   geom_line(aes(x = date, y = predict(lme.gmin_tlp), color = species, group = sample_id)) +
#   facet_wrap(~ year, scales = "free_x") +
#   thesis_theme

## 1.3.3 emmeans -----------------------------------------------------------------

emmeans_gmin_standard <- emmeans(
  glme.gmin,
  ~ species * campaign * year,
  adjust = "mvt",
  type = "response"
)

df.emm.gmin_standard <- emmeans_gmin_standard |>
  multcomp::cld(Letters = letters) |>
  as.data.frame()

# emmeans_gmin_tlp <- emmeans(lme.gmin_tlp, ~ species * campaign * year,
#                             adjust = "mvt",
#                             type = "response")
#
# df.emm.gmin_tlp <- emmeans_gmin_tlp |>
#   multcomp::cld(Letters = letters) |>
#   as.data.frame()

## 1.3.4 plot ----------------------------------------------------------------

df.emm.gmin_standard <- df.emm.gmin_standard |>
  left_join(
    df.gmin_params |>
      filter(rwc_interval == "standard") |>
      dplyr::select(species, campaign, year, date, gmin),
    by = c("species", "campaign", "year")
  ) |>
  mutate(date_plot = date)
year(df.emm.gmin_standard$date_plot) = 2000

plot.gmin_standard.png <- df.emm.gmin_standard |>
  arrange(species, campaign, year) |>
  mutate(
    group_y = case_when(species == "FASY" ~ 24, species == "FREX" ~ 0),
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group = f.reletter_cld(.group)
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = gmin, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_line(
    aes(x = date_plot, y = response, col = species),
    alpha = 0.2,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  scale_color_oi(order = c(6, 2)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  labs(
    y = expression(
      g[min] ~ "[" * mmol ~ m^{
        -2
      } ~ s^{
        -1
      } *
        "]"
    ),
    x = "Date"
  ) +
  lims(y = c(0, 24)) +
  guides(color = guide_legend(title = "Species")) +
  facet_wrap(~year) +
  thesis_theme +
  theme(
    strip.background = element_blank(),
    legend.text = element_text(face = "italic")
  )

plot.gmin_standard.png
ggsave(
  "figures/gmin/plot.gmin_standard.png",
  plot.gmin_standard.png,
  width = 8,
  height = 4,
  dpi = 300
)

df.emm.gmin_standard |>
  arrange(gmin) |>
  dplyr::select(c(species, year, campaign, response, gmin)) |>
  mutate(gmin = gmin |> round(1)) |>
  View()

df.emm.gmin_standard |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(gmin) |> round(1),
    sd = sd(gmin) |> round(1),
    cv = sd(gmin) / mean(gmin) * 100 |> round(1),
    emmean = response
  ) |>
  View()


## 1.3.5 export ------------------------------------------------------------------

df.emm.gmin_standard |>
  arrange(year, species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  select(c(
    species,
    campaign,
    year,
    response,
    SE,
    df,
    asymp.LCL,
    asymp.UCL,
    .group
  )) |>
  distinct() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

glme.gmin |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

# 1.4. Optical vulnerability -----------------------------------------------------

## 1.4.1 prepare ------------------------------------------------------------

df.ov_params <- read_csv("data/calculated_parameters/df.ov_params.csv") |>
  mutate(
    date = as.Date(date, format = "%d.%m.%Y"),
    sample_id = as.factor(sample_id),
    species = sample_id |> str_sub(1, 4) |> as.factor(),
    campaign = as.factor(campaign),
    year = as.factor(year),
    p12 = p12_spline,
    p50 = p50_spline,
    p88 = p88_spline
  ) |>
  filter(year != 2023 | species != "FREX" | !campaign %in% c(1, 2, 3)) |>
  filter(vessel_order == "major")

## 1.4.2 lme -----------------------------------------------------------------

lme.p12 <- lmer(
  p12 ~ species * campaign * year + (1 | sample_id),
  data = df.ov_params
)

lme.p50 <- lmer(
  p50 ~ species * campaign * year + (1 | sample_id),
  data = df.ov_params
)

lme.p88 <- lmer(
  p88 ~ species * campaign * year + (1 | sample_id),
  data = df.ov_params
)

## 4.3 emmeans ----

emmeans_p12 <- emmeans(
  lme.p12,
  ~ species * campaign * year,
  adjust = "bonferroni",
  type = "response"
)


emmeans_p50 <- emmeans(
  lme.p50,
  ~ species * campaign * year,
  adjust = "bonferroni",
  type = "response"
)


emmeans_p88 <- emmeans(
  lme.p88,
  ~ species * campaign * year,
  adjust = "bonferroni",
  type = "response"
)


## 4.4 plot --------------------------------------------------------------

df.emm.p12 <- emmeans_p12 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  left_join(
    df.ov_params |>
      dplyr::select(species, campaign, year, date, p12),
    by = c("species", "campaign", "year")
  ) |>
  arrange(species, campaign, year) |>
  mutate(
    species = dplyr::recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group = .group |> f.reletter_cld(),
    date_plot = date,
    group_y = case_when(
      species == "Fagus sylvatica" ~ 0,
      species == "Fraxinus excelsior" ~ -6
    )
  )

year(df.emm.p12$date_plot) = 2000


plot.p12.png <- df.emm.p12 |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = p12, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 0.2,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = F
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  scale_color_oi(order = c(6, 2)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  labs(y = expression(P[12] ~ "[MPa]"), x = NULL, tag = "a)") +
  lims(y = c(-6, 0)) +
  guides(color = guide_legend(title = "Species")) +
  facet_wrap(~year) +
  thesis_theme +
  theme(strip.background = element_blank()) +
  theme(
    plot.tag.position = c(0.03, 0.95),
    legend.text = element_text(size = 8, face = "italic"),
    legend.justification = c(0.25, 0)
  )
plot.p12.png

ggsave(
  "figures/ov/plot.p12.png",
  plot.p12.png,
  width = 8,
  height = 4,
  dpi = 150
)

df.emm.p12 |>
  arrange(p12) |>
  select(c(species, year, campaign, emmean, p12)) |>
  mutate(p12 = p12 |> round(1)) |>
  View()

df.emm.p12 |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(p12) |> round(1),
    sd = sd(p12) |> round(1),
    emmean = emmean
  ) |>
  View()


df.emm.p50 <- emmeans_p50 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  left_join(
    df.ov_params |>
      dplyr::select(species, campaign, year, date, p50),
    by = c("species", "campaign", "year")
  ) |>
  arrange(species, campaign, year) |>
  mutate(
    species = dplyr::recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group = .group |> f.reletter_cld(),
    date_plot = date,
    group_y = case_when(
      species == "Fagus sylvatica" ~ 0,
      species == "Fraxinus excelsior" ~ -6.5
    )
  )

year(df.emm.p50$date_plot) = 2000

plot.p50.png <- df.emm.p50 |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = p50, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 0.2,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = F
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  scale_color_oi(order = c(6, 2)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  labs(y = expression(P[50] ~ "[MPa]"), x = NULL, tag = "b)") +
  lims(y = c(-6.5, 0)) +
  guides(color = guide_legend(title = "Species")) +
  facet_wrap(~year) +
  thesis_theme +
  theme(strip.background = element_blank()) +
  theme(
    plot.tag.position = c(0.03, 0.95),
    legend.text = element_text(size = 8, face = "italic"),
    legend.justification = c(0.25, 0)
  )
plot.p50.png

ggsave(
  "figures/ov/plot.p50.png",
  plot.p50.png,
  width = 8,
  height = 4,
  dpi = 150
)

df.emm.p50 |>
  arrange(p50) |>
  select(c(species, year, campaign, emmean, p50)) |>
  mutate(p50 = p50 |> round(1)) |>
  View()

df.emm.p50 |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(p50) |> round(1),
    sd = sd(p50) |> round(1),
    emmean = emmean
  ) |>
  View()

df.emm.p88 <- emmeans_p88 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  left_join(
    df.ov_params |>
      dplyr::select(species, campaign, year, date, p88),
    by = c("species", "campaign", "year")
  ) |>
  arrange(species, campaign, year) |>
  mutate(
    species = dplyr::recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group = .group |> f.reletter_cld(),
    date_plot = date,
    group_y = case_when(
      species == "Fagus sylvatica" ~ 0,
      species == "Fraxinus excelsior" ~ -10
    )
  )

year(df.emm.p88$date_plot) = 2000

plot.p88.png <- df.emm.p88 |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = p88, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 0.2,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  scale_color_oi(order = c(6, 2)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  labs(y = expression(P[88] ~ "[MPa]"), x = "Date", tag = "c)") +
  lims(y = c(-10, 0)) +
  guides(color = guide_legend(title = "Species")) +
  facet_wrap(~year) +
  thesis_theme +
  theme(strip.background = element_blank()) +
  theme(
    plot.tag.position = c(0.03, 0.95),
    legend.text = element_text(size = 8, face = "italic"),
    legend.justification = c(0.25, 0)
  )
plot.p88.png

ggsave(
  "figures/ov/plot.p88.png",
  plot.p88.png,
  width = 8,
  height = 4,
  dpi = 150
)

df.emm.p88 |>
  arrange(p88) |>
  select(c(species, year, campaign, emmean, p88)) |>
  mutate(p88 = p88 |> round(1)) |>
  View()

df.emm.p88 |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(p88) |> round(1),
    sd = sd(p88) |> round(1),
    emmean = emmean
  ) |>
  View()

plot.p12p50p88_lme.png <-
  plot.p12.png +
  plot.p50.png +
  plot.p88.png +
  plot_layout(heights = c(6, 6, 6))

plot.p12p50p88_lme.png
ggsave(
  "figures/ov/plot.p12p50p88_lme.png",
  plot.p12p50p88_lme.png,
  width = 16,
  height = 18,
  dpi = 150,
  units = "cm"
)


## 4.5 export ------------------------------------------------------------------

df.emm.p12 |>
  arrange(year, species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  select(c(
    species,
    campaign,
    year,
    emmean,
    SE,
    df,
    lower.CL,
    upper.CL,
    .group
  )) |>
  distinct() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

lme.p12 |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

df.emm.p50 |>
  arrange(year, species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  select(c(
    species,
    campaign,
    year,
    emmean,
    SE,
    df,
    lower.CL,
    upper.CL,
    .group
  )) |>
  distinct() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

lme.p50 |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

df.emm.p88 |>
  arrange(year, species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  select(c(
    species,
    campaign,
    year,
    emmean,
    SE,
    df,
    lower.CL,
    upper.CL,
    .group
  )) |>
  distinct() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

lme.p50 |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

# 1.5. Thermal sensitivity --------------------------------------------------

## 1.5.1 prepare -----------------------------------------------------------

{
  df.tsm_params <- read.csv("data/calculated_parameters/df.tsm_params.csv") |>
    mutate(
      date = as.Date(date),
      sample_id = as.factor(sample_id),
      species = sample_id |> str_sub(1, 4) |> as.factor(),
      campaign = as.factor(campaign),
      year = as.factor(year)
    )
  year(df.tsm_params$date) = 2024
}

## 1.5.2 lme ----------------------------------------------------------------

lme.t5 <- lmer(
  estimate ~ species * campaign + (1 | sample_id),
  data = df.tsm_params |> filter(trait == "T5")
)

lme.t50 <- lmer(
  estimate ~ species * campaign + (1 | sample_id),
  data = df.tsm_params |> filter(trait == "T50")
)

lme.t95 <- lmer(
  estimate ~ species * campaign + (1 | sample_id),
  data = df.tsm_params |> filter(trait == "T95")
)


## 1.5.3 emmeans ----------------------------------------------------------------

emmeans_t5 <- emmeans(
  lme.t5,
  ~ species * campaign,
  adjust = "mvt",
  type = "response"
)

emmeans_t50 <- emmeans(
  lme.t50,
  ~ species * campaign,
  adjust = "mvt",
  type = "response"
)

emmeans_t95 <- emmeans(
  lme.t95,
  ~ species * campaign,
  adjust = "mvt",
  type = "response"
)

## 1.5.4 plot ----------------------------------------------------------------

df.emm.t5 <- emmeans_t5 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  mutate(trait = "T5")

df.emm.t50 <- emmeans_t50 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  mutate(trait = "T50")

df.emm.t95 <- emmeans_t95 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  mutate(trait = "T95")

df.emm.tsm <- bind_rows(df.emm.t5, df.emm.t50, df.emm.t95)

df.emm.tsm <- df.emm.tsm |>
  left_join(
    df.tsm_params |>
      dplyr::select(species, sample_id, campaign, year, date, estimate, trait),
    by = c("species", "campaign", "trait")
  ) |>
  mutate(
    species = dplyr::recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    date_plot = as.Date(date),
    group_y = case_when(
      species == "Fagus sylvatica" ~ 65,
      species == "Fraxinus excelsior" ~ 30
    ),
    plot_number = case_when(
      trait == "T5" ~ "a)",
      trait == "T50" ~ "b)",
      trait == "T95" ~ "c)"
    ),
    trait = case_when(
      trait == "T5" ~ "T[5]",
      trait == "T50" ~ "T[50]",
      trait == "T95" ~ "T[95]"
    ),
  )
year(df.emm.tsm$date_plot) = 2000

plot.tsm.png <- df.emm.tsm |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = estimate, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = .group, col = species),
    show.legend = F
  ) +
  geom_text(
    aes(x = as.Date("2000-05-18"), y = 60, label = plot_number),
    size = 5
  ) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  labs(y = "Temperature [\u00b0C]", x = "Date") +
  lims(y = c(30, 65)) +
  guides(color = guide_legend(title = "Species")) +
  scale_color_oi(order = c(6, 2)) +
  facet_grid(
    cols = vars(trait),
    scales = "free_x",
    space = "free_x",
    labeller = label_parsed
  ) + # Ensure columns are faceted by hsm_type
  thesis_theme +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 15),
    legend.text = element_text(face = "italic")
  )
plot.tsm.png

ggsave(
  "figures/tsm/plot.tsm.png",
  plot.tsm.png,
  width = 10,
  height = 4,
  dpi = 150
)


plot.t5.png <- df.emm.t5 |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = estimate, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = .group, col = species),
    show.legend = F
  ) +
  scale_color_manual(
    values = c("Fagus sylvatica" = "#410866", "Fraxinus excelsior" = "#faad22")
  ) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  scale_y_continuous(limits = c(30, 60), breaks = seq(30, 60, 5)) +
  labs(y = "T5 [\u00b0C]", x = "Date") +
  guides(color = guide_legend(title = "Species")) +
  thesis_theme +
  theme(strip.background = element_blank())
plot.t5.png

ggsave("figures/tsm/plot.t5.png", plot.t5.png, width = 6, height = 4, dpi = 150)

df.emm.t50 <- emmeans_t50 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  left_join(
    df.tsm_params |>
      filter(trait == "T50") |>
      dplyr::select(species, campaign, year, date, estimate),
    by = c("species", "campaign")
  ) |>
  mutate(
    species = dplyr::recode(
      species,
      "beech" = "Fagus sylvatica",
      "ash" = "Fraxinus excelsior"
    ),
    date_plot = as.Date(date),
    group_y = case_when(
      species == "Fagus sylvatica" ~ 60,
      species == "Fraxinus excelsior" ~ 30
    )
  )
year(df.emm.t50$date_plot) = 2000

plot.t50.png <- df.emm.t50 |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = estimate, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = .group, col = species),
    show.legend = F
  ) +
  scale_color_manual(
    values = c("Fagus sylvatica" = "#410866", "Fraxinus excelsior" = "#faad22")
  ) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  scale_y_continuous(limits = c(30, 60), breaks = seq(30, 65, 5)) +
  labs(y = "T50 [\u00b0C]", x = "Date") +
  guides(color = guide_legend(title = "Species")) +
  thesis_theme +
  theme(strip.background = element_blank())
plot.t50.png

ggsave(
  "figures/tsm/plot.t50.png",
  plot.t50.png,
  width = 6,
  height = 4,
  dpi = 150
)

df.emm.t50

df.emm.t95 <- emmeans_t95 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  left_join(
    df.tsm_params |>
      filter(trait == "T95") |>
      dplyr::select(species, campaign, year, date, estimate),
    by = c("species", "campaign")
  ) |>
  mutate(
    species = dplyr::recode(
      species,
      "beech" = "Fagus sylvatica",
      "ash" = "Fraxinus excelsior"
    ),
    date_plot = as.Date(date),
    group_y = case_when(
      species == "Fagus sylvatica" ~ 65,
      species == "Fraxinus excelsior" ~ 30
    )
  )

year(df.emm.t95$date_plot) = 2000

plot.t95.png <- df.emm.t95 |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = estimate, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = .group, col = species),
    show.legend = F
  ) +
  scale_color_manual(
    values = c("Fagus sylvatica" = "#410866", "Fraxinus excelsior" = "#faad22")
  ) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  scale_y_continuous(limits = c(30, 65), breaks = seq(30, 65, 5)) +
  labs(y = "T95 [\u00b0C]", x = "Date") +
  guides(color = guide_legend(title = "Species")) +
  thesis_theme +
  theme(strip.background = element_blank())
plot.t95.png

ggsave(
  "figures/tsm/plot.t95.png",
  plot.t95.png,
  width = 6,
  height = 4,
  dpi = 150
)


df.emm.tsm |>
  group_by(trait, species, year, campaign) |>
  summarise(
    mean = mean(estimate) |> round(1),
    sd = sd(estimate) |> round(1),
    emmean = emmean
  ) |>
  View()

## 1.5.5 export ------------------------------------------------------------------

df.emm.t5 |>
  arrange(species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  dplyr::select(c(
    species,
    campaign,
    emmean,
    SE,
    df,
    lower.CL,
    upper.CL,
    .group
  )) |>
  distinct() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

lme.t5 |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

df.emm.t50 |>
  arrange(species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  dplyr::select(c(
    species,
    campaign,
    emmean,
    SE,
    df,
    lower.CL,
    upper.CL,
    .group
  )) |>
  distinct() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

lme.t50 |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

df.emm.t95 |>
  arrange(species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  dplyr::select(c(
    species,
    campaign,
    emmean,
    SE,
    df,
    lower.CL,
    upper.CL,
    .group
  )) |>
  distinct() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

lme.t95 |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

# 1.6. Hydraulic safety margins ---------------------------------------------

## 1.6.1 prepare ------------------------------------------------------------

df.hsm <- df.psi_midday |>
  dplyr::left_join(
    df.pv_params |> dplyr::select(campaign, year, sample_id, psi_tlp),
    by = c("sample_id", "campaign", "year")
  ) |>
  dplyr::left_join(
    df.ov_params |> dplyr::select(campaign, year, sample_id, p12, p50, p88),
    by = c("sample_id", "campaign", "year")
  )

df.hsm <- df.hsm |>
  mutate(
    hsm_tlp = psi_midday_mpa - psi_tlp,
    hsm_p12 = psi_midday_mpa - p12,
    hsm_p50 = psi_midday_mpa - p50,
    hsm_p88 = psi_midday_mpa - p88,
    date_plot = date
  ) |>
  pivot_longer(
    cols = c(hsm_tlp, hsm_p50, hsm_p12, hsm_p88),
    names_to = "hsm_type",
    values_to = "hsm_mpa"
  )
year(df.hsm$date_plot) = 2000


## 1.6.2 lme -----------------------------------------------------------------

lme.hsm_tlp <- lmer(
  hsm_mpa ~ species * campaign * year + (1 | sample_id),
  data = df.hsm |> filter(hsm_type == "hsm_tlp")
)

lme.hsm_p12 <- lmer(
  hsm_mpa ~ species * campaign * year + (1 | sample_id),
  data = df.hsm |> filter(hsm_type == "hsm_p12")
)

lme.hsm_p50 <- lmer(
  hsm_mpa ~ species * campaign * year + (1 | sample_id),
  data = df.hsm |> filter(hsm_type == "hsm_p50")
)

lme.hsm_p88 <- lmer(
  hsm_mpa ~ species * campaign * year + (1 | sample_id),
  data = df.hsm |> filter(hsm_type == "hsm_p88")
)


## 1.6.3 emmeans -----------------------------------------------------------------

emmeans_hsm_tlp <- emmeans(
  lme.hsm_tlp,
  ~ species * campaign * year,
  adjust = "bonferroni",
  type = "response"
)

emmeans_hsm_p12 <- emmeans(
  lme.hsm_p12,
  ~ species * campaign * year,
  adjust = "bonferroni",
  type = "response"
)

emmeans_hsm_p50 <- emmeans(
  lme.hsm_p50,
  ~ species * campaign * year,
  adjust = "bonferroni",
  type = "response"
)

emmeans_hsm_p88 <- emmeans(
  lme.hsm_p88,
  ~ species * campaign * year,
  adjust = "bonferroni",
  type = "response"
)

df.emm.hsm_tlp <- emmeans_hsm_tlp |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  mutate(hsm_type = "hsm_tlp")

df.emm.hsm_p12 <- emmeans_hsm_p12 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  mutate(hsm_type = "hsm_p12")

df.emm.hsm_p50 <- emmeans_hsm_p50 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  mutate(hsm_type = "hsm_p50")

df.emm.hsm_p88 <- emmeans_hsm_p88 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  mutate(hsm_type = "hsm_p88")

df.emm.hsm <- bind_rows(
  df.emm.hsm_tlp,
  df.emm.hsm_p12,
  df.emm.hsm_p50,
  df.emm.hsm_p88
)

df.hsm_emm <- left_join(
  df.hsm,
  df.emm.hsm,
  by = c("species", "campaign", "year", "hsm_type")
)


## 1.6.4 plot ----------------------------------------------------------------

df.hsm_emm <- df.hsm_emm |>
  mutate(
    species = dplyr::recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    hsm_type = case_when(
      hsm_type == "hsm_p12" ~ "P[12]",
      hsm_type == "hsm_p50" ~ "P[50]",
      hsm_type == "hsm_p88" ~ "P[88]",
      hsm_type == "hsm_tlp" ~ "psi[tlp]"
    ) |>
      factor(levels = c("psi[tlp]", "P[12]", "P[50]", "P[88]")), # Explicit order
    group_y = case_when(
      species == "Fagus sylvatica" ~ 6.5,
      species == "Fraxinus excelsior" ~ -1.5
    ),
    plot_number = case_when(
      hsm_type == "P[12]" & year == 2023 ~ "c)",
      hsm_type == "P[12]" & year == 2024 ~ "d)",
      hsm_type == "P[50]" & year == 2023 ~ "e)",
      hsm_type == "P[50]" & year == 2024 ~ "f)",
      hsm_type == "P[88]" & year == 2023 ~ "g)",
      hsm_type == "P[88]" & year == 2024 ~ "h)",
      hsm_type == "psi[tlp]" & year == 2023 ~ "a)",
      hsm_type == "psi[tlp]" & year == 2024 ~ "b)"
    )
  )

plot.hsm.png <- df.hsm_emm |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = hsm_mpa, col = species),
    position = position_dodge(width = 5),
    size = 1.5,
    alpha = plot_alpha
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    position = position_dodge(width = 5),
    alpha = plot_alpha,
    show.legend = FALSE
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    position = position_dodge(width = 5),
    width = 3,
    alpha = 0.4,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = .group, col = species),
    show.legend = F
  ) +
  geom_text(
    aes(x = as.Date("2000-06-01"), y = 6.5, label = plot_number),
    show.legend = F
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Hydraulic safety margin [MPa]", x = "Date") +
  guides(color = guide_legend(title = "Species")) +
  scale_y_continuous(limits = c(-1.5, 6.5), breaks = seq(-1, 6, 1)) +
  scale_color_oi(order = c(6, 2)) +
  facet_grid(
    cols = vars(year),
    rows = vars(hsm_type),
    scales = "free_x",
    space = "free_x",
    labeller = label_parsed
  ) + # Ensure columns are faceted by hsm_type
  thesis_theme +
  theme(legend.text = element_text(face = "italic"))
plot.hsm.png

ggsave(
  "figures/hsm/plot.hsm.png",
  plot.hsm.png,
  width = 8,
  height = 10,
  dpi = 150
)

df.hsm_emm |>
  group_by(year, species, campaign, hsm_type) |>
  summarise(
    mean = mean(hsm_mpa, na.rm = TRUE) |> round(1),
    sd = sd(hsm_mpa, na.rm = TRUE) |> round(1)
  ) |>
  View()

# 1.7. Sugars ------------------------------------------------------------------

df.sugars_summ <- read.csv(
  "data/calculated_parameters/df_sugars_2023_2024.csv"
) |>
  mutate(
    date = as.Date(date),
    species = as.factor(species),
    sample_id = as.factor(sample_id),
    campaign = as.factor(campaign),
    date_fac = as.factor(date)
  ) %>%
  pivot_wider(names_from = sugar_name, values_from = sugar_conc) %>%
  mutate(
    starch_sugar_ratio = starch_mg_g /
      (fructose_mg_g + glucose_mg_g + sucrose_mg_g)
  ) %>%
  pivot_longer(
    cols = fructose_mg_g:starch_sugar_ratio,
    names_to = "sugar_name",
    values_to = "sugar_conc"
  )

## models ------------------------------------------------------------------

df.sugars_nest <- df.sugars_summ |>
  nest(data = -c(sugar_name)) %>%
  filter(sugar_name %>% str_detect("mg\\_g|ratio"))

library(nlme)

df.sugars_nest <- df.sugars_nest |>
  mutate(
    lme = map(
      data,
      ~ glmmTMB::glmmTMB(
        sugar_conc ~ species * date_fac + (1 | sample_id),
        # ar1(campaign + 0 | species/sample_id),
        family = Gamma(link = "log"),
        data = .x
      )
    )

    # mutate(lme = map(
    #   data,
    #   ~ lme4::lmer(sugar_conc ~ species * campaign * year + (1 | sample_id),
    #          data = .x)
    # )

    # mutate(lme = map(
    #   data,
    #   ~ nlme::lme(sugar_conc ~ species * date_fac,
    #               random = ~ 1 | sample_id,
    #               data = .x)
    # )
  )

df.sugars_nest$lme[[6]] |> summary()
df.sugars_nest$lme[[1]] |> performance::r2()

emmeans::ref_grid(df.sugars_nest$lme[[1]])

df.sugars_nest <- df.sugars_nest |>
  mutate(
    emmeans = map(
      lme,
      ~ emmeans::emmeans(
        .x,
        ~ species * date_fac,
        lmer.df = "asymptotic",
        adjust = "mvt",
        type = "response"
      )
    )
  )

df.sugars_nest$emmeans[[1]]

df.sugars_nest <- df.sugars_nest |>
  mutate(
    emmgrid = map(
      emmeans,
      ~ .x |>
        multcomp::cld(Letters = letters) |>
        as.data.frame()
    )
  )

# multcomp::cld(Letters = letters, df.sugars_nest$emmeans[[1]])

# df.sugars_nest$emmgrid[[1]]

df.sugars_emm <- df.sugars_summ |>
  left_join(
    df.sugars_nest |>
      dplyr::select(-c(data, lme, emmeans)) |>
      unnest(emmgrid),
    by = c("sugar_name", "species", "date_fac")
  ) |>
  mutate(
    date_plot = as.Date(date),
    year = date |> year()
  )
year(df.sugars_emm$date_plot) <- 2000

## plots -------------------------------------------------------------------

df.sugars_emm$sugar_name |> unique()

### glucose ----

## update with gamma dist

plot.glucose <- df.sugars_emm |>
  filter(sugar_name == "glucose_mg_g") |>
  arrange(species, campaign) |>
  mutate(
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group_y = case_when(
      species == "Fagus sylvatica" ~ 70,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = sugar_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = FALSE
  ) +
  geom_line(
    aes(x = date_plot, y = response, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  # geom_errorbar(aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species), width = 3,
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  # scale_y_continuous(limits = c(0, 70), breaks = seq(0,70,10)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(
    y = expression(
      "Glucose [mg" ~ g^{
        -1
      } *
        "]"
    ),
    x = "Date",
    tag = "c)"
  ) +
  facet_wrap(~year) +
  thesis_theme

plot.glucose

# ggsave("figures/sugars/glucose.png", plot.glucose, width = 8, height = 6, dpi = 300, unit = "cm")

### starch ----

## update with gamma dist
plot.starch <- df.sugars_emm |>
  filter(sugar_name == "starch_mg_g") |>
  arrange(species, campaign) |>
  mutate(
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group_y = case_when(
      species == "Fagus sylvatica" ~ 135,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = sugar_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = FALSE
  ) +
  geom_line(
    aes(x = date_plot, y = response, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, 20)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(
    y = expression(
      "Starch [mg" ~ g^{
        -1
      } *
        "]"
    ),
    x = "Date",
    tag = "d)"
  ) +
  facet_wrap(~year) +
  thesis_theme

plot.starch

# ggsave("figures/sugars/starch.png", plot.starch, width = 8, height = 8.2, dpi = 300, unit = "cm")

### sucrose ----

plot.sucrose <- df.sugars_emm |>
  filter(sugar_name == "sucrose_mg_g") |>
  arrange(species, campaign) |>
  mutate(
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group_y = case_when(
      species == "Fagus sylvatica" ~ 85,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = sugar_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = T
  ) +
  geom_line(
    aes(x = date_plot, y = response, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = T
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  scale_y_continuous(limits = c(0, 85), breaks = seq(0, 85, 10)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(
    y = expression(
      "Sucrose [mg" ~ g^{
        -1
      } *
        "]"
    ),
    x = "Date",
    tag = "e)"
  ) +
  facet_wrap(~year) +
  thesis_theme

plot.sucrose

# ggsave("figures/sugars/sucrose.png", plot.sucrose, width = 8, height = 6, dpi = 300, unit = "cm")

### fructose ----

## update with gamma dist
plot.fructose <- df.sugars_emm |>
  filter(sugar_name == "fructose_mg_g") |>
  arrange(species, campaign) |>
  mutate(
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group_y = case_when(
      species == "Fagus sylvatica" ~ 40,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = sugar_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = FALSE
  ) +
  geom_line(
    aes(x = date_plot, y = response, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 70, 10)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(
    y = expression(
      "Fructose [mg" ~ g^{
        -1
      } *
        "]"
    ),
    x = "Date",
    tag = "b)"
  ) +
  facet_wrap(~year) +
  thesis_theme

plot.fructose

# ggsave("figures/sugars/fructose.png", plot.fructose, width = 8, height = 6, dpi = 300, unit = "cm")

### total sugars ----
plot.total_sugars <- df.sugars_emm |>
  filter(sugar_name == "total_sugars_mg_g") |>
  arrange(species, campaign) |>
  mutate(
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group_y = case_when(
      species == "Fagus sylvatica" ~ 120,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = sugar_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_line(
    aes(x = date_plot, y = response, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = F
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = F
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  # scale_y_continuous(limits = c(0, 70), breaks = seq(0,70,10)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(
    y = expression(
      "Total sugars [mg" ~ g^{
        -1
      } *
        "]"
    ),
    x = "Date",
    tag = "a)"
  ) +
  facet_wrap(~year) +
  thesis_theme

plot.total_sugars

### total sugars ----
plot.starch_sugar_ratio <- df.sugars_emm |>
  filter(sugar_name == "starch_sugar_ratio") |>
  arrange(species, campaign) |>
  mutate(
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group_y = case_when(
      species == "Fagus sylvatica" ~ 2.5,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = sugar_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_line(
    aes(x = date_plot, y = response, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = F
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = F
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  # scale_y_continuous(limits = c(0, 70), breaks = seq(0,70,10)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(
    y = expression(
      "Starch to sugar ratio [mg" ~ mg^{
        -1
      } *
        "]"
    ),
    x = "Date",
    tag = "f)"
  ) +
  facet_wrap(~year) +
  thesis_theme

plot.starch_sugar_ratio

# ggsave("figures/sugars/total_sugars.png", plot.total_sugars, width = 8, height = 6, dpi = 300, unit = "cm")

df.sugars_emm |>
  filter(sugar_name == "total_sugars_mg_g") |>
  arrange(species, campaign) |>
  dplyr::select(c(species, year, campaign, response, sugar_conc)) |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(sugar_conc) |> round(1),
    sd = sd(sugar_conc) |> round(1)
  )
df.sugars_emm |>
  filter(sugar_name == "sucrose_mg_g") |>
  arrange(species, campaign) |>
  dplyr::select(c(species, year, campaign, response, sugar_conc)) |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(sugar_conc) |> round(1),
    sd = sd(sugar_conc) |> round(1)
  )


## plot all ----------------------------------------------------------------

extra_theme <- theme(
  axis.title.x = element_blank(),
  plot.tag.position = c(0.03, 0.95),
  legend.text = element_text(size = 8, face = "italic"),
  legend.justification = c(0.25, 0)
)

plots.all_nsc.png <- plot.total_sugars +
  extra_theme +
  plot_spacer() +
  plot.fructose +
  extra_theme +
  # plot_spacer() + plot_spacer() + plot_spacer() +
  plot.glucose +
  extra_theme +
  plot_spacer() +
  plot.starch +
  extra_theme +
  # plot_spacer() + plot_spacer() + plot_spacer() +
  plot.sucrose +
  theme(
    plot.tag.position = c(0.03, 0.95),
    legend.text = element_text(size = 8, face = "italic"),
    legend.justification = c(0.25, 0)
  ) +
  plot_layout(widths = c(8, 0, 8), heights = c(7, 7, 7))
plots.all_nsc.png
ggsave(
  "figures/sugars/all_nsc_both_years.png",
  plots.all_nsc.png,
  width = 30,
  height = 20,
  dpi = 150,
  unit = "cm"
)

# 1.8. Nutrients ---------------------------------------------------------------

df.nutrients_summ <- read.csv(
  "data/calculated_parameters/df.nutrients_2023_2024.csv"
) |>
  mutate(
    date = as.Date(date),
    species = as.factor(species),
    species = relevel(species, ref = "FASY"),
    sample_id = as.factor(sample_id),
    campaign = as.factor(campaign)
  )

## models ------------------------------------------------------------------

df.nutrients_nest <- df.nutrients_summ |>
  nest(data = -c(nutrient_name))

library(nlme)

df.nutrients_nest <- df.nutrients_nest |>
  mutate(
    lme = map(
      data,
      ~ nlme::lme(
        nutrient_conc ~ date * species,
        random = ~ date | species / sample_id,
        correlation = corCAR1(form = ~ date | species / sample_id),
        data = .x
      )
    )
  )

df.nutrients_nest$lme[[1]] |> summary()
df.nutrients_nest$lme[[1]] |> performance::r2()

emmeans::ref_grid(df.nutrients_nest$lme[[1]])

df.nutrients_nest <- df.nutrients_nest |>
  mutate(
    emmeans = map(
      lme,
      ~ emmeans::emmeans(
        .x,
        ~ species * campaign,
        adjust = "mvt",
        type = "response"
      )
    )
  )

df.nutrients_nest$emmeans[[1]]

df.nutrients_nest <- df.nutrients_nest |>
  mutate(
    emmgrid = map(
      emmeans,
      ~ .x |>
        multcomp::cld(Letters = letters) |>
        as.data.frame()
    )
  )

multcomp::cld(Letters = letters, df.nutrients_nest$emmeans[[1]])

df.nutrients_nest$emmgrid[[1]]

df.nutrients_emm <- df.nutrients_summ |>
  left_join(
    df.nutrients_nest |>
      select(-c(data, lme, emmeans)) |>
      unnest(emmgrid),
    by = c("nutrient_name", "species", "campaign")
  )


## plots -------------------------------------------------------------------

df.nutrients_emm <- df.nutrients_emm |>
  group_by(nutrient_name) |>
  mutate(
    min = min(nutrient_conc),
    max = max(nutrient_conc),
    ylim_upr = case_when(
      nutrient_name == "c_con_percent" ~ 50,
      nutrient_name == "n_con_percent" ~ 4,
      nutrient_name == "p_con_mg_kg" ~ 1800,
      nutrient_name == "k_con_mg_kg" ~ 16000,
      nutrient_name == "s_con_mg_kg" ~ 3000,
      nutrient_name == "ca_con_mg_kg" ~ 26000,
      nutrient_name == "mg_con_mg_kg" ~ 3200
    ),
    ylim_lwr = case_when(
      nutrient_name == "c_con_percent" ~ 40,
      nutrient_name == "n_con_percent" ~ 0,
      nutrient_name == "p_con_mg_kg" ~ 0,
      nutrient_name == "k_con_mg_kg" ~ 0,
      nutrient_name == "s_con_mg_kg" ~ 0,
      nutrient_name == "ca_con_mg_kg" ~ 0,
      nutrient_name == "mg_con_mg_kg" ~ 0
    ),
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    )
  )

library(see)

### Carbon ----
plot.c_con_percent <- df.nutrients_emm |>
  filter(nutrient_name == "c_con_percent") |>
  arrange(species, campaign) |>
  mutate(
    group_y = case_when(
      species == "Fagus sylvatica" ~ 52,
      species == "Fraxinus excelsior" ~ 42
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date, y = nutrient_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = FALSE
  ) +
  geom_line(
    aes(x = date, y = emmean, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_errorbar(
    aes(x = date, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  annotate(
    "text",
    x = as.Date("2023-06-01"),
    y = 52,
    label = "a)",
    size = 5,
    vjust = 0.5,
    hjust = 0
  ) +
  scale_y_continuous(limits = c(42, 52), breaks = seq(42, 52, 2)) +
  scale_x_date(limits = c(as.Date("2023-06-01"), as.Date("2023-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(y = "Carbon [%]", x = "Date") +
  thesis_theme

plot.c_con_percent

ggsave(
  "figures/nutrients/c_con_percent.png",
  plot.c_con_percent,
  width = 8,
  height = 6,
  dpi = 150,
  unit = "cm"
)

### Nitrogen ----
plot.n_con_percent <- df.nutrients_emm |>
  filter(nutrient_name == "n_con_percent") |>
  arrange(species, campaign) |>
  mutate(
    group_y = case_when(
      species == "Fagus sylvatica" ~ 4,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date, y = nutrient_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = FALSE
  ) +
  geom_line(
    aes(x = date, y = emmean, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_errorbar(
    aes(x = date, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  annotate(
    "text",
    x = as.Date("2023-06-01"),
    y = 4,
    label = "b)",
    size = 5,
    vjust = 0.5,
    hjust = 0
  ) +
  scale_x_date(limits = c(as.Date("2023-06-01"), as.Date("2023-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  scale_y_continuous(limits = c(0, 4)) +
  labs(y = "Nitrogen [%]", x = "Date") +
  thesis_theme

plot.n_con_percent

ggsave(
  "figures/nutrients/n_con_percent.png",
  plot.n_con_percent,
  width = 8,
  height = 6,
  dpi = 150,
  unit = "cm"
)

### Phosphorus ----

plot.p_con_mg_kg <- df.nutrients_emm |>
  filter(nutrient_name == "p_con_mg_kg") |>
  arrange(species, campaign) |>
  mutate(
    group_y = case_when(
      species == "Fagus sylvatica" ~ 2000,
      species == "Fraxinus excelsior" ~ 500
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date, y = nutrient_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = FALSE
  ) +
  geom_line(
    aes(x = date, y = emmean, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_errorbar(
    aes(x = date, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  annotate(
    "text",
    x = as.Date("2023-06-01"),
    y = 2000,
    label = "c)",
    size = 5,
    vjust = 0.5,
    hjust = 0
  ) +
  scale_x_date(limits = c(as.Date("2023-06-01"), as.Date("2023-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  scale_y_continuous(limits = c(500, 2000), breaks = seq(500, 2000, 250)) +
  labs(y = "Phosphorus [mg kg-1]", x = "Date") +
  thesis_theme

plot.p_con_mg_kg

ggsave(
  "figures/nutrients/p_con_mg_kg.png",
  plot.p_con_mg_kg,
  width = 8,
  height = 6,
  dpi = 150,
  unit = "cm"
)

### Sulfur ----

plot.s_con_mg_kg <- df.nutrients_emm |>
  filter(nutrient_name == "s_con_mg_kg") |>
  arrange(species, campaign) |>
  mutate(
    group_y = case_when(
      species == "Fagus sylvatica" ~ 3000,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date, y = nutrient_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = FALSE
  ) +
  geom_line(
    aes(x = date, y = emmean, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_errorbar(
    aes(x = date, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  annotate(
    "text",
    x = as.Date("2023-06-01"),
    y = 3000,
    label = "d)",
    size = 5,
    vjust = 0.5,
    hjust = 0
  ) +
  scale_x_date(limits = c(as.Date("2023-06-01"), as.Date("2023-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, 500)) +
  labs(y = "Sulfur [mg kg-1]", x = "Date") +
  thesis_theme

plot.s_con_mg_kg

ggsave(
  "figures/nutrients/s_con_mg_kg.png",
  plot.s_con_mg_kg,
  width = 8,
  height = 6,
  dpi = 150,
  unit = "cm"
)

### Calcium ----
plot.ca_con_mg_kg <- df.nutrients_emm |>
  filter(nutrient_name == "ca_con_mg_kg") |>
  arrange(species, campaign) |>
  mutate(
    group_y = case_when(
      species == "Fagus sylvatica" ~ 25000,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date, y = nutrient_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_line(
    aes(x = date, y = emmean, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_errorbar(
    aes(x = date, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  annotate(
    "text",
    x = as.Date("2023-06-01"),
    y = 25000,
    label = "g)",
    size = 5,
    vjust = 0.5,
    hjust = 0
  ) +
  scale_x_date(limits = c(as.Date("2023-06-01"), as.Date("2023-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  scale_y_continuous(limits = c(0, 25000), breaks = seq(0, 25000, 5000)) +
  labs(y = "Calcium [mg kg-1]", x = "Date") +
  thesis_theme

plot.ca_con_mg_kg

ggsave(
  "figures/nutrients/ca_con_mg_kg.png",
  plot.ca_con_mg_kg,
  width = 8,
  height = 6,
  dpi = 150,
  unit = "cm"
)

### Potassium ----
plot.k_con_mg_kg <- df.nutrients_emm |>
  filter(nutrient_name == "k_con_mg_kg") |>
  arrange(species, campaign) |>
  mutate(
    group_y = case_when(
      species == "Fagus sylvatica" ~ 15000,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date, y = nutrient_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = T
  ) +
  geom_line(
    aes(x = date, y = emmean, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = T
  ) +
  geom_errorbar(
    aes(x = date, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  annotate(
    "text",
    x = as.Date("2023-06-01"),
    y = 15000,
    label = "f)",
    size = 5,
    vjust = 0.5,
    hjust = 0
  ) +
  scale_x_date(limits = c(as.Date("2023-06-01"), as.Date("2023-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species", ncol = 1)) +
  scale_y_continuous(limits = c(0, 15000), breaks = seq(0, 15000, 2500)) +
  labs(y = "Potassium [mg kg-1]", x = "Date") +
  thesis_theme

plot.k_con_mg_kg

ggsave(
  "figures/nutrients/k_con_mg_kg.png",
  plot.k_con_mg_kg,
  width = 8,
  height = 8.5,
  dpi = 150,
  unit = "cm"
)

### Magnesium ----

plot.mg_con_mg_kg <- df.nutrients_emm |>
  filter(nutrient_name == "mg_con_mg_kg") |>
  arrange(species, campaign) |>
  mutate(
    group_y = case_when(
      species == "Fagus sylvatica" ~ 3100,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date, y = nutrient_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_line(
    aes(x = date, y = emmean, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = F
  ) +
  geom_errorbar(
    aes(x = date, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  annotate(
    "text",
    x = as.Date("2023-06-01"),
    y = 3100,
    label = "e)",
    size = 5,
    vjust = 0.5,
    hjust = 0
  ) +
  scale_x_date(limits = c(as.Date("2023-06-01"), as.Date("2023-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  scale_y_continuous(limits = c(0, 3100), breaks = seq(0, 3000, 1000)) +
  labs(y = "Magnesium [mg kg-1]", x = "Date") +
  thesis_theme +
  theme(legend.position = "right")

plot.mg_con_mg_kg

ggsave(
  "figures/nutrients/mg_con_mg_kg.png",
  plot.mg_con_mg_kg,
  width = 8,
  height = 6,
  dpi = 150,
  unit = "cm"
)
