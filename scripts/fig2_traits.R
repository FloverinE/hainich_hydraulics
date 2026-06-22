# setup -------------------------------------------------------------------
{
  library(lme4)
  library(tidyverse)
  library(glmmTMB)
  library(emmeans)
  library(performance)
  library(flextable)
  library(see)
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

df.phenology = readxl::read_excel(
  "data/microclimate_new_2026/Hai_TowerCamera_Seasons_Summary.xlsx",
  sheet = "import_to_R"
)

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
  mutate(leaf_age_d = doy_d - sos50_doy, year = as.factor(year)) |>
  dplyr::select(
    -c(
      sos50_doy,
      eos50_doy,
      campaign,
      vessel_order,
      pheno_start_doy,
      date,
      doy_d,
      sugar_name:nutrient_name
    )
  ) |>
  distinct()


# 1. lme models per trait ---------------------------------------------------

df.hydr_traits_long = df.hydr_traits |>
  # filter(vessel_order != "all") |>
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

df.hydr_traits_nest_base = df.hydr_traits_nest |>
  filter(
    !trait %in%
      c("elast_tot_mpa", "gmin_mmol_m2_s", "t5_degc", "t50_degc", "t95_degc")
  ) |>
  mutate(
    lme = map(
      data,
      ~ glmmTMB(
        trait_value ~ species * year * date_fac + (1 | sample_id),
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
        trait_value ~ species * year * date_fac + (1 | sample_id),
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

# df.hydr_traits_nest_base$data[[1]] |> View()

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
    emm_pairs = emm,
    emm_plot = map(
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
df.hydr_traits_nest_mods$emm_plot[[14]]

# 3. unnest and export contrast table ------------------------------------

df.hydr_traits_nest_mods = df.hydr_traits_nest_mods |>
  mutate(
    data_emm = map2(
      .x = data,
      .y = emm_plot,
      ~ left_join(.x, .y, by = c("species", "date_fac"))
    )
  )

df.hydr_traits_mods = df.hydr_traits_nest_mods |>
  select(trait, data_emm) |>
  unnest(data_emm) |>
  mutate(year = unique(c(year.x, year.y)) |> str_sort() |> first()) |>
  select(-c(year.x, year.y))

write_csv(
  df.hydr_traits_mods,
  "data/calculated_parameters/df_hydr_traits_emmeans.csv"
)

write_rds(
  df.hydr_traits_nest_mods |> select(data, trait, lme, emm, emm_plot),
  "output/emmeans/emm_hydr_traits.Rds"
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
    Date = date_fac |> as.Date(),
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
    upper_lim = asymp.UCL,
    # group_y = case_when(year == "2023" ~ group_y + 0.2, T ~ group_y)
  ) |>
  filter(!trait_order %in% c("1_psi_midday_mpa", "6_capacitance_ft_tot"))

trait_labels = c(
  # "1_psi_midday_mpa" = "Ψ<sub>MD</sub> (MPa)",
  "2_psi_ft_mpa" = "Ψ<sub>FT</sub> (MPa)",
  "3_psi_tlp_mpa" = "Ψ<sub>TLP</sub> (MPa)",
  "4_elast_tot_mpa" = "ε (MPa)",
  "5_rwc_tlp_tot_perc" = "RWC<sub>TLP</sub> (%)",
  # "6_capacitance_ft_tot" = "C<sub>FT</sub> (% MPa<sup>-1</sup>)",
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

year(df.plot$Date) = 2000

## 4.1 plot over leaf age --------------------------------------------------

df.plot_dummy_limits = readxl::read_excel("scripts/fig2_limits.xlsx")

df.plot_dummy_limits$trait_order <- factor(
  df.plot_dummy_limits$trait_order,
  levels = names(trait_labels)
)

plot.all_traits.png <-
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
    labeller = labeller(trait_order = as_labeller(trait_labels)),
    ncol = 3
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

plot.all_traits.png

ggsave(
  "figures/fig2_all_traits.png",
  plot.all_traits.png,
  width = 20,
  height = 25,
  units = "cm"
)


## 4.2 plot over date ------------------------------------------------------

df.plot_dummy_limits = readxl::read_excel("scripts/fig2_limits.xlsx", sheet = "date")

df.plot_dummy_limits$trait_order <- factor(
  df.plot_dummy_limits$trait_order,
  levels = names(trait_labels)
)

plot.all_traits.png <-
  ggplot(
    df.plot,
    aes(
      x = Date,
      col = species,
      linetype = Year)
  ) +
  geom_point(
    aes(y = trait_value),
    alpha = plot_specs$plot_alpha,
    position = position_dodge(width = 2),
    show.legend = T
  ) +
  geom_line(
    aes(y = pred_val),
    alpha = plot_specs$plot_alpha,
    position = position_dodge(width = 2),
    show.legend = F
  ) +
  geom_errorbar(
    aes(ymin = lower_lim, ymax = upper_lim),
    alpha = plot_specs$plot_alpha,
    position = position_dodge(width = 2),
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
  # scale_x_date(limits = c("2000-05-15", "2000-09-30")) +
  facet_wrap(
    ~trait_order,
    scales = "free_y",
    strip.position = "top",
    labeller = labeller(trait_order = as_labeller(trait_labels)),
    ncol = 3
  ) +
  labs(
    y = NULL,
    x = "Date",
  ) +
  thesis_theme +
  theme(
    strip.text = ggtext::element_markdown(),
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.text = ggtext::element_markdown(),
  )

plot.all_traits.png

ggsave(
  "figures/fig2_all_traits_date.png",
  plot.all_traits.png,
  width = 20,
  height = 25,
  units = "cm"
)

# export ------------------------------------------------------------------

df.plot |>
  group_by(trait, Year, species, leaf_age_d) |>
  summarise(
    mean = mean(trait_value) |> round(2),
    sd = sd(trait_value) |> round(2)
  ) |>
  View()

df.plot |>
  group_by(trait, Year, species) |>
  summarise(
    mean = mean(trait_value) |> round(1),
    sd = sd(trait_value) |> round(1)
  ) |>
  View()

df.plot |>
  group_by(trait, Year, species) |>
  summarise(
    mean = mean(trait_value) |> round(1),
    sd = sd(trait_value) |> round(1)
  ) |>
  View()
