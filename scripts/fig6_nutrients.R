# setup -------------------------------------------------------------------

library(emmeans)
library(glmmTMB)
library(tidyverse)
library(see)

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

df.phenology = readxl::read_excel(
  "data/microclimate_new_2026/Hai_TowerCamera_Seasons_Summary.xlsx",
  sheet = "import_to_R"
)

df.nutrients = read_csv(
  "data/calculated_parameters/df_nutrients_2023_2024.csv"
) |>
  select(-campaign) |>
  mutate(
    date = as.Date(date),
    species = as.factor(species),
    sample_id = as.factor(sample_id),
    date_fac = as.factor(date)
  ) %>%
  mutate(
    nutrient_name = nutrient_name |> str_remove_all("_con"),
    year = year(date)
  ) |>
  filter(
    nutrient_name %in%
      c("c", "n", "k_mg_kg", "mg_mg_kg", "p_mg_kg", "s_mg_kg")
  ) |>
  left_join(df.phenology, by = c("year", "species")) |>
  mutate(
    doy_d = yday(date),
    leaf_age_d = doy_d - sos50_doy,
    year = as.factor(year),
    date_fac = as.factor(date)
  )

## nest
df.nutrients_nest = df.nutrients |>
  nest(data = -c(nutrient_name))

## lme
df.nutrients_nest = df.nutrients_nest |>
  mutate(
    data = map(data, ~ .x |> filter(!is.na(nutrient_conc)) |> distinct())
  )

df.nutrients_nest = df.nutrients_nest |>
  mutate(
    lme = map(
      data,
      ~ glmmTMB(
        nutrient_conc ~ species * year * date_fac + (1 | sample_id),
        data = .x,
        family = gaussian()
      )
    )
  )

df.nutrients_nest = df.nutrients_nest |>
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

df.nutrients_nest = df.nutrients_nest |>
  mutate(
    emm_plot = map(
      emm,
      ~ .x |> multcomp::cld(Letters = letters) |> as.data.frame(),
      emm_plot = map(
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


# export ------------------------------------------------------------------

df.nutrients_emm = df.nutrients_nest |>
  mutate(
    data_emm = map2(
      .x = data,
      .y = emm_plot,
      ~ left_join(.x, .y |> select(-year), by = c("species", "date_fac"))
    )
  ) |>
  select(nutrient_name, data_emm) |>
  unnest(data_emm)

write_csv(
  df.nutrients_emm,
  "data/calculated_parameters/df_nutrients_emmeans.csv"
)

write_rds(
  df.nutrients_nest |> select(data, nutrient_name, emm, emm_plot),
  "output/emmeans/emm_nutrients.rds"
)


# plot --------------------------------------------------------------------

df.nutrients_emm = read_csv(
  "data/calculated_parameters/df_nutrients_emmeans.csv"
)


## plot over leaf age ------------------------------------------------------

df.plot = df.nutrients_emm |>
  mutate(
    Year = year(date_fac) |> as.factor(),
    Species = recode(
      species,
      "FASY" = "*Fagus sylvatica*",
      "FREX" = "*Fraxinus excelsior*"
    ),
    pred_val = emmean,
    lower_lim = asymp.LCL,
    upper_lim = asymp.UCL,
    # group_y = case_when(year == "2023" ~ group_y + 0.2, T ~ group_y)
  )

df.nutrients_emm$nutrient_name |> unique()

nutrient_labels <- c(
  "k_mg_kg" = "Potassium (mg kg⁻¹)",
  "mg_mg_kg" = "Magnesium (mg kg⁻¹)",
  "p_mg_kg" = "Phosphorus (mg kg⁻¹)",
  "s_mg_kg" = "Sulfur (mg kg⁻¹)",
  "c" = "Carbon (%)",
  "n" = "Nitrogen (%)"
)

df.plot_dummy_limits = df.plot |>
  group_by(nutrient_name) |>
  reframe(
    min_y = case_when(
      min(nutrient_conc - lower_lim) > 0 &
        max(nutrient_conc + upper_lim) < 0.5 ~ 0,
      T ~ min(nutrient_conc / 2 |> trunc()) |> floor() * 2
    ),
    max_y = case_when(
      min(nutrient_conc - lower_lim) > 0 &
        max(nutrient_conc + upper_lim) < 0.5 ~ 0.50,
      T ~ max(nutrient_conc / 2 |> trunc()) |> ceiling() * 2
    ),
  ) |>
  pivot_longer(cols = c("min_y":"max_y"), values_to = "y") |>
  mutate(
    x = rep(c(0, 175), length(unique(nutrient_name))),
    species = "*Fagus sylvatica*",
    Year = "2023"
  )

fig6_nutrients.png =
  ggplot(
    df.plot,
    aes(
      x = leaf_age_d,
      col = Species,
      linetype = Year
    )
  ) +
  geom_point(
    aes(y = nutrient_conc),
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
  scale_color_oi(order = c(6, 2)) +
  scale_x_continuous(limits = c(0, 175), breaks = seq(0, 175, 25)) +
  facet_wrap(
    ~nutrient_name,
    scales = "free_y",
    strip.position = "top",
    labeller = labeller(nutrient_name = as_labeller(nutrient_labels))
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

fig6_nutrients.png

ggsave(
  filename = "publication_figures/fig6_nutrients.png",
  fig6_nutrients.png,
  width = 30,
  height = 16,
  units = "cm"
)


## plot over date ------------------------------------------------------

df.plot = df.nutrients_emm |>
  mutate(
    Date = as.Date(date_fac),
    Year = year(date_fac) |> as.factor(),
    Species = recode(
      species,
      "FASY" = "*Fagus sylvatica*",
      "FREX" = "*Fraxinus excelsior*"
    ),
    pred_val = emmean,
    lower_lim = lower.CL,
    upper_lim = upper.CL,
    # group_y = case_when(year == "2023" ~ group_y + 0.2, T ~ group_y)
  )

year(df.plot$Date) = 2000

df.nutrients_emm$nutrient_name |> unique()

nutrient_labels <- c(
  "k_mg_kg" = "Potassium (mg kg⁻¹)",
  "mg_mg_kg" = "Magnesium (mg kg⁻¹)",
  "p_mg_kg" = "Phosphorus (mg kg⁻¹)",
  "s_mg_kg" = "Sulfur (mg kg⁻¹)",
  "c" = "Carbon (%)",
  "n" = "Nitrogen (%)"
)

df.plot_dummy_limits = df.plot |>
  group_by(nutrient_name) |>
  reframe(
    min_y = case_when(
      min(nutrient_conc - lower_lim) > 0 &
        max(nutrient_conc + upper_lim) < 0.5 ~ 0,
      T ~ min(nutrient_conc / 2 |> trunc()) |> floor() * 2
    ),
    max_y = case_when(
      min(nutrient_conc - lower_lim) > 0 &
        max(nutrient_conc + upper_lim) < 0.5 ~ 0.50,
      T ~ max(nutrient_conc / 2 |> trunc()) |> ceiling() * 2
    ),
  ) |>
  pivot_longer(cols = c("min_y":"max_y"), values_to = "y") |>
  mutate(
    x = rep(as.Date(c("2000-05-15", "2000-09-30")), length(unique(nutrient_name))),
    species = "*Fagus sylvatica*",
    Year = "2023"
  )

fig6_nutrients.png =
  ggplot(
    df.plot,
    aes(
      x = Date,
      col = Species,
      linetype = Year
    )
  ) +
  geom_point(
    aes(y = nutrient_conc),
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
  scale_color_oi(order = c(6, 2)) +
  scale_x_date(limits = as.Date(c("2000-05-15", "2000-09-30"))) +
  facet_wrap(
    ~nutrient_name,
    scales = "free_y",
    strip.position = "top",
    labeller = labeller(nutrient_name = as_labeller(nutrient_labels))
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

fig6_nutrients.png

ggsave(
  filename = "figures/fig6_nutrients_date.png",
  fig6_nutrients.png,
  width = 30,
  height = 16,
  units = "cm"
)
