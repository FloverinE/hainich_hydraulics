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

df.sugars = read.csv(
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
  ) |>
  left_join(df.phenology, by = c("year", "species")) |>
  mutate(
    doy_d = yday(date),
    leaf_age_d = doy_d - sos50_doy,
    year = as.factor(year),
    date_fac = as.factor(date)
  )


## nest
df.sugars_nest = df.sugars |>
  filter(
    sugar_name %in%
      c(
        "fructose_mg_g",
        "glucose_mg_g",
        "starch_mg_g",
        "sucrose_mg_g",
        "total_sugars_mg_g",
        "starch_sugar_ratio"
      )
  ) |>
  nest(data = -c(sugar_name))

## lme
df.sugars_nest = df.sugars_nest |>
  mutate(
    data = map(data, ~ .x |> filter(!is.na(sugar_conc)) |> distinct())
  )

df.sugars_nest = df.sugars_nest |>
  mutate(
    lme = map(
      data,
      ~ glmmTMB(
        sugar_conc ~ species * date_fac * year + (1 | sample_id),
        data = .x,
        family = gaussian()
      )
    )
  )

df.sugars_nest = df.sugars_nest |>
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

df.sugars_nest = df.sugars_nest |>
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


# export ------------------------------------------------------------------

df.sugars_emm = df.sugars_nest |>
  mutate(
    data_emm = map2(
      .x = data,
      .y = emm,
      ~ left_join(.x, .y, by = c("species", "date_fac", "year"))
    )
  ) |>
  select(sugar_name, data_emm) |>
  unnest(data_emm)

write_csv(
  df.sugars_emm,
  "data/calculated_parameters/df_sugars_emmeans.csv"
)


# plot --------------------------------------------------------------------

df.sugars_emm = read_csv("data/calculated_parameters/df_sugars_emmeans.csv")

df.plot = df.sugars_emm |>
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

df.sugars_emm$sugar_name |> unique()

sugar_labels_xml <- c(
  "fructose_mg_g" = "Fructose (mg g<sup>−1</sup>)",
  "glucose_mg_g" = "Glucose (mg g<sup>−1</sup>)",
  "starch_mg_g" = "Starch (mg g<sup>−1</sup>)",
  "sucrose_mg_g" = "Sucrose (mg g<sup>−1</sup>)",
  "total_sugars_mg_g" = "Total Sugars (mg g<sup>−1</sup>)",
  "starch_sugar_ratio" = "Starch:Sucrose Ratio"
)

df.plot_dummy_limits = df.plot |>
  group_by(sugar_name) |>
  reframe(
    min_y = case_when(
      min(sugar_conc) > 0 & max(sugar_conc) < 0.5 ~ 0,
      T ~ min(sugar_conc / 2 |> trunc()) |> floor() * 2
    ),
    max_y = case_when(
      min(sugar_conc) > 0 & max(sugar_conc) < 0.5 ~ 0.5,
      T ~ max(sugar_conc / 2 |> trunc()) |> ceiling() * 2
    ),
  ) |>
  pivot_longer(cols = c("min_y":"max_y"), values_to = "y") |>
  mutate(
    x = rep(c(0, 175), length(unique(sugar_name))),
    species = "*Fagus sylvatica*",
    Year = "2023"
  )

fig5_nsc.png =
  ggplot(
    df.plot,
    aes(
      x = leaf_age_d,
      col = Species,
      linetype = Year
    )
  ) +
  geom_point(
    aes(y = sugar_conc),
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
    ~sugar_name,
    scales = "free_y",
    strip.position = "top",
    labeller = labeller(sugar_name = as_labeller(sugar_labels_xml))
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

fig5_nsc.png

ggsave(
  filename = "publication_figures/fig5_nsc.png",
  fig5_nsc.png,
  width = 30,
  height = 16,
  units = "cm"
)


# emmeans ----------------------------------------------------------------

test = df.sugars_nest$emm[[1]] |> as.emmGrid()
pairs_test = test |> pairs() |> as.data.frame()
pairs_test ## all combinations in reference to baseline, way too many

df.sugars_nest$emm[[1]] |> contrast()
df.sugars_nest$emm[[1]] |> pairs()
df.sugars_nest$emm[[1]] |> regrid()

df.sugars_nest$data[[1]]

# only keep sensible combinations, i.e., species at the same date, within species across dates
# Assuming df.sugars_nest contains your nested data
library(emmeans)
library(dplyr)

# Extract the emmGrid object
test <- df.sugars_nest$emm[[1]] |> as.emmGrid()

# Create pairwise comparisons for species within each date
test |>
  emmeans(pairwise ~ species | date_fac) |>
  pluck("contrasts") |>
  as.data.frame()

test |>
  emmeans(pairwise ~ species | date_fac) |>
  pluck("emmeans") |>
  as.data.frame()

# Create pairwise comparisons for dates within each species
test |>
  emmeans(pairwise ~ date_fac | species | year) |>
  pluck("emmeans") |>
  as.data.frame() |>
  flextable() |>
  ft_theme()


emm <- emmeans(test, ~ species | date_fac)

# means
means_df <- as.data.frame(emm)

# contrasts
contr_df <- contrast(emm, "pairwise") |>
  as.data.frame()

contr_with_means <- contr_df |>
  # as.data.frame() |>
  separate(contrast, into = c("FASY", "FREX"), sep = " - ") |>
  left_join(means_df, by = c("FASY" = "species", "date_fac", "year")) |>
  rename(mean1 = emmean) |>
  left_join(means_df, by = c("FASY" = "species", "date_fac", "year")) |>
  rename(mean2 = emmean)
contr_with_means

expSup <- function(x, digits = 3) {
  sprintf(
    paste0("%03.", digits, "f x 10^%d^"),
    x / 10^floor(log10(abs(x))),
    floor(log10(abs(x)))
  )
}

test |>
  emmeans(pairwise ~ species | date_fac) |>
  pluck("contrasts") |>
  as.data.frame() |>
  rbind(
    test |>
      emmeans(pairwise ~ date_fac | species | year) |>
      pluck("contrasts") |>
      as.data.frame()
  ) |>
  mutate(
    p.value = expSup(p.value)
  ) |>
  # Create pairwise comparisons for dates within each species
  flextable() |>
  ft_theme()

# Combine the contrasts
all_contrasts <- list(
  species_date_contrasts,
  date_species_contrasts
)

# Convert the contrasts to data frames
species_date_pairs <- species_date_contrasts |> as.data.frame()
date_species_pairs <- date_species_contrasts |> as.data.frame()

# Display the results
species_date_pairs
date_species_pairs

test |>
  multcomp::cld(Letters = letters)

test |> pairs() |> as.data.frame() |> nrow()
