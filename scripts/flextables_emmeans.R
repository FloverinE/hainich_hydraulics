# setup -------------------------------------------------------------------

library(tidyverse)
library(flextable)
library(multcompView)
library(emmeans)


df.sugars_emm = read_csv("data/calculated_parameters/df_sugars_emmeans.csv") |>
  mutate(
    "lower CL" = lower.CL,
    "upper CL" = upper.CL,
    "CL type" = "standard",
    .group = .group |> str_remove_all("\\s")
  ) |>
  select(
    year,
    date_fac,
    species,
    sample_id,
    sugar_conc,
    emmean,
    SE,
    `lower CL`,
    `upper CL`,
    `CL type`,
    .group,
    sugar_name
  ) |>
  mutate(year = as.factor(year), across(emmean:`upper CL`, ~ round(.x, 3))) |>
  rename(
    "Year" = "year",
    "Date" = "date_fac",
    "Species" = "species",
    "Tree ID" = "sample_id",
    "Meas. value" = "sugar_conc",
    "Est. mean" = "emmean",
    "Std. error" = "SE",
    "Sign. group" = ".group"
  )

df.nutrients_emm = read_csv(
  "data/calculated_parameters/df_nutrients_emmeans.csv"
) |>
  mutate(
    "lower CL" = asymp.LCL,
    "upper CL" = asymp.UCL,
    "CL type" = "asymp.",
    .group = .group |> str_remove_all("\\s")
  ) |>
  select(
    year,
    date_fac,
    species,
    sample_id,
    nutrient_conc,
    emmean,
    SE,
    `lower CL`,
    `upper CL`,
    `CL type`,
    .group,
    nutrient_name
  ) |>
  mutate(year = as.factor(year), across(emmean:`upper CL`, ~ round(.x, 3))) |>
  rename(
    "Year" = "year",
    "Date" = "date_fac",
    "Species" = "species",
    "Tree ID" = "sample_id",
    "Meas. value" = "nutrient_conc",
    "Est. mean" = "emmean",
    "Std. error" = "SE",
    "Sign. group" = ".group"
  )


df.traits_emm = read_csv(
  "data/calculated_parameters/df_hydr_traits_emmeans.csv"
)

df.traits_emm = df.traits_emm |>
  # read_rds("output/emmeans/emm_hydr_traits.Rds") |>
  # select(trait, data) |>
  # unnest(cols = c(data)) |>
  # left_join(
  #   df.hydr_traits_nest_mods |>
  #     select(trait, emm) |>
  #     unnest(cols = c(emm)),
  #   by = c("trait", "year", "species", "date_fac")
  # ) |>
  mutate(
    "lower CL" = case_when(is.na(asymp.LCL) ~ lower.CL, T ~ asymp.LCL),
    "upper CL" = case_when(is.na(asymp.UCL) ~ upper.CL, T ~ asymp.UCL),
    "CL type" = case_when(is.na(lower.CL) ~ "asymp.", T ~ "standard"),
    .group = .group |> str_remove_all("\\s")
  ) |>
  select(
    year,
    date_fac,
    species,
    sample_id,
    trait_value,
    emmean,
    SE,
    `lower CL`,
    `upper CL`,
    `CL type`,
    .group,
    trait
  ) |>
  mutate(year = as.factor(year), across(emmean:`upper CL`, ~ round(.x, 3))) |>
  rename(
    "Year" = "year",
    "Date" = "date_fac",
    "Species" = "species",
    "Tree ID" = "sample_id",
    "Meas. value" = "trait_value",
    "Est. mean" = "emmean",
    "Std. error" = "SE",
    "Sign. group" = ".group"
  )

source("scripts/ggplot_themes.R")

# trait tables ------------------------------------------------------------

df.hydr_trait_mods = read_rds("output/emmeans/emm_hydr_traits.Rds")

contrast(df.hydr_trait_mods$emm[[1]]) |> multcomp::cld()

regrid(df.hydr_traits_nest_base$emm[[1]]) |> multcomp::cld()

test = pairs(df.hydr_traits_nest_base$emm[[1]]) |> as.data.frame()

## psi_midday --------------------------------------------------------------

ft_midday = df.traits_emm |>
  filter(trait == "psi_midday_mpa") |>
  distinct() |>
  select(-trait)

flextable(ft_midday) |>
  ft_theme() |>
  set_caption("Midday leaf water potential (MPa)") |>
  save_as_docx(path = "tables/psi_midday.docx")

flextable::as_flextable(df.hydr_traits_nest_mods$lme[[2]], add.random = T) |>
  ft_theme() |>
  save_as_docx(path = "tables/lme_psi_midday.docx")

df.hydr_traits_nest_mods$lme[[2]] |> View()

df.hydr_traits_nest_mods$lme[[2]] |> summary()


## emmeans from fig2 script --------------------------------------

df.traits_emm$trait |> unique()

# Create a numbered list of traits
trait_list <- df.traits_emm$trait |> unique()
trait_numbers <- seq(1, length(trait_list))

# Loop through each trait and create the flextable
for (i in seq_along(trait_list)) {
  trait_sel <- trait_list[i]
  caption = trait_sel
  num <- trait_numbers[i]

  # Create flextable for the current trait
  ft <- df.traits_emm |>
    ungroup() |>
    filter(trait == trait_sel) |>
    # distinct() |>
    select(-trait)

  # Set the caption and save the flextable
  flextable(ft) |>
    set_caption(caption) |>
    ft_theme() |>
    save_as_docx(path = paste0("tables/emmeans/", num, "_", trait_sel, ".docx"))
  print(i)
}


# lme flextables ----------------------------------------------------------

# Create a numbered list of traits
trait_list <- df.hydr_traits_nest_mods$trait |> unique()
trait_numbers <- seq(1, length(trait_list))

for (i in seq_along(trait_list)) {
  trait_sel <- trait_list[i]
  caption = trait_sel
  num <- trait_numbers[i]

  flextable::as_flextable(df.hydr_traits_nest_mods$lme[[i]], add.random = T) |>
    set_caption(caption) |>
    ft_theme() |>
    save_as_docx(
      path = paste0("tables/lme_coefs/", num, "_lme_", trait_sel, ".docx")
    )
}

## emmeans from fig5 NSC script --------------------------------------

df.sugars_emm$sugar_name |> unique()

# Create a numbered list of sugars
sugar_list <- df.sugars_emm$sugar_name |> unique()
sugar_numbers <- seq(1, length(sugar_list))

# Loop through each sugar and create the flextable
for (i in seq_along(sugar_list)) {
  sugar_sel <- sugar_list[i]
  caption = sugar_sel
  num <- sugar_numbers[i]

  # Create flextable for the current sugar
  ft <- df.sugars_emm |>
    ungroup() |>
    filter(sugar_name == sugar_sel) |>
    # distinct() |>
    select(-sugar_name)

  # Set the caption and save the flextable
  flextable(ft) |>
    set_caption(caption) |>
    ft_theme() |>
    save_as_docx(
      path = paste0("tables/02_nsc/emmeans/", num, "_", sugar_sel, ".docx")
    )
  print(i)
}

## emmeans from fig6 Nutrient script --------------------------------------

df.nutrients_emm$nutrient_name |> unique()

# Create a numbered list of nutrients
nutrient_list <- df.nutrients_emm$nutrient_name |> unique()
nutrient_numbers <- seq(1, length(nutrient_list))

# Loop through each nutrient and create the flextable
for (i in seq_along(nutrient_list)) {
  nutrient_sel <- nutrient_list[i]
  caption = nutrient_sel
  num <- nutrient_numbers[i]

  # Create flextable for the current nutrient
  ft <- df.nutrients_emm |>
    filter(nutrient_name == nutrient_sel) |>
    select(-c(`Tree ID`, `Meas. value`)) |>
    distinct() |>
    select(-nutrient_name) |>
    flextable() |>
    set_caption(caption) |>
    ft_theme()

  # Set the caption and save the flextable
  ft |>
    save_as_docx(
      path = paste0(
        "tables/03_nutrients/emmeans/",
        num,
        "_",
        nutrient_sel,
        ".docx"
      )
    )
  print(i)
}


# tests ------------------------------------------------------------------

test = df.hydr_traits_nest_mods$emm_pairs[[1]]
pairs_test = test |> pairs() |> as.data.frame()
test |> multcomp::cld(Letters = letters) |> as.data.frame()

test = df.hydr_traits_nest_mods |>
  select(trait, data) |>
  unnest(cols = c(data)) |>
  left_join(
    df.hydr_traits_nest_mods |>
      select(trait, emm) |>
      unnest(cols = c(emm)),
    by = c("trait", "year", "species", "date_fac")
  )

ft1 = df.nutrients_emm |>
  filter(nutrient_name == nutrient_list[1]) |>
  select(-c(`Tree ID`, `Meas. value`)) |>
  distinct() |>
  select(-nutrient_name) |>
  flextable() |>
  ft_theme()
ft2 = df.nutrients_emm |>
  filter(nutrient_name == nutrient_list[2]) |>
  select(-c(`Tree ID`, `Meas. value`)) |>
  distinct() |>
  select(-nutrient_name) |>
  flextable() |>
  ft_theme()


library(officer)
# Source - https://stackoverflow.com/a/49152951
# Posted by Stewart Ross, modified by community. See post 'Timeline' for change history
# Retrieved 2026-05-16, License - CC BY-SA 3.0

word_export <- read_docx()
body_add_flextable(word_export, ft1)
body_add_par(word_export, value = "")
body_add_flextable(word_export, ft2)
print(word_export, 'try.docx')


library(dplyr)
library(flextable)
library(officer)

# Assuming nutrient_list is defined and contains all nutrient names
nutrient_list <- unique(df.nutrients_emm$nutrient_name)

# Initialize a Word document
word_export <- read_docx()

# Loop through each nutrient in the nutrient_list
for (nutrient in nutrient_list) {
  # Filter the dataframe for the current nutrient
  filtered_data <- df.nutrients_emm |>
    filter(nutrient_name == nutrient) |>
    select(-c(`Tree ID`, `Meas. value`)) |>
    distinct() |>
    select(-nutrient_name)

  # Create a flextable from the filtered data
  ft <- flextable(filtered_data) |>
    ft_theme()

  # Add the flextable to the Word document
  word_export <- body_add_flextable(word_export, ft)

  # Add a paragraph with the nutrient name as a caption
  word_export <- body_add_par(word_export, value = paste0("\n", nutrient, "\n"))
}

# Save the Word document
print(word_export, "nutrients_report.docx")

## sugars ----
# Initialize a Word document
word_export <- read_docx()

# Loop through each sugar in the sugar_list
for (sugar in sugar_list) {
  # Filter the dataframe for the current sugar
  filtered_data <- df.sugars_emm |>
    filter(sugar_name == sugar) |>
    select(-c(`Tree ID`, `Meas. value`)) |>
    distinct() |>
    select(-sugar_name)

  # Create a flextable from the filtered data
  ft <- flextable(filtered_data) |>
    ft_theme()

  # Add the flextable to the Word document
  word_export <- body_add_flextable(word_export, ft)

  # Add a paragraph with the sugar name as a caption
  word_export <- body_add_par(word_export, value = paste0("\n", sugar, "\n"))
}

# Save the Word document
print(word_export, "sugars_report.docx")
