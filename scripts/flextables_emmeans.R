# setup -------------------------------------------------------------------

library(tidyverse)
library(flextable)
library(multcompView)
library(emmeans)
source("scripts/ggplot_themes.R") ## this also has the flextable theme

# nested dataframes with emmeans grid objects
df.hydr_traits_emm = read_rds("output/emmeans/emm_hydr_traits.rds")

df.hydr_traits_table = read_csv(
  "data/calculated_parameters/df_hydr_traits_emmeans.csv"
)  |>
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


df.sugars_emm = read_rds("output/emmeans/emm_nsc.rds")

df.sugars_table = read_csv("data/calculated_parameters/df_sugars_emmeans.csv") |>
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

df.nutrients_table = read_csv("data/calculated_parameters/df_nutrients_emmeans.csv") |>
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
    sample_id, nutrient_conc,
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

df.hsm_emm = read_rds("output/emmeans/df_hsm_emmeans.Rds")

df.hsm_table = read_csv(
  "data/calculated_parameters/df_hsm_emmeans.csv"
)  |>
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
    hsm_mpa ,
    emmean,
    SE,
    `lower CL`,
    `upper CL`,
    `CL type`,
    .group,
    threshold   
  ) |>
  mutate(year = as.factor(year), across(emmean:`upper CL`, ~ round(.x, 3))) |>
  rename(
    "Year" = "year",
    "Date" = "date_fac",
    "Species" = "species",
    "Tree ID" = "sample_id",
    "Meas. value" = "hsm_mpa",
    "Est. mean" = "emmean",
    "Std. error" = "SE",
    "Sign. group" = ".group"
  )


# Tables for LME coefficients ----------------------------------------------------------


## Hydr. traits ------------------------------------------------------------

trait_list = df.hydr_traits_emm$trait |> unique()
trait_numbers = seq(1, length(trait_list))

for (i in seq_along(trait_list)) {
  trait_sel = trait_list[i]
  caption = trait_sel
  num = trait_numbers[i]

  as_flextable(df.hydr_traits_emm$lme[[i]], add.random = T) |>
    set_caption(caption) |>
    ft_theme() |>
    save_as_docx(
      path = paste0("tables/01_hydr_traits/lme_coefs/", num, "_lme_", trait_sel, ".docx")
    )
}


## NSC ---------------------------------------------------------------------

sugar_list = df.sugars_emm$sugar_name |> unique()
sugar_numbers = seq(1, length(sugar_list))

for (i in seq_along(sugar_list)) {
  sugar_sel = sugar_list[i]
  caption = sugar_sel
  num = sugar_numbers[i]
  
  try( as_flextable(df.sugars_nest$lme[[i]], add.random = T) |>
    set_caption(caption) |>
    ft_theme() |>
    save_as_docx(
      path = paste0("tables/02_nsc/lme_coefs/", num, "_lme_", sugar_sel, ".docx")
    )
  )
}

## Nutrients --------------------------------------------------------------

nutrient_list = df.nutrients_nest$nutrient_name |> unique()
nutrient_numbers = seq(1, length(nutrient_list))

for (i in seq_along(nutrient_list)) {
  nutrient_sel = nutrient_list[i]
  caption = nutrient_sel
  num = nutrient_numbers[i]
  
  try(df.nutrients_nest$lme[[i]] |> 
        broom.mixed::tidy() |>
        as.data.frame() |> 
        # select(term, estimate, std.error, statistic, p.value) |> 
        mutate(term = term |> str_replace_all(pattern = "date_fac", replacement = "Date ")) |> 
        as_flextable() |>
        set_caption(caption) |>
        ft_theme() |> 
        save_as_docx(path = paste0(
          "tables/03_nutrients/lme_coefs/",
          num,
          "_lme_",
          nutrient_sel,
          ".docx"
        )))
}


# Tables for emmeans results ----------------------------------------------

## Hydr. traits ------------------------------------------------------------

df.hydr_traits_table$trait |> unique()

# Create a numbered list of sugars
trait_list = df.hydr_traits_table$trait |> unique()
trait_numbers = seq(1, length(trait_list))

# Loop through each trait and create the flextable
for (i in seq_along(trait_list)) {
  trait_sel = trait_list[i]
  caption = trait_sel
  num = trait_numbers[i]
  
  # Create flextable for the current trait
  ft = df.hydr_traits_table |>
    ungroup() |>
    filter(trait == trait_sel) |> 
    select(-c(trait, `CL type`))
  
  # Set the caption and save the flextable
  flextable(ft) |>
    set_caption(caption) |>
    ft_theme() |>
    save_as_docx(
      path = paste0("tables/01_hydr_traits/emmeans/", num, "_", trait_sel, ".docx")
    )
  print(i)
}

## NSC --------------------------------------

df.sugars_table$sugar_name |> unique()

# Create a numbered list of sugars
sugar_list = df.sugars_table$sugar_name |> unique()
sugar_numbers = seq(1, length(sugar_list))

# Loop through each sugar and create the flextable
for (i in seq_along(sugar_list)) {
  sugar_sel = sugar_list[i]
  caption = sugar_sel
  num = sugar_numbers[i]
  
  # Create flextable for the current sugar
  ft = df.sugars_table |>
    ungroup() |>
    filter(sugar_name == sugar_sel)|> 
    select(-c(sugar_name, `CL type`))
  
  # Set the caption and save the flextable
  flextable(ft) |>
    set_caption(caption) |>
    ft_theme() |>
    save_as_docx(
      path = paste0("tables/02_nsc/emmeans/", num, "_", sugar_sel, ".docx")
    )
  print(i)
}


## Nutrients ---------------------------------------------------------------

df.nutrients_table$nutrient_name |> unique()

# Create a numbered list of nutrients
nutrient_list = df.nutrients_table$nutrient_name |> unique()
nutrient_numbers = seq(1, length(nutrient_list))

# Loop through each nutrient and create the flextable
for (i in seq_along(nutrient_list)) {
  nutrient_sel = nutrient_list[i]
  caption = nutrient_sel
  num = nutrient_numbers[i]
  
  # Create flextable for the current nutrient
  ft = df.nutrients_table |>
    ungroup() |>
    filter(nutrient_name == nutrient_sel)|> 
    select(-c(nutrient_name, `CL type`))
  
  # Set the caption and save the flextable
  flextable(ft) |>
    set_caption(caption) |>
    ft_theme() |>
    save_as_docx(
      path = paste0("tables/03_nutrients/emmeans/", num, "_", nutrient_sel, ".docx")
    )
  print(i)
}


## HSM  --------------------------------------------------------------------


df.hsm_table$threshold |> unique()

# Create a numbered list of sugars
hsm_list = df.hsm_table$threshold |> unique()
hsm_numbers = seq(1, length(hsm_list))

# Loop through each hsm and create the flextable
for (i in seq_along(hsm_list)) {
  hsm_sel = hsm_list[i]
  caption = hsm_sel
  num = hsm_numbers[i]
  
  # Create flextable for the current hsm
  ft = df.hsm_table |>
    ungroup() |>
    filter(threshold == hsm_sel) |> 
    select(-c(threshold, `CL type`))
  
  # Set the caption and save the flextable
  flextable(ft) |>
    set_caption(caption) |>
    ft_theme() |>
    save_as_docx(
      path = paste0("tables/04_hsm/emmeans/", num, "_", hsm_sel, ".docx")
    )
  print(i)
}

# Tables for emmeans contrasts ---------------------------------------------


## Hydr. traits ------------------------------------------------------------

df.hydr_traits_table$trait |> unique()

# Create a numbered list of sugars
trait_list = df.hydr_traits_emm$trait |> unique()
trait_numbers = seq(1, length(trait_list))

# Loop through each trait and create the flextable
for (i in seq_along(trait_list)) {
  trait_sel = trait_list[i]
  caption = trait_sel
  num = trait_numbers[i]
  
  # Create flextable for the current trait
  ft = df.hydr_traits_emm |>
    ungroup() |>
    filter(trait == trait_sel) |> 
    select(emm) |> 
    pl
  
  # Set the caption and save the flextable
  flextable(ft) |>
    set_caption(caption) |>
    ft_theme() |>
    save_as_docx(
      path = paste0("tables/01_hydr_traits/emmeans/", num, "_", trait_sel, ".docx")
    )
  print(i)
}


# Tables for all values of individual sample trees ---------------------------------------------

## psi_midday --------------------------------------------------------------

ft_midday = df_hydr_traits_emm |>
  filter(trait == "psi_midday_mpa") |>
  distinct() |>
  select(-trait)

flextable(ft_midday) |>
  ft_theme() |>
  set_caption("Midday leaf water potential (MPa)") |>
  save_as_docx(path = "tables/psi_midday.docx")

flextable::as_flextable(df.hydr_traits_nest_emm$lme[[2]], add.random = T) |>
  ft_theme() |>
  save_as_docx(path = "tables/lme_psi_midday.docx")

df.hydr_traits_emm$lme[[2]] |> View()

df.hydr_traits_emm$lme[[2]] |> summary()


# Test  ----------------------------------------------------------------

test = df.sugars_emm$emm[[1]] |> as.emmGrid()
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
test = df.hydr_traits_nest_mods$emm[[2]]

# Create pairwise comparisons for species within each date
test |>
  emmeans(pairwise ~ date_fac | species * year) |>
  as.data.frame() 

pw <- test |>
  emmeans(pairwise ~ date_fac | species * year)

pw_contrasts = pw$contrasts |> as.data.frame()

pw_contrasts$con1 = str_split_fixed(pw_contrasts$contrast, "\\) - \\(", n = 2)[,1] |> str_remove_all("\\(")
pw_contrasts$con2 = str_split_fixed(pw_contrasts$contrast, "\\) - \\(", n = 2)[,2]  |> 
  str_remove_all("\\)") |> 
  str_remove_all("\\(")

test_con = pw_contrasts |> 
  mutate(ident1 = paste0(species, year, con1),
         ident2 = paste0(species, year, con2),
                 ) 

test_emm = pw$emmeans |> as.data.frame() |> 
  mutate(ident1 = paste0(species, year, date_fac),
         ident2 = paste0(species, year, date_fac)) 

test_con |> 
  left_join(test_emm |> select(-ident2), by = "ident1") |> 
  rename("Response 1" = "emmean") |> 
  left_join(test_emm |> select(-ident1), by = "ident2") |> 
  rename("Response 2" = "emmean") |> 
  mutate("Diff. (%)" = (`Response 1` - `Response 2` )/ `Response 1`) |> 
  select(species, year, con1, con2, estimate, `Response 1`, `Response 2`, `Diff. (%)`, 
         df, z.ratio, p.value) |> 
  filter(!is.na(species)) |> 
  flextable()


f.join_emmeans2contrasts = function(emm) {
  df_con
  
  
  
}



test |>
  emmeans(~ date_fac | species * year) |>
  pairs() |>
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


emm = emmeans(test, ~ species | date_fac)

# means
means_df = as.data.frame(emm)

# contrasts
contr_df = contrast(emm, "pairwise") |>
  as.data.frame()

contr_with_means = contr_df |>
  # as.data.frame() |>
  separate(contrast, into = c("FASY", "FREX"), sep = " - ") |>
  left_join(means_df, by = c("FASY" = "species", "date_fac", "year")) |>
  rename(mean1 = emmean) |>
  left_join(means_df, by = c("FASY" = "species", "date_fac", "year")) |>
  rename(mean2 = emmean)
contr_with_means

expSup = function(x, digits = 3) {
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
all_contrasts = list(
  species_date_contrasts,
  date_species_contrasts
)

# Convert the contrasts to data frames
species_date_pairs = species_date_contrasts |> as.data.frame()
date_species_pairs = date_species_contrasts |> as.data.frame()

# Display the results
species_date_pairs
date_species_pairs

test |>
  multcomp::cld(Letters = letters)

test |> pairs() |> as.data.frame() |> nrow()


## emmeans from fig6 Nutrient script --------------------------------------


test = df.hydr_traits_nest_emm |>
  select(trait, data) |>
  unnest(cols = c(data)) |>
  left_join(
    df.hydr_traits_nest_emm |>
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

word_export = read_docx()
body_add_flextable(word_export, ft1)
body_add_par(word_export, value = "")
body_add_flextable(word_export, ft2)
print(word_export, 'try.docx')


library(dplyr)
library(flextable)
library(officer)

# Assuming nutrient_list is defined and contains all nutrient names
nutrient_list = unique(df.nutrients_emm$nutrient_name)

# Initialize a Word document
word_export = read_docx()

# Loop through each nutrient in the nutrient_list
for (nutrient in nutrient_list) {
  # Filter the dataframe for the current nutrient
  filtered_data = df.nutrients_emm |>
    filter(nutrient_name == nutrient) |>
    select(-c(`Tree ID`, `Meas. value`)) |>
    distinct() |>
    select(-nutrient_name)

  # Create a flextable from the filtered data
  ft = flextable(filtered_data) |>
    ft_theme()

  # Add the flextable to the Word document
  word_export = body_add_flextable(word_export, ft)

  # Add a paragraph with the nutrient name as a caption
  word_export = body_add_par(word_export, value = paste0("\n", nutrient, "\n"))
}

# Save the Word document
print(word_export, "nutrients_report.docx")

## sugars ----
# Initialize a Word document
word_export = read_docx()

# Loop through each sugar in the sugar_list
for (sugar in sugar_list) {
  # Filter the dataframe for the current sugar
  filtered_data = df.sugars_emm |>
    filter(sugar_name == sugar) |>
    select(-c(`Tree ID`, `Meas. value`)) |>
    distinct() |>
    select(-sugar_name)

  # Create a flextable from the filtered data
  ft = flextable(filtered_data) |>
    ft_theme()

  # Add the flextable to the Word document
  word_export = body_add_flextable(word_export, ft)

  # Add a paragraph with the sugar name as a caption
  word_export = body_add_par(word_export, value = paste0("\n", sugar, "\n"))
}

# Save the Word document
print(word_export, "sugars_report.docx")
