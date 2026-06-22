# setup -------------------------------------------------------------------

library(tidyverse)
library(officer)
library(flextable)
library(multcompView)
library(emmeans)
source("scripts/ggplot_themes.R") ## this also has the flextable theme
source("scripts/helper_functions.R")

# nested dataframes with emmeans grid objects
df.nutrients_emm = read_rds("output/emmeans/emm_nutrients.rds")

df.nutrients_table = read_csv(
  "data/calculated_parameters/df_nutrients_emmeans.csv"
) |>
  mutate(
    "lower CL" = lower.CL,
    "upper CL" = upper.CL,
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


# Tables for emmeans contrasts ---------------------------------------------

f.emm2flextable(
  emm = df.nutrients_emm$emm[[1]],
  species_var = "species",
  date_var = "date_fac",
  year_var = "year",
  digits = 3,
  caption_txt = "",
  theme_fun = ft_theme
)

table_paths = "tables/04_nutrients/emmeans_contrasts/"
df.nutrients_emm$nutrient_name |> unique()
# [1] "k_mg_kg"  "mg_mg_kg" "p_mg_kg"  "s_mg_kg"  "c"        "n"

df.nutrients_emm$caption = c(
  "Potassium [mg kg-1]",
  "Magnesium [mg kg-1]",
  "Phosphorus [mg kg-1]",
  "Sulfur [mg kg-1]",
  "Carbon [%]",
  "Nitrogen [%]"
)

for (i in 1:6) {
  ft = f.emm2flextable(
    emm = df.nutrients_emm$emm[[i]],
    species_var = "species",
    date_var = "date_fac",
    year_var = "year",
    digits = 3,
    caption_txt = df.nutrients_emm$caption[i],
    theme_fun = ft_theme
  )
  obj_name = paste0("ft.", df.nutrients_emm$caption[[i]])
  assign(x =obj_name, value =  ft)
  print(i)
}

for (i in 1:6) {
  ft = f.emm2flextable_species(
    emm = df.nutrients_emm$emm[[i]],
    species_var = "species",
    date_var = "date_fac",
    year_var = "year",
    digits = 3,
    caption_txt = df.nutrients_emm$caption[i],
    theme_fun = ft_theme
  )
  obj_name = paste0("ft.", df.nutrients_emm$caption[[i]], "_species_comp")
  assign(x =obj_name, value =  ft)
  print(i)
}

# merge tables ------------------------------------------------------------

df.tables = data.frame(table = ls(pattern = "ft\\.")) 

doc = read_docx()

for(i in 1:length(df.tables$table)){
  # for(i in 1:10){ ## test
  doc = doc |> body_add_flextable(get(df.tables$table[[i]]))  |> 
    body_add_par("\n")
}

print(doc, target =  "tables/nutrient_contrasts.docx")

# Tables for LME coefficients ----------------------------------------------------------

nutrient_list = df.nutrients_emm$nutrient_name |> unique()
nutrient_numbers = seq(1, length(nutrient_list))

for (i in seq_along(nutrient_list)) {
  nutrient_sel = nutrient_list[i]
  caption = nutrient_sel
  num = nutrient_numbers[i]

  try(
    as_flextable(df.nutrients_nest$lme[[i]], add.random = T) |>
      set_caption(caption) |>
      ft_theme() |>
      save_as_docx(
        path = paste0(
          "tables/02_nsc/lme_coefs/",
          num,
          "_lme_",
          nutrient_sel,
          ".docx"
        )
      )
  )
}

# Tables for emmeans results ----------------------------------------------

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
    filter(nutrient_name == nutrient_sel) |>
    select(-c(nutrient_name, `CL type`))

  # Set the caption and save the flextable
  flextable(ft) |>
    set_caption(caption) |>
    ft_theme() |>
    save_as_docx(
      path = paste0("tables/02_nsc/emmeans/", num, "_", nutrient_sel, ".docx")
    )
  print(i)
}

# Tables for emmeans contrasts ---------------------------------------------
