# setup -------------------------------------------------------------------

library(tidyverse)
library(officer)
library(emmeans)
library(flextable)
library(multcompView)
library(emmeans)
source("scripts/ggplot_themes.R") ## this also has the flextable theme
source("scripts/helper_functions.R")

# nested dataframes with emmeans grid objects

df.sugars_emm = read_rds("output/emmeans/emm_nsc.rds")

df.sugars_table = read_csv(
  "data/calculated_parameters/df_sugars_emmeans.csv"
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


# Tables for emmeans contrasts ---------------------------------------------

f.emm2flextable(
  emm = df.sugars_emm$emm[[5]],
  species_var = "species",
  date_var = "date_fac",
  year_var = "year",
  digits = 3,
  caption_txt = "",
  theme_fun = ft_theme
)

table_paths = "tables/03_nsc/emmeans_contrasts/"
df.sugars_emm$sugar_name |> unique()

# "psi_midday_mpa"      "psi_tlp_mpa"         "psi_ft_mpa"          "capacitance_tlp_tot" "capacitance_ft_tot"  "rwc_tlp_tot_perc"
# "p12_mpa"             "p50_mpa"             "p88_mpa"
# "elast_tot_mpa"       "gmin_mmol_m2_s"      "t5_degc"             "t50_degc"            "t95_degc"

df.sugars_emm$caption = c(
  "Fructose [mg g-1]",
  "Glucose [mg g-1]",
  "Starch [mg g-1]",
  "Sucrose [mg g-1]",
  "Total sugars [mg g-1]")

for (i in 1:5) {
  ft = f.emm2flextable(
    emm = df.sugars_emm$emm[[i]],
    species_var = "species",
    date_var = "date_fac",
    year_var = "year",
    digits = 3,
    caption_txt = df.sugars_emm$caption[i],
    theme_fun = ft_theme
  )
  obj_name = paste0("ft.", df.sugars_emm$caption[[i]])
  assign(x =obj_name, value =  ft)
  print(i)
}

for (i in 1:5) {
  ft = f.emm2flextable_species(
    emm = df.sugars_emm$emm[[i]],
    species_var = "species",
    date_var = "date_fac",
    year_var = "year",
    digits = 3,
    caption_txt = df.sugars_emm$caption[i],
    theme_fun = ft_theme
  )
  obj_name = paste0("ft.", df.sugars_emm$caption[[i]], "_species_comp")
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

print(doc, target =  "tables/nsc_contrasts.docx")

# Tables for LME coefficients ----------------------------------------------------------

sugar_list = df.sugars_emm$sugar_name |> unique()
sugar_numbers = seq(1, length(sugar_list))

for (i in seq_along(sugar_list)) {
  sugar_sel = sugar_list[i]
  caption = sugar_sel
  num = sugar_numbers[i]

  try(
    as_flextable(df.sugars_nest$lme[[i]], add.random = T) |>
      set_caption(caption) |>
      ft_theme() |>
      save_as_docx(
        path = paste0(
          "tables/02_nsc/lme_coefs/",
          num,
          "_lme_",
          sugar_sel,
          ".docx"
        )
      )
  )
}

# Tables for emmeans results ----------------------------------------------

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
    filter(sugar_name == sugar_sel) |>
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

# Tables for emmeans contrasts ---------------------------------------------
