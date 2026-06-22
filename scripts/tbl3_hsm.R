# setup -------------------------------------------------------------------

library(tidyverse)
library(officer)
library(flextable)
library(multcompView)
library(emmeans)
source("scripts/ggplot_themes.R") ## this also has the flextable theme
source("scripts/helper_functions.R")

# nested dataframes with emmeans grid objects

df.hsm_emm = read_rds("output/emmeans/df_hsm_emmeans.Rds")

df.hsm_table = read_csv(
  "data/calculated_parameters/df_hsm_emmeans.csv"
) |>
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
    hsm_mpa,
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


# Tables for emmeans contrasts ---------------------------------------------

f.emm2flextable(
  emm = df.hsm_emm$emm[[1]],
  species_var = "species",
  date_var = "date_fac",
  year_var = "year",
  digits = 3,
  caption_txt = "",
  theme_fun = ft_theme
)


table_paths = "tables/02_hsm/emmeans_contrasts/"
df.hsm_emm$threshold |> unique()

# "psi_midday_mpa"      "psi_tlp_mpa"         "psi_ft_mpa"          "capacitance_tlp_tot" "capacitance_ft_tot"  "rwc_tlp_tot_perc"
# "p12_mpa"             "p50_mpa"             "p88_mpa"
# "elast_tot_mpa"       "gmin_mmol_m2_s"      "t5_degc"             "t50_degc"            "t95_degc"

df.hsm_emm$threshold_caption = c(
  "Leaf water potential at turgor loss [MPa]",
  "Leaf water potential at 12 % of embolized leaf vessel area [MPa]",
  "Leaf water potential at 50 % of embolized leaf vessel area [MPa]",
  "Leaf water potential at 88 % of embolized leaf vessel area [MPa]"
)

for (i in 1:4) {
  ft = f.emm2flextable(
    emm = df.hsm_emm$emm[[i]],
    species_var = "species",
    date_var = "date_fac",
    year_var = "year",
    digits = 3,
    caption_txt = df.hsm_emm$threshold_caption[i],
    theme_fun = ft_theme
  )
  # ft |>
  #   save_as_docx(
  #     path = paste0(table_paths, df.hsm_emm$threshold[i], ".docx")
  #   )
  obj_name = paste0("ft.",  df.hsm_emm$threshold_caption[i])
  assign(x =obj_name, value =  ft)
  print(i)
}


for (i in 1:4) {
  ft = f.emm2flextable_species(
    emm = df.hsm_emm$emm[[i]],
    species_var = "species",
    date_var = "date_fac",
    year_var = "year",
    digits = 3,
    caption_txt = df.hsm_emm$threshold_caption[i],
    theme_fun = ft_theme
  )
  # ft |>
  #   save_as_docx(
  #     path = paste0(
  #       table_paths,
  #       df.hsm_emm$threshold[i],
  #       "_species_comp.docx"
  #     )
  #   )
  # print(i)
    obj_name = paste0("ft.", df.hsm_emm$threshold_caption[i], "_species_comp")
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

print(doc, target =  "tables/hsm_contrasts.docx")
