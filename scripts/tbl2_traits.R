# setup -------------------------------------------------------------------

library(tidyverse)
library(flextable)
library(multcompView)
library(emmeans)
source("scripts/ggplot_themes.R") ## this also has the flextable theme
source("scripts/helper_functions.R")

# nested dataframes with emmeans grid objects
df.hydr_traits_emm = read_rds("output/emmeans/emm_hydr_traits.rds")

df.hydr_traits_table = read_csv(
  "data/calculated_parameters/df_hydr_traits_emmeans.csv"
) |>
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


# Tables for emmeans contrasts ---------------------------------------------

f.emm2flextable(
  emm = df.hydr_traits_emm$emm[[7]],
  species_var = "species",
  date_var = "date_fac",
  year_var = "year",
  digits = 3,
  caption_txt = "",
  theme_fun = ft_theme
)

table_paths = "tables/01_hydr_traits/emmeans_contrasts/"
df.hydr_traits_emm$trait |> unique()

# "psi_midday_mpa"      "psi_tlp_mpa"         "psi_ft_mpa"          "capacitance_tlp_tot" "capacitance_ft_tot"  "rwc_tlp_tot_perc"
# "p12_mpa"             "p50_mpa"             "p88_mpa"
# "elast_tot_mpa"       "gmin_mmol_m2_s"      "t5_degc"             "t50_degc"            "t95_degc"

df.hydr_traits_emm$trait_caption = c(
  "Midday leaf water potential [MPa]",
  "Leaf water potential at turgor loss [MPa]",
  "Leaf water potential at full turgor [MPa]",
  "Total capacitance at turgor loss [% MPa-1]",
  "Total capacitance at full turgor [% MPa-1]",
  "Relative water content at turgor loss [%]",
  "Leaf water potential at 12 % of embolized leaf vessel area [MPa]",
  "Leaf water potential at 50 % of embolized leaf vessel area [MPa]",
  "Leaf water potential at 88 % of embolized leaf vessel area [MPa]",
  "Total elasticity  [MPa]",
  "Minimum leaf conductance [mmol m-2 s-1]",
  "Thermal sensitivity at 5 % loss of FvFm [°C]",
  "Thermal sensitivity at 50 % loss of FvFm [°C]",
  "Thermal sensitivity at 95 % loss of FvFm [°C]"
)

# for (i in 1:11) {
#   ft = f.emm2flextable(
#     emm = df.hydr_traits_emm$emm[[i]],
#     species_var = "species",
#     date_var = "date_fac",
#     year_var = "year",
#     digits = 3,
#     caption_txt = df.hydr_traits_emm$trait_caption[i],
#     theme_fun = ft_theme
#   )
#   ft |>
#     save_as_docx(
#       path = paste0(table_paths, df.hydr_traits_emm$trait[i], ".docx")
#     )
#   print(i)
# }

for (i in 1:11) {
  ft = f.emm2flextable(
    emm = df.hydr_traits_emm$emm[[i]],
    species_var = "species",
    date_var = "date_fac",
    year_var = "year",
    digits = 3,
    caption_txt = df.hydr_traits_emm$trait_caption[i],
    theme_fun = ft_theme
  )
  obj_name = paste0("ft.", df.hydr_traits_emm$trait[i])
  assign(x =obj_name, value =  ft)
  print(i)
}

f.emm2flextable_wo_year(
  emm = df.hydr_traits_emm$emm[[12]],
  species_var = "species",
  date_var = "date_fac",
  digits = 3,
  caption_txt = "",
  theme_fun = ft_theme
)

for (i in 12:14) {
  ft = f.emm2flextable_wo_year(
    emm = df.hydr_traits_emm$emm[[i]],
    species_var = "species",
    date_var = "date_fac",
    digits = 3,
    caption_txt = df.hydr_traits_emm$trait_caption[i],
    theme_fun = ft_theme
  )
  obj_name = paste0("ft.", df.hydr_traits_emm$trait[i])
  assign(x =obj_name, value =  ft)
}

# for (i in 12:14) {
#   ft = f.emm2flextable_wo_year(
#     emm = df.hydr_traits_emm$emm[[i]],
#     species_var = "species",
#     date_var = "date_fac",
#     digits = 3,
#     caption_txt = df.hydr_traits_emm$trait_caption[i],
#     theme_fun = ft_theme
#   )
#   ft |>
#     save_as_docx(
#       path = paste0(table_paths, df.hydr_traits_emm$trait[i], ".docx")
#     )
#   print(i)
# }


# Tables for species comparisons  ----------------------------------------

f.emm2flextable_species(
  emm = df.hydr_traits_emm$emm[[12]],
  species_var = "species",
  date_var = "date_fac",
  year_var = "year",
  digits = 3,
  caption_txt = "",
  theme_fun = ft_theme
)

for (i in 1:11) {
  ft = f.emm2flextable_species(
    emm = df.hydr_traits_emm$emm[[i]],
    species_var = "species",
    date_var = "date_fac",
    digits = 3,
    caption_txt = df.hydr_traits_emm$trait_caption[i],
    theme_fun = ft_theme
  )
  obj_name = paste0("ft.", df.hydr_traits_emm$trait[i], "_species_comp")
  assign(x =obj_name, value =  ft)
  print(i)
}


f.emm2flextable_species_wo_year(
  emm = df.hydr_traits_emm$emm[[12]],
  species_var = "species",
  date_var = "date_fac",
  digits = 3,
  caption_txt = "",
  theme_fun = ft_theme
)

for (i in 12:14) {
  ft = f.emm2flextable_species_wo_year(
    emm = df.hydr_traits_emm$emm[[i]],
    species_var = "species",
    date_var = "date_fac",
    digits = 3,
    caption_txt = df.hydr_traits_emm$trait_caption[i],
    theme_fun = ft_theme
  )
  obj_name = paste0("ft.", df.hydr_traits_emm$trait[i], "_species_comp")
  assign(x =obj_name, value =  ft)
  print(i)
}

# 
# for (i in 12:14) {
#   ft = f.emm2flextable_species_wo_year(
#     emm = df.hydr_traits_emm$emm[[i]],
#     species_var = "species",
#     date_var = "date_fac",
#     digits = 3,
#     caption_txt = df.hydr_traits_emm$trait_caption[i],
#     theme_fun = ft_theme
#   )
#   ft |>
#     save_as_docx(
#       path = paste0(
#         table_paths,
#         df.hydr_traits_emm$trait[i],
#         "_species_comp.docx"
#       )
#     )
#   print(i)
# }


## merge tables ------------------------------------------------------------

df.tables = data.frame(table = ls(pattern = "ft\\.")) 

doc = read_docx()

for(i in 1:length(df.tables$table)){
# for(i in 1:10){ ## test
  doc = doc |> body_add_flextable(get(df.tables$table[[i]]))  |> 
    body_add_par("\n")
}

print(doc, target =  "tables/test.docx")

# Tables for LME coefficients ----------------------------------------------------------

trait_list = df.hydr_traits_emm$trait |> unique()
trait_numbers = seq(1, length(trait_list))

for (i in seq_along(trait_list)) {
  trait_sel = trait_list[i]
  caption = trait_sel
  num = trait_numbers[i]

  df.hydr_traits_emm$lme[[i]] |>
    as_flextable(add.random = T) |>
    set_caption(caption) |>
    ft_theme() #|>
  # save_as_docx(
  #   path = paste0(
  #     "tables/01_hydr_traits/lme_coefs/",
  #     num,
  #     "_lme_",
  #     trait_sel,
  #     ".docx"
  #   )
  # )
}

test = df.hydr_traits_emm$lme[[1]]

test |> summary()
test |>
  broom.mixed::tidy() |>
  View()


f.lme2flextable <- function(
  lme_obj,
  trait_name,
  add_random = TRUE,
  ft_theme_fun = ft_theme,
  digits = 3
) {}

f.lme2flextable(
  df.hydr_traits_emm$lme[[1]],
  trait_name = "Midday leaf water potential [MPa]",
  add_random = T,
  digits = 3
)


# Tables for emmeans results ----------------------------------------------

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
      path = paste0(
        "tables/01_hydr_traits/emmeans/",
        num,
        "_",
        trait_sel,
        ".docx"
      )
    )
  print(i)
}
