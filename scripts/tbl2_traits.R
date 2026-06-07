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

f.emm2flextable <- function(
  emm,
  species_var = "species",
  date_var = "date_fac",
  year_var = "year",
  digits = 3,
  caption_txt = NULL,
  theme_fun = ft_theme
) {
  specs <- as.formula(
    paste0("pairwise ~ ", date_var, " | ", species_var, " * ", year_var)
  )
  has_na = case_when(
    as.data.frame(emm)$emmean |> is.na() |> sum() > 1 ~ T,
    T ~ F
  )

  if (has_na) {
    # there are non‑estimable rows → keep the original object
    emm <- emm
  } else {
    # all rows are estimable → safe to call regrid()
    emm <- regrid(emm)
  }

  pw <- emmeans(emm, specs, type = "response")

  contr <- pw$contrasts %>% as.data.frame()
  emmean <- pw$emmeans %>% as.data.frame()

  contr <- contr %>%
    mutate(
      con1 = str_split_fixed(contrast, "\\) - \\(", n = 2)[, 1] %>%
        str_remove_all("[()]"),
      con2 = str_split_fixed(contrast, "\\) - \\(", n = 2)[, 2] %>%
        str_remove_all("[()]"),
      ident1 = paste0(!!sym(species_var), !!sym(year_var), con1),
      ident2 = paste0(!!sym(species_var), !!sym(year_var), con2)
    ) |>
    as.data.frame()

  emmean <- emmean %>%
    mutate(
      ident1 = paste0(!!sym(species_var), !!sym(year_var), !!sym(date_var)),
      ident2 = ident1
    ) |>
    as.data.frame()

  tbl <- contr %>%
    left_join(emmean %>% select(-ident2), by = "ident1") %>%
    rename(`Response 1` = any_of(c("emmean", "response"))) |>
    left_join(emmean %>% select(-ident1), by = "ident2") %>%
    rename(`Response 2` = any_of(c("emmean", "response"))) |>
    mutate(`Diff. (%)` = (`Response 1` - `Response 2`) / `Response 1`) %>%
    rename(`t.ratio` = any_of(c("t.ratio", "z.ratio"))) |>
    select(
      !!sym(species_var),
      !!sym(year_var),
      con1,
      con2,
      `Response 1`,
      `Response 2`,
      estimate,
      `Diff. (%)`,
      df,
      t.ratio,
      p.value
    ) %>%
    mutate(
      across(
        c(estimate, `Diff. (%)`, `Response 1`, `Response 2`, t.ratio),
        ~ f.round(.x, digits)
      ),
      p.value = f.p_value_bins(p.value)
    ) %>%
    rename(
      Species = !!sym(species_var),
      Year = !!sym(year_var),
      `Date 1` = con1,
      `Date 2` = con2,
      Contrast = estimate,
    ) %>%
    filter(!is.na(Species))

  ft <- tbl %>%
    flextable() %>%
    merge_v(j = c("Species", "Year", "Date 1")) %>%
    hline() %>%
    vline(j = 1:ncol(tbl)) %>%
    theme_fun() %>%
    set_caption(caption_txt) %>%
    fix_border_issues()

  ft
}

f.emm2flextable(
  emm = df.hydr_traits_nest_mods$emm[[7]],
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
  ft |>
    save_as_docx(
      path = paste0(table_paths, df.hydr_traits_emm$trait[i], ".docx")
    )
  print(i)
}

f.emm2flextable_wo_year <- function(
  emm,
  species_var = "species",
  date_var = "date_fac",
  digits = 3,
  caption_txt = NULL,
  theme_fun = ft_theme
) {
  specs <- as.formula(paste0("pairwise ~ ", date_var, " | ", species_var))

  est_col <- if ("emmean" %in% names(as.data.frame(emm))) {
    "emmean"
  } else {
    "response"
  }
  has_na <- any(is.na(as.data.frame(emm)[[est_col]]))

  if (!has_na) {
    emm <- regrid(emm)
  }

  pw <- emmeans(emm, specs, type = "response")
  contr <- pw$contrasts %>% as.data.frame()
  emmean <- pw$emmeans %>% as.data.frame()

  contr <- contr %>%
    mutate(
      con1 = str_split_fixed(contrast, "\\) - \\(", n = 2)[, 1] %>%
        str_remove_all("[()]"),
      con2 = str_split_fixed(contrast, "\\) - \\(", n = 2)[, 2] %>%
        str_remove_all("[()]"),
      ident1 = paste0(!!sym(species_var), con1),
      ident2 = paste0(!!sym(species_var), con2)
    )

  emmean <- emmean %>%
    mutate(
      ident1 = paste0(!!sym(species_var), !!sym(date_var)),
      ident2 = ident1
    ) |>
    select(ident1, ident2, emmean, date_fac)

  tbl <- contr %>%
    left_join(emmean %>% select(-ident2), by = "ident1") %>%
    rename(`Response 1` = any_of(c("emmean", "response"))) %>%
    left_join(emmean %>% select(-c(ident1, date_fac)), by = "ident2") %>%
    rename(`Response 2` = any_of(c("emmean", "response"))) %>%
    mutate(`Diff. (%)` = (`Response 1` - `Response 2`) / `Response 1`) %>%
    rename(`t.ratio` = any_of(c("t.ratio", "z.ratio"))) %>%
    select(
      !!sym(species_var),
      # !!sym(date_var),
      con1,
      con2,
      `Response 1`,
      `Response 2`,
      estimate,
      `Diff. (%)`,
      df,
      t.ratio,
      p.value
    ) %>%
    mutate(
      across(
        c(estimate, `Diff. (%)`, `Response 1`, `Response 2`, t.ratio),
        ~ f.round(.x, digits)
      ),
      p.value = f.p_value_bins(p.value)
    ) %>%
    rename(
      Species = !!sym(species_var),
      `Date 1` = con1,
      `Date 2` = con2,
      Contrast = estimate
    ) %>%
    filter(!is.na(Species))

  ft <- tbl %>%
    flextable() %>%
    merge_v(j = c("Species", "Date 1")) %>%
    hline() %>%
    vline(j = 1:ncol(tbl)) %>%
    theme_fun() %>%
    set_caption(caption_txt) %>%
    fix_border_issues()

  ft
}

f.emm2flextable_wo_year(
  emm = df.hydr_traits_nest_mods$emm[[12]],
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
  ft |>
    save_as_docx(
      path = paste0(table_paths, df.hydr_traits_emm$trait[i], ".docx")
    )
  print(i)
}

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
