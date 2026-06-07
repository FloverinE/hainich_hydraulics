# setup -------------------------------------------------------------------

library(tidyverse)
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
  ft |>
    save_as_docx(
      path = paste0(table_paths, df.nutrients_emm$nutrient_name[i], ".docx")
    )
  print(i)
}


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
