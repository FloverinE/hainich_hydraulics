# setup -------------------------------------------------------------------

library(tidyverse)
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
  ft |>
    save_as_docx(
      path = paste0(table_paths, df.hsm_emm$threshold[i], ".docx")
    )
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
  ft |>
    save_as_docx(
      path = paste0(
        table_paths,
        df.hsm_emm$threshold[i],
        "_species_comp.docx"
      )
    )
  print(i)
}
