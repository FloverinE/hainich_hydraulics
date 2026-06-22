f.round <- function(x, dec = 3) {
  sprintf(paste0("%.", dec, "f"), round(x, dec))
}

f.p_value_bins <- function(pval) {
  case_when(
    pval > 0.1 ~ paste0(f.round(pval, 3), " n.s."),
    pval > 0.05 & pval <= 0.1 ~ paste0(f.round(pval, 3), " ."),
    pval > 0.01 & pval <= 0.05 ~ paste0(f.round(pval, 3), " *"),
    pval > 0.001 & pval <= 0.01 ~ paste0(f.round(pval, 3), " **"),
    pval <= 0.001 ~ paste0(f.round(pval, 3), " ***"),
    TRUE ~ NA_character_
  )
}

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

f.emm2flextable_species_wo_year <- function(
  emm,
  species_var = "species",
  date_var = "date_fac",
  digits = 3,
  caption_txt = NULL,
  theme_fun = ft_theme
) {
  specs = as.formula(
    paste0("pairwise ~ ", species_var, " | ", date_var)
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
      con1 = str_split_fixed(contrast, "-", n = 2)[, 1] %>%
        str_remove_all("\\s"),
      con2 = str_split_fixed(contrast, "-", n = 2)[, 2] %>%
        str_remove_all("\\s"),
      ident1 = paste0(con1, date_fac),
      ident2 = paste0(con2, date_fac),
    ) |>
    as.data.frame()

  emmean <- emmean %>%
    mutate(
      ident1 = paste0(!!sym(species_var), !!sym(date_var)),
      ident2 = ident1
    ) |>
    as.data.frame()

  tbl <- contr %>%
    left_join(emmean %>% select(-ident2), by = "ident1") %>%
    rename(`Response FASY` = any_of(c("emmean", "response"))) |>
    left_join(emmean %>% select(-ident1), by = "ident2") %>%
    rename(`Response FREX` = any_of(c("emmean", "response"))) |>
    mutate(
      `Diff. (%)` = (`Response FASY` - `Response FREX`) / `Response FASY`,
    ) %>%
    rename(`t.ratio` = any_of(c("t.ratio", "z.ratio"))) |>
    select(
      date_fac,
      `Response FASY`,
      `Response FREX`,
      estimate,
      `Diff. (%)`,
      df,
      t.ratio,
      p.value
    ) %>%
    mutate(
      across(
        c(estimate, `Diff. (%)`, `Response FASY`, `Response FREX`, t.ratio),
        ~ f.round(.x, digits)
      ),
      p.value = f.p_value_bins(p.value)
    ) %>%
    rename(
      Date = date_fac,
      Contrast = estimate,
    )
  # filter(!is.na(Species))

  ft <- tbl %>%
    flextable() %>%
    merge_v(j = c("Date")) %>%
    hline() %>%
    vline(j = 1:ncol(tbl)) %>%
    theme_fun() %>%
    set_caption(caption_txt) %>%
    fix_border_issues()

  ft
}


f.emm2flextable_species <- function(
  emm,
  species_var = "species",
  date_var = "date_fac",
  year_var = "year",
  digits = 3,
  caption_txt = NULL,
  theme_fun = ft_theme
) {
  specs = as.formula(
    paste0("pairwise ~ ", species_var, " | ", date_var, " * ", year_var)
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
      con1 = str_split_fixed(contrast, "-", n = 2)[, 1] %>%
        str_remove_all("\\s"),
      con2 = str_split_fixed(contrast, "-", n = 2)[, 2] %>%
        str_remove_all("\\s"),
      ident1 = paste0(con1, year, date_fac),
      ident2 = paste0(con2, year, date_fac),
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
    rename(`Response FASY` = any_of(c("emmean", "response"))) |>
    left_join(emmean %>% select(-ident1), by = "ident2") %>%
    rename(`Response FREX` = any_of(c("emmean", "response"))) |>
    mutate(
      `Diff. (%)` = (`Response FASY` - `Response FREX`) / `Response FASY`,
    ) %>%
    rename(`t.ratio` = any_of(c("t.ratio", "z.ratio"))) |>
    select(
      # !!sym(species_var),
      !!sym(year_var),
      date_fac,
      # con1,
      # con2,
      `Response FASY`,
      `Response FREX`,
      estimate,
      `Diff. (%)`,
      df,
      t.ratio,
      p.value
    ) %>%
    mutate(
      across(
        c(estimate, `Diff. (%)`, `Response FASY`, `Response FREX`, t.ratio),
        ~ f.round(.x, digits)
      ),
      p.value = f.p_value_bins(p.value)
    ) %>%
    rename(
      # Species = !!sym(species_var),
      Year = !!sym(year_var),
      Date = date_fac,
      Contrast = estimate,
    )
  # filter(!is.na(Species))

  ft <- tbl %>%
    flextable() %>%
    merge_v(j = c("Year", "Date")) %>%
    hline() %>%
    vline(j = 1:ncol(tbl)) %>%
    theme_fun() %>%
    set_caption(caption_txt) %>%
    fix_border_issues()

  ft
}
