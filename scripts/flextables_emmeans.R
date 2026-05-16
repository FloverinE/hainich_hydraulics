# setup -------------------------------------------------------------------

library(tidyverse)
library(flextable)

df.sugars_emm = read_csv("data/calculated_parameters/df_sugars_emmeans.csv")
df.nutrients_emm = read_csv("data/calculated_parameters/df_nutrients_emmeans.csv")
df.traits_emm = read_csv("data/calculated_parameters/df_hydr_traits_emmeans.csv") 

df.traits_emm = read_rds("output/emmeans/emm_hydr_traits.Rds") |>
  select(trait, data) |>
  unnest(cols = c(data)) |>
  left_join(
    df.hydr_traits_nest_mods |>
      select(trait, emm) |>
      unnest(cols = c(emm)),
    by = c("trait", "year", "species", "date_fac")
  ) |>
  mutate(year = as.factor(year), across(emmean:asymp.UCL, ~ round(.x, 3))) |>
  select(
    year,
    date_fac,
    species,
    sample_id,
    trait_value,
    emmean,
    SE,
    asymp.LCL,
    asymp.UCL,
    .group,
    trait
  ) |>
  rename(
    "Year" = "year",
    "Date" = "date_fac",
    "Species" = "species",
    "Tree ID" = "sample_id",
    "Meas. value" = "trait_value",
    "Est. mean" = "emmean",
    "Std. error" = "SE",
    "asymp. LCL" = "asymp.LCL",
    "asymp. UCL" = "asymp.UCL",
    "Sign. group" = ".group"
  )

source("scripts/ggplot_themes.R")

# trait tables ------------------------------------------------------------


## psi_midday --------------------------------------------------------------

ft_midday = df.traits_emm |> 
  filter(trait == "psi_midday_mpa") |> 
  distinct() |> 
  select(-trait)

flextable(ft_midday) |> 
  ft_theme() |> 
  save_as_docx(path = "output/tables/fig2/psi_midday.docx")

flextable::as_flextable(df.hydr_traits_nest_mods$lme[[2]], add.random = T) |> 
  ft_theme() |> 
  save_as_docx(path = "output/tables/lme_psi_midday.docx")

df.hydr_traits_nest_mods$lme[[2]] |> View()

df.hydr_traits_nest_mods$lme[[2]] |> summary()


# emmeans contrasts from fig2 script --------------------------------------

emmeans::pairs(df.hydr_traits_nest_mods$emm[[1]])

test = df.hydr_traits_nest_mods$emm_pairs[[2]]
pairs_test = test |> pairs() |> as.data.frame()
test |> multcomp::cld(Letters = letters) |> as.data.frame()

test = df.hydr_traits_nest_mods |> 
  select(trait, data) |> 
  unnest(cols = c(data)) |> 
  left_join(df.hydr_traits_nest_mods |>
              select(trait, emm) |> 
              unnest(cols = c(emm)), by = c("trait", "year", "species", "date_fac"))


