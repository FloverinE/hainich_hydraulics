# setup -------------------------------------------------------------------

library(tidyverse)
library(GGally)

source("scripts/ggplot_themes.R")

# set locale to US
Sys.setlocale("LC_TIME", "en_US.UTF-8")

plot_specs <- list(
  "plot_position_offset" = 5,
  "plot_alpha" = 0.5
)

## reletter compact letter display from top left to bottom right of graps
f.reletter_cld <- function(cld_column) {
  cld_column <- cld_column |>
    str_remove_all("\\s")

  cld_letters <- cld_column |>
    paste(collapse = "") |>
    strsplit("") |>
    unlist() |>
    unique()

  reletter_mat <- data.frame(
    old = cld_letters,
    proxy = 1:length(cld_letters) |> as.character(),
    new = letters(1:length(cld_letters))
  )
  reletter_mat

  for (i in 1:nrow(reletter_mat)) {
    cld_column <- cld_column |>
      str_replace_all(reletter_mat$old(i), reletter_mat$proxy(i))
  }
  for (i in 1:nrow(reletter_mat)) {
    cld_column <- cld_column |>
      str_replace_all(reletter_mat$proxy(i), reletter_mat$new(i))
  }
  cld_column |>
    map(
      ~ .x |>
        str_split("") |>
        unlist() |>
        sort() |>
        paste0(collapse = "")
    ) |>
    unlist()
}

# Seasonal plasticity - interspecific differences ------------------------

df.hydr_traits_corr <- readxl::read_excel(
  "data/calculated_parameters/df_hydr_traits.xlsx",
  sheet = "df_hydr_traits_subsel"
) |>
  select(year:t95_degc) |>
  # filter(vessel_order == "major") |>
  mutate(
    t5_degc = as.numeric(t5_degc),
    t50_degc = as.numeric(t50_degc),
    t95_degc = as.numeric(t95_degc),
    date = as.factor(date)
  ) |>
  select(-vessel_order) |>
  distinct() |>
  pivot_longer(
    c(psi_midday_mpa:t95_degc),
    names_to = "trait",
    values_to = "trait_value"
  )

## aggregate per date
df.hydr_traits_corr_mean <- df.hydr_traits_corr |>
  select(-sample_id) |>
  # filter(species == "FREX") |>
  group_by(species, date, year, trait) |>
  mutate(trait_value = mean(trait_value, na.rm = T))

f.lm_in_ggpairs <- function(data, mapping, ...) {
  ggplot(data, mapping) +
    geom_point(size = 0.7) +
    geom_smooth(formula = y ~ x, method = "lm", aes(col = species)) +
    see::scale_color_oi(order = c(6, 2))
}

# Nutrients and NSC ------------------------

df.sugars <- read.csv(
  "data/calculated_parameters/df_sugars_2023_2024.csv"
) |>
  mutate(
    nsc_nutrient = sugar_name,
    concentration = sugar_conc,
    identity = "sugar"
  ) |>
  select(sample_id, species, date, nsc_nutrient, concentration, identity)

df.nutrients <- read_csv(
  "data/calculated_parameters/df_nutrients_2023_2024.csv"
) |>
  mutate(
    nutrient_name = nutrient_name |> str_remove_all("_con"),
    nsc_nutrient = nutrient_name,
    concentration = nutrient_conc,
    identity = "nutrient",
    date = as.factor(date)
  ) |>
  select(sample_id, species, date, nsc_nutrient, concentration, identity)

df.nsc_nutrient <- rbind(df.sugars, df.nutrients)


# with nsc ---------------------------------------------------------------

df.hydr_traits_corr_nsc <- df.hydr_traits_corr |>
  left_join(df.nsc_nutrient, by = c("date", "species", "sample_id")) |>
  filter(
    !trait %in%
      c(
        "psi_midday_mpa",
        "psi_ft_mpa",
        "capacitance_ft_tot",
        "p88_mpa",
        "t95_degc"
      ),
    nsc_nutrient %in%
      c(
        "fructose_mg_g",
        "glucose_mg_g",
        "starch_mg_g",
        "sucrose_mg_g",
        "total_sugars_mg_g"
        # "c",
        # "n",
        # "ca_mg_kg",
        # "k_mg_kg",
        # "mg_mg_kg",
        # "p_mg_kg",
        # "s_mg_kg"
      )
  ) |>
  mutate(
    trait_label = case_when(
      trait ==
        # "Ψ<sub>FT</sub> (MPa)",
        "psi_tlp_mpa" ~ "Ψ<sub>TLP</sub> (MPa)",
      trait == "elast_tot_mpa" ~ "ε (MPa)",
      trait == "rwc_tlp_tot_perc" ~ "RWC<sub>TLP</sub> (%)",
      trait == "capacitance_tlp_tot" ~ "C<sub>TLP</sub> (% MPa<sup>-1</sup>)",
      trait ==
        "gmin_mmol_m2_s" ~ "g<sub>min</sub><br>(mmol m<sup>-2</sup> s<sup>-1</sup>)",
      trait == "p12_mpa" ~ "P<sub>12</sub> (MPa)",
      trait == "p50_mpa" ~ "P<sub>50</sub> (MPa)",
      # "P<sub>88</sub> (MPa)",
      trait == "t5_degc" ~ "T<sub>5</sub> (°C)",
      trait == "t50_degc" ~ "T<sub>50</sub> (°C)"
      # "T<sub>95</sub> (°C)"
    ) |>
      factor(),
    nsc_nutrient_label = case_when(
      nsc_nutrient == "fructose_mg_g" ~ "Fructose<br>mg g<sup>−1</sup>)",
      nsc_nutrient == "glucose_mg_g" ~ "Glucose<br>mg g<sup>−1</sup>)",
      nsc_nutrient == "starch_mg_g" ~ "Starch<br>mg g<sup>−1</sup>)",
      nsc_nutrient == "sucrose_mg_g" ~ "Sucrose<br>mg g<sup>−1</sup>)",
      nsc_nutrient == "total_sugars_mg_g" ~ "Total Sugars<br>mg g<sup>−1</sup>)"
      # "starch_sugar_ratio" = "Starch:Sucrose Ratio"
    ) |>
      factor(),
    Species = species |>
      as.factor() |>
      recode(
        "FASY" = "*Fagus sylvatica*",
        "FREX" = "*Fraxinus excelsior*"
      )
  )

plot.corr_trait_nsc.png <-
  df.hydr_traits_corr_nsc |>
  ggplot() +
  geom_point(aes(x = concentration, y = trait_value, col = Species)) +
  geom_smooth(
    aes(x = concentration, y = trait_value, col = Species, fill = Species),
    method = "lm",
    alpha = 0.3,
    show.legend = F
  ) +
  geom_smooth(
    aes(x = concentration, y = trait_value, col = Species, fill = Species),
    method = "lm",
    se = F,
    alpha = 1
  ) +
  see::scale_color_oi(order = c(6, 2)) +
  labs(y = NULL, x = NULL) +
  facet_grid(
    trait_label ~ nsc_nutrient_label,
    scales = "free",
    shrink = T,
    switch = "both",
  ) +
  thesis_theme +
  theme(
    strip.text.x.bottom = ggtext::element_markdown(),
    strip.text.y.left = ggtext::element_markdown(),
    strip.placement = "outside",
    legend.text = ggtext::element_markdown(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
plot.corr_trait_nsc.png

ggsave(
  "figures/fig7_corr_trait_nsc.png",
  plot.corr_trait_nsc.png,
  width = 20,
  height = 25,
  units = "cm"
)


# with nutrients ---------------------------------------------------------

df.hydr_traits_corr_nutrient <- df.hydr_traits_corr |>
  left_join(df.nsc_nutrient, by = c("date", "species", "sample_id")) |>
  filter(
    !trait %in%
      c(
        "psi_midday_mpa",
        "psi_ft_mpa",
        "capacitance_ft_tot",
        "p88_mpa",
        "t95_degc"
      ),
    nsc_nutrient %in%
      c(
        "c",
        "n",
        "ca_mg_kg",
        "k_mg_kg",
        "mg_mg_kg",
        "p_mg_kg",
        "s_mg_kg"
      )
  ) |>
  mutate(
    trait_label = case_when(
      trait ==
        # "Ψ<sub>FT</sub> (MPa)",
        "psi_tlp_mpa" ~ "Ψ<sub>TLP</sub> (MPa)",
      trait == "elast_tot_mpa" ~ "ε (MPa)",
      trait == "rwc_tlp_tot_perc" ~ "RWC<sub>TLP</sub> (%)",
      trait == "capacitance_tlp_tot" ~ "C<sub>TLP</sub> (% MPa<sup>-1</sup>)",
      trait ==
        "gmin_mmol_m2_s" ~ "g<sub>min</sub><br>(mmol m<sup>-2</sup> s<sup>-1</sup>)",
      trait == "p12_mpa" ~ "P<sub>12</sub> (MPa)",
      trait == "p50_mpa" ~ "P<sub>50</sub> (MPa)",
      # "P<sub>88</sub> (MPa)",
      trait == "t5_degc" ~ "T<sub>5</sub> (°C)",
      trait == "t50_degc" ~ "T<sub>50</sub> (°C)"
      # "T<sub>95</sub> (°C)"
    ) |>
      factor(),
    nsc_nutrient_label = case_when(
      nsc_nutrient == "c" ~ "Carbon<br>(%)",
      nsc_nutrient == "n" ~ "Nitrogen<br>(%)",
      nsc_nutrient == "ca_mg_kg" ~ "Calcium<br>(mg kg<sup>-1</sup>)",
      nsc_nutrient == "k_mg_kg" ~ "Potassium<br>(mg kg<sup>-1</sup>)",
      nsc_nutrient == "mg_mg_kg" ~ "Magnesium<br>(mg kg<sup>-1</sup>)",
      nsc_nutrient == "mg_mg_kg" ~ "Magnesium<br>(mg kg<sup>-1</sup>)",
      nsc_nutrient == "p_mg_kg" ~ "Phosphorus<br>(mg kg<sup>-1</sup>)",
      nsc_nutrient == "s_mg_kg" ~ "Sulfur<br>(mg kg<sup>-1</sup>)"
    ) |>
      factor(),
    Species = species |>
      as.factor() |>
      recode(
        "FASY" = "*Fagus sylvatica*",
        "FREX" = "*Fraxinus excelsior*"
      )
  )

plot.corr_trait_nutrients.png <-
  df.hydr_traits_corr_nutrient |>
  ggplot() +
  geom_point(aes(x = concentration, y = trait_value, col = Species)) +
  geom_smooth(
    aes(x = concentration, y = trait_value, col = Species, fill = Species),
    method = "lm",
    alpha = 0.3,
    show.legend = F
  ) +
  geom_smooth(
    aes(x = concentration, y = trait_value, col = Species, fill = Species),
    method = "lm",
    se = F,
    alpha = 1
  ) +
  see::scale_color_oi(order = c(6, 2)) +
  labs(y = NULL, x = NULL) +
  facet_grid(
    trait_label ~ nsc_nutrient_label,
    scales = "free",
    shrink = T,
    switch = "both",
  ) +
  thesis_theme +
  theme(
    strip.text.x.bottom = ggtext::element_markdown(),
    strip.text.y.left = ggtext::element_markdown(),
    strip.placement = "outside",
    legend.text = ggtext::element_markdown(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
plot.corr_trait_nutrients.png

ggsave(
  "figures/fig8_corr_trait_nutrients.png",
  plot.corr_trait_nutrients.png,
  width = 20,
  height = 25,
  units = "cm"
)


# Trait - NSC corr matrix ------------------------------------------------

df.corrmat_nsc <- df.hydr_traits_corr |>
  left_join(df.sugars, by = c("date", "species", "sample_id")) |>
  filter(
    !trait %in%
      c(
        "psi_midday_mpa",
        "psi_ft_mpa",
        "capacitance_ft_tot",
        "p88_mpa",
        "t95_degc"
      ),
    nsc_nutrient %in%
      c(
        "fructose_mg_g",
        "glucose_mg_g",
        "starch_mg_g",
        "sucrose_mg_g",
        "total_sugars_mg_g",
        "c",
        "n",
        "ca_mg_kg",
        "k_mg_kg",
        "mg_mg_kg",
        "p_mg_kg",
        "s_mg_kg"
      ),
  ) |>
  mutate(
    trait_label = case_when(
      trait ==
        # "Ψ<sub>FT</sub> (MPa)",
        "psi_tlp_mpa" ~ "Ψ<sub>TLP</sub> (MPa)",
      trait == "elast_tot_mpa" ~ "ε (MPa)",
      trait == "rwc_tlp_tot_perc" ~ "RWC<sub>TLP</sub> (%)",
      trait == "capacitance_tlp_tot" ~ "C<sub>TLP</sub> (% MPa<sup>-1</sup>)",
      trait ==
        "gmin_mmol_m2_s" ~ "g<sub>min</sub> (mmol m<sup>-2</sup> s<sup>-1</sup>)",
      trait == "p12_mpa" ~ "P<sub>12</sub> (MPa)",
      trait == "p50_mpa" ~ "P<sub>50</sub> (MPa)",
      # "P<sub>88</sub> (MPa)",
      trait == "t5_degc" ~ "T<sub>5</sub> (°C)",
      trait == "t50_degc" ~ "T<sub>50</sub> (°C)"
      # "T<sub>95</sub> (°C)"
    ) |>
      factor(),
    nsc_nutrient_label = case_when(
      nsc_nutrient == "fructose_mg_g" ~ "Fructose<br>(mg g<sup>−1</sup>)",
      nsc_nutrient == "glucose_mg_g" ~ "Glucose<br>(mg g<sup>−1</sup>)",
      nsc_nutrient == "starch_mg_g" ~ "Starch<br>(mg g<sup>−1</sup>)",
      nsc_nutrient == "sucrose_mg_g" ~ "Sucrose<br>(mg g<sup>−1</sup>)",
      nsc_nutrient ==
        "total_sugars_mg_g" ~ "Total Sugars<br>(mg g<sup>−1</sup>)",
      nsc_nutrient == "c" ~ "Carbon<br>(\u0025)",
      nsc_nutrient == "n" ~ "Nitrogen<br>(\u0025)",
      nsc_nutrient == "ca_mg_kg" ~ "Calcium<br> (mg kg<sup>-1</sup>)",
      nsc_nutrient == "k_mg_kg" ~ "Potassium<br> (mg kg<sup>-1</sup>)",
      nsc_nutrient == "mg_mg_kg" ~ "Magnesium<br> (mg kg<sup>-1</sup>)",
      nsc_nutrient == "mg_mg_kg" ~ "Magnesium<br> (mg kg<sup>-1</sup>)",
      nsc_nutrient == "p_mg_kg" ~ "Phosphorus<br> (mg kg<sup>-1</sup>)",
      nsc_nutrient == "s_mg_kg" ~ "Sulfur<br> (mg kg<sup>-1</sup>)"
    ) |>
      factor()
  ) |>
  group_by(
    trait,
    nsc_nutrient,
    trait_label,
    nsc_nutrient_label,
    identity,
    species
  ) |>
  summarise(
    cor = cor.test(concentration, trait_value, method = "spearman")$estimate,
    p_val = cor.test(concentration, trait_value, method = "spearman")$p.value
  )

plot.trait_nsc_corrmat.png <-
  df.corrmat_nsc |>
  ggplot(aes(x = nsc_nutrient_label, y = trait_label, fill = cor)) +
  geom_tile() +
  geom_text(aes(
    label = paste(sprintf("%.2f", cor), "\np = ", sprintf("%.2f", p_val))
  )) +
  scale_fill_gradient2(
    low = "blue",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation (Trait ~ NSC)"
  ) +
  labs(y = NULL, x = NULL) +
  facet_wrap(~species) +
  thesis_theme +
  theme(
    axis.text.y.left = ggtext::element_markdown(),
    axis.text.x.bottom = ggtext::element_markdown()
  )

plot.trait_nsc_corrmat.png

ggsave(
  "figures/fig7_trait_nsc_corrmat.png",
  plot.trait_nsc_corrmat.png,
  width = 25,
  height = 25,
  units = "cm"
)

# Trait - Nutrient corr matrix ---------------------------------------------

df.corrmat_nutrients <- df.hydr_traits_corr |>
  left_join(df.nutrients, by = c("date", "species", "sample_id")) |>
  filter(
    !trait %in%
      c(
        "psi_midday_mpa",
        "psi_ft_mpa",
        "capacitance_ft_tot",
        "p88_mpa",
        "t95_degc"
      ),
    nsc_nutrient %in%
      c(
        "fructose_mg_g",
        "glucose_mg_g",
        "starch_mg_g",
        "sucrose_mg_g",
        "total_sugars_mg_g",
        "c",
        "n",
        "ca_mg_kg",
        "k_mg_kg",
        "mg_mg_kg",
        "p_mg_kg",
        "s_mg_kg"
      ),
  ) |>
  mutate(
    trait_label = case_when(
      trait ==
        # "Ψ<sub>FT</sub> (MPa)",
        "psi_tlp_mpa" ~ "Ψ<sub>TLP</sub> (MPa)",
      trait == "elast_tot_mpa" ~ "ε (MPa)",
      trait == "rwc_tlp_tot_perc" ~ "RWC<sub>TLP</sub> (%)",
      trait == "capacitance_tlp_tot" ~ "C<sub>TLP</sub> (% MPa<sup>-1</sup>)",
      trait ==
        "gmin_mmol_m2_s" ~ "g<sub>min</sub> (mmol m<sup>-2</sup> s<sup>-1</sup>)",
      trait == "p12_mpa" ~ "P<sub>12</sub> (MPa)",
      trait == "p50_mpa" ~ "P<sub>50</sub> (MPa)",
      # "P<sub>88</sub> (MPa)",
      trait == "t5_degc" ~ "T<sub>5</sub> (°C)",
      trait == "t50_degc" ~ "T<sub>50</sub> (°C)"
      # "T<sub>95</sub> (°C)"
    ) |>
      factor(),
    nsc_nutrient_label = case_when(
      nsc_nutrient == "fructose_mg_g" ~ "Fructose<br>(mg g<sup>−1</sup>)",
      nsc_nutrient == "glucose_mg_g" ~ "Glucose<br>(mg g<sup>−1</sup>)",
      nsc_nutrient == "starch_mg_g" ~ "Starch<br>(mg g<sup>−1</sup>)",
      nsc_nutrient == "sucrose_mg_g" ~ "Sucrose<br>(mg g<sup>−1</sup>)",
      nsc_nutrient ==
        "total_sugars_mg_g" ~ "Total Sugars<br>(mg g<sup>−1</sup>)",
      nsc_nutrient == "c" ~ "Carbon<br>(\u0025)",
      nsc_nutrient == "n" ~ "Nitrogen<br>(\u0025)",
      nsc_nutrient == "ca_mg_kg" ~ "Calcium<br> (mg kg<sup>-1</sup>)",
      nsc_nutrient == "k_mg_kg" ~ "Potassium<br> (mg kg<sup>-1</sup>)",
      nsc_nutrient == "mg_mg_kg" ~ "Magnesium<br> (mg kg<sup>-1</sup>)",
      nsc_nutrient == "mg_mg_kg" ~ "Magnesium<br> (mg kg<sup>-1</sup>)",
      nsc_nutrient == "p_mg_kg" ~ "Phosphorus<br> (mg kg<sup>-1</sup>)",
      nsc_nutrient == "s_mg_kg" ~ "Sulfur<br> (mg kg<sup>-1</sup>)"
    ) |>
      factor()
  ) |>
  group_by(
    trait,
    nsc_nutrient,
    trait_label,
    nsc_nutrient_label,
    identity,
    species
  ) |>
  summarise(
    cor = cor.test(concentration, trait_value, method = "spearman")$estimate,
    p_val = cor.test(concentration, trait_value, method = "spearman")$p.value
  )

plot.trait_nutrient_corrmat.png <-
  df.corrmat_nutrients |>
  ggplot(aes(x = nsc_nutrient_label, y = trait_label, fill = cor)) +
  geom_tile() +
  geom_text(aes(
    label = paste(sprintf("%.2f", cor), "\np = ", sprintf("%.2f", p_val))
  )) +
  scale_fill_gradient2(
    low = "blue",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation (Trait ~ NSC)"
  ) +
  labs(y = NULL, x = NULL) +
  facet_wrap(~species) +
  thesis_theme +
  theme(
    axis.text.y.left = ggtext::element_markdown(),
    axis.text.x.bottom = ggtext::element_markdown()
  )

plot.trait_nutrient_corrmat.png

ggsave(
  "figures/fig8_trait_nutrient_corrmat.png",
  plot.trait_nutrient_corrmat.png,
  width = 30,
  height = 27,
  units = "cm"
)
