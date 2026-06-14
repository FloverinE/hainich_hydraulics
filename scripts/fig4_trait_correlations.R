# setup -------------------------------------------------------------------

library(tidyverse)
library(GGally)

source("scripts/ggplot_themes.R")

# set locale to US
Sys.setlocale("LC_TIME", "en_US.UTF-8")

plot_specs = list(
  "plot_position_offset" = 5,
  "plot_alpha" = 0.5
)

## reletter compact letter display from top left to bottom right of graps
f.reletter_cld = function(cld_column) {
  cld_column = cld_column |>
    str_remove_all("\\s")

  cld_letters = cld_column |>
    paste(collapse = "") |>
    strsplit("") |>
    unlist() |>
    unique()

  reletter_mat = data.frame(
    old = cld_letters,
    proxy = 1:length(cld_letters) |> as.character(),
    new = letters[1:length(cld_letters)]
  )
  reletter_mat

  for (i in 1:nrow(reletter_mat)) {
    cld_column = cld_column |>
      str_replace_all(reletter_mat$old[i], reletter_mat$proxy[i])
  }
  for (i in 1:nrow(reletter_mat)) {
    cld_column = cld_column |>
      str_replace_all(reletter_mat$proxy[i], reletter_mat$new[i])
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

df.hydr_traits_corr = readxl::read_excel(
  "data/calculated_parameters/df_hydr_traits.xlsx",
  sheet = "df_hydr_traits_subsel"
) |>
  select(year:t95_degc) |>
  # filter(vessel_order == "major") |>
  mutate(
    t5_degc = as.numeric(t5_degc),
    t50_degc = as.numeric(t50_degc),
    t95_degc = as.numeric(t95_degc)
  ) |>
  select(-vessel_order) |>
  distinct()

## aggregate per date
df.hydr_traits_corr_mean = df.hydr_traits_corr |>
  select(-sample_id) |>
  # filter(species == "FREX") |>
  group_by(species, date, year) |>
  mutate(across(c(psi_tlp_mpa:t95_degc), ~ mean(.x, na.rm = T)))

f.lm_in_ggpairs <- function(data, mapping, ...) {
  ggplot(data, mapping) +
    geom_point(size = 0.7) +
    geom_smooth(formula = y ~ x, method = "lm", aes(col = species)) +
    see::scale_color_oi(order = c(6, 2))
}

f.den_in_ggpairs <- function(data, mapping, ...) {
  ggplot(data, mapping) +
    geom_density(aes(col = species), fill = NA, size = 1) +
    see::scale_color_oi(order = c(6, 2)) +
    theme(legend.position = "none")
}

f.cor_by_species <- function(data, mapping, ...) {
  x <- rlang::as_name(mapping$x)
  y <- rlang::as_name(mapping$y)

  cor_df <- data |>
    dplyr::group_by(species) |>
    dplyr::summarise(
      r = cor(.data[[x]], .data[[y]], use = "complete.obs"),
      p = cor.test(.data[[x]], .data[[y]])$p.value,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      sig = dplyr::case_when(
        p < 0.001 ~ "***",
        p < 0.01 ~ "**",
        p < 0.05 ~ "*",
        TRUE ~ ""
      ),
      label = paste0(species, ": ", sprintf("%.2f", r), sig),
      ypos = dplyr::row_number()
    )

  ggplot(cor_df, aes(0.82, ypos)) + # move text to left side
    geom_text(
      aes(label = label, colour = species),
      hjust = 0, # left align
      size = 4
    ) +
    scale_colour_manual(values = species_cols) +
    coord_cartesian(xlim = c(0.8, 1.2), ylim = c(0.5, nrow(cor_df) + 0.5)) +
    theme_void() +
    theme(legend.position = "none")
}

species_cols <- c(
  "FREX" = "#D55E00",
  "FASY" = "#56b4e9"
)

plot.corr_reduced_traits =
  ggpairs(
    df.hydr_traits_corr |>
      select(
        -c(
          "sample_id",
          "psi_midday_mpa",
          "psi_tlp_mpa",
          "capacitance_ft_tot",
          "p88_mpa",
          "t95_degc"
        )
      ),
    columns = 5:13,
    mapping = aes(col = species, fill = species),
    lower = list(continuous = f.lm_in_ggpairs),
    diag = list(continuous = f.den_in_ggpairs),
    upper = list(continuous = f.cor_by_species),
    columnLabels = c(
      # "Ψ<sub>FT</sub> (MPa)",
      "Ψ<sub>TLP</sub> (MPa)",
      "ε (MPa)",
      "RWC<sub>TLP</sub> (%)",
      "C<sub>TLP</sub> (% MPa<sup>-1</sup>)",
      "g<sub>min</sub> (mmol m<sup>-2</sup> s<sup>-1</sup>)",
      "P<sub>12</sub> (MPa)",
      "P<sub>50</sub> (MPa)",
      # "P<sub>88</sub> (MPa)",
      "T<sub>5</sub> (°C)",
      "T<sub>50</sub> (°C)"
      # "T<sub>95</sub> (°C)"
    )
  ) +
  thesis_theme +
  theme(strip.text = ggtext::element_markdown())
plot.corr_reduced_traits

ggsave(
  plot.corr_reduced_traits,
  filename = "publication_figures/fig4_reduced_traits.png",
  width = 30,
  height = 30,
  units = "cm"
)


plot.corr_all_traits.png = ggpairs(
  df.hydr_traits_corr_mean |>
    select(-c("psi_midday_mpa", "capacitance_ft_tot")),
  columns = 5:16,
  mapping = aes(col = species, fill = species),
  lower = list(continuous = f.lm_in_ggpairs),
  diag = list(continuous = f.den_in_ggpairs),
  upper = list(continuous = f.cor_by_species),
  columnLabels = c(
    "Ψ<sub>FT</sub> (MPa)",
    "Ψ<sub>TLP</sub> (MPa)",
    "ε (MPa)",
    "RWC<sub>TLP</sub> (%)",
    "C<sub>TLP</sub> (% MPa<sup>-1</sup>)",
    "g<sub>min</sub> (mmol m<sup>-2</sup> s<sup>-1</sup>)",
    "P<sub>12</sub> (MPa)",
    "P<sub>50</sub> (MPa)",
    "P<sub>88</sub> (MPa)",
    "T<sub>5</sub> (°C)",
    "T<sub>50</sub> (°C)",
    "T<sub>95</sub> (°C)"
  )
) +
  thesis_theme +
  theme(strip.text = ggtext::element_markdown())

plot.corr_all_traits.png

ggsave(
  plot.corr_all_traits.png,
  filename = "publication_figures/fig4_all_traits.png",
  width = 30,
  height = 30,
  units = "cm"
)


# corrmorant -------------------------------------------------------------

library(corrmorant)

test = cor.test(
  x = df.hydr_traits_corr$psi_tlp_mpa,
  y = df.hydr_traits_corr$elast_tot_mpa,
  method = "spearman"
)

test$estimate |> as.numeric()

f.spearman = function(x, y) {
  cor.test(x = x, y = y, method = "spearman")$estimate |>
    as.numeric() |>
    round(3)
}

f.spearman(df.hydr_traits_corr$psi_tlp_mpa, df.hydr_traits_corr$elast_tot_mpa)

df.corr <- df.hydr_traits_corr |>
  select(
    species,
    # psi_ft_mpa,
    psi_tlp_mpa,
    elast_tot_mpa,
    rwc_tlp_tot_perc,
    gmin_mmol_m2_s,
    p12_mpa,
    # p50_mpa,
    t5_degc,
    # t50_degc
  ) |>
  mutate(
    species = factor(species),
    Species = recode(
      species,
      "FASY" = "*Fagus sylvatica*",
      "FREX" = "*Fraxinus excelsior*"
    )
  ) |>
  rename(
    # "Ψ<sub>FT</sub> (MPa)" = psi_ft_mpa,
    "Ψ<sub>TLP</sub> (MPa)" = psi_tlp_mpa,
    "ε (MPa)" = elast_tot_mpa,
    "RWC<sub>TLP</sub> (%)" = rwc_tlp_tot_perc,
    "g<sub>min</sub> (mmol m<sup>-2</sup> s<sup>-1</sup>)" = gmin_mmol_m2_s,
    "P<sub>12</sub> (MPa)" = p12_mpa,
    # "P<sub>50</sub> (MPa)" = p50_mpa,
    "T<sub>5</sub> (°C)" = t5_degc,
    # "T<sub>50</sub> (°C)" = t50_degc
  )

fig4_trait_corr_mat.png = ggcorrm(
  df.corr,
  mapping = aes(col = Species, fill = Species)
) +
  lotri(geom_smooth(method = "lm")) +
  lotri(geom_point(alpha = 0.5)) +
  utri_corrtext(corr_method = "spearman", squeeze = 0.6, nrow = 2) +
  scale_color_manual(
    values = c(
      "*Fraxinus excelsior*" = "#56b4e9",
      "*Fagus sylvatica*" = "#D55E00"
    )
  ) +
  scale_fill_manual(
    values = c(
      "*Fraxinus excelsior*" = "#56b4e9",
      "*Fagus sylvatica*" = "#D55E00"
    )
  ) +
  # dia_names(y_pos = 0.15, size = 3) +
  dia_density(lower = 0, color = 1) +
  theme(
    # axis.title.x = ggtext::element_markdown(),
    # axis.title.y = ggtext::element_markdown(),
    strip.text.x.top = ggtext::element_markdown(size = 8),
    strip.text.y.right = ggtext::element_markdown(size = 8),
    strip.text = element_blank(),
    # axis.text = element_text(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.text = ggtext::element_markdown()
  )
fig4_trait_corr_mat.png

ggsave(
  "figures/fig4_trait_corr_mat.png",
  fig4_trait_corr_mat.png,
  width = 20,
  height = 20,
  units = "cm"
)


