# setup -------------------------------------------------------------------

library(tidyverse)
library(glmmTMB)
library(emmeans)
library(multcompView)
library(see)

source("scripts/ggplot_themes.R")

plot_specs = list(
  "lme_width_cm" = 12,
  "lme_height_cm" = 6,
  "plot_position_offset" = 5,
  "plot_alpha" = 0.5
)

# data --------------------------------------------------------------------

df.phenology = readxl::read_excel(
  "data/microclimate_new_2026/Hai_TowerCamera_Seasons_Summary.xlsx",
  sheet = "import_to_R"
)

df.hsm = readxl::read_excel(
  "data/calculated_parameters/df_hydr_traits.xlsx",
  sheet = "df_hydr_traits_subsel"
) |>
  select(
    year,
    species,
    doy_d,
    date_fac,
    sample_id,
    psi_midday_mpa,
    psi_tlp_mpa,
    p12_mpa,
    p50_mpa,
    p88_mpa
  ) |>
  distinct() |>
  left_join(df.phenology, by = c("year", "species")) |>
  mutate(
    doy_d = as.numeric(doy_d),
    leaf_age_d = doy_d - sos50_doy,
    year = as.factor(year)
  ) |>
  pivot_longer(
    cols = c(psi_tlp_mpa, p12_mpa, p50_mpa, p88_mpa),
    names_to = "threshold",
    values_to = "trait_value"
  ) |>
  mutate(
    hsm_mpa = psi_midday_mpa - trait_value,
    leaf_age_d = as.integer(leaf_age_d),
    date_fac = as.factor(date_fac)
  ) |>
  na.omit()

# model -------------------------------------------------------------------

df.hsm_nest = df.hsm |>
  nest(data = -c(threshold)) |>
  mutate(
    lme = map(
      data,
      ~ glmmTMB(
        hsm_mpa ~ species * date_fac * year + (1 | sample_id),
        data = .x,
        family = gaussian()
      )
    )
  )

df.hsm_nest = df.hsm_nest |>
  mutate(
    emm = map(
      lme,
      ~ emmeans(
        .x,
        ~ species + date_fac + year,
        adjust = "bonferroni",
        type = "response"
      )
    )
  )

df.hsm_nest = df.hsm_nest |>
  mutate(
    emm_plot = map(
      emm,
      ~ .x |> multcomp::cld(Letters = letters) |> as.data.frame(),
      emm_plot = map(
        emm,
        ~ .x |>
          mutate(
            Year = year,
            group = f.reletter_cld(.group)
          )
      )
    )
  )

# 3. unnest and export contrast table ------------------------------------

df.hsm_nest = df.hsm_nest |>
  mutate(
    data_emm = map2(
      .x = data,
      .y = emm_plot,
      ~ left_join(.x, .y, by = c("species", "date_fac"))
    )
  )

df.hsm_table = df.hsm_nest |>
  select(threshold, data_emm) |>
  unnest(data_emm) |>
  mutate(year = unique(c(year.x, year.y)) |> str_sort() |> first()) |>
  select(-c(year.x, year.y))

write_csv(
  df.hsm_table,
  "data/calculated_parameters/df_hsm_emmeans.csv"
)

write_rds(
  df.hsm_nest |> select(data, threshold, lme, emm, emm_plot),
  "output/emmeans/df_hsm_emmeans.Rds"
)


# 4. plot --------------------------------------------------------------------


# 4.1 plot over leaf age --------------------------------------------------

df.plot = df.hsm_nest |>
  select(threshold, emm) |>
  unnest(cols = emm) |>
  left_join(
    df.hsm |>
      select(threshold, date_fac, species, hsm_mpa, leaf_age_d) |>
      distinct(),
    by = c("date_fac", "species", "threshold")
  ) |>
  mutate(
    Species = recode(
      species,
      "FASY" = "*Fagus sylvatica*",
      "FREX" = "*Fraxinus excelsior*"
    ),
    Year = as.factor(year)
  )

trait_labels = c(
  "psi_tlp_mpa" = "Ψ<sub>TLP</sub> (MPa)",
  "p12_mpa" = "P<sub>12</sub> (MPa)",
  "p50_mpa" = "P<sub>50</sub> (MPa)"
)

plot.hsm.png = df.plot |>
  ggplot(aes(x = leaf_age_d, col = Species, linetype = Year)) +
  geom_hline(yintercept = 0) +
  geom_point(
    aes(y = hsm_mpa),
    alpha = plot_specs$plot_alpha,
    show.legend = T,
    size = 1
  ) +
  geom_line(
    aes(y = emmean),
    alpha = plot_specs$plot_alpha,
    show.legend = T,
    linewidth = 0.5
  ) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    alpha = plot_specs$plot_alpha,
    width = 3,
    linewidth = 0.5,
    show.legend = T
  ) +
  see::scale_color_oi(order = c(6, 2)) +
  scale_x_continuous(limits = c(0, 175), breaks = seq(0, 175, 25)) +
  facet_wrap(
    ~threshold,
    # scales = "free_y",
    strip.position = "top",
    labeller = labeller(threshold = as_labeller(trait_labels)),
  ) +
  lims(y = c(-1, 3.5)) +
  labs(
    y = "Hydraulic safety margin (MPa)",
    x = "Days since full leaf expansion"
  ) +
  thesis_theme +
  theme(
    strip.text = ggtext::element_markdown(),
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.text = ggtext::element_markdown(),
  )

plot.hsm.png

ggsave(
  "publication_figures/fig3_hsm.png",
  plot.hsm.png,
  width = 20,
  height = 12,
  units = "cm"
)


# 4.2 plot over date --------------------------------------------------


df.plot = df.hsm_nest |>
  select(threshold, emm_plot) |>
  unnest(cols = emm_plot) |>
  left_join(
    df.hsm |>
      select(threshold, date_fac, species, hsm_mpa, leaf_age_d) |>
      distinct(),
    by = c("date_fac", "species", "threshold")
  ) |>
  mutate(
    Date = as.Date(date_fac),
    Species = recode(
      species,
      "FASY" = "*Fagus sylvatica*",
      "FREX" = "*Fraxinus excelsior*"
    ),
    Year = as.factor(year)
  ) |> 
  filter(threshold != "p88_mpa") 

trait_labels = c(
  "psi_tlp_mpa" = "Ψ<sub>TLP</sub> (MPa)",
  "p12_mpa" = "P<sub>12</sub> (MPa)",
  "p50_mpa" = "P<sub>50</sub> (MPa)"
)

year(df.plot$Date) = 2000

plot.hsm.png = df.plot |>
  ggplot(aes(x = Date, col = Species, linetype = Year)) +
  geom_hline(yintercept = 0) +
  geom_point(
    aes(y = hsm_mpa),
    alpha = plot_specs$plot_alpha,
    show.legend = T,
    position = position_dodge(width = 2),
    size = 1
  ) +
  geom_line(
    aes(y = emmean),
    alpha = plot_specs$plot_alpha,
    show.legend = T,
    position = position_dodge(width = 2),
    linewidth = 0.5
  ) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    alpha = plot_specs$plot_alpha,
    width = 3,
    linewidth = 0.5,
    position = position_dodge(width = 2),
    show.legend = T
  ) +
  see::scale_color_oi(order = c(6, 2)) +
  scale_x_date(limits = as.Date(c("2000-05-15", "2000-09-30"))) +
  scale_y_continuous(limits = c(-1.5, 3.5), breaks = seq(-1.5, 3.5, 0.5)) +
  facet_wrap(
    ~threshold,
    # scales = "free_y",
    strip.position = "top",
    labeller = labeller(threshold = as_labeller(trait_labels)),
  ) +

  labs(
    y = "Hydraulic safety margin (MPa)",
    x = "Date"
  ) +
  thesis_theme +
  theme(
    strip.text = ggtext::element_markdown(),
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.text = ggtext::element_markdown(),
  )

plot.hsm.png

ggsave(
  "figures/fig3_hsm_date.png",
  plot.hsm.png,
  width = 20,
  height = 12,
  units = "cm"
)

