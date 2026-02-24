# setup -------------------------------------------------------------------
{
  library(lme4)
  library(MASS)
  library(tidyverse)
  library(nlme)
  library(glmmTMB)
  library(emmeans)
  library(performance)
  library(flextable)
  library(see)
  library(cowplot)
  library(patchwork)
  library(sjPlot)
  library(modelr)

  ## prefer select dplyr
  select <- dplyr::select
}

source("scripts/ggplot_themes.R")

# set locale to US
Sys.setlocale("LC_TIME", "en_US.UTF-8")

plot_position_offset = 5
plot_alpha = 0.5

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


# 1. Seasonal plasticity - interspecific differences ------------------------

# 1.1 Midday water potentials -------------------------------------------------

## 1.1.1 prepare -------------------------------------------------------------

df_psi_midday <- read.csv("data/calculated_parameters/df_psi_midday.csv") |>
  mutate(
    date = as.Date(date),
    doy = yday(date),
    date_fac = as.factor(date),
    species = as.factor(species),
    sample_id = as.factor(sample_id),
    campaign = as.factor(campaign),
    year = as.factor(year)
  ) |>
  dplyr::select(-c(psi_1, psi_2))


## 1.1.2 lme ------------------------------------------------

lme_psi_midday <- lmer(
  psi_midday_mpa ~ species * date_fac + (1 | sample_id),
  data = df_psi_midday
)

lme_psi_midday |> summary()
lme_psi_midday |> r2()
lme_psi_midday |> AIC()

performance::check_model(lme_psi_midday)

df_psi_midday |>
  ggplot() +
  geom_point(aes(x = doy, y = psi_midday_mpa, color = species)) +
  geom_line(aes(
    x = doy,
    y = predict(lme_psi_midday),
    color = species,
    group = interaction(year, sample_id),
    lty = year
  )) +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme


## 1.1.3 emmeans ------------------------------------------------

emm_psi_midday <- emmeans(
  lme_psi_midday,
  ~ species * date_fac,
  adjust = "bonferroni",
  type = "response"
)

emm_psi_midday

df_emm_psi_midday <- emm_psi_midday |>
  multcomp::cld(Letters = letters) |>
  as.data.frame()

df_emm_psi_midday

## 1.1.4 plot --------------------------------------------------------------

df_psi_midday_emmeans <- df_psi_midday |>
  left_join(df_emm_psi_midday, by = c("species", "date_fac")) |>
  mutate(
    group_y = case_when(species == "FASY" ~ 0, species == "FREX" ~ -4),
    date_plot = date
  )
year(df_psi_midday_emmeans$date_plot) = 2000

plot.psi_midday.png <- df_psi_midday_emmeans |>
  arrange(species, campaign, year) |>
  mutate(
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group = f.reletter_cld(.group)
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = psi_midday_mpa, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 0.2,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  scale_color_oi(order = c(6, 2)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  lims(y = c(-4, 0)) +
  labs(y = "Midday water potential [MPa]", x = "Date") +
  guides(color = guide_legend(title = "Species")) +
  facet_wrap(~year) +
  thesis_theme +
  theme(legend.text = element_text(size = 10, face = "italic"))
plot.psi_midday.png

ggsave(
  "figures/psi_midday/plot.psi_midday_datefac.png",
  plot.psi_midday.png,
  width = 8,
  height = 4,
  dpi = 150
)

df_psi_midday_emmeans |>
  arrange(psi_midday_mpa) |>
  dplyr::select(c(
    species,
    year,
    sample_id,
    campaign,
    emmean,
    psi_midday_mpa
  )) |>
  mutate(psi_midday_mpa = psi_midday_mpa |> round(1)) |>
  View()

df_psi_midday_emmeans |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(psi_midday_mpa) |> round(1),
    sd = sd(psi_midday_mpa) |> round(1),
    emmean = emmean
  ) |>
  View()

plot_model(lme_psi_midday)

## 1.1.5 export ------------------------------------------------------------------

df_emm_psi_midday |>
  arrange(year, species, date_fac) |>
  mutate(.group = f.reletter_cld(.group)) |>
  write.table("clipboard", sep = "\t", row.names = F)

lme_psi_midday |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F)


# 1.2 Pressure-Volume ---------------------------------------------------------------

## 1.2.1 prepare ------------------------------------------------------------
df_pv_params <- read.csv("data/calculated_parameters/df_pv_params_summ.csv") |>
  mutate(
    date = as.Date(date),
    date_fac = as.factor(date),
    species = as.factor(species),
    sample_id = as.factor(sample_id),
    campaign = as.factor(campaign),
    year = as.factor(year),
    pheno_start_doy = case_when(
      species == "FASY" & year == 2023 ~ 121,
      species == "FASY" & year == 2024 ~ 104,
      species == "FREX" & year == 2023 ~ 140,
      species == "FREX" & year == 2024 ~ 138
    ),
    doy = yday(date),
    leaf_age_d = doy - pheno_start_doy
  )

## 1.2.2 lme ---------------------------------------------------------------------

lme_psi_ft <- lmer(
  psi_ft ~ species * date_fac * year + (1 | sample_id),
  data = df_pv_params
)

lme_psi_ft |> summary()
lme_psi_ft |> r2()

ggplot(df_pv_params) +
  geom_point(aes(x = doy, y = psi_ft, color = species)) +
  # geom_line(aes(x = doy, y = psi_ft, color = species, group = sample_id)) +
  geom_line(aes(
    x = doy,
    y = predict(lme_psi_ft),
    color = species,
    group = sample_id
  )) +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme

# lme_psi_ft23 <- lmer(psi_ft ~ species * date_fac  + (1 | sample_id),
#                    data = df_pv_params |> filter(year == "2023"))
#
# lme_psi_ft23 |> summary()
# lme_psi_ft23 |> r2()
#
# df_pv_params |> filter(year == "2023") |>
# ggplot() +
#   geom_point(aes(x = doy, y = psi_ft, color = species)) +
#   # geom_line(aes(x = doy, y = psi_ft, color = species, group = sample_id)) +
#   geom_line(aes(x = doy, y = predict(lme_psi_ft23), color = species, group = sample_id)) +
#   # facet_wrap(~ year, scales = "free_x") +
#   thesis_theme
#
#
# lme_psi_ft24 <- lmer(psi_ft ~ species * date_fac  + (1 | sample_id),
#                      data = df_pv_params |> filter(year == "2024"))
#
# lme_psi_ft24 |> summary()
# lme_psi_ft24 |> r2()
#
# df_pv_params |> filter(year == "2024") |>
#   ggplot() +
#   geom_point(aes(x = doy, y = psi_ft, color = species)) +
#   # geom_line(aes(x = doy, y = psi_ft, color = species, group = sample_id)) +
#   geom_line(aes(x = doy, y = predict(lme_psi_ft24), color = species, group = sample_id)) +
#   # facet_wrap(~ year, scales = "free_x") +
#   thesis_theme

lme_psi_tlp <- lmer(
  psi_tlp ~ species * date_fac * year + (1 | sample_id),
  data = df_pv_params
)

lme_psi_tlp |> summary()
lme_psi_tlp |> r2()
lme_psi_tlp |> AIC()

ggplot(df_pv_params) +
  geom_point(aes(x = date, y = psi_tlp, color = species)) +
  geom_line(aes(
    x = date,
    y = predict(lme_psi_tlp),
    color = species,
    group = sample_id
  )) +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme

# nlme_psi_tlp <- nlme(psi_tlp ~ b0 + b1 * exp(b2 * leaf_age_d),
#                      data = df_pv_params |> filter(species == "FREX"),
#                      fixed = b0 + b1 + b2 ~ 1,
#                      random = b0 ~ 1 | year/sample_id,
#                      start = c(b0 = -1, b1 = 0.5, b2 = -0.05),
#                      verbose = T,
#                      control = list(maxIter = 1000, msMaxIter = 1000, msVerbose = TRUE)
# )
#
# nlme_psi_tlp |> summary()
#
# ggplot(df_pv_params |> filter(species == "FREX") ) +
#   geom_point(aes(x = leaf_age_d, y = psi_tlp)) +
#   geom_line(aes(x = leaf_age_d, y = predict(nlme_psi_tlp), group = sample_id)) +
#   facet_wrap(~ year, scales = "free_x") +
#   thesis_theme

lme_elast <- lmer(
  elast_tot ~ species * date_fac * year + (leaf_age_d | sample_id),
  data = df_pv_params
)

lme_elast |> summary()
lme_elast |> r2()

ggplot(df_pv_params) +
  geom_point(aes(x = date, y = elast_tot, color = species)) +
  geom_line(aes(
    x = date,
    y = predict(lme_elast),
    color = species,
    group = sample_id
  )) +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme

lme_rwc_tlp <- lmer(
  rwc_tot_tlp ~ species * date_fac * year + (1 | year / sample_id),
  data = df_pv_params
)
lme_rwc_tlp |> summary()
lme_rwc_tlp |> r2()

ggplot(df_pv_params) +
  geom_point(aes(x = date, y = rwc_tot_tlp, color = species)) +
  geom_line(aes(
    x = date,
    y = predict(lme_rwc_tlp),
    color = species,
    group = sample_id
  )) +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme

lme_cap_ft_tot <- lmer(
  capacitance_ft_tot ~ species * date_fac * year + (1 | sample_id),
  data = df_pv_params
)
lme_cap_ft_tot |> summary()
lme_cap_ft_tot |> r2()

lme_cap_ft_sym <- lmer(
  capacitance_ft_sym ~ species * date_fac * year + (1 | sample_id),
  data = df_pv_params
)
lme_cap_ft_sym |> summary()
lme_cap_ft_sym |> r2()

lme_cap_tlp_tot <- lmer(
  capacitance_tlp_tot ~ species * date_fac * year + (1 | sample_id),
  data = df_pv_params
)
lme_cap_tlp_tot |> summary()
lme_cap_tlp_tot |> r2()

lme_cap_tlp_sym <- lmer(
  capacitance_tlp_sym ~ species * date_fac * year + (1 | sample_id),
  data = df_pv_params
)
lme_cap_tlp_sym |> summary()
lme_cap_tlp_sym |> r2()

## symplastic capacitance at full turgor
ggplot(df_pv_params) +
  geom_point(aes(x = date, y = capacitance_ft_tot, color = species)) +
  geom_line(aes(
    x = date,
    y = predict(lme_cap_ft_tot),
    color = species,
    group = sample_id
  )) +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme

## total capacitance at full turgor
ggplot(df_pv_params) +
  geom_point(aes(x = date, y = capacitance_tlp_sym, color = species)) +
  geom_line(aes(
    x = date,
    y = predict(lme_cap_tlp_sym),
    color = species,
    group = sample_id
  )) +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme

## ratio of total / symplastic capacitance
## => in 2023, ash cap. ratio increased except for the first campaign
## => in 2024, ash cap. ratio decreased while it increased for beech
ggplot(df_pv_params) +
  geom_point(aes(
    x = date,
    y = capacitance_ft_tot / capacitance_tlp_tot,
    color = species
  ))

df_pv_params$capacitance_ft_tot / df_pv_params$capacitance_tlp_tot

## 1.2.3 emmeans ----------------------------------------------------------------

emm_psi_ft <- emmeans(
  lme_psi_ft,
  ~ species * date_fac * year,
  adjust = "mvt",
  trans = "response"
)

df_emm_psi_ft <- emm_psi_ft |>
  multcomp::cld(Letters = letters) |>
  as.data.frame()
df_emm_psi_ft

emm_psi_tlp <- emmeans(
  lme_psi_tlp,
  ~ species * date_fac * year,
  adjust = "mvt",
  type = "response"
)

df_emm_psi_tlp <- emm_psi_tlp |>
  multcomp::cld(Letters = letters) |>
  as.data.frame()

emm_elast <- emmeans(
  lme_elast,
  ~ species * date_fac * year,
  adjust = "mvt",
  type = "response"
)

df_emm_elast <- emm_elast |>
  multcomp::cld(Letters = letters) |>
  as.data.frame()

emm_rwc_tlp <- emmeans(
  lme_rwc_tlp,
  ~ species * date_fac * year,
  adjust = "mvt",
  type = "response"
)

df_emm_rwc_tlp <- emm_rwc_tlp |>
  multcomp::cld(Letters = letters) |>
  as.data.frame()


## capacitance

## 1.2.4 plot --------------------------------------------------------------

#### psi_ft ----

df_emm_psi_ft <- df_emm_psi_ft |>
  left_join(
    df_pv_params |> dplyr::select(species, date_fac, year, date, psi_ft),
    by = c("species", "date_fac", "year")
  ) |>
  mutate(
    group_y = case_when(species == "FASY" ~ 0, species == "FREX" ~ -4),
    date_plot = date
  )
year(df_emm_psi_ft$date_plot) = 2000

plot.psi_ft.png <- df_emm_psi_ft |>
  arrange(species, date_fac, year) |>
  mutate(
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group = f.reletter_cld(.group)
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = psi_ft, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 0.2,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = F
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  scale_color_oi(order = c(6, 2)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  lims(y = c(-4, 0)) +
  labs(y = expression(Psi[ft] ~ "[MPa]"), x = "Date", tag = "a)") +
  guides(color = guide_legend(title = "Species")) +
  facet_wrap(~year) +
  thesis_theme +
  theme(strip.background = element_blank())
plot.psi_ft.png

ggsave(
  "figures/pv/plot.psi_ft.png",
  plot.psi_ft.png,
  width = 8,
  height = 4,
  dpi = 150
)

df_emm_psi_ft |>
  arrange(psi_midday_mpa) |>
  select(c(species, year, sample_id, campaign, emmean, psi_midday_mpa)) |>
  mutate(psi_midday_mpa = psi_midday_mpa |> round(1)) |>
  View()

df_emm_psi_ft |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(psi_ft) |> round(1),
    sd = sd(psi_ft) |> round(1),
    emmean = emmean
  ) |>
  View()

#### psi_tlp ----

df_emm_psi_tlp <- df_emm_psi_tlp |>
  left_join(
    df_pv_params |> dplyr::select(species, date_fac, year, date, psi_tlp),
    by = c("species", "date_fac", "year")
  ) |>
  mutate(
    group_y = case_when(species == "FASY" ~ 0, species == "FREX" ~ -4),
    date_plot = date
  )

year(df_emm_psi_tlp$date_plot) = 2000

plot.psi_tlp.png <- df_emm_psi_tlp |>
  arrange(species, date_fac, year) |>
  mutate(
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group = f.reletter_cld(.group)
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = psi_tlp, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 0.2,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = F
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  scale_color_oi(order = c(6, 2)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  lims(y = c(-4, 0)) +
  labs(y = expression(Psi[tlp] ~ "[MPa]"), x = "Date", tag = "b)") +
  guides(color = guide_legend(title = "Species")) +
  facet_wrap(~year) +
  thesis_theme +
  theme(strip.background = element_blank())
plot.psi_tlp.png

plot.psi_tlp.png <- df_emm_psi_tlp |>
  arrange(species, date_fac, year) |>
  mutate(
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group = f.reletter_cld(.group)
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = psi_tlp, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 0.2,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  scale_color_oi(order = c(6, 2)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  lims(y = c(-4, 0)) +
  labs(y = expression(Psi[tlp] ~ "[MPa]"), x = "Date") +
  guides(color = guide_legend(title = "Species")) +
  facet_wrap(~year) +
  thesis_theme +
  theme(legend.text = element_text(size = 10, face = "italic"))
plot.psi_tlp.png

ggsave(
  "figures/pv/plot.psi_tlp_datefac.png",
  plot.psi_tlp.png,
  width = 8,
  height = 4,
  dpi = 150
)

df_emm_psi_tlp |>
  arrange(psi_tlp) |>
  select(c(species, year, sample_id, campaign, emmean, psi_tlp)) |>
  mutate(psi_tlp = psi_tlp |> round(1)) |>
  View()

df_emm_psi_tlp |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(psi_tlp) |> round(1),
    sd = sd(psi_tlp) |> round(1),
    emmean = emmean
  ) |>
  View()

#### elast ----

df_emm_elast <- df_emm_elast |>
  left_join(
    df_pv_params |>
      dplyr::select(species, campaign, year, date, elasticity_tot),
    by = c("species", "campaign", "year")
  ) |>
  mutate(date_plot = date)

year(df_emm_elast$date_plot) = 2000

plot.elast.png <- df_emm_elast |>
  arrange(species, campaign, year) |>
  mutate(
    group_y = case_when(species == "FASY" ~ 40, species == "FREX" ~ 0),
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group = f.reletter_cld(.group)
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = elasticity_tot, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 0.2,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = F
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  scale_color_oi(order = c(6, 2)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  lims(y = c(0, 40)) +
  labs(y = expression(epsilon ~ "[MPa]"), x = "Date", tag = "c)") +
  guides(color = guide_legend(title = "Species")) +
  facet_wrap(~year) +
  thesis_theme +
  theme(strip.background = element_blank())
plot.elast.png

ggsave(
  "figures/pv/plot.elast.png",
  plot.elast.png,
  width = 8,
  height = 4,
  dpi = 150
)

df_emm_elast |>
  arrange(elasticity_tot) |>
  select(c(species, year, campaign, emmean, elasticity_tot)) |>
  mutate(elasticity_tot = elasticity_tot |> round(1)) |>
  View()

df_emm_elast |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(elasticity_tot) |> round(1),
    sd = sd(elasticity_tot) |> round(1),
    emmean = emmean
  ) |>
  View()

#### rwc_tlp ----

df_emm_rwc_tlp <- df_emm_rwc_tlp |>
  left_join(
    df_pv_params |> dplyr::select(species, campaign, year, date, rwc_tot_tlp),
    by = c("species", "campaign", "year")
  ) |>
  mutate(date_plot = date)

year(df_emm_rwc_tlp$date_plot) = 2000

plot.rwc_tlp.png <- df_emm_rwc_tlp |>
  arrange(species, campaign, year) |>
  mutate(
    group_y = case_when(species == "FASY" ~ 100, species == "FREX" ~ 65),
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group = f.reletter_cld(.group)
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = rwc_tot_tlp, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset),
    show.legend = T
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 0.2,
    position = position_dodge(width = plot_position_offset),
    show.legend = T
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = T
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  scale_color_oi(order = c(6, 2)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  lims(y = c(65, 100)) +
  labs(y = expression(RWC[tlp] ~ "[%]"), x = "Date", tag = "d)") +
  guides(color = guide_legend(title = "Species")) +
  facet_wrap(~year) +
  thesis_theme +
  theme(
    strip.background = element_blank(),
    legend.text = element_text(face = "italic"),
    axis.title.y = element_text(size = 8),
    plot.tag.position = c(0.00, 0.95),
  )
plot.rwc_tlp.png

ggsave(
  "figures/pv/plot.rwc_tlp.png",
  plot.rwc_tlp.png,
  width = 8,
  height = 4,
  dpi = 150
)

df_emm_rwc_tlp |>
  arrange(rwc_tot_tlp) |>
  select(c(species, year, campaign, emmean, rwc_tot_tlp)) |>
  mutate(rwc_tot_tlp = rwc_tot_tlp |> round(1)) |>
  View()

df_emm_rwc_tlp |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(rwc_tot_tlp) |> round(1),
    sd = sd(rwc_tot_tlp) |> round(1),
    emmean = emmean
  ) |>
  View()

extra_theme_pv <- theme(
  axis.title.x = element_blank(),
  axis.title.y = element_text(size = 8),
  # strip.text = element_blank(),
  plot.tag.position = c(0.00, 0.95),
)

plot.all_PV.png <-
  plot.psi_ft.png +
  extra_theme_pv +
  plot.psi_tlp.png +
  extra_theme_pv +
  plot.elast.png +
  extra_theme_pv +
  plot.rwc_tlp.png +
  plot_layout(heights = c(5, 5, 5, 5))
plot.all_PV.png

ggsave(
  "figures/pv/plot.all_PV.png",
  plot.all_PV.png,
  width = 16,
  height = 23,
  dpi = 150,
  unit = "cm"
)

## 1.2.5 export ------------------------------------------------------------------
lme_psi_ft |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F)

df_emm_psi_ft |>
  arrange(year, species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  write.table("clipboard", sep = "\t", row.names = F)

lme_psi_ft |>
  r2() |>
  as.data.frame() |>
  write.table("clipboard", sep = "\t", row.names = F)

lme_psi_tlp |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F)

df_emm_psi_tlp |>
  arrange(year, species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  write.table("clipboard", sep = "\t", row.names = F)

lme_psi_tlp |>
  r2() |>
  as.data.frame() |>
  write.table("clipboard", sep = "\t", row.names = F)

lme_elast |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F)

df_emm_elast |>
  arrange(year, species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  write.table("clipboard", sep = "\t", row.names = F)

lme_elast |>
  r2() |>
  as.data.frame() |>
  write.table("clipboard", sep = "\t", row.names = F)

lme_rwc_tlp |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F)

df_emm_rwc_tlp |>
  arrange(year, species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  write.table("clipboard", sep = "\t", row.names = F)

lme_elast |>
  r2() |>
  as.data.frame() |>
  write.table("clipboard", sep = "\t", row.names = F)

# 1.3. Gmin -----------------------------------------------------------------

## 1.3.1 prepare -------------------------------------------------------------

df_gmin_params <- read.csv("data/calculated_parameters/df_gmin_params.csv") |>
  mutate(
    sample_id = as.factor(sample_id),
    species = sample_id |> str_sub(1, 4) |> as.factor(),
    campaign = as.factor(campaign),
    year = as.factor(year),
    gmin = gmin_mean,
    date = date |>
      as.Date(format = "%d.%m.%Y")
  ) |>
  mutate(
    date = case_when(
      date == "2023-07-19" ~ "2023-07-17" |> as.Date(),
      date == "2023-08-11" ~ "2023-08-09" |> as.Date(),
      T ~ date
    ),
    date_fac = date %>% as.factor()
  )

unique(df_gmin_params$date)

## 1.3.2 lme -----------------------------------------------------------------

glme_gmin <- glmmTMB::glmmTMB(
  gmin ~ species * date_fac * year + (1 | sample_id),
  data = df_gmin_params |> filter(rwc_interval == "standard"),
  family = Gamma(link = "log")
)
# glm_test |> summary()
emm_glm <- emmeans(
  glme_gmin,
  ~ species * date_fac * year,
  adjust = "mvt",
  type = "response"
)
emm_glm

df_gmin_params |>
  filter(rwc_interval == "standard") |>
  ggplot() +
  geom_point(aes(x = date, y = gmin, color = species)) +
  geom_line(aes(
    x = date,
    y = predict(glm_test, type = "response"),
    color = species,
    group = sample_id
  )) +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme


# lme_gmin_standard <- lmer(gmin ~ species * campaign * year + (1 | sample_id),
#                           data = df_gmin_params |> filter(rwc_interval == "standard"))
# lme_gmin_standard |> summary()
# lme_gmin_standard |> r2()

# lme_gmin_tlp <- lmer(gmin ~ species * campaign * year + (1 | sample_id),
#                      data = df_gmin_params |> filter(rwc_interval == "rwc_tlp_90"))
# lme_gmin_tlp |> summary()
# lme_gmin_tlp |> r2()
#
# df_gmin_params |> filter(rwc_interval == "rwc_tlp_90") |>
#   ggplot() +
#   geom_point(aes(x = date, y = gmin, color = species)) +
#   geom_line(aes(x = date, y = predict(lme_gmin_tlp), color = species, group = sample_id)) +
#   facet_wrap(~ year, scales = "free_x") +
#   thesis_theme

## 1.3.3 emmeans -----------------------------------------------------------------

emmeans_gmin_standard <- emmeans(
  glme_gmin,
  ~ species * date_fac * year,
  adjust = "mvt",
  type = "response"
)

df_emm_gmin_standard <- emmeans_gmin_standard |>
  multcomp::cld(Letters = letters) |>
  as.data.frame()

# emmeans_gmin_tlp <- emmeans(lme_gmin_tlp, ~ species * campaign * year,
#                             adjust = "mvt",
#                             type = "response")
#
# df_emm_gmin_tlp <- emmeans_gmin_tlp |>
#   multcomp::cld(Letters = letters) |>
#   as.data.frame()

## 1.3.4 plot ----------------------------------------------------------------

df_emm_gmin_standard <- df_emm_gmin_standard |>
  left_join(
    df_gmin_params |>
      filter(rwc_interval == "standard") |>
      dplyr::select(species, date_fac, year, date, gmin),
    by = c("species", "date_fac", "year")
  ) |>
  mutate(date_plot = date)
year(df_emm_gmin_standard$date_plot) = 2000

plot.gmin_standard.png <- df_emm_gmin_standard |>
  arrange(species, date_fac, year) |>
  mutate(
    group_y = case_when(species == "FASY" ~ 20, species == "FREX" ~ 0),
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group = f.reletter_cld(.group)
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = gmin, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_line(
    aes(x = date_plot, y = response, col = species),
    alpha = 0.2,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  scale_color_oi(order = c(6, 2)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  labs(
    y = expression(
      g[min] ~ "[" * mmol ~ m^{
        -2
      } ~ s^{
        -1
      } *
        "]"
    ),
    x = "Date"
  ) +
  lims(y = c(0, 20)) +
  guides(color = guide_legend(title = "Species")) +
  facet_wrap(~year) +
  thesis_theme +
  theme(
    strip.background = element_blank(),
    legend.text = element_text(face = "italic")
  )

plot.gmin_standard.png
ggsave(
  "figures/gmin/plot.gmin_datefac.png",
  plot.gmin_standard.png,
  width = 8,
  height = 4,
  dpi = 300
)

df_emm_gmin_standard |>
  arrange(gmin) |>
  dplyr::select(c(species, year, campaign, response, gmin)) |>
  mutate(gmin = gmin |> round(1)) |>
  View()

df_emm_gmin_standard |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(gmin) |> round(1),
    sd = sd(gmin) |> round(1),
    cv = sd(gmin) / mean(gmin) * 100 |> round(1),
    emmean = response
  ) |>
  View()


## 1.3.5 export ------------------------------------------------------------------

df_emm_gmin_standard |>
  arrange(year, species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  select(c(
    species,
    campaign,
    year,
    response,
    SE,
    df,
    asymp.LCL,
    asymp.UCL,
    .group
  )) |>
  distinct() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

glme_gmin |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

# 1.4. Optical vulnerability -----------------------------------------------------

## 1.4.1 prepare ------------------------------------------------------------

df_ov_params <- read.csv(
  "data/calculated_parameters/df_ov_params_updated.csv"
) |>
  mutate(
    date = as.Date(date, format = "%Y-%m-%d"),
    sample_id = as.factor(sample_id),
    species = sample_id |> str_sub(1, 4) |> as.factor(),
    campaign = as.factor(campaign),
    date_fac = as.factor(date),
    year = as.factor(year),
    p12 = p12_spline,
    p50 = p50_spline,
    p88 = p88_spline
  ) |>
  filter(year != 2023 | species != "FREX" | !campaign %in% c(1, 2, 3)) |>
  filter(vessel_order == "major")

## 1.4.2 lme -----------------------------------------------------------------

lme_p12 <- lmer(
  p12 ~ species * date_fac * year + (1 | sample_id),
  data = df_ov_params
)

lme_p50 <- lmer(
  p50 ~ species * date_fac * year + (1 | sample_id),
  data = df_ov_params
)

lme_p88 <- lmer(
  p88 ~ species * date_fac * year + (1 | sample_id),
  data = df_ov_params
)

## 1.4.3 emmeans ----

emmeans_p12 <- emmeans(
  lme_p12,
  ~ species * date_fac * year,
  adjust = "bonferroni",
  type = "response"
)

emmeans_p50 <- emmeans(
  lme_p50,
  ~ species * date_fac * year,
  adjust = "bonferroni",
  type = "response"
)

emmeans_p88 <- emmeans(
  lme_p88,
  ~ species * date_fac * year,
  adjust = "bonferroni",
  type = "response"
)


## 1.4.4 plot --------------------------------------------------------------

df_emm_p12 <- emmeans_p12 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  left_join(
    df_ov_params |>
      dplyr::select(species, date_fac, year, date, p12),
    by = c("species", "date_fac", "year")
  ) |>
  arrange(species, date_fac, year) |>
  mutate(
    species = dplyr::recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group = .group |> f.reletter_cld(),
    date_plot = date,
    group_y = case_when(
      species == "Fagus sylvatica" ~ 0,
      species == "Fraxinus excelsior" ~ -6
    )
  )

year(df_emm_p12$date_plot) = 2000


plot.p12.png <- df_emm_p12 |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = p12, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 0.2,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = F
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  scale_color_oi(order = c(6, 2)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  # labs(y = expression(P[12] ~ "[MPa]"), x = NULL, tag = "a)") +
  labs(y = expression(P[12] ~ "[MPa]"), x = "Date") +
  lims(y = c(-6, 0)) +
  guides(color = guide_legend(title = "Species")) +
  facet_wrap(~year) +
  thesis_theme +
  theme(strip.background = element_blank()) +
  theme(
    plot.tag.position = c(0.03, 0.95),
    legend.text = element_text(size = 8, face = "italic"),
    legend.justification = c(0.25, 0)
  )
plot.p12.png

ggsave(
  "figures/ov/plot.p12.png",
  plot.p12.png,
  width = 8,
  height = 4,
  dpi = 150
)

df_emm_p12 |>
  arrange(p12) |>
  select(c(species, year, campaign, emmean, p12)) |>
  mutate(p12 = p12 |> round(1)) |>
  View()

df_emm_p12 |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(p12) |> round(1),
    sd = sd(p12) |> round(1),
    emmean = emmean
  ) |>
  View()


df_emm_p50 <- emmeans_p50 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  left_join(
    df_ov_params |>
      dplyr::select(species, date_fac, year, date, p50),
    by = c("species", "date_fac", "year")
  ) |>
  arrange(species, date_fac, year) |>
  mutate(
    species = dplyr::recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group = .group |> f.reletter_cld(),
    date_plot = date,
    group_y = case_when(
      species == "Fagus sylvatica" ~ 0,
      species == "Fraxinus excelsior" ~ -6.5
    )
  )

year(df_emm_p50$date_plot) = 2000

plot.p50.png <- df_emm_p50 |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = p50, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset),
    show.legend = T
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 0.2,
    position = position_dodge(width = plot_position_offset),
    show.legend = T
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = T
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  scale_color_oi(order = c(6, 2)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  # labs(y = expression(P[12] ~ "[MPa]"), x = NULL, tag = "a)") +
  labs(y = expression(P[50] ~ "[MPa]"), x = "Date") +
  lims(y = c(-6.5, 0)) +
  guides(color = guide_legend(title = "Species")) +
  facet_wrap(~year) +
  thesis_theme +
  theme(strip.background = element_blank()) +
  thesis_theme +
  theme(
    strip.background = element_blank(),
    legend.text = element_text(face = "italic")
  )
plot.p50.png

ggsave(
  "figures/ov/plot.p50_datefac.png",
  plot.p50.png,
  width = 8,
  height = 4,
  dpi = 150
)

df_emm_p50 |>
  arrange(p50) |>
  select(c(species, year, campaign, emmean, p50)) |>
  mutate(p50 = p50 |> round(1)) |>
  View()

df_emm_p50 |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(p50) |> round(1),
    sd = sd(p50) |> round(1),
    emmean = emmean
  ) |>
  View()

df_emm_p88 <- emmeans_p88 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  left_join(
    df_ov_params |>
      dplyr::select(species, campaign, year, date, p88),
    by = c("species", "campaign", "year")
  ) |>
  arrange(species, campaign, year) |>
  mutate(
    species = dplyr::recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group = .group |> f.reletter_cld(),
    date_plot = date,
    group_y = case_when(
      species == "Fagus sylvatica" ~ 0,
      species == "Fraxinus excelsior" ~ -10
    )
  )

year(df_emm_p88$date_plot) = 2000

plot.p88.png <- df_emm_p88 |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = p88, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 0.2,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  scale_color_oi(order = c(6, 2)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  labs(y = expression(P[88] ~ "[MPa]"), x = "Date", tag = "c)") +
  lims(y = c(-10, 0)) +
  guides(color = guide_legend(title = "Species")) +
  facet_wrap(~year) +
  thesis_theme +
  theme(strip.background = element_blank()) +
  theme(
    plot.tag.position = c(0.03, 0.95),
    legend.text = element_text(size = 8, face = "italic"),
    legend.justification = c(0.25, 0)
  )
plot.p88.png

ggsave(
  "figures/ov/plot.p88.png",
  plot.p88.png,
  width = 8,
  height = 4,
  dpi = 150
)

df_emm_p88 |>
  arrange(p88) |>
  select(c(species, year, campaign, emmean, p88)) |>
  mutate(p88 = p88 |> round(1)) |>
  View()

df_emm_p88 |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(p88) |> round(1),
    sd = sd(p88) |> round(1),
    emmean = emmean
  ) |>
  View()

plot.p12p50p88_lme.png <-
  plot.p12.png +
  plot.p50.png +
  plot.p88.png +
  plot_layout(heights = c(6, 6, 6))

plot.p12p50p88_lme.png
ggsave(
  "figures/ov/plot.p12p50p88_lme.png",
  plot.p12p50p88_lme.png,
  width = 16,
  height = 18,
  dpi = 150,
  units = "cm"
)


## 1.4.5 export ------------------------------------------------------------------

df_emm_p12 |>
  arrange(year, species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  select(c(
    species,
    campaign,
    year,
    emmean,
    SE,
    df,
    lower.CL,
    upper.CL,
    .group
  )) |>
  distinct() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

lme_p12 |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

df_emm_p50 |>
  arrange(year, species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  select(c(
    species,
    campaign,
    year,
    emmean,
    SE,
    df,
    lower.CL,
    upper.CL,
    .group
  )) |>
  distinct() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

lme_p50 |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

df_emm_p88 |>
  arrange(year, species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  select(c(
    species,
    campaign,
    year,
    emmean,
    SE,
    df,
    lower.CL,
    upper.CL,
    .group
  )) |>
  distinct() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

lme_p50 |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

# 1.5. Thermal sensitivity --------------------------------------------------

## 1.5.1 prepare -----------------------------------------------------------

{
  df_tsm_params <- read.csv("data/calculated_parameters/df_tsm_params.csv") |>
    mutate(
      date = as.Date(date),
      date_fac = as.factor(date),
      sample_id = as.factor(sample_id),
      species = sample_id |> str_sub(1, 4) |> as.factor(),
      campaign = as.factor(campaign),
      year = as.factor(year)
    )
  year(df_tsm_params$date) = 2024
}

## 1.5.2 lme ----------------------------------------------------------------

lme_t5 <- lmer(
  estimate ~ species * date_fac + (1 | sample_id),
  data = df_tsm_params |> filter(trait == "T5")
)

lme_t50 <- lmer(
  estimate ~ species * date_fac + (1 | sample_id),
  data = df_tsm_params |> filter(trait == "T50")
)

lme_t95 <- lmer(
  estimate ~ species * date_fac + (1 | sample_id),
  data = df_tsm_params |> filter(trait == "T95")
)


## 1.5.3 emmeans ----------------------------------------------------------------

emmeans_t5 <- emmeans(
  lme_t5,
  ~ species * date_fac,
  adjust = "mvt",
  type = "response"
)

emmeans_t50 <- emmeans(
  lme_t50,
  ~ species * date_fac,
  adjust = "mvt",
  type = "response"
)

emmeans_t95 <- emmeans(
  lme_t95,
  ~ species * date_fac,
  adjust = "mvt",
  type = "response"
)

## 1.5.4 plot ----------------------------------------------------------------

df_emm_t5 <- emmeans_t5 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  mutate(trait = "T5", date_plot = date_fac %>% as.Date())
year(df_emm_t5$date_plot) = 2000

df_emm_t50 <- emmeans_t50 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  mutate(trait = "T50", date_plot = date_fac %>% as.Date())
year(df_emm_t50$date_plot) = 2000

df_emm_t95 <- emmeans_t95 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  mutate(trait = "T95", date_plot = date_fac %>% as.Date())
year(df_emm_t95$date_plot) = 2000

df_emm_tsm <- bind_rows(df_emm_t5, df_emm_t50, df_emm_t95)

df_emm_tsm <- df_emm_tsm |>
  left_join(
    df_tsm_params |>
      dplyr::select(species, sample_id, date_fac, year, date, estimate, trait),
    by = c("species", "date_fac", "trait")
  ) |>
  mutate(
    species = dplyr::recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    date_plot = as.Date(date),
    group_y = case_when(
      species == "Fagus sylvatica" ~ 65,
      species == "Fraxinus excelsior" ~ 30
    ),
    plot_number = case_when(
      trait == "T5" ~ "a)",
      trait == "T50" ~ "b)",
      trait == "T95" ~ "c)"
    ),
    trait = case_when(
      trait == "T5" ~ "T[5]",
      trait == "T50" ~ "T[50]",
      trait == "T95" ~ "T[95]"
    ),
  )
year(df_emm_tsm$date_plot) = 2000

plot.tsm.png <- df_emm_tsm |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = estimate, col = species),
    alpha = 0.5,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 0.2,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = .group, col = species),
    show.legend = F
  ) +
  # geom_text(aes(x = as.Date("2000-05-18"), y = 60, label = plot_number), size = 5) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  labs(y = "Temperature [°C]", x = "Date") +
  lims(y = c(30, 65)) +
  guides(color = guide_legend(title = "Species")) +
  scale_color_oi(order = c(6, 2)) +
  facet_grid(
    cols = vars(trait),
    scales = "free_x",
    space = "free_x",
    labeller = label_parsed
  ) + # Ensure columns are faceted by hsm_type
  thesis_theme +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 15),
    legend.text = element_text(face = "italic")
  )
plot.tsm.png

ggsave(
  "figures/tsm/plot.tsm_datefac.png",
  plot.tsm.png,
  width = 10,
  height = 4,
  dpi = 150
)


plot.t5.png <- df_emm_t5 |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = estimate, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = .group, col = species),
    show.legend = F
  ) +
  scale_color_manual(
    values = c("Fagus sylvatica" = "#410866", "Fraxinus excelsior" = "#faad22")
  ) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  scale_y_continuous(limits = c(30, 60), breaks = seq(30, 60, 5)) +
  labs(y = "T5 [°C]", x = "Date") +
  guides(color = guide_legend(title = "Species")) +
  thesis_theme +
  theme(strip.background = element_blank())
plot.t5.png

ggsave("figures/tsm/plot.t5.png", plot.t5.png, width = 6, height = 4, dpi = 150)

df_emm_t50 <- emmeans_t50 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  left_join(
    df_tsm_params |>
      filter(trait == "T50") |>
      dplyr::select(species, campaign, year, date, estimate),
    by = c("species", "campaign")
  ) |>
  mutate(
    species = dplyr::recode(
      species,
      "beech" = "Fagus sylvatica",
      "ash" = "Fraxinus excelsior"
    ),
    date_plot = as.Date(date),
    group_y = case_when(
      species == "Fagus sylvatica" ~ 60,
      species == "Fraxinus excelsior" ~ 30
    )
  )
year(df_emm_t50$date_plot) = 2000

plot.t50.png <- df_emm_t50 |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = estimate, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = .group, col = species),
    show.legend = F
  ) +
  scale_color_manual(
    values = c("Fagus sylvatica" = "#410866", "Fraxinus excelsior" = "#faad22")
  ) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  scale_y_continuous(limits = c(30, 60), breaks = seq(30, 65, 5)) +
  labs(y = "T50 [°C]", x = "Date") +
  guides(color = guide_legend(title = "Species")) +
  thesis_theme +
  theme(strip.background = element_blank())
plot.t50.png

ggsave(
  "figures/tsm/plot.t50.png",
  plot.t50.png,
  width = 6,
  height = 4,
  dpi = 150
)

df_emm_t50

df_emm_t95 <- emmeans_t95 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  left_join(
    df_tsm_params |>
      filter(trait == "T95") |>
      dplyr::select(species, campaign, year, date, estimate),
    by = c("species", "campaign")
  ) |>
  mutate(
    species = dplyr::recode(
      species,
      "beech" = "Fagus sylvatica",
      "ash" = "Fraxinus excelsior"
    ),
    date_plot = as.Date(date),
    group_y = case_when(
      species == "Fagus sylvatica" ~ 65,
      species == "Fraxinus excelsior" ~ 30
    )
  )

year(df_emm_t95$date_plot) = 2000

plot.t95.png <- df_emm_t95 |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = estimate, col = species),
    alpha = plot_alpha,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset)
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = .group, col = species),
    show.legend = F
  ) +
  scale_color_manual(
    values = c("Fagus sylvatica" = "#410866", "Fraxinus excelsior" = "#faad22")
  ) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  scale_y_continuous(limits = c(30, 65), breaks = seq(30, 65, 5)) +
  labs(y = "T95 [°C]", x = "Date") +
  guides(color = guide_legend(title = "Species")) +
  thesis_theme +
  theme(strip.background = element_blank())
plot.t95.png

ggsave(
  "figures/tsm/plot.t95.png",
  plot.t95.png,
  width = 6,
  height = 4,
  dpi = 150
)


df_emm_tsm |>
  group_by(trait, species, year, campaign) |>
  summarise(
    mean = mean(estimate) |> round(1),
    sd = sd(estimate) |> round(1),
    emmean = emmean
  ) |>
  View()

## 1.5.5 export ------------------------------------------------------------------

df_emm_t5 |>
  arrange(species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  dplyr::select(c(
    species,
    campaign,
    emmean,
    SE,
    df,
    lower.CL,
    upper.CL,
    .group
  )) |>
  distinct() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

lme_t5 |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

df_emm_t50 |>
  arrange(species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  dplyr::select(c(
    species,
    campaign,
    emmean,
    SE,
    df,
    lower.CL,
    upper.CL,
    .group
  )) |>
  distinct() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

lme_t50 |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

df_emm_t95 |>
  arrange(species, campaign) |>
  mutate(.group = f.reletter_cld(.group)) |>
  dplyr::select(c(
    species,
    campaign,
    emmean,
    SE,
    df,
    lower.CL,
    upper.CL,
    .group
  )) |>
  distinct() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

lme_t95 |>
  broom.mixed::tidy() |>
  write.table("clipboard", sep = "\t", row.names = F, na = "")

# 1.6. Hydraulic safety margins ---------------------------------------------

## 1.6.1 prepare ------------------------------------------------------------

df_hsm <- df_psi_midday |>
  dplyr::left_join(
    df_pv_params |> dplyr::select(campaign, year, sample_id, psi_tlp),
    by = c("sample_id", "campaign", "year")
  ) |>
  dplyr::left_join(
    df_ov_params |> dplyr::select(campaign, year, sample_id, p12, p50, p88),
    by = c("sample_id", "campaign", "year")
  )

df_hsm <- df_hsm |>
  mutate(
    hsm_tlp = psi_midday_mpa - psi_tlp,
    hsm_p12 = psi_midday_mpa - p12,
    hsm_p50 = psi_midday_mpa - p50,
    hsm_p88 = psi_midday_mpa - p88,
    date_plot = date
  ) |>
  pivot_longer(
    cols = c(hsm_tlp, hsm_p50, hsm_p12, hsm_p88),
    names_to = "hsm_type",
    values_to = "hsm_mpa"
  )
year(df_hsm$date_plot) = 2000


## 1.6.2 lme -----------------------------------------------------------------

lme_hsm_tlp <- lmer(
  hsm_mpa ~ species * campaign * year + (1 | sample_id),
  data = df_hsm |> filter(hsm_type == "hsm_tlp")
)

lme_hsm_p12 <- lmer(
  hsm_mpa ~ species * campaign * year + (1 | sample_id),
  data = df_hsm |> filter(hsm_type == "hsm_p12")
)

lme_hsm_p50 <- lmer(
  hsm_mpa ~ species * campaign * year + (1 | sample_id),
  data = df_hsm |> filter(hsm_type == "hsm_p50")
)

lme_hsm_p88 <- lmer(
  hsm_mpa ~ species * campaign * year + (1 | sample_id),
  data = df_hsm |> filter(hsm_type == "hsm_p88")
)


## 1.6.3 emmeans -----------------------------------------------------------------

emmeans_hsm_tlp <- emmeans(
  lme_hsm_tlp,
  ~ species * campaign * year,
  adjust = "bonferroni",
  type = "response"
)

emmeans_hsm_p12 <- emmeans(
  lme_hsm_p12,
  ~ species * campaign * year,
  adjust = "bonferroni",
  type = "response"
)

emmeans_hsm_p50 <- emmeans(
  lme_hsm_p50,
  ~ species * campaign * year,
  adjust = "bonferroni",
  type = "response"
)

emmeans_hsm_p88 <- emmeans(
  lme_hsm_p88,
  ~ species * campaign * year,
  adjust = "bonferroni",
  type = "response"
)

df_emm_hsm_tlp <- emmeans_hsm_tlp |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  mutate(hsm_type = "hsm_tlp")

df_emm_hsm_p12 <- emmeans_hsm_p12 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  mutate(hsm_type = "hsm_p12")

df_emm_hsm_p50 <- emmeans_hsm_p50 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  mutate(hsm_type = "hsm_p50")

df_emm_hsm_p88 <- emmeans_hsm_p88 |>
  multcomp::cld(Letters = letters) |>
  as.data.frame() |>
  mutate(hsm_type = "hsm_p88")

df_emm_hsm <- bind_rows(
  df_emm_hsm_tlp,
  df_emm_hsm_p12,
  df_emm_hsm_p50,
  df_emm_hsm_p88
)

df_hsm_emm <- left_join(
  df_hsm,
  df_emm_hsm,
  by = c("species", "campaign", "year", "hsm_type")
)


## 1.6.4 plot ----------------------------------------------------------------

df_hsm_emm <- df_hsm_emm |>
  mutate(
    species = dplyr::recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    hsm_type = case_when(
      hsm_type == "hsm_p12" ~ "P[12]",
      hsm_type == "hsm_p50" ~ "P[50]",
      hsm_type == "hsm_p88" ~ "P[88]",
      hsm_type == "hsm_tlp" ~ "psi[tlp]"
    ) |>
      factor(levels = c("psi[tlp]", "P[12]", "P[50]", "P[88]")), # Explicit order
    group_y = case_when(
      species == "Fagus sylvatica" ~ 6.5,
      species == "Fraxinus excelsior" ~ -1.5
    ),
    plot_number = case_when(
      hsm_type == "P[12]" & year == 2023 ~ "c)",
      hsm_type == "P[12]" & year == 2024 ~ "d)",
      hsm_type == "P[50]" & year == 2023 ~ "e)",
      hsm_type == "P[50]" & year == 2024 ~ "f)",
      hsm_type == "P[88]" & year == 2023 ~ "g)",
      hsm_type == "P[88]" & year == 2024 ~ "h)",
      hsm_type == "psi[tlp]" & year == 2023 ~ "a)",
      hsm_type == "psi[tlp]" & year == 2024 ~ "b)"
    )
  )

plot.hsm.png <- df_hsm_emm |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = hsm_mpa, col = species),
    position = position_dodge(width = 5),
    size = 1.5,
    alpha = plot_alpha
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    position = position_dodge(width = 5),
    alpha = plot_alpha,
    show.legend = FALSE
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species),
    position = position_dodge(width = 5),
    width = 3,
    alpha = 0.4,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = .group, col = species),
    show.legend = F
  ) +
  geom_text(
    aes(x = as.Date("2000-06-01"), y = 6.5, label = plot_number),
    show.legend = F
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Hydraulic safety margin [MPa]", x = "Date") +
  guides(color = guide_legend(title = "Species")) +
  scale_y_continuous(limits = c(-1.5, 6.5), breaks = seq(-1, 6, 1)) +
  scale_color_oi(order = c(6, 2)) +
  facet_grid(
    cols = vars(year),
    rows = vars(hsm_type),
    scales = "free_x",
    space = "free_x",
    labeller = label_parsed
  ) + # Ensure columns are faceted by hsm_type
  thesis_theme +
  theme(legend.text = element_text(face = "italic"))
plot.hsm.png

ggsave(
  "figures/hsm/plot.hsm.png",
  plot.hsm.png,
  width = 8,
  height = 10,
  dpi = 150
)

df_hsm_emm |>
  group_by(year, species, campaign, hsm_type) |>
  summarise(
    mean = mean(hsm_mpa, na.rm = TRUE) |> round(1),
    sd = sd(hsm_mpa, na.rm = TRUE) |> round(1)
  ) |>
  View()

# 1.7. Sugars ------------------------------------------------------------------

df_sugars_summ <- read.csv(
  "data/calculated_parameters/df_sugars_2023_2024.csv"
) |>
  mutate(
    date = as.Date(date),
    species = as.factor(species),
    sample_id = as.factor(sample_id),
    campaign = as.factor(campaign),
    date_fac = as.factor(date)
  ) %>%
  pivot_wider(names_from = sugar_name, values_from = sugar_conc) %>%
  mutate(
    starch_sugar_ratio = starch_mg_g /
      (fructose_mg_g + glucose_mg_g + sucrose_mg_g)
  ) %>%
  pivot_longer(
    cols = fructose_mg_g:starch_sugar_ratio,
    names_to = "sugar_name",
    values_to = "sugar_conc"
  )

## models ------------------------------------------------------------------

df_sugars_nest <- df_sugars_summ |>
  nest(data = -c(sugar_name)) %>%
  filter(sugar_name %>% str_detect("mg\\_g|ratio"))

library(nlme)

df_sugars_nest <- df_sugars_nest |>
  mutate(
    lme = map(
      data,
      ~ glmmTMB::glmmTMB(
        sugar_conc ~ species * date_fac + (1 | sample_id),
        # ar1(campaign + 0 | species/sample_id),
        family = Gamma(link = "log"),
        data = .x
      )
    )

    # mutate(lme = map(
    #   data,
    #   ~ lme4::lmer(sugar_conc ~ species * campaign * year + (1 | sample_id),
    #          data = .x)
    # )

    # mutate(lme = map(
    #   data,
    #   ~ nlme::lme(sugar_conc ~ species * date_fac,
    #               random = ~ 1 | sample_id,
    #               data = .x)
    # )
  )

df_sugars_nest$lme[[6]] |> summary()
df_sugars_nest$lme[[1]] |> performance::r2()

emmeans::ref_grid(df_sugars_nest$lme[[1]])

df_sugars_nest <- df_sugars_nest |>
  mutate(
    emmeans = map(
      lme,
      ~ emmeans::emmeans(
        .x,
        ~ species * date_fac,
        lmer.df = "asymptotic",
        adjust = "mvt",
        type = "response"
      )
    )
  )

df_sugars_nest$emmeans[[1]]

df_sugars_nest <- df_sugars_nest |>
  mutate(
    emmgrid = map(
      emmeans,
      ~ .x |>
        multcomp::cld(Letters = letters) |>
        as.data.frame()
    )
  )

# multcomp::cld(Letters = letters, df_sugars_nest$emmeans[[1]])

# df_sugars_nest$emmgrid[[1]]

df_sugars_emm <- df_sugars_summ |>
  left_join(
    df_sugars_nest |>
      dplyr::select(-c(data, lme, emmeans)) |>
      unnest(emmgrid),
    by = c("sugar_name", "species", "date_fac")
  ) |>
  mutate(
    date_plot = as.Date(date),
    year = date |> year()
  )
year(df_sugars_emm$date_plot) <- 2000

## plots -------------------------------------------------------------------

df_sugars_emm$sugar_name |> unique()

### glucose ----

## update with gamma dist

plot.glucose <- df_sugars_emm |>
  filter(sugar_name == "glucose_mg_g") |>
  arrange(species, campaign) |>
  mutate(
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group_y = case_when(
      species == "Fagus sylvatica" ~ 70,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = sugar_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = FALSE
  ) +
  geom_line(
    aes(x = date_plot, y = response, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  # geom_errorbar(aes(x = date_plot, ymin = lower.CL, ymax = upper.CL, color = species), width = 3,
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  # scale_y_continuous(limits = c(0, 70), breaks = seq(0,70,10)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(
    y = expression(
      "Glucose [mg" ~ g^{
        -1
      } *
        "]"
    ),
    x = "Date",
    tag = "c)"
  ) +
  facet_wrap(~year) +
  thesis_theme

plot.glucose

# ggsave("figures/sugars/glucose.png", plot.glucose, width = 8, height = 6, dpi = 300, unit = "cm")

### starch ----

## update with gamma dist
plot.starch <- df_sugars_emm |>
  filter(sugar_name == "starch_mg_g") |>
  arrange(species, campaign) |>
  mutate(
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group_y = case_when(
      species == "Fagus sylvatica" ~ 135,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = sugar_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = FALSE
  ) +
  geom_line(
    aes(x = date_plot, y = response, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, 20)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(
    y = expression(
      "Starch [mg" ~ g^{
        -1
      } *
        "]"
    ),
    x = "Date",
    tag = "d)"
  ) +
  facet_wrap(~year) +
  thesis_theme

plot.starch

# ggsave("figures/sugars/starch.png", plot.starch, width = 8, height = 8.2, dpi = 300, unit = "cm")

### sucrose ----

plot.sucrose <- df_sugars_emm |>
  filter(sugar_name == "sucrose_mg_g") |>
  arrange(species, campaign) |>
  mutate(
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group_y = case_when(
      species == "Fagus sylvatica" ~ 85,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = sugar_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = T
  ) +
  geom_line(
    aes(x = date_plot, y = response, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = T
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  scale_y_continuous(limits = c(0, 85), breaks = seq(0, 85, 10)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(
    y = expression(
      "Sucrose [mg" ~ g^{
        -1
      } *
        "]"
    ),
    x = "Date",
    tag = "e)"
  ) +
  facet_wrap(~year) +
  thesis_theme

plot.sucrose

# ggsave("figures/sugars/sucrose.png", plot.sucrose, width = 8, height = 6, dpi = 300, unit = "cm")

### fructose ----

## update with gamma dist
plot.fructose <- df_sugars_emm |>
  filter(sugar_name == "fructose_mg_g") |>
  arrange(species, campaign) |>
  mutate(
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group_y = case_when(
      species == "Fagus sylvatica" ~ 40,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = sugar_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = FALSE
  ) +
  geom_line(
    aes(x = date_plot, y = response, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 70, 10)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(
    y = expression(
      "Fructose [mg" ~ g^{
        -1
      } *
        "]"
    ),
    x = "Date",
    tag = "b)"
  ) +
  facet_wrap(~year) +
  thesis_theme

plot.fructose

# ggsave("figures/sugars/fructose.png", plot.fructose, width = 8, height = 6, dpi = 300, unit = "cm")

### total sugars ----
plot.total_sugars <- df_sugars_emm |>
  filter(sugar_name == "total_sugars_mg_g") |>
  arrange(species, campaign) |>
  mutate(
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group_y = case_when(
      species == "Fagus sylvatica" ~ 120,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = sugar_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_line(
    aes(x = date_plot, y = response, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = F
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = F
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  # scale_y_continuous(limits = c(0, 70), breaks = seq(0,70,10)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(
    y = expression(
      "Total sugars [mg" ~ g^{
        -1
      } *
        "]"
    ),
    x = "Date",
    tag = "a)"
  ) +
  facet_wrap(~year) +
  thesis_theme

plot.total_sugars

### total sugars ----
plot.starch_sugar_ratio <- df_sugars_emm |>
  filter(sugar_name == "starch_sugar_ratio") |>
  arrange(species, campaign) |>
  mutate(
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    group_y = case_when(
      species == "Fagus sylvatica" ~ 2.5,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = sugar_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = F
  ) +
  geom_line(
    aes(x = date_plot, y = response, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = F
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = F
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = F
  ) +
  # scale_y_continuous(limits = c(0, 70), breaks = seq(0,70,10)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(
    y = expression(
      "Starch to sugar ratio [mg" ~ mg^{
        -1
      } *
        "]"
    ),
    x = "Date",
    tag = "f)"
  ) +
  facet_wrap(~year) +
  thesis_theme

plot.starch_sugar_ratio

# ggsave("figures/sugars/total_sugars.png", plot.total_sugars, width = 8, height = 6, dpi = 300, unit = "cm")

df_sugars_emm |>
  filter(sugar_name == "total_sugars_mg_g") |>
  arrange(species, campaign) |>
  dplyr::select(c(species, year, campaign, response, sugar_conc)) |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(sugar_conc) |> round(1),
    sd = sd(sugar_conc) |> round(1)
  )
df_sugars_emm |>
  filter(sugar_name == "sucrose_mg_g") |>
  arrange(species, campaign) |>
  dplyr::select(c(species, year, campaign, response, sugar_conc)) |>
  group_by(species, year, campaign) |>
  summarise(
    mean = mean(sugar_conc) |> round(1),
    sd = sd(sugar_conc) |> round(1)
  )


## plot all ----------------------------------------------------------------

extra_theme <- theme(
  axis.title.x = element_blank(),
  plot.tag.position = c(0.03, 0.95),
  legend.text = element_text(size = 8, face = "italic"),
  legend.justification = c(0.25, 0)
)

plots.all_nsc.png <- plot.total_sugars +
  extra_theme +
  plot_spacer() +
  plot.fructose +
  extra_theme +
  # plot_spacer() + plot_spacer() + plot_spacer() +
  plot.glucose +
  extra_theme +
  plot_spacer() +
  plot.starch +
  extra_theme +
  # plot_spacer() + plot_spacer() + plot_spacer() +
  plot.sucrose +
  theme(
    plot.tag.position = c(0.03, 0.95),
    legend.text = element_text(size = 8, face = "italic"),
    legend.justification = c(0.25, 0)
  ) +
  plot_layout(widths = c(8, 0, 8), heights = c(7, 7, 7))
plots.all_nsc.png
ggsave(
  "figures/sugars/all_nsc_both_years.png",
  plots.all_nsc.png,
  width = 30,
  height = 20,
  dpi = 150,
  unit = "cm"
)

# 1.8. Nutrients ---------------------------------------------------------------

df_nutrients_summ <- read.csv(
  "data/calculated_parameters/df_nutrients_2023_2024.csv"
) |>
  mutate(
    date = as.Date(date),
    date_fac = as.factor(date),
    species = as.factor(species),
    species = relevel(species, ref = "FASY"),
    sample_id = as.factor(sample_id),
    campaign = as.factor(campaign)
  ) %>%
  mutate(
    nutrient_name = nutrient_name |> str_remove_all("_con"),
    nutrient_name = case_when(
      nutrient_name == "c" ~ "c_perc",
      nutrient_name == "n" ~ "n_perc",
      T ~ nutrient_name
    )
  )

## models ------------------------------------------------------------------

df_nutrients_nest <- df_nutrients_summ |>
  nest(data = -c(nutrient_name))

library(nlme)

# df_nutrients_nest <- df_nutrients_nest |>
#   mutate(lme = map(
#     data,
#     ~ nlme::lme(
#       nutrient_conc ~ date * species,
#       random = ~ date | species / sample_id,
#       # correlation = corCAR1(form = ~ date | species/sample_id),
#       data = .x
#     )
#   ))

df_nutrients_nest <- df_nutrients_nest |>
  mutate(
    lme = map(
      data,
      ~ glmmTMB::glmmTMB(
        nutrient_conc ~ species * date_fac + (1 | sample_id),
        # ar1(campaign + 0 | species/sample_id),
        family = Gamma(link = "log"),
        data = .x
      )
    )
  )

df_nutrients_nest$lme[[1]] |> summary()
df_nutrients_nest$lme[[1]] |> performance::r2()

emmeans::ref_grid(df_nutrients_nest$lme[[1]])

df_nutrients_nest <- df_nutrients_nest |>
  mutate(
    emmeans = map(
      lme,
      ~ emmeans::emmeans(
        .x,
        ~ species * date_fac,
        adjust = "mvt",
        type = "response"
      )
    )
  )

df_nutrients_nest$emmeans[[1]]

df_nutrients_nest <- df_nutrients_nest |>
  mutate(
    emmgrid = map(
      emmeans,
      ~ .x |>
        multcomp::cld(Letters = letters) |>
        as.data.frame()
    )
  )

multcomp::cld(Letters = letters, df_nutrients_nest$emmeans[[1]])

df_nutrients_nest$emmgrid[[1]]

df_nutrients_emm <- df_nutrients_summ |>
  left_join(
    df_nutrients_nest |>
      select(-c(data, lme, emmeans)) |>
      unnest(emmgrid),
    by = c("nutrient_name", "species", "date_fac")
  )


## plots -------------------------------------------------------------------

df_nutrients_emm <- df_nutrients_emm |>
  group_by(nutrient_name) |>
  mutate(
    min = min(nutrient_conc),
    max = max(nutrient_conc),
    ylim_upr = case_when(
      nutrient_name == "c_percent" ~ 50,
      nutrient_name == "n_percent" ~ 4,
      nutrient_name == "p_mg_kg" ~ 1800,
      nutrient_name == "k_mg_kg" ~ 16000,
      nutrient_name == "s_mg_kg" ~ 3000,
      nutrient_name == "ca_mg_kg" ~ 26000,
      nutrient_name == "mg_mg_kg" ~ 3200
    ),
    ylim_lwr = case_when(
      nutrient_name == "c_percent" ~ 40,
      nutrient_name == "n_percent" ~ 0,
      nutrient_name == "p_mg_kg" ~ 0,
      nutrient_name == "k_mg_kg" ~ 0,
      nutrient_name == "s_mg_kg" ~ 0,
      nutrient_name == "ca_mg_kg" ~ 0,
      nutrient_name == "mg_mg_kg" ~ 0
    ),
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    date_plot = as.Date(date),
    year = date |> year()
  )
year(df_nutrients_emm$date_plot) <- 2000

library(see)

### Carbon ----
plot.c_con_percent <- df_nutrients_emm |>
  filter(nutrient_name == "c_con") |>
  arrange(species, date_fac) |>
  mutate(
    group_y = case_when(
      species == "Fagus sylvatica" ~ 52,
      species == "Fraxinus excelsior" ~ 42
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = nutrient_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = FALSE
  ) +
  geom_line(
    aes(x = date_plot, y = response, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  # annotate("text", x = as.Date("2000-06-01"), y = 52, label = "a)", size = 5, vjust = 0.5, hjust = 0) +
  scale_y_continuous(limits = c(42, 52), breaks = seq(42, 52, 2)) +
  # scale_x_date(limits = c(as.Date("2023-06-01"), as.Date("2023-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(y = "Carbon [%]", x = "Date") +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme

plot.c_con_percent

ggsave(
  "figures/nutrients/c_con_percent.png",
  plot.c_con_percent,
  width = 8,
  height = 6,
  dpi = 150,
  unit = "cm"
)

### Nitrogen ----
plot.n_con_percent <- df_nutrients_emm |>
  filter(nutrient_name == "n_con") |>
  arrange(species, date_fac) |>
  mutate(
    group_y = case_when(
      species == "Fagus sylvatica" ~ 5,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = nutrient_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = FALSE
  ) +
  geom_line(
    aes(x = date_plot, y = response, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  # annotate("text", x = as.Date("2000-06-01"), y = 52, label = "a)", size = 5, vjust = 0.5, hjust = 0) +
  # scale_y_continuous(limits = c(42, 52), breaks = seq(42,52,2)) +
  # scale_x_date(limits = c(as.Date("2023-06-01"), as.Date("2023-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(y = "Nitrogen [%]", x = "Date") +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme

plot.n_con_percent

ggsave(
  "figures/nutrients/n_con_percent.png",
  plot.n_con_percent,
  width = 8,
  height = 6,
  dpi = 150,
  unit = "cm"
)

### Phosphorus ----

plot.p_con_mg_kg <- df_nutrients_emm |>
  filter(nutrient_name == "p_mg_kg") |>
  arrange(species, date_fac) |>
  mutate(
    group_y = case_when(
      species == "Fagus sylvatica" ~ 4000,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = nutrient_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = FALSE
  ) +
  geom_line(
    aes(x = date_plot, y = response, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  # annotate("text", x = as.Date("2000-06-01"), y = 52, label = "a)", size = 5, vjust = 0.5, hjust = 0) +
  # scale_y_continuous(limits = c(42, 52), breaks = seq(42,52,2)) +
  # scale_x_date(limits = c(as.Date("2023-06-01"), as.Date("2023-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(y = "Phosphorus [%]", x = "Date") +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme

plot.p_con_mg_kg

ggsave(
  "figures/nutrients/p_con_mg_kg.png",
  plot.p_con_mg_kg,
  width = 8,
  height = 6,
  dpi = 150,
  unit = "cm"
)

### Sulfur ----

plot.s_con_mg_kg <- df_nutrients_emm |>
  filter(nutrient_name == "s_mg_kg") |>
  arrange(species, campaign) |>
  mutate(
    group_y = case_when(
      species == "Fagus sylvatica" ~ 3000,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = nutrient_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = FALSE
  ) +
  geom_line(
    aes(x = date_plot, y = response, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  # annotate("text", x = as.Date("2000-06-01"), y = 52, label = "a)", size = 5, vjust = 0.5, hjust = 0) +
  # scale_y_continuous(limits = c(42, 52), breaks = seq(42,52,2)) +
  # scale_x_date(limits = c(as.Date("2023-06-01"), as.Date("2023-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(y = "Sulfur [%]", x = "Date") +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme

plot.s_con_mg_kg

ggsave(
  "figures/nutrients/s_con_mg_kg.png",
  plot.s_con_mg_kg,
  width = 8,
  height = 6,
  dpi = 150,
  unit = "cm"
)

### Calcium ----
plot.ca_con_mg_kg <- df_nutrients_emm |>
  filter(nutrient_name == "ca_mg_kg") |>
  arrange(species, campaign) |>
  mutate(
    group_y = case_when(
      species == "Fagus sylvatica" ~ 25000,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = nutrient_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = FALSE
  ) +
  geom_line(
    aes(x = date_plot, y = response, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  # annotate("text", x = as.Date("2000-06-01"), y = 52, label = "a)", size = 5, vjust = 0.5, hjust = 0) +
  # scale_y_continuous(limits = c(42, 52), breaks = seq(42,52,2)) +
  # scale_x_date(limits = c(as.Date("2023-06-01"), as.Date("2023-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(y = "Calcium [%]", x = "Date") +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme

plot.ca_con_mg_kg

ggsave(
  "figures/nutrients/ca_con_mg_kg.png",
  plot.ca_con_mg_kg,
  width = 8,
  height = 6,
  dpi = 150,
  unit = "cm"
)

### Potassium ----
plot.k_con_mg_kg <- df_nutrients_emm |>
  filter(nutrient_name == "k_mg_kg") |>
  arrange(species, campaign) |>
  mutate(
    group_y = case_when(
      species == "Fagus sylvatica" ~ 18000,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = nutrient_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = FALSE
  ) +
  geom_line(
    aes(x = date_plot, y = response, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  # annotate("text", x = as.Date("2000-06-01"), y = 52, label = "a)", size = 5, vjust = 0.5, hjust = 0) +
  # scale_y_continuous(limits = c(42, 52), breaks = seq(42,52,2)) +
  # scale_x_date(limits = c(as.Date("2023-06-01"), as.Date("2023-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(y = "Potassium [%]", x = "Date") +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme

plot.k_con_mg_kg

ggsave(
  "figures/nutrients/k_con_mg_kg.png",
  plot.k_con_mg_kg,
  width = 8,
  height = 8.5,
  dpi = 150,
  unit = "cm"
)

### Magnesium ----

plot.mg_con_mg_kg <- df_nutrients_emm |>
  filter(nutrient_name == "mg_mg_kg") |>
  arrange(species, campaign) |>
  mutate(
    group_y = case_when(
      species == "Fagus sylvatica" ~ 3500,
      species == "Fraxinus excelsior" ~ 0
    ),
    group = .group |> f.reletter_cld()
  ) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = nutrient_conc, col = species),
    alpha = 1,
    position = position_dodge(width = plot_position_offset),
    show.legend = FALSE
  ) +
  geom_line(
    aes(x = date_plot, y = response, group = sample_id, col = species),
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3,
    position = position_dodge(width = plot_position_offset),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = group, col = species),
    show.legend = FALSE
  ) +
  # annotate("text", x = as.Date("2000-06-01"), y = 52, label = "a)", size = 5, vjust = 0.5, hjust = 0) +
  # scale_y_continuous(limits = c(42, 52), breaks = seq(42,52,2)) +
  # scale_x_date(limits = c(as.Date("2023-06-01"), as.Date("2023-09-30"))) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(y = "Magnesium [%]", x = "Date") +
  facet_wrap(~year, scales = "free_x") +
  thesis_theme

plot.mg_con_mg_kg

ggsave(
  "figures/nutrients/mg_con_mg_kg.png",
  plot.mg_con_mg_kg,
  width = 8,
  height = 6,
  dpi = 150,
  unit = "cm"
)

## plot all ----------------------------------------------------------------

extra_theme <- theme(
  axis.title.x = element_blank(),
  plot.tag.position = c(0.03, 0.95),
  legend.text = element_text(size = 8, face = "italic"),
  legend.justification = c(0.25, 0)
)

plots.all_nutrients.png <-
  plot.c_con_percent +
  extra_theme +
  plot_spacer() +
  plot.n_con_percent +
  extra_theme +
  plot_spacer() +
  plot.p_con_mg_kg +
  extra_theme +
  plot_spacer() +
  plot.s_con_mg_kg +
  extra_theme +
  plot_spacer() +
  plot.ca_con_mg_kg +
  extra_theme +
  plot_spacer() +
  plot.k_con_mg_kg +
  extra_theme +
  plot_spacer() +
  plot.mg_con_mg_kg +
  extra_theme +
  plot_spacer() +
  theme(
    plot.tag.position = c(0.03, 0.95),
    legend.text = element_text(size = 8, face = "italic"),
    legend.justification = c(0.25, 0)
  ) +
  plot_layout(widths = c(8, 0, 8), heights = c(7, 7, 7))
plots.all_nutrients.png
ggsave(
  "figures/sugars/all_nsc_both_years.png",
  plots.all_nsc.png,
  width = 30,
  height = 20,
  dpi = 150,
  unit = "cm"
)

library(ggpubr)

figure <- ggarrange(
  plot.c_con_percent,
  plot.n_con_percent,
  plot.p_con_mg_kg,
  plot.s_con_mg_kg,
  plot.ca_con_mg_kg,
  plot.k_con_mg_kg,
  plot.mg_con_mg_kg,
  labels = c("a)", "b)", "c)", "d)", "e)", "f)", "g)"),
  label.x = -0.02,
  widths = c(2, 2, 2),
  heights = c(2, 2, 2),
  ncol = 3,
  nrow = 3
)

figure
ggsave(
  "figures/nutrients/all_nsc_both_years.png",
  figure,
  width = 35,
  height = 25,
  dpi = 150,
  unit = "cm"
)

# 2. meteorological models ---------------------------------------------------

## traits
df_traits <- read_csv("data/calculated_parameters/traits_2023_2024.csv")

## all meteo and fluxes
df_meteo_flux_30min <- read.csv(
  "data/microclimate/meteo_fluxes_2023_2024_30min.csv",
  header = TRUE
) |>
  mutate(
    timestamp = timestamp |> as.POSIXct(format = "%Y-%m-%d %H:%M:%S"),
    pet_mm = case_when(pet_mm < 0 ~ 1e-5, TRUE ~ pet_mm),
    et_mm = case_when(et_mm < 0 ~ 1e-5, TRUE ~ et_mm)
  )


f.calculate_meteo_summary <- function(data, start_date, end_date) {
  data |>
    filter(timestamp >= start_date, timestamp <= end_date) |>
    group_by(day(timestamp)) |>
    mutate(max_ta = max(ta_44m, na.rm = TRUE)) |>
    ungroup() |>
    summarise(
      sum_p = sum(p_1m, na.rm = TRUE),
      mean_ta = mean(ta_44m, na.rm = TRUE),
      max_ta = mean(max_ta, na.rm = TRUE),
      # min_rew = mean(min_rew, na.rm = TRUE),
      # swc_8cm = mean(swc_8cm_1, na.rm = TRUE),
      # swc_16cm = mean(swc_16cm_1, na.rm = TRUE),
      # swc_32cm = mean(swc_32cm_1, na.rm = TRUE),
      mean_rh = mean(rh_44m, na.rm = TRUE),
      mean_vpd = mean(vpd_44m, na.rm = TRUE),
      sum_pet = sum(pet_mm, na.rm = TRUE),
      sum_et = sum(et_mm, na.rm = TRUE),
      sum_p_et = sum(p_1m - et_mm, na.rm = TRUE),
      mean_et_pet = mean(et_mm / pet_mm, na.rm = TRUE),
      mean_rew = mean(rew, na.rm = TRUE) * 100,
      sum_ppfd = sum(ppfd_in_44m, na.rm = TRUE),
      # mean_gpp = mean(gpp_f, na.rm = TRUE),
      ## add net Radiation / PPFD, GPP, Rn lambdaP-1
      timeframe = start_date - end_date
    ) |>
    ungroup()
}

f.calculate_meteo_summary_h <- function(data, start_date, end_date) {
  data |>
    filter(timestamp >= start_date, timestamp <= end_date) |>
    group_by(timestamp) |>
    mutate(
      max_ta = max(ta_44m, na.rm = TRUE),
      min_rew = min(rew, na.rm = TRUE) * 100
    ) |>
    ungroup() |>
    summarise(
      sum_p = sum(p_1m, na.rm = TRUE),
      mean_ta = mean(ta_44m, na.rm = TRUE),
      max_ta = mean(max_ta, na.rm = TRUE),
      # min_rew = mean(min_rew, na.rm = TRUE),
      # swc_8cm = mean(swc_8cm_1, na.rm = TRUE),
      # swc_16cm = mean(swc_16cm_1, na.rm = TRUE),
      # swc_32cm = mean(swc_32cm_1, na.rm = TRUE),
      mean_rh = mean(rh_44m, na.rm = TRUE),
      mean_vpd = mean(vpd_44m, na.rm = TRUE),
      sum_pet = sum(pet_mm, na.rm = TRUE),
      sum_et = sum(et_mm, na.rm = TRUE),
      sum_p_et = sum(p_1m - et_mm, na.rm = TRUE),
      mean_et_pet = mean(et_mm / pet_mm, na.rm = TRUE),
      mean_rew = mean(rew, na.rm = TRUE) * 100,
      sum_ppfd = sum(ppfd_in_44m, na.rm = TRUE),
      # mean_gpp = mean(gpp_f, na.rm = TRUE),
      ## add net Radiation / PPFD, GPP, Rn lambdaP-1
      timeframe = start_date - end_date
    ) |>
    ungroup()
}

f.make_formula = function(data) {
  paste("trait_val", "~", paste(names(data)[9:ncol(data)], collapse = " + ")) |>
    as.formula()
}

f.make_formula_char = function(data) {
  paste("trait_val", "~", paste(names(data)[9:ncol(data)], collapse = " + ")) |>
    as.character()
}

f.make_formula_dredge <- function(dredge_model_line, write_clip = F) {
  ## transpose to get rid of NA predictors (not chosen in dregde selection)
  predictors = dredge_model_line |>
    as.data.frame() |>
    t() |>
    na.omit() |>
    ## transpose back
    t() |>
    as.data.frame() |>
    names()
  ## get column names of predictors
  predictors = predictors[
    !predictors %in%
      c("(Intercept)", "df", "logLik", "AICc", "delta", "weight")
  ]
  ## paste
  formula = paste(c("value ~ ", predictors), collapse = " + ")
  if (write_clip) {
    writeClipboard(formula)
  } else {
    formula
  }
}

# Function to extract base predictor names (removes trailing numbers)
extract_base_name <- function(x) {
  str_remove(x, "_\\d+$") # Removes underscore + digits at the end
}

test <- predictor_labels[extract_base_name("k_mg_kg")]

match <- str_match(test, "(.*?)\\[(.*?)\\]") # Extract "PET" and "sum"


# Function to format labels
format_label <- function(p) {
  base_name <- extract_base_name(p) # Remove number suffix
  days <- str_extract(p, "\\d+$") # Extract the number
  long_name <- predictor_labels[base_name] # Get full name

  if (!is.na(long_name)) {
    # Extract variable name and statistic type (if available)
    match <- str_match(long_name, "(.*?)\\[(.*?)\\]") # Extract "PET" and "sum"
    var_name <- match[, 2] # First part (e.g., "PET")
    stat_type <- match[, 3] # Second part (e.g., "sum")

    ## for leaf age, type is placeholder "x"
    if (stat_type == "x") {
      return(var_name)
    }
    if (stat_type %>% str_detect("mg")) {
      return(paste0(var_name, "\n(", stat_type, ")"))
    }
    if (stat_type %>% str_detect("%")) {
      return(paste0(var_name, "\n(%)"))
    } else {
      return(paste0(var_name, "\n(", stat_type, ": ", days, " d)"))
    }
  } else {
    return(p) # Keep original if no match
  }
}

# Function to format labels in export tables
format_label_export <- function(p) {
  base_name <- extract_base_name(p) # Remove number suffix
  days <- str_extract(p, "\\d+$") # Extract the number
  long_name <- predictor_labels[base_name] # Get full name

  if (!is.na(long_name)) {
    # Extract variable name and statistic type (if available)
    match <- str_match(long_name, "(.*?)\\[(.*?)\\]") # Extract "PET" and "sum"
    var_name <- match[, 2] # First part (e.g., "PET")
    stat_type <- match[, 3] # Second part (e.g., "sum")

    ## for leaf age, type is placeholder "x"
    if (stat_type == "x") {
      return(var_name)
    } else {
      return(paste0(var_name, " (", stat_type, ": ", days, " d)"))
    }
  } else {
    return(p) # Keep original if no match
  }
}

df_predictors = data.frame(
  abbr = c(
    "sum_p",
    "mean_ta",
    "max_ta",
    "mean_rh",
    "mean_vpd",
    "sum_pet",
    "sum_et",
    "sum_p_et",
    "mean_et_pet",
    "mean_rew",
    "sum_ppfd",
    "mean_gpp",
    "leaf_age_d",
    "starch_sugar_ratio",
    "k_mg_kg",
    "total_sugars_mg_g",
    "p_mg_kg",
    "s_mg_kg",
    "n",
    "glucose_starch_1",
    "sucrose_mg_g",
    "ca_mg_kg",
    "leaf_age_scale"
  ),
  long = c(
    "P[sum]",
    "TA[mean]",
    "TA[max]",
    "RH[mean]",
    "VPD[mean]",
    "PET[sum]",
    "ET[sum]",
    "P-ET[sum]",
    "ET/PET[mean]",
    "REW[mean]",
    "PPFD[sum]",
    "GPP[mean]",
    "Leaf age [x]",
    "Starch / Sugar[%]",
    "K conc.[mg / kg]",
    "Total sugars[mg / g]",
    "S conc.[mg / kg]",
    "P conc.[mg / kg]",
    "N conc.[%]",
    "Glucose / Starch[%]",
    "Sucrose[mg / g]",
    "Ca conc.[mg / kg]",
    "Leaf age [x]"
  )
)

## 2.1 aggregate all traits into one df ----------------------------------------

#
# df_traits <- df_pv_params |>
#   left_join(df_psi_midday |> select(-date),
#             by = c("species", "sample_id", "campaign", "year")) |>
#   left_join(df_gmin_params |> filter(rwc_interval == "standard") |>
#               select(-c(date, rwc_interval, rwc_class, gmin_mean, gmin_min, gmin_max)) ,
#             by = c("species", "sample_id", "campaign", "year")) |>
#   left_join(df_ov_params |> select(year, campaign, sample_id, species, p12, p50, p88),
#             by = c("species", "sample_id", "campaign", "year")) |>
#   left_join(df_tsm_params |> pivot_wider(names_from = "trait", values_from = "estimate") |> select(-date),
#             by = c("species", "sample_id", "campaign", "year")) |>
#   mutate(pheno_start_doy = case_when(species == "FASY" & year == 2023 ~ 121,
#                                      species == "FASY" & year == 2024 ~ 104,
#                                      species == "FREX" & year == 2023 ~ 140,
#                                      species == "FREX" & year == 2024 ~ 138),
#          doy = yday(date),
#          leaf_age_d = doy - pheno_start_doy)
#
# df_traits |> head()
#
# write.csv(df_traits, "data/calculated_parameters/traits_2023_2024.csv", row.names = FALSE)

# ## microclimate
# df_meteo_H <- read.csv("data/microclimate/meteo_fluxes_2023_2024_H.csv", header = TRUE) |>
#   mutate(timestamp = timestamp |> as.POSIXct(format = "%Y-%m-%d %H:%M:%S"))
#
# df_meteo_30min <- read.csv("data/microclimate/meteo_2023_2024_30min.csv", header = TRUE) |>
#   mutate(timestamp = timestamp |> as.POSIXct(format = "%Y-%m-%d %H:%M:%S"))
#
# ## fluxes
# df_fluxes_30 <- read.csv("data/microclimate/fluxes_2023_2024_H.csv", header = TRUE)

### days --------------------------------------------------------------------

f.calculate_meteo_summary(
  df_meteo_flux_30min,
  as.Date("2023-05-15"),
  as.Date("2023-05-16")
)

day_seq <- seq(5, 60, 5)
day_seq <- c(1, 2, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)

df_traits_nest <- df_traits |>
  nest(data = -c(year, date, campaign, species))

df_traits_nest <- df_traits_nest |>
  mutate(
    meteo = NA,
    meteo = map(
      meteo,
      ~ f.calculate_meteo_summary(
        df_meteo_flux_30min,
        df_traits_nest$date[1] - 1,
        df_traits_nest$date[1]
      )
    )
  )

df_traits_nest_h <- df_traits |>
  nest(data = -c(year, date, campaign))

for (i in 1:nrow(df_traits_nest)) {
  for (j in 1:length(day_seq)) {
    df_traits_nest$meteo[[i]][j, ] = f.calculate_meteo_summary(
      df_meteo_flux_30min,
      df_traits_nest$date[i] - day_seq[j],
      df_traits_nest$date[i]
    )
  }
  print(i)
}
df_traits_nest$meteo[[1]]

df_traitvals <- df_traits_nest |>
  select(-meteo) |>
  unnest(data) |>
  pivot_longer(
    cols = -c(year, campaign, date, species, sample_id),
    names_to = "trait",
    values_to = "trait_val"
  )
df_traitvals

df_meteovals <- df_traits_nest |>
  select(date, meteo, species) |>
  unnest(meteo) |>
  pivot_longer(
    cols = -c(date, timeframe, species),
    names_to = "meteo_var",
    values_to = "meteo_val"
  )

df_meteovals

df_meteovals |>
  filter(species == "FREX") |>
  ggplot() +
  geom_line(aes(x = timeframe, y = meteo_val, col = as.factor(date))) +
  facet_wrap(~meteo_var, scales = "free_y") +
  thesis_theme

df_meteo_corr = left_join(df_traitvals, df_meteovals, by = c("date", "species"))

df_meteo_corr_nest <- df_meteo_corr |>
  nest(data = -c(species, timeframe, trait, meteo_var)) |>
  mutate(
    cor = map(
      data,
      ~ cor.test(y = .x$trait_val, x = .x$meteo_val, method = "spearman")
    ),
    corrcoef = map_dbl(cor, ~ .x$estimate),
    pval = map_dbl(cor, ~ .x$p.value),
    timeframe = as.numeric(timeframe)
  )

df_best_meteo_corr <- df_meteo_corr_nest |>
  filter(pval < 0.1) |>
  select(-cor) |>
  group_by(species, meteo_var, trait) |>
  mutate(abs_corr = abs(corrcoef)) |>
  slice_max(abs_corr, n = 1, with_ties = FALSE)
df_best_meteo_corr

df_meteo_corr_nest$trait |> unique()
df_meteo_corr_nest$meteo_var |> unique()

df_meteo_corr_nest |>
  filter(trait == "psi_midday_mpa") |>
  ggplot(aes(x = timeframe, y = corrcoef, shape = species)) +
  geom_point(aes(col = ifelse(pval < 0.05, "darkgreen", "grey"))) +
  scale_color_identity() +
  facet_wrap(~meteo_var) +
  labs(
    y = "correlation coefficient",
    x = "cumulative timeframe [days]",
    title = "Midday Water Potential"
  ) +
  thesis_theme

df_meteo_corr_nest |>
  filter(trait == "psi_tlp") |>
  ggplot(aes(x = timeframe, y = corrcoef, shape = species)) +
  geom_point(aes(col = ifelse(pval < 0.05, "darkgreen", "grey"))) +
  scale_color_identity() +
  facet_wrap(~meteo_var) +
  labs(
    y = "correlation coefficient",
    x = "cumulative timeframe [days]",
    title = "Turgor loss point"
  ) +
  thesis_theme

df_meteo_corr_nest |>
  filter(trait == "gmin") |>
  ggplot(aes(x = timeframe, y = corrcoef, shape = species)) +
  geom_point(aes(col = ifelse(pval < 0.05, "darkgreen", "grey"))) +
  scale_color_identity() +
  facet_wrap(~meteo_var) +
  labs(
    y = "correlation coefficient",
    x = "cumulative timeframe [days]",
    title = "Gmin"
  ) +
  thesis_theme

df_meteo_corr_nest |>
  filter(trait == "psi_midday_mpa", ) |>
  ggplot(aes(x = timeframe, y = corrcoef)) +
  geom_point(aes(col = ifelse(pval < 0.05, "darkgreen", "grey"))) +
  scale_color_identity() +
  facet_wrap(~meteo_var) +
  labs(
    y = "correlation coefficient",
    x = "cumulative timeframe [days]",
    title = "Midday water potential"
  ) +
  thesis_theme

df_test <- df_meteo_corr_nest |>
  filter(trait == "psi_midday_mpa", meteo_var == "mean_vpd") |>
  unnest(data)

df_test |>
  mutate(
    species = recode(
      species,
      "FREX" = "Fraxinus excelsior",
      "FASY" = "Fagus sylvatica"
    )
  ) |>
  ggplot(aes(x = meteo_val, y = trait_val, col = species)) +
  geom_smooth(method = "lm") +
  geom_point() +
  facet_wrap(~timeframe) +
  scale_color_manual(
    values = c("Fagus sylvatica" = "#410866", "Fraxinus excelsior" = "#D55E00")
  ) +
  labs(
    y = "Midday water potential [MPa]",
    x = "mean relative extractable soil water [%]",
    title = "Midday water potential vs. mean relative extractable soil water",
    subtitle = "Cumulative timeframe [days]"
  ) +
  thesis_theme


### hours -------------------------------------------------------------------

f.calculate_meteo_summary_h(
  df_meteo_flux_30min,
  as.POSIXct("2023-05-15 12:00:00"),
  as.POSIXct("2023-05-16 12:00:00")
)

hour_seq <- c(1, 2, 3, 4, 5, 6, 8, 10, 15, 20, 25, 30, 35, 40)

df_traits_nest_h <- df_traits_nest |>
  mutate(
    date = as.POSIXct(date) + hours(12),
    meteo = NA,
    meteo = map(
      meteo,
      ~ f.calculate_meteo_summary(
        df_meteo_flux_30min,
        df_traits_nest$date[1] - 1,
        df_traits_nest$date[1]
      )
    )
  )

df_traits_nest_h$date[[1]] - hours(1)

for (i in 1:nrow(df_traits_nest_h)) {
  for (j in 1:length(hour_seq)) {
    df_traits_nest_h$meteo[[i]][j, ] = f.calculate_meteo_summary_h(
      df_meteo_flux_30min,
      df_traits_nest_h$date[i] - hours(hour_seq[j]),
      df_traits_nest_h$date[i]
    )
  }
  print(i)
}

df_traits_nest_h$meteo[[1]]


df_traitvals_h <- df_traits_nest_h |>
  select(-meteo) |>
  unnest(data) |>
  pivot_longer(
    cols = -c(year, campaign, date, species, sample_id),
    names_to = "trait",
    values_to = "trait_val"
  )
df_traitvals_h

df_meteovals_h <- df_traits_nest_h |>
  select(date, meteo) |>
  unnest(meteo) |>
  pivot_longer(
    cols = -c(date, timeframe),
    names_to = "meteo_var",
    values_to = "meteo_val"
  )

df_meteovals_h

df_meteovals_h |>
  ggplot() +
  geom_line(aes(x = timeframe, y = meteo_val, col = as.factor(date))) +
  facet_wrap(~meteo_var, scales = "free_y") +
  thesis_theme

df_meteo_corr_h = left_join(df_traitvals_h, df_meteovals_h, by = "date")

df_meteo_corr_nest_h <- df_meteo_corr_h |>
  nest(data = -c(timeframe, trait, meteo_var)) |>
  mutate(
    cor = map(
      data,
      ~ cor.test(y = .x$trait_val, x = .x$meteo_val, method = "spearman")
    ),
    corrcoef = map_dbl(cor, ~ .x$estimate),
    pval = map_dbl(cor, ~ .x$p.value),
    timeframe = as.numeric(timeframe)
  )

df_best_meteo_corr_h <- df_meteo_corr_nest_h |>
  filter(pval < 0.1) |>
  select(-cor) |>
  group_by(meteo_var, trait) |>
  mutate(abs_corr = abs(corrcoef)) |>
  slice_max(abs_corr, n = 1, with_ties = FALSE)
df_best_meteo_corr_h

df_meteo_corr_nest_h |>
  filter(trait == "psi_midday_mpa") |>
  ggplot(aes(x = timeframe * 24, y = corrcoef)) +
  geom_point(aes(col = ifelse(pval < 0.05, "darkgreen", "grey"))) +
  scale_color_identity() +
  facet_wrap(~meteo_var) +
  labs(
    y = "correlation coefficient",
    x = "cumulative timeframe [hours]",
    title = "Midday water potential"
  ) +
  thesis_theme

df_meteo_corr_nest |>
  filter(trait == "psi_midday_mpa", ) |>
  ggplot(aes(x = timeframe, y = corrcoef)) +
  geom_point(aes(col = ifelse(pval < 0.05, "darkgreen", "grey"))) +
  scale_color_identity() +
  facet_wrap(~meteo_var) +
  labs(
    y = "correlation coefficient",
    x = "cumulative timeframe [days]",
    title = "Midday water potential"
  ) +
  thesis_theme

df_test <- df_meteo_corr_nest |>
  filter(trait == "psi_midday_mpa", meteo_var == "mean_vpd") |>
  unnest(data)

df_test |>
  mutate(
    species = recode(
      species,
      "FREX" = "Fraxinus excelsior",
      "FASY" = "Fagus sylvatica"
    )
  ) |>
  ggplot(aes(x = meteo_val, y = trait_val, col = species)) +
  geom_smooth(method = "lm") +
  geom_point() +
  facet_wrap(~timeframe) +
  scale_color_manual(
    values = c("Fagus sylvatica" = "#410866", "Fraxinus excelsior" = "#D55E00")
  ) +
  labs(
    y = "Midday water potential [MPa]",
    x = "mean relative extractable soil water [%]",
    title = "Midday water potential vs. mean relative extractable soil water",
    subtitle = "Cumulative timeframe [days]"
  ) +
  thesis_theme

## 2.2 single models -------------------------------------------------------

alpha <- 0.05
z.value <- qnorm(1 - alpha / 2) # 1.96 for 95% CI

recode = dplyr::recode

### 2.2.1 midday water potentials ---------------------------------------------

df_psi_md_meteo <- df_best_meteo_corr |>
  ungroup() |>
  filter(trait == "psi_midday_mpa") |>
  arrange(desc(abs_corr)) |>
  unnest(data) |>
  mutate(
    meteo_predictor = paste(meteo_var, abs(timeframe), sep = "_"),
    pheno_start_doy = case_when(
      species == "FASY" & year == 2023 ~ 121,
      species == "FASY" & year == 2024 ~ 104,
      species == "FREX" & year == 2023 ~ 140,
      species == "FREX" & year == 2024 ~ 138
    ),
    doy = yday(date),
    leaf_age_d = doy - pheno_start_doy
  ) |>
  select(-c(timeframe, meteo_var, corrcoef, pval, abs_corr, pheno_start_doy)) |>
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val)
df_psi_md_meteo |> head()

df_psi_md_meteo_scaled <- df_psi_md_meteo |>
  select(c(
    trait,
    date,
    year,
    species,
    sample_id,
    campaign,
    trait_val,
    leaf_age_d
  )) |>
  cbind(
    df_psi_md_meteo |>
      select(
        -c(
          trait,
          date,
          year,
          species,
          sample_id,
          campaign,
          trait_val,
          leaf_age_d
        )
      ) |>
      scale()
  )

f.make_formula_char(df_psi_md_meteo) |> writeClipboard()

lm_psi_md <- lm(
  trait_val ~ leaf_age_d +
    max_ta_50 +
    sum_p_45 +
    mean_rew_1 +
    mean_ta_60 +
    sum_p_et_60 +
    sum_et_35 +
    mean_et_pet_1 +
    mean_vpd_35,
  data = df_psi_md_meteo_scaled,
  na.action = "na.omit"
)

lm_psi_md |> summary()

MASS::stepAIC(lm_psi_md, direction = "both") |> summary()
MASS::stepAIC(lme_psi_md, direction = "backward") |> formula()
MuMIn::dredge(lme_psi_md, rank = "AICc") |> head()

lme_psi_md <- lme(
  trait_val ~ sum_p_10 + sum_p_et_60 + max_ta_50 + sum_et_35,
  random = ~ 1 | species / sample_id,
  data = df_psi_md_meteo_scaled,
  control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 200),
  method = "ML"
)

lme_psi_md |> check_collinearity()
lme_psi_md |> summary()
lme_psi_md |> r2()
lme_psi_md |> check_model()


#### plots ----

model_predictors <- names(fixef(lme_psi_md)) # Extract fixed effect names

# Create a named vector for label replacement
df_predictors <- df_predictors %>%
  mutate(base_abbr = extract_base_name(abbr)) # Create a column without numbers

# Generate a named vector for matching
predictor_labels <- setNames(df_predictors$long, df_predictors$base_abbr)

# Apply formatting to all predictors
new_labels <- sapply(model_predictors, format_label)

plot.psi_md_coefs.png <-
  sjPlot::plot_model(
    lme_psi_md,
    show.values = TRUE,
    show.p = T,
    value.offset = 0.3,
    line.size = 0.5,
    dot.size = 1.3
  ) +
  geom_hline(aes(yintercept = 0), alpha = 0.2) +
  scale_x_discrete(labels = new_labels) +
  labs(y = "Coefficient estimates", title = NULL, tag = "b)") +
  # annotate("text", x = 3.4, y = -0.95, label = "b)") +
  thesis_theme +
  theme(axis.text = element_text(size = 8), text = element_text(size = 6))
plot.psi_md_coefs.png
# ggsave("figures/psi_midday/plot.psi_midday_coefs.png",
#        plot.psi_midday_coefs.png, width = 6, height = 8, dpi = 150, units = "cm")

df_psi_md_meteo <- df_psi_md_meteo |>
  mutate(
    species = dplyr::recode(
      species,
      "FREX" = "Fraxinus excelsior",
      "FASY" = "Fagus sylvatica"
    ),
    pred = predict(lme_psi_md, type = "response"),
    date_plot = date,
    year = as.factor(year)
  )
year(df_psi_md_meteo$date_plot) = 2000

plot.psi_md_meteo.png <- df_psi_md_meteo |>
  ggplot() +
  geom_point(
    aes(x = leaf_age_d, y = trait_val, col = species, shape = year),
    show.legend = F
  ) +
  geom_line(
    aes(
      x = leaf_age_d,
      y = pred,
      col = species,
      group = interaction(species, year, sample_id),
      linetype = year
    ),
    alpha = 0.5,
    linewidth = 0.5,
    show.legend = F
  ) +
  scale_x_continuous(limits = c(0, 170), breaks = seq(0, 165, 15)) +
  scale_y_continuous(limits = c(-4, 0)) +
  scale_shape_manual(values = c("2023" = 16, "2024" = 1)) +
  scale_color_oi(order = c(6, 2)) +
  labs(y = "Midday water potential [MPa]", x = "Leaf age [days]", tag = "a)") +
  guides(
    color = guide_legend(title = "Species", ncol = 2),
    shape = guide_legend(title = "Year", ncol = 2),
    linetype = guide_legend(title = "Year", ncol = 2)
  ) +
  # annotate("text", x = 0, y = 0, label = "a)") +
  thesis_theme +
  theme(
    legend.text = element_text(size = 8, face = "italic"),
    legend.justification = c(0.1, 0)
  )
plot.psi_md_meteo.png
# ggsave("figures/psi_midday/plot.psi_midday_meteo.png",
#        plot.psi_midday_meteo.png, width = 10, height = 8, dpi = 150, units = "cm")

plot.psi_midday_comb.png <- (plot.psi_md_meteo.png +
  plot_spacer() +
  plot.psi_md_coefs.png) +
  plot_layout(widths = c(11, -0.9, 5))
plot.psi_midday_comb.png
# Save the updated plot
ggsave(
  "figures/psi_midday/plot.psi_midday_comb.png",
  plot.psi_midday_comb.png,
  width = 16,
  height = 8,
  dpi = 150,
  units = "cm"
)

df_psi_md_meteo |>
  ggplot() +
  geom_point(aes(x = sum_et_35, y = trait_val, col = species, shape = year))

mgcv::gamm(
  trait_val ~ mean_rew_1 + sum_et_35,
  random = 1 | species / sample_id,
  data = df_psi_md_meteo
) |>
  summary()

df_psi_md_meteo$preds_test <- mgcv::gam(
  trait_val ~ sum_p_et_60 + mean_rew_1 + max_ta_50 + sum_et_35,
  data = df_psi_md_meteo
) |>
  predict()

ggplot(df_psi_md_meteo) +
  geom_point(aes(x = leaf_age_d, y = trait_val, col = species, shape = year)) +
  geom_line(
    aes(
      x = leaf_age_d,
      y = preds_test,
      col = species,
      group = interaction(species, year, sample_id),
      linetype = year
    ),
    alpha = 0.5,
    linewidth = 0.5
  )

#### export ----

lme_psi_md |>
  r2() |>
  as.data.frame() |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

lme_psi_md |>
  broom.mixed::tidy() |>
  mutate(term = sapply(model_predictors, format_label_export)) |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

lme_psi_md$modelStruct$reStruct

lme_psi_md |>
  ranef() |>
  as.data.frame() |>
  select(-group) |>
  pivot_wider(names_from = "term", values_from = "estimate") |>
  rename("Intercept" = `(Intercept)`) |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")


### 2.2.2 psi_ft ----

test_vals_meteo_nest |>
  filter(trait == "psi_ft") |>
  ggplot(aes(x = timeframe, y = corrcoef)) +
  geom_point(aes(col = ifelse(pval <= 0.05, "darkgreen", "grey"))) +
  scale_color_identity() +
  facet_wrap(~meteo_var)

df_psi_ft_meteo <- df_best_meteo_corr |>
  ungroup() |>
  filter(trait == "psi_ft") |>
  arrange(desc(abs_corr)) |>
  unnest(data) |>
  mutate(
    meteo_predictor = paste(meteo_var, abs(timeframe), sep = "_"),
    pheno_start_doy = case_when(
      species == "FASY" & year == 2023 ~ 121,
      species == "FASY" & year == 2024 ~ 104,
      species == "FREX" & year == 2023 ~ 140,
      species == "FREX" & year == 2024 ~ 138
    ),
    doy = yday(date),
    leaf_age_d = doy - pheno_start_doy
  ) |>
  select(-c(timeframe, meteo_var, corrcoef, pval, abs_corr, pheno_start_doy)) |>
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val) |>
  na.omit()

df_psi_ft_meteo_scaled <- df_psi_ft_meteo |>
  select(c(trait, date, year, species, sample_id, campaign, trait_val)) |>
  cbind(
    df_psi_ft_meteo |>
      select(-c(trait, date, year, species, sample_id, campaign, trait_val)) |>
      scale()
  )

f.make_formula_char(df_psi_ft_meteo) |> writeClipboard()

lm_psi_ft <- lm(
  trait_val ~ leaf_age_d +
    mean_vpd_45 +
    mean_rh_45 +
    max_ta_35 +
    mean_rew_60 +
    sum_pet_60 +
    mean_ta_40 +
    sum_ppfd_60 +
    sum_p_et_45 +
    sum_et_50 +
    mean_et_pet_20 +
    sum_p_45,
  data = df_psi_ft_meteo_scaled,
  na.action = "na.fail"
)

lm_psi_ft |> summary()

stepAIC(lm_psi_ft, direction = "both") |> summary()
stepAIC(lm_psi_ft, direction = "both") |> formula()

lm_psi_ft_dregde <- MuMIn::dredge(lm_psi_ft, rank = "AICc")
lm_psi_ft_dregde |> head()

lme_psi_ft <- lme(
  trait_val ~ leaf_age_d + mean_rh_45 + max_ta_35 + sum_pet_60,
  random = ~ leaf_age_d | species / sample_id,
  data = df_psi_ft_meteo_scaled,
  correlation = corAR1(form = ~ date | species / sample_id),
  control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 200)
)

lme_psi_ft |> summary()
lme_psi_ft |> r2()
lme_psi_ft |> check_model()
lme_psi_ft |> check_collinearity()


#### plots --------------------------------------------------------------------

model_predictors <- names(fixef(lme_psi_ft)) # Extract fixed effect names

# Create a named vector for label replacement
df_predictors <- df_predictors %>%
  mutate(base_abbr = extract_base_name(abbr)) # Create a column without numbers

# Generate a named vector for matching
predictor_labels <- setNames(df_predictors$long, df_predictors$base_abbr)

# Apply formatting to all predictors
new_labels <- sapply(model_predictors, format_label)

plot.psi_ft_coefs.png <-
  sjPlot::plot_model(
    lme_psi_ft,
    show.values = TRUE,
    show.p = T,
    value.offset = 0.3,
    line.size = 0.5,
    dot.size = 1.3
  ) +
  geom_hline(aes(yintercept = 0), alpha = 0.2) +
  scale_x_discrete(labels = new_labels) +
  labs(y = "Coefficient estimates", title = NULL, tag = "d)") +
  # annotate("text", x = 5.4, y = -0.95, label = "b)") +
  thesis_theme +
  theme(axis.text = element_text(size = 8), text = element_text(size = 6))
plot.psi_ft_coefs.png
# ggsave("figures/psi_ft/plot.psi_ft_coefs.png",
#        plot.psi_ft_coefs.png, width = 6, height = 8, dpi = 150, units = "cm")

df_psi_ft_meteo <- df_psi_ft_meteo |>
  mutate(
    species = dplyr::recode(
      species,
      "FREX" = "Fraxinus excelsior",
      "FASY" = "Fagus sylvatica"
    ),
    pred = predict(lme_psi_ft, type = "response"),
    date_plot = date,
    year = as.factor(year)
  )
year(df_psi_ft_meteo$date_plot) = 2000

plot.psi_ft_meteo.png <- df_psi_ft_meteo |>
  ggplot() +
  geom_point(
    aes(x = leaf_age_d, y = trait_val, col = species, shape = year),
    show.legend = F
  ) +
  geom_line(
    aes(
      x = leaf_age_d,
      y = pred,
      col = species,
      group = interaction(species, year, sample_id),
      linetype = year
    ),
    alpha = 0.5,
    linewidth = 0.5,
    show.legend = F
  ) +
  scale_x_continuous(limits = c(0, 180), breaks = seq(0, 180, 20)) +
  scale_y_continuous(limits = c(-4, 0)) +
  scale_shape_manual(values = c("2023" = 16, "2024" = 2)) +
  scale_shape_manual(values = c("2023" = 16, "2024" = 1)) +
  scale_color_oi(order = c(6, 2)) +
  labs(
    y = "Osm. potential at full turgor [MPa]",
    x = "Leaf age [days]",
    tag = "c)"
  ) +
  guides(
    color = guide_legend(title = "Species", ncol = 2),
    shape = guide_legend(title = "Year", ncol = 2),
    linetype = guide_legend(title = "Year", ncol = 2)
  ) +
  # annotate("text", x = 0, y = 0, label = "a)") +
  thesis_theme +
  theme(
    legend.text = element_text(size = 8, face = "italic"),
    legend.justification = c(0, 0)
  )
plot.psi_ft_meteo.png
# ggsave("figures/psi_ft/plot.psi_ft_meteo.png",
#        plot.psi_ft_meteo.png, width = 10, height = 8, dpi = 150, units = "cm")

plot.psi_ft_comb.png <- (plot.psi_ft_meteo.png +
  plot_spacer() +
  plot.psi_ft_coefs.png) +
  plot_layout(widths = c(10, 0, 6))
plot.psi_ft_comb.png

ggsave(
  "figures/psi_ft/plot.psi_ft_comb.png",
  plot.psi_ft_comb.png,
  width = 16,
  height = 8,
  dpi = 150,
  units = "cm"
)

#### export ----

lme_psi_ft |>
  r2() |>
  as.data.frame() |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

lme_psi_ft |>
  broom.mixed::tidy() |>
  mutate(term = sapply(model_predictors, format_label_export)) |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

lme_psi_ft$modelStruct$reStruct

lme_psi_ft |>
  ranef() |>
  as.data.frame() |>
  select(-group) |>
  pivot_wider(names_from = "term", values_from = "estimate") |>
  rename("Intercept" = `(Intercept)`, "Leaf age" = leaf_age_d) |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

### 2.2.3 psi_tlp ----

df_meteo_corr_nest |>
  filter(trait == "psi_tlp") |>
  ggplot(aes(x = timeframe, y = corrcoef)) +
  geom_point(aes(col = ifelse(pval <= 0.05, "darkgreen", "grey"))) +
  scale_color_identity() +
  facet_wrap(~meteo_var)

df_psi_tlp_meteo <- df_best_meteo_corr |>
  ungroup() |>
  filter(trait == "psi_tlp") |>
  arrange(desc(abs_corr)) |>
  unnest(data) |>
  mutate(
    meteo_predictor = paste(meteo_var, abs(timeframe), sep = "_"),
    pheno_start_doy = case_when(
      species == "FASY" & year == 2023 ~ 121,
      species == "FASY" & year == 2024 ~ 104,
      species == "FREX" & year == 2023 ~ 140,
      species == "FREX" & year == 2024 ~ 138
    ),
    doy = yday(date),
    leaf_age_d = doy - pheno_start_doy
  ) |>
  select(-c(timeframe, meteo_var, corrcoef, pval, abs_corr, pheno_start_doy)) |>
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val) |>
  na.omit()

df_psi_tlp_meteo_scaled <- df_psi_tlp_meteo |>
  select(c(trait, date, year, species, sample_id, campaign, trait_val, doy)) |>
  cbind(
    df_psi_tlp_meteo |>
      select(
        -c(trait, date, year, species, sample_id, campaign, trait_val, doy)
      ) |>
      scale()
  )

f.make_formula_char(df_psi_tlp_meteo) |> writeClipboard()

lm_psi_tlp <- lm(
  trait_val ~ leaf_age_d +
    mean_vpd_35 +
    mean_rew_60 +
    mean_rh_45 +
    sum_pet_60 +
    max_ta_35 +
    mean_ta_35 +
    sum_ppfd_60 +
    sum_p_et_45 +
    sum_et_1 +
    sum_p_45 +
    mean_et_pet_60,
  data = df_psi_tlp_meteo_scaled,
  na.action = "na.fail"
)

lm_psi_tlp |> summary()

stepAIC(lm_psi_tlp, direction = "both") |> summary()
stepAIC(lm_psi_tlp, direction = "both") |> formula()

lm_psi_tlp_dregde <- MuMIn::dredge(lme_psi_tlp, rank = "AICc")
lm_psi_tlp_dregde |> head()
lm_psi_tlp_dregde[1, ] |> f.make_formula_dredge()

lme_psi_tlp <- nlme::lme(
  trait_val ~ leaf_age_d + mean_rew_60 + mean_et_pet_60 + max_ta_35,
  random = ~ 1 | species / sample_id,
  data = df_psi_tlp_meteo_scaled,
  # correlation = corAR1(form = ~ date | species/sample_id),
  control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 200),
  method = "ML"
)

lme_psi_tlp |> summary()
lme_psi_tlp |> r2()
lme_psi_tlp |> check_model()
lme_psi_tlp |> check_collinearity()
lme_psi_tlp |> check_autocorrelation()

#### plots --------------------------------------------------------------------

model_predictors <- names(fixef(lme_psi_tlp)) # Extract fixed effect names

# Create a named vector for label replacement
df_predictors <- df_predictors %>%
  mutate(base_abbr = extract_base_name(abbr)) # Create a column without numbers

# Generate a named vector for matching
predictor_labels <- setNames(df_predictors$long, df_predictors$base_abbr)

# Apply formatting to all predictors
new_labels <- sapply(model_predictors, format_label)

plot.psi_tlp_coefs.png <-
  sjPlot::plot_model(
    lme_psi_tlp,
    show.values = TRUE,
    show.p = T,
    value.offset = 0.3,
    line.size = 0.5,
    dot.size = 1.3
  ) +
  geom_hline(aes(yintercept = 0), alpha = 0.2) +
  scale_x_discrete(labels = new_labels) +
  labs(y = "Coefficient estimates", title = NULL, tag = "f)") +
  # annotate("text", x = 4.4, y = -0.95, label = "b)") +
  thesis_theme +
  theme(axis.text = element_text(size = 8), text = element_text(size = 6))
plot.psi_tlp_coefs.png
# ggsave("figures/psi_tlp/plot.psi_tlp_coefs.png",
#        plot.psi_tlp_coefs.png, width = 6, height = 8, dpi = 150, units = "cm")

df_psi_tlp_meteo <- df_psi_tlp_meteo |>
  mutate(
    species = dplyr::recode(
      species,
      "FREX" = "Fraxinus excelsior",
      "FASY" = "Fagus sylvatica"
    ),
    pred = predict(lme_psi_tlp, type = "response"),
    date_plot = date,
    year = as.factor(year)
  )
year(df_psi_tlp_meteo$date_plot) = 2000

plot.psi_tlp_meteo.png <- df_psi_tlp_meteo |>
  ggplot() +
  geom_point(aes(x = leaf_age_d, y = trait_val, col = species, shape = year)) +
  geom_line(
    aes(
      x = leaf_age_d,
      y = pred,
      col = species,
      group = interaction(species, year),
      linetype = year
    ),
    alpha = 0.5,
    linewidth = 0.5
  ) +
  scale_x_continuous(limits = c(0, 180), breaks = seq(0, 180, 20)) +
  scale_y_continuous(limits = c(-4, 0)) +
  scale_shape_manual(values = c("2023" = 16, "2024" = 1)) +
  scale_color_oi(order = c(6, 2)) +
  labs(
    y = "Osm. potential at turgor loss [MPa]",
    x = "Leaf age [days]",
    tag = "e)"
  ) +
  guides(
    color = guide_legend(title = "Species", ncol = 2),
    shape = guide_legend(title = "Year", ncol = 2),
    linetype = guide_legend(title = "Year", ncol = 2)
  ) +
  # annotate("text", x = 0, y = 0, label = "a)") +
  thesis_theme +
  theme(
    legend.text = element_text(size = 8, face = "italic"),
    legend.justification = c(0.25, 0)
  )
plot.psi_tlp_meteo.png
# ggsave("figures/psi_tlp/plot.psi_tlp_meteo.png",
#        plot.psi_tlp_meteo.png, width = 10, height = 8, dpi = 150, units = "cm")

plot.psi_tlp_comb.png <- (plot.psi_tlp_meteo.png +
  plot_spacer() +
  plot.psi_tlp_coefs.png) +
  plot_layout(widths = c(11, -0.9, 5))
plot.psi_tlp_comb.png

ggsave(
  "figures/psi_tlp/plot.psi_tlp_comb.png",
  plot.psi_tlp_comb.png,
  width = 16,
  height = 8,
  dpi = 150,
  units = "cm"
)

#### export ----

lme_psi_tlp |>
  r2() |>
  as.data.frame() |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

lme_psi_tlp |>
  broom.mixed::tidy() |>
  mutate(
    term = sapply(model_predictors, format_label_export) |>
      str_replace("\\(Intercept\\)", "Intercept")
  ) |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

lme_psi_tlp$modelStruct$reStruct

lme_psi_tlp |>
  ranef() |>
  as.data.frame() |>
  select(-group) |>
  pivot_wider(names_from = "term", values_from = "estimate") |>
  rename("Intercept" = `(Intercept)`) |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")


# plot md, ft, tlp --------------------------------------------------------

plot.psi_md_ft_tlp.png <-
  plot.psi_md_meteo.png +
  plot_spacer() +
  plot.psi_md_coefs.png +
  plot.psi_ft_meteo.png +
  plot_spacer() +
  plot.psi_ft_coefs.png +
  plot.psi_tlp_meteo.png +
  plot_spacer() +
  plot.psi_tlp_coefs.png +
  plot_layout(widths = c(11, -0.9, 5), heights = c(8, 8, 8))
plot.psi_md_ft_tlp.png

ggsave(
  "figures/plot.psi_md_ft_tlp.png",
  plot.psi_md_ft_tlp.png,
  width = 16,
  height = 22,
  units = "cm"
)


### 2.2.4 elast ----

df_meteo_corr_nest |>
  filter(trait == "elasticity_tot") |>
  ggplot(aes(x = timeframe, y = corrcoef)) +
  geom_point(aes(col = ifelse(pval <= 0.05, "darkgreen", "grey"))) +
  scale_color_identity() +
  facet_wrap(~meteo_var)

df_elast_meteo <- df_best_meteo_corr |>
  ungroup() |>
  filter(trait == "elasticity_tot") |>
  arrange(desc(abs_corr)) |>
  unnest(data) |>
  mutate(
    meteo_predictor = paste(meteo_var, abs(timeframe), sep = "_"),
    pheno_start_doy = case_when(
      species == "FASY" & year == 2023 ~ 121,
      species == "FASY" & year == 2024 ~ 104,
      species == "FREX" & year == 2023 ~ 140,
      species == "FREX" & year == 2024 ~ 138
    ),
    doy = yday(date),
    leaf_age_d = doy - pheno_start_doy
  ) |>
  select(-c(timeframe, meteo_var, corrcoef, pval, abs_corr, pheno_start_doy)) |>
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val) |>
  na.omit()

df_elast_meteo_scaled <- df_elast_meteo |>
  select(c(trait, date, year, species, sample_id, campaign, trait_val, doy)) |>
  cbind(
    df_elast_meteo |>
      select(
        -c(trait, date, year, species, sample_id, campaign, trait_val, doy)
      ) |>
      scale()
  )

f.make_formula_char(df_elast_meteo) |> writeClipboard()

lm_elast <- lm(
  trait_val ~ leaf_age_d +
    mean_ta_60 +
    max_ta_55 +
    sum_pet_20 +
    sum_et_60 +
    sum_ppfd_5 +
    mean_rh_20 +
    mean_rew_60 +
    sum_p_55 +
    mean_vpd_15,
  data = df_elast_meteo_scaled,
  na.action = "na.fail"
)

lm_elast |> summary()

stepAIC(lm_elast, direction = "both") |> summary()
stepAIC(lm_elast, direction = "both") |> formula()
MuMIn::dredge(glme_elast) |> head(10)

glme_elast <- glmmTMB::glmmTMB(
  trait_val ~ leaf_age_d + max_ta_55 + sum_pet_20 + sum_et_60 + (1 | species),
  data = df_elast_meteo_scaled,
  family = Gamma(link = "log")
)

glme_elast |> summary()
glme_elast |> check_collinearity()
glme_elast |> r2()
glme_elast |> check_model()
glme_elast |> ranef()

#### plots --------------------------------------------------------------------

lme_elast = lmer(
  trait_val ~ leaf_age_d + max_ta_55 + sum_pet_20 + sum_et_60 + +(1 | species),
  # ar1(as.factor(leaf_age_d) + 0 | species/sample_id),
  data = df_elast_meteo_scaled,
)

model_predictors <- names(fixef(lme_elast)) # Extract fixed effect names

# Create a named vector for label replacement
df_predictors <- df_predictors %>%
  mutate(base_abbr = extract_base_name(abbr)) # Create a column without numbers

# Generate a named vector for matching
predictor_labels <- setNames(df_predictors$long, df_predictors$base_abbr)

# Apply formatting to all predictors
new_labels <- sapply(model_predictors, format_label)

# Extract fixed effects on link scale
coefs <- summary(glme_elast)$coefficients$cond |>
  as.data.frame()

# Create dataframe for plotting
coef_df <- data.frame(term = rownames(coefs)) |>
  mutate(
    estimate = coefs[, 1],
    lower = coefs[, 1] - 1.96 * coefs[, 2], # Confidence interval
    upper = coefs[, 1] + 1.96 * coefs[, 2],
    est_label = round(estimate, 2),
    p = case_when(
      coefs[, 4] >= 0.05 ~ "",
      coefs[, 4] < 0.05 & coefs[, 4] >= 0.01 ~ "*",
      coefs[, 4] < 0.01 & coefs[, 4] >= 0.001 ~ "**",
      coefs[, 4] < 0.001 ~ "***"
    ),
    labels = paste(est_label, p, sep = " ")
  )

plot.elast_coefs.png <- coef_df |>
  filter(term != "(Intercept)") |>
  mutate(
    coef_color = factor(case_when(estimate > 0 ~ "blue", TRUE ~ "red")), # Use labels instead of hex
    term = fct_rev(factor(term))
  ) |>
  ggplot(aes(y = term, x = estimate, color = coef_color)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0) +
  geom_text(aes(label = labels), vjust = -1, size = 4) +
  # annotate("text", x = -0.45, y = 4.4, label = "b)") +
  scale_y_discrete(labels = new_labels) +
  # scale_x_continuous(limits = c(0, 2)) +
  scale_color_manual(values = c("blue" = "#377eb8", "red" = "#e41a1c")) + # Manually define colors
  labs(y = "", x = "Coefficient estimates", tag = "b)") +
  thesis_theme +
  theme(
    axis.text = element_text(size = 8),
    text = element_text(size = 6),
    legend.position = "none"
  )
plot.elast_coefs.png

# plot.elast_coefs.png <-
#   sjPlot::plot_model(glme_elast, type = "est",
#                      show.values = TRUE, show.p = T, value.offset = 0.3, line.size = 0.5, dot.size = 1.3) +
#   geom_hline(aes(yintercept = 0), alpha= 0.2)+
#   scale_x_discrete(labels = new_labels) +
#   labs(y = "Coefficient estimates", title = NULL) +
#   annotate("text", x = 4.4, y = 0.12, label = "b)") +
#   thesis_theme +
#   theme(axis.text = element_text(size = 8),
#         text = element_text(size = 6))
# plot.elast_coefs.png

df_elast_meteo <- df_elast_meteo |>
  mutate(
    pred = predict(glme_elast, type = "response"),
    date_plot = date,
    year = as.factor(year)
  )
year(df_elast_meteo$date_plot) = 2000

plot.elast_meteo.png <- df_elast_meteo |>
  mutate(
    species = dplyr::recode(
      species,
      "FREX" = "Fraxinus excelsior",
      "FASY" = "Fagus sylvatica"
    )
  ) |>
  ggplot() +
  geom_point(
    aes(x = leaf_age_d, y = trait_val, col = species, shape = year),
    show.legend = F
  ) +
  geom_line(
    aes(
      x = leaf_age_d,
      y = pred,
      col = species,
      group = interaction(species, year, sample_id),
      linetype = year
    ),
    alpha = 0.2,
    linewidth = 0.5,
    show.legend = F
  ) +
  scale_x_continuous(limits = c(0, 180), breaks = seq(0, 180, 20)) +
  scale_y_continuous(limits = c(0, 40)) +
  scale_shape_manual(values = c("2023" = 16, "2024" = 1)) +
  scale_color_oi(order = c(6, 2)) +
  labs(
    y = "Bulk modulus of elasticity [MPa]",
    x = "Leaf age [days]",
    tag = "a)"
  ) +
  guides(
    color = guide_legend(title = "Species", ncol = 2),
    shape = guide_legend(title = "Year", ncol = 2),
    linetype = guide_legend(title = "Year", ncol = 2)
  ) +
  # annotate("text", x = 0, y = 40, label = "a)") +
  thesis_theme +
  theme(
    legend.text = element_text(size = 8, face = "italic"),
    legend.justification = c(0.25, 0)
  )
plot.elast_meteo.png
# ggsave("figures/elast/plot.elast_meteo.png",
#        plot.elast_meteo.png, width = 10, height = 8, dpi = 150, units = "cm")

# plot.elast_comb.png <- (plot.elast_meteo.png + plot_spacer() + plot.elast_coefs.png) +
#   plot_layout(widths = c(11, -0.9, 5))
# plot.elast_comb.png
#
# ggsave("figures/elast/plot.elast_comb.png",
#        plot.elast_comb.png, width = 16, height = 8, dpi = 150, units = "cm")

#### export ----

glme_elast |>
  r2() |>
  as.data.frame() |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

broom.mixed::tidy(glme_elast)[1:5, ] |>
  select(-c(effect, component, group)) |>
  mutate(
    term = sapply(model_predictors, format_label_export) |>
      str_replace("\\(Intercept\\)", "Intercept")
  ) |>
  as.data.frame() |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

glme_elast |>
  ranef() |>
  as.data.frame() |>
  dplyr::select(term, grp, condval) |>
  pivot_wider(names_from = "term", values_from = "condval") |>
  rename("Intercept" = "(Intercept)") |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

### 2.2.5 rwc_tlp --------------------------------------------------------------------

df_meteo_corr_nest |>
  filter(trait == "rwc_tot_tlp") |>
  ggplot(aes(x = timeframe, y = corrcoef)) +
  geom_point(aes(col = ifelse(pval <= 0.05, "darkgreen", "grey"))) +
  scale_color_identity() +
  facet_wrap(~meteo_var)

df_rwc_tlp_meteo <- df_best_meteo_corr |>
  ungroup() |>
  filter(trait == "rwc_tot_tlp") |>
  arrange(desc(abs_corr)) |>
  unnest(data) |>
  mutate(
    meteo_predictor = paste(meteo_var, abs(timeframe), sep = "_"),
    pheno_start_doy = case_when(
      species == "FASY" & year == 2023 ~ 121,
      species == "FASY" & year == 2024 ~ 104,
      species == "FREX" & year == 2023 ~ 140,
      species == "FREX" & year == 2024 ~ 138
    ),
    doy = yday(date),
    leaf_age_d = doy - pheno_start_doy
  ) |>
  select(-c(timeframe, meteo_var, corrcoef, pval, abs_corr, pheno_start_doy)) |>
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val) |>
  na.omit()

df_rwc_tlp_meteo_scaled <- df_rwc_tlp_meteo |>
  select(c(trait, date, year, species, sample_id, campaign, trait_val, doy)) |>
  cbind(
    df_rwc_tlp_meteo |>
      select(
        -c(trait, date, year, species, sample_id, campaign, trait_val, doy)
      ) |>
      scale()
  )

f.make_formula_char(df_rwc_tlp_meteo) |> writeClipboard()

lm_rwc_tlp <- lm(
  trait_val ~ leaf_age_d +
    max_ta_60 +
    sum_et_60 +
    mean_ta_60 +
    sum_pet_20 +
    sum_ppfd_5 +
    mean_et_pet_50 +
    mean_rew_30 +
    sum_p_et_50 +
    mean_rh_20,
  data = df_rwc_tlp_meteo_scaled,
  na.action = "na.fail"
)

lm_rwc_tlp |> summary()

stepAIC(lm_rwc_tlp, direction = "both") |> summary()
stepAIC(lm_rwc_tlp, direction = "both") |> formula()

lm_rwc_tlp_dregde <- MuMIn::dredge(lm_rwc_tlp, rank = "AICc")
lm_rwc_tlp_dregde |> head()
lm_rwc_tlp_dregde[2, ]

f.make_formula_dregde(lm_rwc_tlp_dregde[1, ], F)

lme_rwc_tlp <- nlme::lme(
  trait_val ~ leaf_age_d + sum_et_60 + sum_pet_20 + mean_rew_30,
  random = ~ leaf_age_d | species / sample_id,
  data = df_rwc_tlp_meteo,
  control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 200)
)

lme_rwc_tlp |> summary()
lme_rwc_tlp |> r2()
lme_rwc_tlp |> check_collinearity()
lme_rwc_tlp |> check_model()

#### plots --------------------------------------------------------------------

model_predictors <- names(fixef(lme_rwc_tlp)) # Extract fixed effect names

# Create a named vector for label replacement
df_predictors <- df_predictors %>%
  mutate(base_abbr = extract_base_name(abbr)) # Create a column without numbers

# Generate a named vector for matching
predictor_labels <- setNames(df_predictors$long, df_predictors$base_abbr)

# Apply formatting to all predictors
new_labels <- sapply(model_predictors, format_label)

plot.rwc_tlp_coefs.png <-
  sjPlot::plot_model(
    lme_rwc_tlp,
    show.values = TRUE,
    show.p = T,
    value.offset = 0.3,
    line.size = 0.5,
    dot.size = 1.3
  ) +
  geom_hline(aes(yintercept = 0), alpha = 0.2) +
  scale_x_discrete(labels = new_labels) +
  labs(y = "Coefficient estimates", title = NULL, tag = "d)") +
  # annotate("text", x = 5.35, y = -0.95, label = "b)") +
  thesis_theme +
  theme(axis.text = element_text(size = 8), text = element_text(size = 6))
plot.rwc_tlp_coefs.png
# ggsave("figures/rwc_tlp/plot.rwc_tlp_coefs.png",
#        plot.rwc_tlp_coefs.png, width = 6, height = 8, dpi = 150, units = "cm")

df_rwc_tlp_meteo <- df_rwc_tlp_meteo |>
  mutate(
    species = dplyr::recode(
      species,
      "FREX" = "Fraxinus excelsior",
      "FASY" = "Fagus sylvatica"
    ),
    pred = predict(lme_rwc_tlp, type = "response"),
    date_plot = date,
    year = as.factor(year)
  )
year(df_rwc_tlp_meteo$date_plot) = 2000

plot.rwc_tlp_meteo.png <- df_rwc_tlp_meteo |>
  ggplot() +
  geom_point(
    aes(x = leaf_age_d, y = trait_val, col = species, shape = year),
    show.legend = F
  ) +
  geom_line(
    aes(
      x = leaf_age_d,
      y = pred,
      col = species,
      group = interaction(species, year, sample_id),
      linetype = year
    ),
    alpha = 0.5,
    linewidth = 0.5,
    show.legend = F
  ) +
  scale_x_continuous(limits = c(0, 180), breaks = seq(0, 180, 20)) +
  scale_y_continuous(limits = c(68, 100)) +
  scale_shape_manual(values = c("2023" = 16, "2024" = 1)) +
  scale_color_oi(order = c(6, 2)) +
  labs(y = "RWC at turgor loss [%]", x = "Leaf age [days]", tag = "c)") +
  guides(
    color = guide_legend(title = "Species", ncol = 2),
    shape = guide_legend(title = "Year", ncol = 2),
    linetype = guide_legend(title = "Year", ncol = 2)
  ) +
  # annotate("text", x = 0, y = 100, label = "a)") +
  thesis_theme +
  theme(
    legend.text = element_text(size = 8, face = "italic"),
    legend.justification = c(0, 0)
  )
plot.rwc_tlp_meteo.png
# ggsave("figures/rwc_tlp/plot.rwc_tlp_meteo.png",
#        plot.rwc_tlp_meteo.png, width = 10, height = 8, dpi = 150, units = "cm")

# plot.rwc_tlp_comb.png <- (plot.rwc_tlp_meteo.png + plot_spacer() + plot.rwc_tlp_coefs.png) +
#   plot_layout(widths = c(11, -0.9, 5))
# plot.rwc_tlp_comb.png
#
# ggsave("figures/rwc_tlp/plot.rwc_tlp_comb.png",
#        plot.rwc_tlp_comb.png, width = 16, height = 8, dpi = 150, units = "cm")

#### export ----

lme_rwc_tlp |>
  r2() |>
  as.data.frame() |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

lme_rwc_tlp |>
  broom.mixed::tidy() |>
  mutate(term = sapply(model_predictors, format_label_export)) |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

lme_rwc_tlp$modelStruct$reStruct
lme_rwc_tlp$modelStruct$corStruct

lme_rwc_tlp |>
  ranef() |>
  as.data.frame() |>
  select(-group) |>
  pivot_wider(names_from = "term", values_from = "estimate") |>
  rename("Intercept" = `(Intercept)`, "Leaf age" = leaf_age_d) |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

### 2.2.6 gmin --------------------------------------------------------------------

df_meteo_corr_nest |>
  filter(trait == "gmin") |>
  ggplot(aes(x = timeframe, y = corrcoef)) +
  geom_point(aes(col = ifelse(pval <= 0.05, "darkgreen", "grey"))) +
  scale_color_identity() +
  facet_wrap(~meteo_var)

df_gmin_meteo <- df_best_meteo_corr |>
  ungroup() |>
  filter(trait == "gmin") |>
  arrange(desc(abs_corr)) |>
  unnest(data) |>
  mutate(
    meteo_predictor = paste(meteo_var, abs(timeframe), sep = "_"),
    pheno_start_doy = case_when(
      species == "FASY" & year == 2023 ~ 121,
      species == "FASY" & year == 2024 ~ 104,
      species == "FREX" & year == 2023 ~ 140,
      species == "FREX" & year == 2024 ~ 138
    ),
    doy = yday(date),
    leaf_age_d = doy - pheno_start_doy
  ) |>
  select(-c(timeframe, meteo_var, corrcoef, pval, abs_corr, pheno_start_doy)) |>
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val) |>
  na.omit()

df_gmin_meteo_scaled <- df_gmin_meteo |>
  select(c(trait, date, year, species, sample_id, campaign, trait_val, doy)) |>
  cbind(
    df_gmin_meteo |>
      select(
        -c(trait, date, year, species, sample_id, campaign, trait_val, doy)
      ) |>
      scale()
  )

f.make_formula_char(df_gmin_meteo) |> writeClipboard()

lm_gmin <- lm(
  trait_val ~ leaf_age_d +
    mean_et_pet_60 +
    max_ta_55 +
    sum_pet_30 +
    sum_ppfd_30 +
    mean_ta_60 +
    mean_rh_35 +
    mean_vpd_15 +
    sum_p_2 +
    sum_et_10 +
    sum_p_et_2,
  data = df_gmin_meteo_scaled,
  na.action = "na.fail"
)

lm_gmin |> summary()

stepAIC(lm_gmin, direction = "both") |> summary()
stepAIC(lm_gmin, direction = "both") |> formula()

lm_gmin_dregde <- MuMIn::dredge(glme_gmin, rank = "AICc")
lm_gmin_dregde |> head()
lm_gmin_dregde[1, ]

glme_gmin <- glmmTMB::glmmTMB(
  trait_val ~ max_ta_55 + sum_p_2 + sum_pet_30 + (1 | species),
  data = df_gmin_meteo_scaled,
  family = Gamma(link = "log"),
  na.action = "na.fail"
)

glme_gmin |> summary()
glme_gmin |> r2()
glme_gmin |> check_collinearity()
glme_gmin |> check_model()

#### plots --------------------------------------------------------------------

lme_gmin = lmer(
  trait_val ~ max_ta_55 + sum_p_2 + sum_pet_30 + (1 | species / sample_id),
  data = df_gmin_meteo_scaled,
)

lme_gmin |> summary()

model_predictors <- names(fixef(lme_gmin)) # Extract fixed effect names

# Create a named vector for label replacement
df_predictors <- df_predictors %>%
  mutate(base_abbr = extract_base_name(abbr)) # Create a column without numbers

# Generate a named vector for matching
predictor_labels <- setNames(df_predictors$long, df_predictors$base_abbr)

# Apply formatting to all predictors
new_labels <- sapply(model_predictors, format_label)

# Extract fixed effects on link scale
coefs <- summary(glme_gmin)$coefficients$cond |>
  as.data.frame()

# Create dataframe for plotting
coef_df <- data.frame(term = rownames(coefs)) |>
  mutate(
    estimate = coefs[, 1],
    lower = coefs[, 1] - 1.96 * coefs[, 2], # Confidence interval
    upper = coefs[, 1] + 1.96 * coefs[, 2],
    est_label = round(estimate, 2),
    p = case_when(
      coefs[, 4] >= 0.05 ~ "",
      coefs[, 4] < 0.05 & coefs[, 4] >= 0.01 ~ "*",
      coefs[, 4] < 0.01 & coefs[, 4] >= 0.001 ~ "**",
      coefs[, 4] < 0.001 ~ "***"
    ),
    labels = paste(est_label, p, sep = " ")
  )

# plot.gmin_coefs.png <- coef_df |>
#   filter(term != "(Intercept)") |>
#   mutate(
#     coef_color = factor(case_when(estimate > 0 ~ "blue", TRUE ~ "red")),  # Use labels instead of hex
#     term = fct_rev(factor(term))
#   ) |>
#   ggplot(aes(y = term, x = estimate, color = coef_color)) +
#   geom_point(size = 1.5) +
#   geom_errorbar(aes(xmin = lower, xmax = upper), width = 0) +
#   geom_text(aes(label = labels), vjust = -1, size = 4) +
#   # annotate("text", x = -0.4, y = 3.4, label = "b)") +
#   scale_y_discrete(labels = new_labels) +
#   scale_x_continuous(limits = c(-0.5, 0.5)) +
#   scale_color_manual(values = c("blue" = "#377eb8", "red" = "#e41a1c")) +  # Manually define colors
#   labs(y = "", x = "Coefficient estimates", tag = "f)") +
#   thesis_theme +
#   theme(axis.text = element_text(size = 8),
#         text = element_text(size = 6),
#         legend.position = "none")
# plot.gmin_coefs.png

plot.gmin_coefs.png <-
  sjPlot::plot_model(
    glme_gmin,
    type = "est",
    show.values = TRUE,
    show.p = T,
    value.offset = 0.3,
    line.size = 0.5,
    dot.size = 1.3
  ) +
  geom_hline(aes(yintercept = 0), alpha = 0.2) +
  scale_x_discrete(labels = new_labels) +
  labs(y = "Coefficient estimates", title = NULL, tag = "f)") +
  # annotate("text", x = 3.4, y = 0.12, label = "b)") +
  thesis_theme +
  theme(axis.text = element_text(size = 8), text = element_text(size = 6))
plot.gmin_coefs.png
ggsave(
  "figures/gmin/plot.gmin_coefs.png",
  plot.gmin_coefs.png,
  width = 6,
  height = 8,
  dpi = 150,
  units = "cm"
)

df_gmin_meteo <- df_gmin_meteo |>
  mutate(
    species = dplyr::recode(
      species,
      "FREX" = "Fraxinus excelsior",
      "FASY" = "Fagus sylvatica"
    ),
    pred = predict(glme_gmin, type = "response"),
    date_plot = date,
    year = as.factor(year)
  )
year(df_gmin_meteo$date_plot) = 2000

plot.gmin_meteo.png <- df_gmin_meteo |>
  ggplot() +
  geom_point(aes(x = leaf_age_d, y = trait_val, col = species, shape = year)) +
  geom_line(
    aes(
      x = leaf_age_d,
      y = pred,
      col = species,
      group = interaction(species, year, sample_id),
      linetype = year
    ),
    alpha = 0.5,
    linewidth = 0.5
  ) +
  scale_x_continuous(limits = c(0, 180), breaks = seq(0, 180, 20)) +
  scale_y_continuous(limits = c(0, 18)) +
  scale_shape_manual(values = c("2023" = 16, "2024" = 1)) +
  scale_color_oi(order = c(6, 2)) +
  labs(
    y = expression(
      g[min] ~ "[" * mmol ~ m^{
        -2
      } ~ s^{
        -1
      } *
        "]"
    ),
    x = "Leaf age [days]",
    tag = "e)"
  ) +
  guides(
    color = guide_legend(title = "Species", ncol = 2),
    shape = guide_legend(title = "Year", ncol = 2),
    linetype = guide_legend(title = "Year", ncol = 2)
  ) +
  # annotate("text", x = 0, y = 18, label = "a)") +
  thesis_theme +
  theme(
    legend.text = element_text(size = 8, face = "italic"),
    legend.justification = c(0.25, 0)
  )
plot.gmin_meteo.png
# ggsave("figures/gmin/plot.gmin_meteo.png",
#        plot.gmin_meteo.png, width = 10, height = 8, dpi = 150, units = "cm")

# plot.gmin_comb.png <- (plot.gmin_meteo.png + plot_spacer() + plot.gmin_coefs.png) +
#   plot_layout(widths = c(11, -0.9, 5))
# plot.gmin_comb.png
#
# ggsave("figures/gmin/plot.gmin_comb.png",
#        plot.gmin_comb.png, width = 16, height = 8, dpi = 150, units = "cm")

#### export ----

glme_gmin |>
  r2() |>
  as.data.frame() |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

broom.mixed::tidy(glme_gmin)[1:4, ] |>
  select(-c(effect, component, group)) |>
  mutate(
    term = sapply(model_predictors, format_label_export) |>
      str_replace("\\(Intercept\\)", "Intercept")
  ) |>
  as.data.frame() |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

glme_gmin |>
  ranef() |>
  as.data.frame() |>
  dplyr::select(term, grp, condval) |>
  pivot_wider(names_from = "term", values_from = "condval") |>
  rename("Intercept" = "(Intercept)") |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

glme_gmin

# plot epsilon, rwctlp and gmin -------------------------------------------

plot.eps_rwc_gmin.png <-
  plot.elast_meteo.png +
  plot_spacer() +
  plot.elast_coefs.png +
  plot.rwc_tlp_meteo.png +
  plot_spacer() +
  plot.rwc_tlp_coefs.png +
  plot.gmin_meteo.png +
  plot_spacer() +
  plot.gmin_coefs.png +
  plot_layout(widths = c(11, -0.9, 5), heights = c(8, 8, 8))
plot.eps_rwc_gmin.png

ggsave(
  "figures/plot.eps_rwc_gmin.png",
  plot.eps_rwc_gmin.png,
  width = 16,
  height = 22,
  units = "cm"
)

### 2.2.7 p12 ----

df_meteo_corr_nest |>
  filter(trait == "p12") |>
  ggplot(aes(x = timeframe, y = corrcoef)) +
  geom_point(aes(col = ifelse(pval <= 0.05, "darkgreen", "grey"))) +
  scale_color_identity() +
  facet_wrap(~meteo_var)

df_p12_meteo <- df_best_meteo_corr |>
  ungroup() |>
  filter(trait == "p12") |>
  arrange(desc(abs_corr)) |>
  unnest(data) |>
  mutate(
    meteo_predictor = paste(meteo_var, abs(timeframe), sep = "_"),
    pheno_start_doy = case_when(
      species == "FASY" & year == 2023 ~ 121,
      species == "FASY" & year == 2024 ~ 104,
      species == "FREX" & year == 2023 ~ 140,
      species == "FREX" & year == 2024 ~ 138
    ),
    doy = yday(date),
    leaf_age_d = doy - pheno_start_doy
  ) |>
  select(-c(timeframe, meteo_var, corrcoef, pval, abs_corr, pheno_start_doy)) |>
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val) |>
  na.omit()

df_p12_meteo_scaled <- df_p12_meteo |>
  select(c(trait, date, year, species, sample_id, campaign, trait_val, doy)) |>
  cbind(
    df_p12_meteo |>
      select(
        -c(trait, date, year, species, sample_id, campaign, trait_val, doy)
      ) |>
      scale()
  )

f.make_formula_char(df_p12_meteo) |> writeClipboard()

lm_p12 <- lm(
  trait_val ~ leaf_age_d +
    sum_p_2 +
    max_ta_55 +
    sum_p_et_5 +
    mean_et_pet_25 +
    mean_ta_60,
  data = df_p12_meteo_scaled,
  na.action = "na.fail"
)

stepAIC(lm_p12, direction = "both") |> summary()
stepAIC(lme_p12, direction = "both") |> formula()

lm_p12_dregde <- MuMIn::dredge(lme_p12, rank = "AICc")
lm_p12_dregde |> head()
lm_p12_dregde[1, ]

lme_p12 <- nlme::lme(
  trait_val ~ sum_p_2 + max_ta_55,
  random = ~ 1 | species / sample_id,
  data = df_p12_meteo_scaled,
  # correlation = corCAR1(form = ~ date | species/sample_id),
  control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 200),
  method = "ML"
)

lme_p12 |> summary()
lme_p12 |> r2()
lme_p12 |> check_collinearity()
lme_p12 |> check_model()

#### plots --------------------------------------------------------------------

model_predictors <- names(fixef(lme_p12)) # Extract fixed effect names

# Create a named vector for label replacement
df_predictors <- df_predictors %>%
  mutate(base_abbr = extract_base_name(abbr)) # Create a column without numbers

# Generate a named vector for matching
predictor_labels <- setNames(df_predictors$long, df_predictors$base_abbr)

# Apply formatting to all predictors
new_labels <- sapply(model_predictors, format_label)

plot.p12_coefs.png <-
  sjPlot::plot_model(
    lme_p12,
    show.values = TRUE,
    show.p = T,
    value.offset = 0.3,
    line.size = 0.5,
    dot.size = 1.3
  ) +
  geom_hline(aes(yintercept = 0), alpha = 0.2) +
  scale_x_discrete(labels = new_labels) +
  labs(y = "Coefficient estimates", title = NULL, tag = "b)", size = 2) +
  thesis_theme +
  theme(
    axis.text = element_text(size = 8),
    text = element_text(size = 6),
    plot.tag.position = c(0.05, 0.95)
  )
plot.p12_coefs.png
# ggsave("figures/p12/plot.p12_coefs.png",
#        plot.p12_coefs.png, width = 6, height = 8, dpi = 150, units = "cm")

df_p12_meteo <- df_p12_meteo |>
  mutate(
    species = dplyr::recode(
      species,
      "FREX" = "Fraxinus excelsior",
      "FASY" = "Fagus sylvatica"
    ),
    pred = predict(lme_p12, type = "response"),
    date_plot = date,
    year = as.factor(year)
  )
year(df_p12_meteo$date_plot) = 2000

plot.p12_meteo.png <- df_p12_meteo |>
  ggplot() +
  geom_point(
    aes(x = leaf_age_d, y = trait_val, col = species, shape = year),
    show.legend = F
  ) +
  geom_line(
    aes(
      x = leaf_age_d,
      y = pred,
      col = species,
      group = interaction(species, year, sample_id),
      linetype = year
    ),
    alpha = 0.5,
    linewidth = 0.5,
    show.legend = F
  ) +
  scale_x_continuous(limits = c(0, 180), breaks = seq(0, 180, 20)) +
  scale_y_continuous(limits = c(-6, 0)) +
  scale_shape_manual(values = c("2023" = 16, "2024" = 1)) +
  scale_color_oi(order = c(6, 2)) +
  labs(y = expression(P[12] ~ "[MPa]"), x = "Leaf age [days]", tag = "a)") +
  guides(
    color = guide_legend(title = "Species", ncol = 2),
    shape = guide_legend(title = "Year", ncol = 2),
    linetype = guide_legend(title = "Year", ncol = 2)
  ) +
  thesis_theme +
  theme(
    legend.text = element_text(size = 8, face = "italic"),
    legend.justification = c(0.25, 0)
  )
plot.p12_meteo.png
# ggsave("figures/p12/plot.p12_meteo.png",
#        plot.p12_meteo.png, width = 10, height = 8, dpi = 150, units = "cm")

plot.p12_comb.png <- (plot.p12_meteo.png + plot_spacer() + plot.p12_coefs.png) +
  plot_layout(widths = c(11, -0.9, 5))
plot.p12_comb.png

ggsave(
  "figures/p12/plot.p12_comb.png",
  plot.p12_comb.png,
  width = 16,
  height = 8,
  dpi = 150,
  units = "cm"
)

#### export ----

lme_p12 |>
  r2() |>
  as.data.frame() |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

lme_p12 |>
  broom.mixed::tidy() |>
  mutate(term = sapply(model_predictors, format_label_export)) |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

lme_p12$modelStruct$reStruct

lme_p12 |>
  ranef() |>
  as.data.frame() |>
  select(-group) |>
  pivot_wider(names_from = "term", values_from = "estimate") |>
  rename("Intercept" = `(Intercept)`) |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

### 2.2.8 p50 ----

df_meteo_corr_nest |>
  filter(trait == "p50") |>
  ggplot(aes(x = timeframe, y = corrcoef)) +
  geom_point(aes(col = ifelse(pval <= 0.1, "darkgreen", "grey"))) +
  scale_color_identity() +
  facet_wrap(~meteo_var)

df_p50_meteo <- df_best_meteo_corr |>
  ungroup() |>
  filter(trait == "p50") |>
  arrange(desc(abs_corr)) |>
  unnest(data) |>
  mutate(
    meteo_predictor = paste(meteo_var, abs(timeframe), sep = "_"),
    pheno_start_doy = case_when(
      species == "FASY" & year == 2023 ~ 121,
      species == "FASY" & year == 2024 ~ 104,
      species == "FREX" & year == 2023 ~ 140,
      species == "FREX" & year == 2024 ~ 138
    ),
    doy = yday(date),
    leaf_age_d = doy - pheno_start_doy
  ) |>
  select(-c(timeframe, meteo_var, corrcoef, pval, abs_corr, pheno_start_doy)) |>
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val) |>
  na.omit()

df_p50_meteo_scaled <- df_p50_meteo |>
  select(c(trait, date, year, species, sample_id, campaign, trait_val, doy)) |>
  cbind(
    df_p50_meteo |>
      select(
        -c(trait, date, year, species, sample_id, campaign, trait_val, doy)
      ) |>
      scale()
  )

f.make_formula_char(df_p50_meteo) |> writeClipboard()

lm_p50 <- lm(
  trait_val ~ leaf_age_d +
    sum_p_2 +
    mean_et_pet_25 +
    sum_pet_30 +
    mean_rh_40 +
    max_ta_55 +
    sum_ppfd_40 +
    mean_ta_60 +
    sum_p_et_2,
  data = df_p50_meteo_scaled,
  na.action = "na.fail"
)
lm_p50 |> summary()

stepAIC(lm_p50, direction = "both") |> summary()
stepAIC(lme_p50, direction = "both") |> formula()

lm_p50_dregde <- MuMIn::dredge(lme_p50, rank = "AICc")
lm_p50_dregde |> head()
lm_p50_dregde[1, ]

lme_p50 <- nlme::lme(
  trait_val ~ mean_et_pet_25,
  # trait_val ~ mean_et_pet_25,
  random = ~ 1 | species / sample_id,
  data = df_p50_meteo_scaled,
  # correlation = corAR(form = ~ date | species/sample_id),
  control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 200),
  method = "ML"
)

lme_p50 |> summary()
lme_p50 |> r2()
lme_p50 |> check_collinearity()
lme_p50 |> check_autocorrelation()
lme_p50 |> check_model()

#### plots --------------------------------------------------------------------

model_predictors <- names(fixef(lme_p50)) # Extract fixed effect names

# Create a named vector for label replacement
df_predictors <- df_predictors %>%
  mutate(base_abbr = extract_base_name(abbr)) # Create a column without numbers

# Generate a named vector for matching
predictor_labels <- setNames(df_predictors$long, df_predictors$base_abbr)

# Apply formatting to all predictors
new_labels <- sapply(model_predictors, format_label)

plot.p50_coefs.png <-
  sjPlot::plot_model(
    lme_p50,
    show.values = TRUE,
    show.p = T,
    value.offset = 0.3,
    line.size = 0.5,
    dot.size = 1.3
  ) +
  geom_hline(aes(yintercept = 0), alpha = 0.2) +
  scale_x_discrete(labels = new_labels) +
  labs(y = "Coefficient estimates", title = NULL, tag = "d)", size = 2) +
  thesis_theme +
  theme(
    axis.text = element_text(size = 8),
    text = element_text(size = 6),
    plot.tag.position = c(0.05, 0.95)
  )
plot.p50_coefs.png
# ggsave("figures/p50/plot.p50_coefs.png",
#        plot.p50_coefs.png, width = 6, height = 8, dpi = 150, units = "cm")

df_p50_meteo <- df_p50_meteo |>
  mutate(
    species = dplyr::recode(
      species,
      "FREX" = "Fraxinus excelsior",
      "FASY" = "Fagus sylvatica"
    ),
    pred = predict(lme_p50, type = "response"),
    date_plot = date,
    year = as.factor(year)
  )
year(df_p50_meteo$date_plot) = 2000

plot.p50_meteo.png <- df_p50_meteo |>
  ggplot() +
  geom_point(
    aes(x = leaf_age_d, y = trait_val, col = species, shape = year),
    show.legend = F
  ) +
  geom_line(
    aes(
      x = leaf_age_d,
      y = pred,
      col = species,
      group = interaction(species, year, sample_id),
      linetype = year
    ),
    alpha = 0.5,
    linewidth = 0.5,
    show.legend = F
  ) +
  scale_x_continuous(limits = c(0, 180), breaks = seq(0, 180, 20)) +
  scale_y_continuous(limits = c(-6, 0)) +
  scale_shape_manual(values = c("2023" = 16, "2024" = 1)) +
  scale_color_oi(order = c(6, 2)) +
  labs(y = expression(P[50] ~ "[MPa]"), x = "Leaf age [days]", tag = "c)") +
  guides(
    color = guide_legend(title = "Species", ncol = 2),
    shape = guide_legend(title = "Year", ncol = 2),
    linetype = guide_legend(title = "Year", ncol = 2)
  ) +
  thesis_theme +
  theme(
    legend.text = element_text(size = 8, face = "italic"),
    legend.justification = c(0.25, 0)
  )
plot.p50_meteo.png
# ggsave("figures/p50/plot.p50_meteo.png",
#        plot.p50_meteo.png, width = 10, height = 8, dpi = 150, units = "cm")

plot.p50_comb.png <- (plot.p50_meteo.png + plot_spacer() + plot.p50_coefs.png) +
  plot_layout(widths = c(11, -0.9, 5))
plot.p50_comb.png

ggsave(
  "figures/p50/plot.p50_comb.png",
  plot.p50_comb.png,
  width = 16,
  height = 8,
  dpi = 150,
  units = "cm"
)

#### export ----

lme_p50 |>
  r2() |>
  as.data.frame() |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

broom.mixed::tidy(lme_p50) |>
  mutate(term = sapply(model_predictors, format_label_export)) |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

lme_p50$modelStruct$corStruct
lme_p50$modelStruct$reStruct

lme_p50 |>
  ranef() |>
  as.data.frame() |>
  select(-group) |>
  pivot_wider(names_from = "term", values_from = "estimate") |>
  rename("Intercept" = `(Intercept)`) |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

### 2.2.9 p88 ----

df_meteo_corr_nest |>
  filter(trait == "p88") |>
  ggplot(aes(x = timeframe, y = corrcoef)) +
  geom_point(aes(col = ifelse(pval <= 0.1, "darkgreen", "grey"))) +
  scale_color_identity() +
  facet_wrap(~meteo_var)

df_p88_meteo <- df_best_meteo_corr |>
  ungroup() |>
  filter(trait == "p88") |>
  arrange(desc(abs_corr)) |>
  unnest(data) |>
  mutate(
    meteo_predictor = paste(meteo_var, abs(timeframe), sep = "_"),
    pheno_start_doy = case_when(
      species == "FASY" & year == 2023 ~ 121,
      species == "FASY" & year == 2024 ~ 104,
      species == "FREX" & year == 2023 ~ 140,
      species == "FREX" & year == 2024 ~ 138
    ),
    doy = yday(date),
    leaf_age_d = doy - pheno_start_doy
  ) |>
  select(-c(timeframe, meteo_var, corrcoef, pval, abs_corr, pheno_start_doy)) |>
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val) |>
  na.omit()

df_p88_meteo_scaled <- df_p88_meteo |>
  select(c(trait, date, year, species, sample_id, campaign, trait_val, doy)) |>
  cbind(
    df_p88_meteo |>
      select(
        -c(trait, date, year, species, sample_id, campaign, trait_val, doy)
      ) |>
      scale()
  )

f.make_formula_char(df_p88_meteo) |> writeClipboard()

lm_p88 <- lm(
  trait_val ~ leaf_age_d +
    sum_p_2 +
    mean_rh_55 +
    mean_et_pet_25 +
    sum_p_et_2 +
    sum_pet_30 +
    max_ta_55 +
    mean_vpd_35,
  data = df_p88_meteo_scaled,
  na.action = "na.fail"
)
lm_p88 |> summary()

stepAIC(lme_p88, direction = "both") |> summary()
stepAIC(lme_p88, direction = "both") |> formula()

lm_p88_dregde <- MuMIn::dredge(lme_p88, rank = "AICc")
lm_p88_dregde |> head()
lm_p88_dregde[1, ] |> f.make_formula_dredge()

stepAIC(lme_p88, direction = "both") |> summary()
stepAIC(lme_p88, direction = "both") |> formula()

lme_p88 <- nlme::lme(
  trait_val ~ mean_et_pet_25,
  random = ~ 1 | species / sample_id,
  data = df_p88_meteo_scaled,
  # correlation = corAR1(form = ~ date | species/sample_id),
  # control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 200),
  method = "ML"
)

lme_p88 |> summary()
lme_p88 |> r2()
lme_p88 |> check_collinearity()
lme_p88 |> check_autocorrelation()
lme_p88 |> check_model()

#### plots --------------------------------------------------------------------

model_predictors <- names(fixef(lme_p88)) # Extract fixed effect names

# Create a named vector for label replacement
df_predictors <- df_predictors %>%
  mutate(base_abbr = extract_base_name(abbr)) # Create a column without numbers

# Generate a named vector for matching
predictor_labels <- setNames(df_predictors$long, df_predictors$base_abbr)

# Apply formatting to all predictors
new_labels <- sapply(model_predictors, format_label)

plot.p88_coefs.png <-
  sjPlot::plot_model(
    lme_p88,
    show.values = TRUE,
    show.p = T,
    value.offset = 0.3,
    line.size = 0.5,
    dot.size = 1.3
  ) +
  geom_hline(aes(yintercept = 0), alpha = 0.2) +
  scale_x_discrete(labels = new_labels) +
  labs(y = "Coefficient estimates", title = NULL, tag = "f)", size = 2) +
  thesis_theme +
  theme(
    axis.text = element_text(size = 8),
    text = element_text(size = 6),
    plot.tag.position = c(0.05, 0.95)
  )

plot.p88_coefs.png
# ggsave("figures/p88/plot.p88_coefs.png",
#        plot.p88_coefs.png, width = 6, height = 8, dpi = 150, units = "cm")

df_p88_meteo <- df_p88_meteo |>
  mutate(
    species = dplyr::recode(
      species,
      "FREX" = "Fraxinus excelsior",
      "FASY" = "Fagus sylvatica"
    ),
    pred = predict(lme_p88, type = "response"),
    date_plot = date,
    year = as.factor(year)
  )
year(df_p88_meteo$date_plot) = 2000

plot.p88_meteo.png <- df_p88_meteo |>
  ggplot() +
  geom_point(aes(x = leaf_age_d, y = trait_val, col = species, shape = year)) +
  geom_line(
    aes(
      x = leaf_age_d,
      y = pred,
      col = species,
      group = interaction(species, year, sample_id),
      linetype = year
    ),
    alpha = 0.5,
    linewidth = 0.5
  ) +
  scale_x_continuous(limits = c(0, 180), breaks = seq(0, 180, 20)) +
  scale_y_continuous(limits = c(-10, 0)) +
  scale_shape_manual(values = c("2023" = 16, "2024" = 1)) +
  scale_color_oi(order = c(6, 2)) +
  labs(y = expression(P[88] ~ "[MPa]"), x = "Leaf age [days]", tag = "e)") +
  guides(
    color = guide_legend(title = "Species", ncol = 2),
    shape = guide_legend(title = "Year", ncol = 2),
    linetype = guide_legend(title = "Year", ncol = 2)
  ) +
  thesis_theme +
  theme(
    legend.text = element_text(size = 8, face = "italic"),
    legend.justification = c(0.25, 0)
  )
plot.p88_meteo.png
# ggsave("figures/p88/plot.p88_meteo.png",
#        plot.p88_meteo.png, width = 10, height = 8, dpi = 150, units = "cm")

plot.p88_comb.png <- (plot.p88_meteo.png + plot_spacer() + plot.p88_coefs.png) +
  plot_layout(widths = c(11, -0.9, 5))
plot.p88_comb.png

ggsave(
  "figures/p88/plot.p88_comb.png",
  plot.p88_comb.png,
  width = 16,
  height = 8,
  dpi = 150,
  units = "cm"
)

#### export ----

lme_p88 |>
  r2() |>
  as.data.frame() |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

lme_p88 |>
  broom.mixed::tidy() |>
  mutate(term = sapply(model_predictors, format_label_export)) |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

lme_p88$modelStruct$reStruct

lme_p88 |>
  ranef() |>
  as.data.frame() |>
  select(-group) |>
  pivot_wider(names_from = "term", values_from = "estimate") |>
  rename("Intercept" = `(Intercept)`) |>
  write.table("clipboard", sep = "\t", row.names = FALSE, na = "")

### plot all xylem vulnerability ----

plot.p12p50p88_comb.png <-
  plot.p12_meteo.png +
  plot_spacer() +
  plot.p12_coefs.png +
  plot.p50_meteo.png +
  plot_spacer() +
  plot.p50_coefs.png +
  plot.p88_meteo.png +
  plot_spacer() +
  plot.p88_coefs.png +
  plot_layout(widths = c(11, -0.9, 5), heights = c(8, 8, 8))

plot.p12p50p88_comb.png
ggsave(
  "figures/plot.p12p50p88_comb.png",
  plot.p12p50p88_comb.png,
  width = 16,
  height = 24,
  dpi = 150,
  units = "cm"
)


# 3. NSC models -----------------------------------------------------------

df_psi_md_NSC <- df_psi_midday %>%
  left_join(
    df_sugars_emm %>%
      select(date, species, sample_id, sugar_name, sugar_conc) %>%
      na.omit() %>%
      mutate(
        date = case_when(
          date == "2023-06-23" ~ "2023-06-13" %>% as.Date(),
          date == "2023-08-08" ~ "2023-08-09" %>% as.Date(),
          T ~ date
        )
      ),
    by = c("species", "sample_id", "date")
  )

df_psi_midday$date %>% unique()
df_psi_md_NSC$date %>% unique()

lme_psimd_totalsugars_1 <- nlme::lme(
  psi_midday_mpa ~ leaf_age_d + sugar_conc,
  random = ~ 1 | species / sample_id,
  data = df_psi_md_NSC %>% filter(sugar_name == "total_sugars_mg_g")
)

lme_psimd_totalsugars_2 <- nlme::lme(
  psi_midday_mpa ~ leaf_age_d * sugar_conc,
  random = ~ 1 | species / sample_id,
  data = df_psi_md_NSC %>% filter(sugar_name == "total_sugars_mg_g")
)

lme_psimd_totalsugars_1 %>% summary()

AIC(lme_psimd_totalsugars_1, lme_psimd_totalsugars_2)

df_psi_md_NSC %>%
  filter(sugar_name == "total_sugars_mg_g") %>%
  mutate(
    date_plot = date %>% str_replace("2023|2024", "2000") %>% as.Date()
  ) %>%
  ggplot() +
  geom_point(aes(x = date_plot, y = psi_midday_mpa, col = species)) +
  geom_line(aes(
    x = date_plot,
    y = predict(lme_psimd_totalsugars_2),
    col = species,
    group = interaction(species, sample_id)
  )) +
  scale_color_oi(order = c(6, 2)) +
  guides(color = guide_legend(title = "Species")) +
  labs(
    y = expression(
      "Total sugars [mg" ~ g^{
        -1
      } *
        "]"
    ),
    x = "Date",
    tag = "a)"
  ) +
  facet_wrap(~year) +
  thesis_theme


## 3.1 all traits in one ---------------------------------------------------

df_hydr_traits <- read.csv("data/calculated_parameters/df_hydr_traits.csv")

df_hydr_traits <- df_hydr_traits |>
  select(-c(mean_psi_bar, gmin_max, gmin_min, gmin_mean)) |>
  pivot_wider(names_from = sugar_name, values_from = sugar_conc) %>%
  mutate(
    starch_sugar_ratio = starch_mg_g /
      (fructose_mg_g + glucose_mg_g + sucrose_mg_g)
  ) %>%
  pivot_longer(
    cols = fructose_mg_g:starch_sugar_ratio,
    names_to = "sugar_name",
    values_to = "sugar_conc"
  ) %>%
  pivot_longer(
    cols = -c(
      "date",
      "species",
      "sample_id",
      "campaign",
      "date_fac",
      "nutrient_name",
      "nutrient_conc",
      "year",
      "vessel_order",
      "sugar_name",
      "sugar_conc",
      "doy",
      "leaf_age_d",
      "pheno_start_doy"
    ),
    names_to = "trait",
    values_to = "value"
  ) |>
  mutate(
    date_plot = date |> str_replace("2023|2024", "2000") |> as.Date(),
    year = as.factor(year)
  )

df_hydr_traits_nest <- df_hydr_traits |>
  select(-vessel_order) |>
  nest(data = -c(trait)) |>
  mutate(
    data = map(
      data,
      ~ .x |>
        pivot_wider(names_from = sugar_name, values_from = sugar_conc)
    ),
    data = map(
      data,
      ~ .x |>
        pivot_wider(names_from = nutrient_name, values_from = nutrient_conc)
    ),
    data = map(data, ~ .x[!is.na(.x$value), ])
  )

df_hydr_traits_nest <- df_hydr_traits_nest |>
  mutate(
    mod.leaf_age = map(
      data,
      ~ try(nlme::lme(
        value ~ leaf_age_d,
        random = ~ 1 | species / sample_id,
        data = .x
      ))
    )
  )

df_hydr_traits_nest$data[[1]] |>
  ggplot() +
  geom_point(aes(x = date_plot, y = value, col = species, group = species)) +
  # geom_line(aes(x = date_plot, y = pred_leaf_age, col = species, group = species)) +
  facet_wrap(~year)

## total sugars

df_hydr_traits_nest$data[[1]] |> names()

df_hydr_traits_nest <- df_hydr_traits_nest |>
  mutate(
    mod.total_sugars = map(
      data,
      ~ try(nlme::lme(
        value ~ total_sugars_mg_g * glucose_mg_g,
        random = ~ 1 | species,
        correlation = corAR1(form = ~ leaf_age_d | species / sample_id),
        data = .x
      ))
    )
  )

df_hydr_traits_nest <- df_hydr_traits_nest |>
  mutate(
    data = map2(
      data,
      mod.total_sugars,
      ~ .x |>
        add_predictions(.y, var = "pred_total_sugars", type = "response") |>
        try()
    )
  )

df_hydr_traits_nest$mod.total_sugars[[1]] |> summary()

df_hydr_traits_nest$data[[11]] |>
  ggplot() +
  geom_point(aes(x = date_plot, y = value, col = species, group = species)) +
  geom_line(aes(
    x = date_plot,
    y = pred_total_sugars,
    col = species,
    group = interaction(species, sample_id)
  )) +
  facet_wrap(~year)

## all (then backwards selection?)

df_hydr_traits_nest <- df_hydr_traits_nest |>
  mutate(
    mod.all = map(
      data,
      ~ try(nlme::lme(
        value ~ glucose_mg_g +
          fructose_mg_g +
          sucrose_mg_g +
          starch_mg_g +
          glucose_starch_1,
        random = ~ 1 | year + 1 | species / sample_id,
        # correlation = corAR1(form = ~ leaf_age_d|species/sample_id),
        data = .x
      ))
    )
  )

df_hydr_traits_nest$trait

trait_num = 3
df_hydr_traits_nest$mod.all[[trait_num]] |> summary()

df_hydr_traits_nest$data[[trait_num]] |>
  mutate(pred = predict(df_hydr_traits_nest$mod.all[[trait_num]])) |>
  ggplot() +
  geom_point(aes(x = date_plot, y = value, col = species, group = species)) +
  geom_line(aes(
    x = date_plot,
    y = pred,
    col = species,
    group = interaction(species, sample_id)
  )) +
  facet_wrap(~year)

## extract coefficients per sugar

df_hydr_traits_nest <- df_hydr_traits_nest |>
  mutate(
    coefs.all = map(
      mod.all,
      ~ try(
        summary(.x)$coef$fixed |> t()
      )
    )
  )

df_hydr_traits_nest$mod.all[[2]] |> summary() |> coefs()
df_hydr_traits_nest$coefs.all |> as.vector() |> unlist()

df_hydr_traits_coefs <- df_hydr_traits_nest |>
  mutate(type = map(coefs.all, ~ .x |> str_detect("Error") %>% first())) |>
  filter(type != TRUE) |>
  select(-c(data, mod.leaf_age, mod.total_sugars, mod.all, type)) |>
  unnest(cols = coefs.all) |>
  as.matrix() |>
  as.data.frame()

colnames(df_hydr_traits_coefs) = c(
  "trait",
  "intercept",
  "glucose_mg_g",
  "fructose_mg_g",
  "sucrose_mg_g",
  "starch_mg_g",
  "glucose/starch"
)

df_hydr_traits_coefs |>
  write.table(file = "clipboard", sep = "\t", dec = ".", row.names = F)
## process data ------------------------------------------------------------

select <- dplyr::select

df_hydr_traits <- df_pv_params |> ## this first because campaign 1 2024 didn't have psi midday
  left_join(
    df_psi_midday %>%
      dplyr::select(-c(doy, date_fac)),
    by = c("date", "species", "sample_id", "campaign", "year")
  ) |>
  left_join(
    df_gmin_params |>
      filter(rwc_class == "rwc_class_standard") |>
      select(-c(rwc_class, rwc_interval, date_fac)),
    by = c("date", "species", "sample_id", "campaign", "year")
  ) |>
  left_join(
    df_ov_params |>
      select(-c(rmse_fitplc:p88_spline, date_plot:date_fac)),
    by = c("date", "species", "sample_id", "campaign", "year")
  ) |>
  left_join(
    df_tsm_params |>
      select(-c(date_fac)) |>
      pivot_wider(names_from = trait, values_from = estimate),
    by = c("date", "species", "sample_id", "campaign", "year")
  ) |>
  left_join(
    df_sugars_summ %>%
      select(date, species, sample_id, sugar_name, sugar_conc) %>%
      na.omit() %>%
      mutate(
        date = case_when(
          date == "2023-06-23" ~ "2023-06-13" %>% as.Date(),
          date == "2023-08-08" ~ "2023-08-09" %>% as.Date(),
          T ~ date
        )
      ),
    by = c("species", "sample_id", "date")
  ) %>%
  left_join(
    df_nutrients_summ %>%
      select(date, species, sample_id, nutrient_conc, nutrient_name) %>%
      mutate(
        date = case_when(
          date == "2023-06-23" ~ "2023-06-13" %>% as.Date(),
          date == "2023-08-08" ~ "2023-08-09" %>% as.Date(),
          T ~ date
        )
      ),
    by = c("species", "sample_id", "date")
  )


write.csv(
  file = "data/calculated_parameters/df_hydr_traits.csv",
  df_hydr_traits,
  row.names = F
)

df_hydr_traits$date |> unique()


## 3.2 trait v trait correlations ------------------------------------------

df_hydr_traits %>%
  filter(trait == "capacitance_tlp_tot")

df_traits_wide <- df_hydr_traits %>%
  group_by(year, campaign, species, trait) %>%
  # summarise(value = mean(value, na.rm = T)) %>%
  select(trait, value) %>%
  mutate(row = row_number()) %>%
  # distinct() %>%
  # as_tibble() %>%
  pivot_wider(names_from = trait, values_from = value) %>%
  ungroup() %>%
  select(-row)

df_traits_wide %>%
  filter(species == "FREX") %>%
  select(-c(year, campaign, species)) %>%
  GGally::ggpairs() +
  labs(title = "FREX")

df_traits_wide %>%
  filter(species == "FASY") %>%
  select(-c(year, campaign, species)) %>%
  GGally::ggpairs() +
  labs(title = "FASY")

## tlp vs p12

df_traits_wide %>%
  mutate(campaign = as.factor(campaign)) %>%
  ggplot() +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(x = psi_tlp, y = p12, col = campaign)) +
  lims(y = c(-4, 0), x = c(-4, 0)) +
  facet_wrap(~species) +
  thesis_theme

df_traits_wide %>%
  mutate(campaign = as.factor(campaign)) %>%
  ggplot() +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(x = rwc_tot_tlp, y = gmin, col = campaign)) +
  # lims(y = c(-6.5, 0), x = c(-6.5, 0)) +
  facet_wrap(~species) +
  thesis_theme

df_traits_wide %>%
  pivot_longer(names_to = "trait", cols = c(psi_tlp:p88)) %>%
  group_by(year, species, campaign, trait) %>%
  summarise(trait_val = mean(value, na.rm = T)) %>%
  pivot_wider(names_from = "trait", values_from = trait_val) %>%
  mutate(campaign = as.factor(campaign)) %>%
  filter(species == "FASY") %>%
  ungroup() %>%
  select(-c(year, campaign, species)) %>%
  GGally::ggpairs() +
  labs(title = "FASY")

## tlp vs gmin

df_traits_wide %>%
  mutate(campaign = as.factor(campaign)) %>%
  ggplot() +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(x = elast_sym, y = gmin, col = campaign)) +
  # lims(y = c(-4, 0), x = c(-4, 0)) +
  facet_wrap(~species) +
  thesis_theme


# 4. Microclimate X NSC X Nutrients ---------------------------------------

df_hydr_traits <- read.csv("data/calculated_parameters/df_hydr_traits.csv")

df_hydr_traits <- df_hydr_traits |>
  select(-c(mean_psi_bar, gmin_max, gmin_min, gmin_mean)) |>
  pivot_wider(names_from = sugar_name, values_from = sugar_conc) %>%
  mutate(
    starch_sugar_ratio = starch_mg_g /
      (fructose_mg_g + glucose_mg_g + sucrose_mg_g)
  ) %>%
  pivot_longer(
    cols = fructose_mg_g:starch_sugar_ratio,
    names_to = "sugar_name",
    values_to = "sugar_conc"
  ) %>%
  pivot_longer(
    cols = -c(
      "date",
      "species",
      "sample_id",
      "campaign",
      "date_fac",
      "nutrient_name",
      "nutrient_conc",
      "year",
      "vessel_order",
      "sugar_name",
      "sugar_conc",
      "doy",
      "leaf_age_d",
      "pheno_start_doy"
    ),
    names_to = "trait",
    values_to = "value"
  ) |>
  mutate(
    date_plot = date |> str_replace("2023|2024", "2000") |> as.Date(),
    year = as.factor(year)
  )

## Corrplot

df_hydr_traits_corr <- read.csv(
  "data/calculated_parameters/df_hydr_traits.csv"
) |>
  select(year, date, species, sample_id, psi_tlp:T95) |>
  filter(vessel_order == "major") |>
  select(
    -c(
      pheno_start_doy,
      doy,
      date_fac,
      vessel_order,
      mean_psi_bar,
      gmin_max,
      gmin_min,
      gmin_mean
    )
  ) |>
  distinct()

df_hydr_traits_corr_FREX = df_hydr_traits_corr |>
  filter(species == "FREX") |>
  mutate(across(c(psi_tlp:p88), ~ as.numeric(scale(.x)), .names = "{.col}_z"))

library(GGally)

ggpairs(df_hydr_traits_corr_FREX, columns = 5:20, title = "FREX")

df_hydr_traits_corr_FASY = df_hydr_traits_corr |>
  filter(species == "FASY") |>
  mutate(across(c(psi_tlp:p88), ~ as.numeric(scale(.x)), .names = "{.col}_z"))

ggpairs(df_hydr_traits_corr_FASY, columns = 5:20, title = "FASY")

## aggregate per date
df_hydr_traits_corr_FREX_mean = df_hydr_traits_corr |>
  filter(species == "FREX") |>
  group_by(species, date, year) |>
  mutate(across(c(psi_tlp:p88), ~ mean(.x)))

ggpairs(
  df_hydr_traits_corr_FREX_mean,
  columns = 5:20,
  title = "FREX; aggregated to mean per campaign"
)

df_hydr_traits_corr_FASY_mean = df_hydr_traits_corr |>
  filter(species == "FASY") |>
  group_by(species, date, year) |>
  mutate(across(c(psi_tlp:p88), ~ mean(.x)))

ggpairs(
  df_hydr_traits_corr_FASY_mean,
  columns = 5:20,
  title = "FASY; aggregated to mean per campaign"
)

## aggregate per date
df_hydr_traits_corr_mean = df_hydr_traits_corr |>
  # filter(species == "FREX") |>
  group_by(species, date, year) |>
  mutate(across(c(psi_tlp:T95), ~ mean(.x, na.rm = T)))

my_func <- function(data, mapping, ...) {
  ggplot(data, mapping) +
    geom_point(size = 0.7) +
    geom_smooth(formula = y ~ x, method = "lm", color = "black")
}

g = ggpairs(
  df_hydr_traits_corr_mean,
  columns = 5:23,
  mapping = aes(col = species),
  title = "FASY & FREX; aggregated to mean per campaign",
  lower = list(continuous = my_func)
)
g

ggpairs(
  df_hydr_traits_corr_mean,
  columns = 5:23,
  mapping = aes(col = species),
  title = "FASY & FREX; aggregated to mean per campaign"
)

## NSC & nutrient PCA
library(FactoMineR)

nutrient_names = df_hydr_traits$nutrient_name |> unique()
sugar_names = df_hydr_traits$sugar_name |> unique()

df_hydr_traits_pca <- df_hydr_traits |>
  ungroup() |>
  pivot_wider(names_from = nutrient_name, values_from = nutrient_conc) |>
  pivot_wider(names_from = sugar_name, values_from = sugar_conc) |>
  pivot_longer(
    cols = c(nutrient_names, sugar_names),
    names_to = "sugar_nutrient_name",
    values_to = "sugar_nutrient_conc"
  ) |>
  mutate(
    sugar_nutrient_name = sugar_nutrient_name |> str_remove_all("_con"),
    sugar_nutrient_name = case_when(
      sugar_nutrient_name == "c" ~ "c_perc",
      sugar_nutrient_name == "n" ~ "n_perc",
      T ~ sugar_nutrient_name
    )
  ) |>
  group_by(year, campaign, date, species, sample_id, sugar_nutrient_name) |>
  summarise(
    sugar_nutrient_conc = sugar_nutrient_conc |> as.numeric() |> mean(na.rm = T)
  )

df_hydr_traits_pca_rel <- df_hydr_traits_pca |>
  ungroup() |>
  # select(c(sugar_nutrient_name, sugar_nutrient_conc)) |>
  pivot_wider(
    names_from = sugar_nutrient_name,
    values_from = sugar_nutrient_conc
  ) |>
  select(-c(year, campaign, date, species, sample_id))

pca_sugar_nutrient <- PCA(
  df_hydr_traits_pca_rel,
  scale.unit = T,
  graph = T,
  ncp = 5
)
pca_sugar_nutrient$ind$coord
pca_sugar_nutrient$eig

df_hydr_traits_pca_res <- df_hydr_traits_pca |>
  ungroup() |>
  # select(c(sugar_nutrient_name, sugar_nutrient_conc)) |>
  pivot_wider(
    names_from = sugar_nutrient_name,
    values_from = sugar_nutrient_conc
  ) |>
  cbind(pca_sugar_nutrient$ind$coord) |>
  mutate(campaign = campaign |> as.factor())

df_hydr_traits_pca_res |>
  ggplot() +
  geom_point(aes(x = Dim.1, y = Dim.2, col = campaign, shape = species)) +
  facet_wrap(~year) +
  theme_bw() +
  lims(y = c(-4, 4), x = c(-4, 4)) +
  theme(aspect.ratio = 1)

df_hydr_traits$trait |> unique()

df_hydr_traits |>
  select(-c(nutrient_name, nutrient_conc, sugar_name, sugar_conc)) |>
  distinct() |>
  pivot_wider(names_from = trait, values_from = value) |>
  cbind(pca_sugar_nutrient$ind$coord) |>
  mutate(campaign = campaign |> as.factor()) |>
  pivot_longer(cols = Dim.1:Dim.5, names_to = "dim", values_to = "dim_value") |>
  filter(dim %in% c("Dim.1", "Dim.2", "Dim.3")) |>
  ggplot() +
  geom_point(aes(x = dim_value, y = gmin, col = campaign, shape = year)) +
  geom_smooth(
    aes(x = dim_value, y = gmin, col = year, group = year),
    method = "lm"
  ) +
  facet_wrap(species ~ dim, scales = "free") +
  theme_bw()


## 4.2 join microclimate --------------------------------------------------------

df_best_meteo_corr_all <- df_best_meteo_corr |>
  filter(
    !trait %in% c("doy", "mean_psi_bar", "pheno_start_doy", "leaf_age_d")
  ) |>
  ungroup() |>
  ## instead of filtering by single trait, now group by!
  # filter(trait == "psi_midday_mpa") |>
  group_by(trait) |>
  arrange(desc(abs_corr)) |>
  unnest(data) |>
  mutate(
    meteo_predictor = paste(meteo_var, abs(timeframe), sep = "_"),
    pheno_start_doy = case_when(
      species == "FASY" & year == 2023 ~ 121,
      species == "FASY" & year == 2024 ~ 104,
      species == "FREX" & year == 2023 ~ 140,
      species == "FREX" & year == 2024 ~ 138
    ),
    doy = yday(date),
    leaf_age_d = doy - pheno_start_doy
  ) |>
  select(-c(timeframe, meteo_var, corrcoef, pval, abs_corr, pheno_start_doy)) |>
  # pivot_wider(names_from = meteo_predictor, values_from = meteo_val) |>
  mutate(year = year |> as.factor(), date = date |> as.character()) |>
  distinct()

## join with traits dataframe containing NSC and nutrients
df_hydr_traits_best_meteo <- df_hydr_traits |>
  select(-c(doy, pheno_start_doy, leaf_age_d, campaign)) |>
  distinct() |>
  left_join(
    df_best_meteo_corr_all %>%
      mutate(
        trait = trait %>%
          str_replace_all("elasticity_tot", "elast_tot") %>%
          str_replace_all("elasticity_sym", "elast_sym")
      ),
    by = c("year", "date", "species", "sample_id", "trait")
  )

df_hydr_traits_best_meteo_scaled <- df_hydr_traits_best_meteo |>
  mutate(nutrient_name = nutrient_name |> str_remove("\\_con")) |>
  ## grouped by species as meteo variables are unique by species
  group_by(species, meteo_predictor) |>
  mutate(meteo_val_scaled = scale(meteo_val)) |>
  ungroup() |>
  group_by(species, sugar_name) |>
  mutate(sugar_conc_scaled = scale(sugar_conc)) |>
  ungroup() |>
  group_by(species, nutrient_name) |>
  mutate(nutrient_conc_scaled = scale(nutrient_conc)) |>
  ungroup()

## 4.3 model ---------------------------------------------------------------

### 4.3.1 psi_tlp  -----------------------------------------------------------

#### FREX ----

df_psi_tlp_frex <- df_hydr_traits_best_meteo_scaled |>
  # filter(species == "FREX", trait == "psi_tlp") |>
  filter(species == "FREX", trait == "psi_tlp") |>
  select(-c(nutrient_conc, sugar_conc, meteo_val)) |>
  distinct()

names(df_psi_tlp_frex) = names(df_psi_tlp_frex) |>
  str_remove_all("\\[\\, 1\\]")

df_psi_tlp_frex

df_psi_tlp_frex_wide <- df_psi_tlp_frex |>
  filter(!is.na(nutrient_conc_scaled)) |>
  filter(!sugar_name |> str_detect("ppm")) |>
  pivot_wider(names_from = nutrient_name, values_from = nutrient_conc_scaled) |>
  pivot_wider(names_from = sugar_name, values_from = sugar_conc_scaled) |>
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val_scaled) %>%
  mutate(leaf_age_scale = scale(leaf_age_d))

names(df_psi_tlp_frex_wide) = names(df_psi_tlp_frex_wide) |>
  str_remove_all("\\[\\, 1\\]")

## get all predictors for massive model
predictor_names <- df_psi_tlp_frex |>
  filter(
    !sugar_name |> str_detect("ppm"),
    !is.na(nutrient_conc_scaled),
    !nutrient_name %in%
      c(
        "fe_mg_kg",
        "mn_mg_kg",
        "zn_mg_kg",
        # "na_mg_kg",
        "al_mg_kg"
      )
  ) |>
  select(c(sugar_name, nutrient_name, meteo_predictor)) |>
  as.vector() |>
  unlist() |>
  unique()

## copy predictors
str_c(predictor_names, collapse = " + ") |>
  paste()

mod.all_psi_tlp_frex <- lm(
  value ~
    leaf_age_d +
    ## insert copied predictor names
    fructose_mg_g +
    glucose_mg_g +
    glucose_starch_1 +
    starch_mg_g +
    sucrose_mg_g +
    sugar_weight_mg +
    total_sugars_mg_g +
    starch_sugar_ratio +
    c +
    ca_mg_kg +
    k_mg_kg +
    mg_mg_kg +
    n +
    p_mg_kg +
    s_mg_kg +
    mean_ta_55 +
    max_ta_55 +
    mean_rew_60 +
    mean_vpd_45 +
    sum_et_50 +
    sum_ppfd_5 +
    sum_pet_50 +
    mean_rh_2,
  data = df_psi_tlp_frex_wide,
  na.action = "na.fail"
)

mod.all_psi_tlp_frex |> summary()

## reduce model

mod.red_psi_tlp_frex <- mod.all_psi_tlp_frex |>
  MASS::stepAIC(direction = "both")
mod.red_psi_tlp_frex |> summary()

mod.dregde_psi_tlp_frex <- mod.red_psi_tlp_frex |>
  MuMIn::dredge(rank = "AICc", m.max = 7)

mod.avg <- model.avg(mod.dregde_psi_tlp_frex, delta < 2)
mod.avg %>% summary()

f.make_formula_dredge(mod.dregde_psi_tlp_frex[1, ])

mod.dregded_psi_tlp_frex <- lm(
  value ~
    fructose_mg_g +
    glucose_mg_g +
    mean_ta_55 +
    p_mg_kg +
    s_mg_kg +
    sucrose_mg_g +
    total_sugars_mg_g,
  data = df_psi_tlp_frex_wide,
  na.action = "na.fail"
)

df_psi_tlp_frex_wide |>
  ggplot() +
  geom_point(aes(x = leaf_age_d, y = value)) +
  geom_line(aes(
    x = leaf_age_d,
    y = predict(mod.all_psi_tlp_frex),
    col = "all predictors",
    group = sample_id
  )) +
  geom_line(aes(
    x = leaf_age_d,
    y = predict(mod.red_psi_tlp_frex),
    group = sample_id,
    col = "reduced with StepAIC"
  )) +
  geom_line(aes(
    x = leaf_age_d,
    y = predict(mod.dregded_psi_tlp_frex),
    group = sample_id,
    col = "reduced with Dredge"
  )) +
  facet_wrap(~year) +
  labs(title = "Turgor loss point model with NSC, Nutrients and Microclimate") +
  theme_bw() +
  guides(color = guide_legend(title = "Model")) +
  theme(legend.position = "bottom")

## lme

library(MuMIn)
mod.red_psi_tlp_frex |> formula()

formula.red <- mod.red_psi_tlp_frex |> formula()

mod.lme_psi_tlp_frex <- nlme::lme(
  formula.red,
  random = ~ 1 | sample_id,
  data = df_psi_tlp_frex_wide,
  # correlation = corAR(form = ~ date | sample_id),
  control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 200),
  method = "ML"
)

formula.red2 <- mod.lme_psi_tlp_frex |> stepAIC(direction = "both") |> formula()

mod.lme_red_psi_tlp_frex <- nlme::lme(
  formula.red2,
  random = ~ 1 | sample_id,
  data = df_psi_tlp_frex_wide,
  # correlation = corAR(form = ~ date | sample_id),
  control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 200),
  method = "ML"
)

mod.lme_psi_tlp_frex |> summary()
mod.lme_psi_tlp_frex |> r2()
mod.lme_psi_tlp_frex |> check_model()

mod.lme_psi_tlp_frex %>% summary()

mod.lme_dredge_psi_tlp_frex <- mod.lme_psi_tlp_frex |>
  MuMIn::dredge(rank = "AICc", m.max = 7)

mod.avg <- model.avg(mod.lme_dredge_psi_tlp_frex, delta < 2)
mod.avg %>% summary()

mod.avg %>% f.make_formula_dredge()

mod.lme_dredged_psi_tlp_frex <- nlme::lme(
  value ~ leaf_age_scale + max_ta_55 + p_mg_kg + s_mg_kg + total_sugars_mg_g,
  random = ~ 1 | sample_id,
  data = df_psi_tlp_frex_wide,
  # correlation = corAR(form = ~ date | sample_id),
  control = lmeControl(
    opt = "optim",
    maxIter = 100,
    msMaxIter = 200,
    tolerance = 1e-4
  ),
  method = "ML"
)

mod.lme_dredged_psi_tlp_frex |> summary()
mod.lme_dredged_psi_tlp_frex |> r2()
mod.lme_dredged_psi_tlp_frex |> check_model()

### plot effect sizes
model_predictors <- names(fixef(mod.lme_dredged_psi_tlp_frex)) # Extract fixed effect names

# Create a named vector for label replacement
df_predictors <- df_predictors %>%
  mutate(base_abbr = extract_base_name(abbr)) # Create a column without numbers

# Generate a named vector for matching
predictor_labels <- setNames(df_predictors$long, df_predictors$base_abbr)

# Apply formatting to all predictors
new_labels <- sapply(model_predictors, format_label)

plot.psi_tlp_frex_coefs.png <-
  sjPlot::plot_model(
    mod.lme_dredged_psi_tlp_frex,
    show.values = TRUE,
    show.p = T,
    value.offset = 0.3,
    line.size = 0.5,
    dot.size = 1.3
  ) +
  geom_hline(aes(yintercept = 0), alpha = 0.2) +
  scale_x_discrete(labels = new_labels) +
  labs(
    x = "",
    y = "Scaled coefficient estimates",
    title = "Effect sizes",
    # subtitle = "Fraxinus excelsior"
  ) +
  thesis_theme +
  # lims(x = c(-0.05, 0.3)) +
  theme(
    axis.text = element_text(size = 15),
    title = element_text(size = 15),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.margin = margin(t = 0, r = 10, b = 0, l = 0) # add margin on the right
  )
plot.psi_tlp_frex_coefs.png
ggsave(
  "figures/poster/plot.psi_tlp_frex_coefs.png",
  width = 10,
  height = 10,
  units = "cm"
)


#### FASY ----

df_psi_tlp_fasy <- df_hydr_traits_best_meteo_scaled |>
  # filter(species == "fasy", trait == "psi_tlp") |>
  filter(species == "FASY", trait == "psi_tlp") |>
  select(-c(nutrient_conc, sugar_conc, meteo_val)) |>
  distinct()

names(df_psi_tlp_fasy) = names(df_psi_tlp_fasy) |>
  str_remove_all("\\[\\, 1\\]")

df_psi_tlp_fasy

df_psi_tlp_fasy_wide <- df_psi_tlp_fasy |>
  filter(!is.na(nutrient_conc_scaled)) |>
  filter(!sugar_name |> str_detect("ppm")) |>
  pivot_wider(names_from = nutrient_name, values_from = nutrient_conc_scaled) |>
  pivot_wider(names_from = sugar_name, values_from = sugar_conc_scaled) |>
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val_scaled) %>%
  mutate(leaf_age_scale = scale(leaf_age_d))

names(df_psi_tlp_fasy_wide) = names(df_psi_tlp_fasy_wide) |>
  str_remove_all("\\[\\, 1\\]")

## get all predictors for massive model
predictor_names <- df_psi_tlp_fasy |>
  filter(
    !sugar_name |> str_detect("ppm"),
    !is.na(nutrient_conc_scaled),
    !nutrient_name %in%
      c("fe_mg_kg", "mn_mg_kg", "zn_mg_kg", "al_mg_kg", "na_mg_kg")
  ) |>
  select(c(sugar_name, nutrient_name, meteo_predictor)) |>
  as.vector() |>
  unlist() |>
  unique()

## copy predictors
str_c(predictor_names, collapse = " + ") |>
  paste()

mod.all_psi_tlp_fasy <- lm(
  value ~
    leaf_age_d +
    ## insert copied predictor names
    fructose_mg_g +
    glucose_mg_g +
    glucose_starch_1 +
    starch_mg_g +
    sucrose_mg_g +
    sugar_weight_mg +
    total_sugars_mg_g +
    starch_sugar_ratio +
    c +
    ca_mg_kg +
    k_mg_kg +
    mg_mg_kg +
    n +
    p_mg_kg +
    s_mg_kg +
    mean_rh_40 +
    mean_et_pet_45 +
    mean_vpd_30 +
    sum_pet_40 +
    mean_rew_15 +
    mean_ta_10 +
    sum_p_60 +
    sum_p_et_45 +
    max_ta_35 +
    sum_et_1 +
    sum_ppfd_40,
  data = df_psi_tlp_fasy_wide,
  na.action = "na.fail"
)

mod.all_psi_tlp_fasy |> summary()

## reduce model

mod.red_psi_tlp_fasy <- mod.all_psi_tlp_fasy |>
  MASS::stepAIC(direction = "both")
mod.red_psi_tlp_fasy |> summary()

mod.dregde_psi_tlp_fasy <- mod.red_psi_tlp_fasy |> MuMIn::dredge(rank = "AICc")

mod.dregde_psi_tlp_fasy[1, ] %>% f.make_formula_dredge() %>% as.formula()

mod.dregded_psi_tlp_fasy <- lm(
  value ~ k_mg_kg +
    leaf_age_d +
    mean_rh_40 +
    mean_ta_10 +
    mean_vpd_30 +
    sum_pet_40,
  data = df_psi_tlp_fasy_wide,
  na.action = "na.fail"
)

df_psi_tlp_fasy_wide |>
  ggplot() +
  geom_point(aes(x = leaf_age_d, y = value)) +
  geom_line(aes(
    x = leaf_age_d,
    y = predict(mod.all_psi_tlp_fasy),
    col = "all predictors",
    group = sample_id
  )) +
  geom_line(aes(
    x = leaf_age_d,
    y = predict(mod.red_psi_tlp_fasy),
    group = sample_id,
    col = "reduced with StepAIC"
  )) +
  geom_line(aes(
    x = leaf_age_d,
    y = predict(mod.dregded_psi_tlp_fasy),
    group = sample_id,
    col = "reduced with Dredge"
  )) +
  facet_wrap(~year) +
  labs(title = "Turgor loss point model with NSC, Nutrients and Microclimate") +
  theme_bw() +
  guides(color = guide_legend(title = "Model")) +
  theme(legend.position = "bottom")

## lme

library(MuMIn)
mod.red_psi_tlp_fasy |> formula()

formula.red <- mod.red_psi_tlp_fasy |> formula()

mod.lme_psi_tlp_fasy <- nlme::lme(
  formula.red,
  random = ~ 1 | sample_id,
  data = df_psi_tlp_fasy_wide,
  # correlation = corAR(form = ~ date | sample_id),
  control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 200),
  method = "ML"
)

formula.red2 <- mod.lme_psi_tlp_fasy |> stepAIC(direction = "both") |> formula()

mod.lme_red_psi_tlp_fasy <- nlme::lme(
  value ~ leaf_age_d +
    fructose_mg_g +
    glucose_mg_g +
    starch_mg_g +
    sucrose_mg_g +
    total_sugars_mg_g +
    starch_sugar_ratio +
    k_mg_kg +
    mg_mg_kg +
    n +
    mean_et_pet_45 +
    mean_vpd_30 +
    sum_pet_40 +
    mean_ta_10,
  random = ~ 1 | sample_id,
  data = df_psi_tlp_fasy_wide,
  # correlation = corAR(form = ~ date | sample_id),
  control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 200),
  method = "ML"
)

mod.lme_red_psi_tlp_fasy |> summary()
mod.lme_red_psi_tlp_fasy |> r2()
mod.lme_red_psi_tlp_fasy |> check_model()

mod.lme_psi_tlp_fasy %>% summary()

mod.lme_dredge_psi_tlp_fasy <- mod.lme_red_psi_tlp_fasy |>
  MuMIn::dredge(rank = "AICc", m.max = 7)

mod.lme_dredge_psi_tlp_fasy[1, ] %>% f.make_formula_dredge() %>% as.formula()

mod.lme_dredged_psi_tlp_fasy <- nlme::lme(
  value ~ k_mg_kg +
    leaf_age_d +
    sum_pet_40 +
    mean_et_pet_45 +
    mean_ta_10 +
    mean_vpd_30 +
    starch_sugar_ratio,
  random = ~ 1 | sample_id,
  data = df_psi_tlp_fasy_wide,
  # correlation = corAR(form = ~ date | sample_id),
  control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 200),
  method = "ML"
)

mod.lme_dredged_psi_tlp_fasy |> summary()
mod.lme_dredged_psi_tlp_fasy |> r2()
mod.lme_dredged_psi_tlp_fasy |> check_model()

mod.lme_final_psi_tlp_fasy <- nlme::lme(
  value ~ leaf_age_scale +
    sum_pet_40 +
    mean_et_pet_45 +
    mean_ta_10 +
    k_mg_kg +
    starch_sugar_ratio,
  random = ~ 1 | sample_id,
  data = df_psi_tlp_fasy_wide,
  # correlation = corAR(form = ~ date | sample_id),
  control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 200),
  method = "ML"
)

mod.lme_final_psi_tlp_fasy |> summary()
mod.lme_final_psi_tlp_fasy |> r2()
mod.lme_final_psi_tlp_fasy |> check_model()

### plot effect sizes
model_predictors <- names(fixef(mod.lme_final_psi_tlp_fasy)) # Extract fixed effect names

# Create a named vector for label replacement
df_predictors <- df_predictors %>%
  mutate(base_abbr = extract_base_name(abbr)) # Create a column without numbers

# Generate a named vector for matching
predictor_labels <- setNames(df_predictors$long, df_predictors$base_abbr)

# Apply formatting to all predictors
new_labels <- sapply(model_predictors, format_label)

plot.psi_tlp_fasy_coefs.png <-
  sjPlot::plot_model(
    mod.lme_final_psi_tlp_fasy,
    show.values = TRUE,
    show.p = T,
    value.offset = 0.3,
    line.size = 0.5,
    dot.size = 1.3
  ) +
  geom_hline(aes(yintercept = 0), alpha = 0.2) +
  scale_x_discrete(labels = new_labels) +
  labs(
    x = "",
    y = "Scaled coefficient estimates",
    title = "Effect sizes",
    # subtitle = "Fraxinus excelsior"
  ) +
  thesis_theme +
  # lims(x = c(-0.05, 0.3)) +
  theme(
    axis.text = element_text(size = 12),
    title = element_text(size = 15),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.margin = margin(t = 0, r = 10, b = 0, l = 0) # add margin on the right
  )
plot.psi_tlp_fasy_coefs.png
ggsave(
  "figures/poster/psi_tlp_fasy_effect_sizes.png",
  width = 10,
  height = 10,
  units = "cm"
)


#### Plot for both species ---------------------------------------------------

# Extract R² (marginal) and format with 2 decimals
r2_fasy <- sprintf("%.2f", r2(mod.lme_final_psi_tlp_fasy)$R2_marginal)
r2_frex <- sprintf("%.2f", r2(mod.lme_dredged_psi_tlp_frex)$R2_marginal)

# Custom labels using plotmath (italic species, plain R²)
species_labels <- c(
  bquote(italic("Fagus sylvatica") ~ "(R"^2 * " = " ~ .(r2_fasy) * ")"),
  bquote(italic("Fraxinus excelsior") ~ "(R"^2 * " = " ~ .(r2_frex) * ")")
)

ggplot() +
  geom_point(
    data = df_psi_tlp_fasy_wide,
    aes(x = leaf_age_d, y = value, shape = "Fagus sylvatica"),
    col = "#dc782a"
  ) +
  geom_point(
    data = df_psi_tlp_frex_wide,
    aes(x = leaf_age_d, y = value, shape = "Fraxinus excelsior"),
    col = "#56b4e9"
  ) +
  geom_line(
    data = df_psi_tlp_fasy_wide,
    aes(
      x = leaf_age_d,
      y = predict(mod.lme_final_psi_tlp_fasy),
      group = sample_id,
      linetype = "Fagus sylvatica"
    ),
    col = "#dc782a",
    linewidth = 1,
    alpha = 0.5
  ) +
  geom_line(
    data = df_psi_tlp_frex_wide,
    aes(
      x = leaf_age_d,
      y = predict(mod.lme_dredged_psi_tlp_frex),
      group = sample_id,
      linetype = "Fraxinus excelsior"
    ),
    col = "#56b4e9",
    linewidth = 1,
    alpha = 0.5
  ) +
  facet_wrap(~year) +
  labs(
    y = "Osm. potential at turgor loss [MPa]",
    x = "Leaf age (days since leaf out)"
  ) +
  lims(y = c(-3.5, 0), x = c(10, 170)) +
  theme_bw() +
  guides(
    shape = guide_legend(title = "Species"),
    linetype = guide_legend(title = "Species")
  ) +
  scale_shape_manual(
    values = c("Fagus sylvatica" = 16, "Fraxinus excelsior" = 17),
    labels = species_labels
  ) +
  scale_linetype_manual(
    values = c("Fagus sylvatica" = "solid", "Fraxinus excelsior" = "solid"),
    labels = species_labels
  ) +
  thesis_theme +
  theme(
    strip.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )

ggsave(
  "figures/poster/psi_tlp_model.png",
  height = 11.5,
  width = 22,
  units = "cm"
)


### 4.3.1 Elasticity  -----------------------------------------------------------

#### FREX ----

df_elast_frex <- df_hydr_traits_best_meteo_scaled |>
  # filter(species == "FREX", trait == "elast") |>
  filter(species == "FREX", trait == "elast_sym") |>
  select(-c(nutrient_conc, sugar_conc, meteo_val)) |>
  distinct()

names(df_elast_frex) = names(df_elast_frex) |>
  str_remove_all("\\[\\, 1\\]")

df_elast_frex

df_elast_frex_wide <- df_elast_frex |>
  filter(!is.na(nutrient_conc_scaled)) |>
  filter(!sugar_name |> str_detect("ppm")) |>
  pivot_wider(names_from = nutrient_name, values_from = nutrient_conc_scaled) |>
  pivot_wider(names_from = sugar_name, values_from = sugar_conc_scaled) |>
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val_scaled) %>%
  mutate(leaf_age_scale = scale(leaf_age_d))

names(df_elast_frex_wide) = names(df_elast_frex_wide) |>
  str_remove_all("\\[\\, 1\\]")

## get all predictors for massive model
predictor_names <- df_elast_frex |>
  filter(
    !sugar_name |> str_detect("ppm"),
    !is.na(nutrient_conc_scaled),
    !nutrient_name %in% c("fe_mg_kg", "mn_mg_kg", "zn_mg_kg", "al_mg_kg"),
    !sugar_name %in% c("sugar_weight_mg")
  ) |>
  select(sugar_name, nutrient_name, meteo_predictor) |>
  unlist(use.names = FALSE) |>
  unique() %>%
  na.omit()

## copy predictors
paste(predictor_names, collapse = " + ") %>% cat()

## lm

mod.all_elast_frex <- lm(
  value ~ leaf_age_d +
    fructose_mg_g +
    glucose_mg_g +
    glucose_starch_1 +
    starch_mg_g +
    sucrose_mg_g +
    total_sugars_mg_g +
    starch_sugar_ratio +
    c +
    ca_mg_kg +
    k_mg_kg +
    mg_mg_kg +
    n +
    p_mg_kg +
    s_mg_kg +
    mean_et_pet_1 +
    mean_rh_25 +
    mean_vpd_30 +
    sum_pet_40,
  data = df_elast_frex_wide,
  na.action = "na.fail"
)

## reduce model

mod.red_elast_frex <- mod.all_elast_frex |> MASS::stepAIC(direction = "both")
mod.red_elast_frex |> summary()
mod.red_elast_frex |> formula()

mod.dredge_elast_frex <- mod.red_elast_frex |>
  MuMIn::dredge(rank = "AICc", m.max = 7)

mod.dredge_elast_frex[1, ] %>% f.make_formula_dredge()

mod.lme_elast_frex <- glmmTMB::glmmTMB(
  value ~ leaf_age_d +
    ## paste
    leaf_age_d +
    mean_vpd_30 +
    ## end paste
    (1 | sample_id),
  data = df_elast_frex_wide,
  family = Gamma(link = "log"),
  na.action = "na.fail"
)

mod.lme_elast_frex |> summary()
mod.lme_elast_frex |> r2()
mod.lme_elast_frex |> check_model()

mod.lme_dredge_elast_frex <- mod.lme_elast_frex |>
  MuMIn::dredge(rank = "AICc", m.max = 7)

mod.lme_dredge_elast_frex[1, ] %>% f.make_formula_dredge() %>% cat()

mod.lme_dredged_elast_frex <- glmmTMB::glmmTMB(
  value ~ leaf_age_d +
    ## paste
    mean_vpd_30 +
    ## end paste
    (1 | sample_id),
  data = df_elast_frex_wide,
  family = Gamma(link = "log"),
  na.action = "na.fail"
)

mod.lme_dredged_elast_frex |> summary()
mod.lme_dredged_elast_frex |> r2()
mod.lme_dredged_elast_frex |> check_model()

mod.lme_elast_frex_for_labels <-
  nlme::lme(
    value ~ leaf_age_d +
      ## paste
      mean_vpd_30,
    ## end paste
    random = ~ 1 | sample_id,
    data = df_elast_frex_wide,
    na.action = "na.fail"
  )
mod.lme_elast_frex_for_labels %>% summary()
### plot effect sizes
model_predictors <- names(fixef(mod.lme_elast_frex_for_labels)) # Extract fixed effect names

# Create a named vector for label replacement
df_predictors <- df_predictors %>%
  mutate(base_abbr = extract_base_name(abbr)) # Create a column without numbers

# Generate a named vector for matching
predictor_labels <- setNames(df_predictors$long, df_predictors$base_abbr)

# Apply formatting to all predictors
new_labels <- sapply(model_predictors, format_label)

# Extract fixed effects on link scale
coefs <- summary(mod.lme_dredged_elast_frex)$coefficients$cond |>
  as.data.frame()

# Create dataframe for plotting
coef_df <- data.frame(term = rownames(coefs)) |>
  mutate(
    estimate = coefs[, 1],
    lower = coefs[, 1] - 1.96 * coefs[, 2], # Confidence interval
    upper = coefs[, 1] + 1.96 * coefs[, 2],
    est_label = round(estimate, 2),
    p = case_when(
      coefs[, 4] >= 0.05 ~ "",
      coefs[, 4] < 0.05 & coefs[, 4] >= 0.01 ~ "*",
      coefs[, 4] < 0.01 & coefs[, 4] >= 0.001 ~ "**",
      coefs[, 4] < 0.001 ~ "***"
    ),
    labels = paste(est_label, p, sep = " ")
  )

plot.elast_frex_coefs.png <- coef_df |>
  filter(term != "(Intercept)") |>
  mutate(
    coef_color = factor(case_when(estimate > 0 ~ "blue", TRUE ~ "red")), # Use labels instead of hex
    term = fct_rev(factor(term))
  ) |>
  ggplot(aes(y = term, x = estimate, color = coef_color)) +
  geom_point(size = 1.5, show.legend = F) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0, show.legend = F) +
  geom_text(aes(label = labels), vjust = -1, size = 4, show.legend = F) +
  # annotate("text", x = -0.45, y = 4.4, label = "b)") +
  scale_y_discrete(labels = new_labels) +
  # scale_x_continuous(limits = c(0, 2)) +
  scale_color_manual(values = c("blue" = "#377eb8", "red" = "#e41a1c")) + # Manually define colors
  labs(
    y = "",
    x = "Scaled coefficient estimates",
    # title = "Effect sizes",
    # subtitle = "Fraxinus excelsior"
  ) +
  thesis_theme +
  # lims(x = c(-0.05, 0.3)) +
  theme(
    axis.text = element_text(size = 15),
    title = element_text(size = 15),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.margin = margin(t = 0, r = 10, b = 0, l = 0) # add margin on the right
  )
plot.elast_frex_coefs.png
ggsave(
  "figures/poster/plot.elast_frex_coefs.png",
  width = 10,
  height = 10,
  units = "cm"
)

#### FASY ----

df_elast_fasy <- df_hydr_traits_best_meteo_scaled |>
  filter(species == "FASY", trait == "elast_sym") |>
  select(-c(nutrient_conc, sugar_conc, meteo_val)) |>
  distinct()

names(df_elast_fasy) = names(df_elast_fasy) |>
  str_remove_all("\\[\\, 1\\]")

df_elast_fasy

df_elast_fasy_wide <- df_elast_fasy |>
  filter(!is.na(nutrient_conc_scaled)) |>
  filter(!sugar_name |> str_detect("ppm")) |>
  pivot_wider(names_from = nutrient_name, values_from = nutrient_conc_scaled) |>
  pivot_wider(names_from = sugar_name, values_from = sugar_conc_scaled) |>
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val_scaled) %>%
  mutate(leaf_age_scale = scale(leaf_age_d))

names(df_elast_fasy_wide) = names(df_elast_fasy_wide) |>
  str_remove_all("\\[\\, 1\\]")

## get all predictors for massive model
predictor_names <- df_elast_fasy |>
  filter(
    !sugar_name |> str_detect("ppm"),
    !is.na(nutrient_conc_scaled),
    !nutrient_name %in%
      c("fe_mg_kg", "mn_mg_kg", "zn_mg_kg", "al_mg_kg", "na_mg_kg"),
    !sugar_name %in% c("sugar_weight_mg")
  ) |>
  select(sugar_name, nutrient_name, meteo_predictor) |>
  unlist(use.names = FALSE) |>
  unique() %>%
  na.omit()

## copy predictors
paste(predictor_names, collapse = " + ")

## lm

mod.all_elast_fasy <- lm(
  value ~ leaf_age_d +
    fructose_mg_g +
    glucose_mg_g +
    glucose_starch_1 +
    starch_mg_g +
    sucrose_mg_g +
    total_sugars_mg_g +
    starch_sugar_ratio +
    c +
    ca_mg_kg +
    k_mg_kg +
    mg_mg_kg +
    n +
    p_mg_kg +
    s_mg_kg +
    mean_rh_45 +
    mean_et_pet_45 +
    mean_vpd_35 +
    mean_rew_60 +
    sum_et_1 +
    sum_pet_55 +
    sum_p_et_2 +
    sum_ppfd_5 +
    max_ta_35 +
    sum_p_2 +
    mean_ta_10,
  data = df_elast_fasy_wide,
  na.action = "na.fail"
)

## reduce model

mod.red_elast_fasy <- mod.all_elast_fasy |> MASS::stepAIC(direction = "both")
mod.red_elast_fasy |> summary()
mod.red_elast_fasy |> formula()

mod.dredge_elast_fasy <- mod.red_elast_fasy |>
  MuMIn::dredge(rank = "AICc", m.max = 7)

mod.dredge_elast_fasy[1, ] %>% f.make_formula_dredge()

mod.lme_elast_fasy <- glmmTMB::glmmTMB(
  value ~
    ## paste
    leaf_age_d +
    fructose_mg_g +
    glucose_starch_1 +
    starch_mg_g +
    sucrose_mg_g +
    starch_sugar_ratio +
    c +
    ca_mg_kg +
    k_mg_kg +
    n +
    s_mg_kg +
    sum_et_60 +
    max_ta_55 +
    mean_et_pet_50 +
    ## end paste
    (1 | sample_id),
  data = df_elast_fasy_wide,
  family = Gamma(link = "log"),
  na.action = "na.fail"
)

mod.lme_elast_fasy |> summary()
mod.lme_elast_fasy |> r2()
mod.lme_elast_fasy |> check_collinearity()

mod.lme_dredge_elast_fasy <- mod.lme_dredged_elast_fasy |>
  MuMIn::dredge(rank = "AICc", m.max = 7)

mod.lme_dredge_elast_fasy[1, ] %>% f.make_formula_dredge() %>% cat()

mod.lme_dredged_elast_fasy <- glmmTMB::glmmTMB(
  value ~ leaf_age_d +
    ## paste
    mean_vpd_35 +
    sum_pet_55 +
    # + total_sugars_mg_g + sum_et_60 + max_ta_55 + mean_et_pet_50
    ## end paste
    (1 | sample_id),
  data = df_elast_fasy_wide,
  family = Gamma(link = "log"),
  na.action = "na.fail"
)

mod.lme_dredged_elast_fasy |> summary()
mod.lme_dredged_elast_fasy |> r2()
mod.lme_dredged_elast_fasy |> check_collinearity()

mod.lme_elast_fasy_for_labels <-
  nlme::lme(
    value ~ leaf_age_d +
      ## paste
      mean_vpd_35 +
      sum_pet_55,
    ## end paste
    random = ~ 1 | sample_id,
    data = df_elast_fasy_wide,
    na.action = "na.fail"
  )

### plot effect sizes
model_predictors <- names(fixef(mod.lme_elast_fasy_for_labels)) # Extract fixed effect names

# Create a named vector for label replacement
df_predictors <- df_predictors %>%
  mutate(base_abbr = extract_base_name(abbr)) # Create a column without numbers

# Generate a named vector for matching
predictor_labels <- setNames(df_predictors$long, df_predictors$base_abbr)

# Apply formatting to all predictors
new_labels <- sapply(model_predictors, format_label)

# Extract fixed effects on link scale
coefs <- summary(mod.lme_dredged_elast_fasy)$coefficients$cond |>
  as.data.frame()

# Create dataframe for plotting
coef_df <- data.frame(term = rownames(coefs)) |>
  mutate(
    estimate = coefs[, 1],
    lower = coefs[, 1] - 1.96 * coefs[, 2], # Confidence interval
    upper = coefs[, 1] + 1.96 * coefs[, 2],
    est_label = round(estimate, 2),
    p = case_when(
      coefs[, 4] >= 0.05 ~ "",
      coefs[, 4] < 0.05 & coefs[, 4] >= 0.01 ~ "*",
      coefs[, 4] < 0.01 & coefs[, 4] >= 0.001 ~ "**",
      coefs[, 4] < 0.001 ~ "***"
    ),
    labels = paste(est_label, p, sep = " ")
  )

plot.elast_fasy_coefs.png <- coef_df |>
  filter(term != "(Intercept)") |>
  mutate(
    coef_color = factor(case_when(estimate > 0 ~ "blue", TRUE ~ "red")), # Use labels instead of hex
    term = fct_rev(factor(term))
  ) |>
  ggplot(aes(y = term, x = estimate, color = coef_color)) +
  geom_point(size = 1.5, show.legend = F) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0, show.legend = F) +
  geom_text(aes(label = labels), vjust = -1, size = 4, show.legend = F) +
  # annotate("text", x = -0.45, y = 4.4, label = "b)") +
  scale_y_discrete(labels = new_labels) +
  # scale_x_continuous(limits = c(0, 2)) +
  scale_color_manual(values = c("blue" = "#377eb8", "red" = "#e41a1c")) + # Manually define colors
  labs(
    y = "",
    x = "Scaled coefficient estimates",
    # title = "Effect sizes",
    # subtitle = "Fraxinus excelsior"
  ) +
  thesis_theme +
  # lims(x = c(-0.05, 0.3)) +
  theme(
    axis.text = element_text(size = 15),
    title = element_text(size = 15),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.margin = margin(t = 0, r = 10, b = 0, l = 0) # add margin on the right
  )
plot.elast_fasy_coefs.png
ggsave(
  "figures/poster/plot.elast_fasy_coefs.png",
  width = 10,
  height = 10,
  units = "cm"
)

#### Plot for both species ---------------------------------------------------

# Extract R² (marginal) and format with 2 decimals
r2_fasy <- sprintf("%.2f", r2(mod.lme_dredged_elast_fasy)$R2_marginal)
r2_frex <- sprintf("%.2f", r2(mod.lme_dredged_elast_frex)$R2_marginal)

# Custom labels using plotmath (italic species, plain R²)
species_labels <- c(
  bquote(italic("Fagus sylvatica") ~ "(R"^2 * " = " ~ .(r2_fasy) * ")"),
  bquote(italic("Fraxinus excelsior") ~ "(R"^2 * " = " ~ .(r2_frex) * ")")
)

df_elast_fasy_wide |>
  ggplot() +
  geom_point(
    data = df_elast_fasy_wide,
    aes(x = leaf_age_d, y = value, shape = "Fagus sylvatica"),
    col = "#dc782a"
  ) +
  geom_point(
    data = df_elast_frex_wide,
    aes(x = leaf_age_d, y = value, shape = "Fraxinus excelsior"),
    col = "#56b4e9"
  ) +
  geom_line(
    data = df_elast_fasy_wide,
    aes(
      x = leaf_age_d,
      y = predict(mod.lme_dredged_elast_fasy, type = "response"),
      group = sample_id,
      linetype = "Fagus sylvatica"
    ),
    col = "#dc782a",
    linewidth = 1,
    alpha = 0.5
  ) +
  geom_line(
    data = df_elast_frex_wide,
    aes(
      x = leaf_age_d,
      y = predict(mod.lme_dredged_elast_frex, type = "response"),
      group = sample_id,
      linetype = "Fraxinus excelsior"
    ),
    col = "#56b4e9",
    linewidth = 1,
    alpha = 0.5
  ) +
  facet_wrap(~year) +
  labs(
    y = "Bulk modulus of elasticity [MPa]",
    x = "Leaf age (days since leaf out)"
  ) +
  lims(y = c(0, 30), x = c(10, 170)) +
  theme_bw() +
  guides(
    shape = guide_legend(title = "Species"),
    linetype = guide_legend(title = "Species")
  ) +
  scale_shape_manual(
    values = c("Fagus sylvatica" = 16, "Fraxinus excelsior" = 17),
    labels = species_labels
  ) +
  scale_linetype_manual(
    values = c("Fagus sylvatica" = "solid", "Fraxinus excelsior" = "solid"),
    labels = species_labels
  ) +
  thesis_theme +
  theme(
    strip.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )

ggsave(
  "figures/poster/elast_model.png",
  height = 11.5,
  width = 22,
  units = "cm"
)

### 4.3.3 Gmin  -----------------------------------------------------------

#### FREX ----

df_gmin_frex <- df_hydr_traits_best_meteo_scaled |>
  # filter(species == "FREX", trait == "gmin") |>
  filter(species == "FREX", trait == "gmin") |>
  select(-c(nutrient_conc, sugar_conc, meteo_val)) |>
  distinct()

names(df_gmin_frex) = names(df_gmin_frex) |>
  str_remove_all("\\[\\, 1\\]")

df_gmin_frex

df_gmin_frex_wide <- df_gmin_frex |>
  filter(!is.na(nutrient_conc_scaled)) |>
  filter(!sugar_name |> str_detect("ppm")) |>
  pivot_wider(names_from = nutrient_name, values_from = nutrient_conc_scaled) |>
  pivot_wider(names_from = sugar_name, values_from = sugar_conc_scaled) |>
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val_scaled) %>%
  mutate(leaf_age_scale = scale(leaf_age_d))

names(df_gmin_frex_wide) = names(df_gmin_frex_wide) |>
  str_remove_all("\\[\\, 1\\]")

## get all predictors for massive model
predictor_names <- df_gmin_frex |>
  filter(
    !sugar_name |> str_detect("ppm"),
    !is.na(nutrient_conc_scaled),
    !nutrient_name %in% c("fe_mg_kg", "mn_mg_kg", "zn_mg_kg", "al_mg_kg"),
    !sugar_name %in% c("sugar_weight_mg")
  ) |>
  select(sugar_name, nutrient_name, meteo_predictor) |>
  unlist(use.names = FALSE) |>
  unique() %>%
  na.omit()

## copy predictors
paste(predictor_names, collapse = " + ") %>% cat()

## lm

mod.all_gmin_frex <- lm(
  value ~ leaf_age_d +
    fructose_mg_g +
    glucose_mg_g +
    glucose_starch_1 +
    starch_mg_g +
    sucrose_mg_g +
    total_sugars_mg_g +
    starch_sugar_ratio +
    c +
    ca_mg_kg +
    k_mg_kg +
    mg_mg_kg +
    n +
    p_mg_kg +
    s_mg_kg +
    max_ta_55 +
    mean_ta_60 +
    mean_et_pet_50 +
    sum_ppfd_25 +
    sum_pet_30 +
    mean_rh_2 +
    sum_p_2 +
    sum_et_5,
  data = df_gmin_frex_wide,
  na.action = "na.fail"
)

## reduce model

mod.red_gmin_frex <- mod.all_gmin_frex |> MASS::stepAIC(direction = "both")
mod.red_gmin_frex |> summary()
mod.red_gmin_frex |> formula()

mod.dredge_gmin_frex <- mod.red_gmin_frex |>
  MuMIn::dredge(rank = "AICc", m.max = 7)

mod.dredge_gmin_frex[1, ] %>% f.make_formula_dredge()

mod.lme_gmin_frex <- glmmTMB::glmmTMB(
  value ~ leaf_age_d +
    ## paste
    max_ta_55 +
    mean_et_pet_50 +
    mean_rh_2 +
    ## end paste
    (1 | sample_id),
  data = df_gmin_frex_wide,
  family = Gamma(link = "log"),
  na.action = "na.fail"
)

mod.lme_gmin_frex |> summary()
mod.lme_gmin_frex |> r2()
mod.lme_gmin_frex |> check_model()
mod.lme_gmin_frex |> check_collinearity()

mod.lme_dredge_gmin_frex <- mod.lme_gmin_frex |>
  MuMIn::dredge(rank = "AICc", m.max = 7)

mod.lme_dredge_gmin_frex[1, ] %>% f.make_formula_dredge() %>% cat()

mod.lme_dredged_gmin_frex <- glmmTMB::glmmTMB(
  value ~ leaf_age_d +
    ## paste
    max_ta_55 +
    mean_et_pet_50 +
    mean_rh_2 +
    ## end paste
    (1 | sample_id),
  data = df_gmin_frex_wide,
  family = Gamma(link = "log"),
  na.action = "na.fail"
)

mod.lme_dredged_gmin_frex |> summary()
mod.lme_dredged_gmin_frex |> r2()
mod.lme_dredged_gmin_frex |> check_model()

mod.lme_gmin_frex_for_labels <-
  nlme::lme(
    value ~ leaf_age_d +
      ## paste
      max_ta_55 +
      mean_et_pet_50 +
      mean_rh_2,
    ## end paste
    random = ~ 1 | sample_id,
    data = df_gmin_frex_wide,
    na.action = "na.fail"
  )

### plot effect sizes
model_predictors <- names(fixef(mod.lme_gmin_frex_for_labels)) # Extract fixed effect names

# Create a named vector for label replacement
df_predictors <- df_predictors %>%
  mutate(base_abbr = extract_base_name(abbr)) # Create a column without numbers

# Generate a named vector for matching
predictor_labels <- setNames(df_predictors$long, df_predictors$base_abbr)

# Apply formatting to all predictors
new_labels <- sapply(model_predictors, format_label)

# Extract fixed effects on link scale
coefs <- summary(mod.lme_dredged_gmin_frex)$coefficients$cond |>
  as.data.frame()

# Create dataframe for plotting
coef_df <- data.frame(term = rownames(coefs)) |>
  mutate(
    estimate = coefs[, 1],
    lower = coefs[, 1] - 1.96 * coefs[, 2], # Confidence interval
    upper = coefs[, 1] + 1.96 * coefs[, 2],
    est_label = round(estimate, 2),
    p = case_when(
      coefs[, 4] >= 0.05 ~ "",
      coefs[, 4] < 0.05 & coefs[, 4] >= 0.01 ~ "*",
      coefs[, 4] < 0.01 & coefs[, 4] >= 0.001 ~ "**",
      coefs[, 4] < 0.001 ~ "***"
    ),
    labels = paste(est_label, p, sep = " ")
  )

plot.gmin_frex_coefs.png <- coef_df |>
  filter(term != "(Intercept)") |>
  mutate(
    coef_color = factor(case_when(estimate > 0 ~ "blue", TRUE ~ "red")), # Use labels instead of hex
    term = fct_rev(factor(term))
  ) |>
  ggplot(aes(y = term, x = estimate, color = coef_color)) +
  geom_point(size = 1.5, show.legend = F) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0, show.legend = F) +
  geom_text(aes(label = labels), vjust = -1, size = 4, show.legend = F) +
  # annotate("text", x = -0.45, y = 4.4, label = "b)") +
  scale_y_discrete(labels = new_labels) +
  # scale_x_continuous(limits = c(0, 2)) +
  scale_color_manual(values = c("blue" = "#377eb8", "red" = "#e41a1c")) + # Manually define colors
  labs(
    y = "",
    x = "Scaled coefficient estimates",
    # title = "Effect sizes",
    # subtitle = "Fraxinus excelsior"
  ) +
  thesis_theme +
  # lims(x = c(-0.05, 0.3)) +
  theme(
    axis.text = element_text(size = 15),
    title = element_text(size = 15),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.margin = margin(t = 0, r = 10, b = 0, l = 0) # add margin on the right
  )
plot.gmin_frex_coefs.png
ggsave(
  "figures/poster/plot.gmin_frex_coefs.png",
  width = 10,
  height = 10,
  units = "cm"
)

#### FASY ----

df_gmin_fasy <- df_hydr_traits_best_meteo_scaled |>
  filter(species == "FASY", trait == "gmin") |>
  select(-c(nutrient_conc, sugar_conc, meteo_val)) |>
  distinct()

names(df_gmin_fasy) = names(df_gmin_fasy) |>
  str_remove_all("\\[\\, 1\\]")

df_gmin_fasy

df_gmin_fasy_wide <- df_gmin_fasy |>
  filter(!is.na(nutrient_conc_scaled)) |>
  filter(!sugar_name |> str_detect("ppm")) |>
  pivot_wider(names_from = nutrient_name, values_from = nutrient_conc_scaled) |>
  pivot_wider(names_from = sugar_name, values_from = sugar_conc_scaled) |>
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val_scaled) %>%
  mutate(leaf_age_scale = scale(leaf_age_d))

names(df_gmin_fasy_wide) = names(df_gmin_fasy_wide) |>
  str_remove_all("\\[\\, 1\\]")

df_gmin_fasy_wide %>%
  ggplot() +
  geom_point(aes(x = mean_vpd_30, y = value))

## get all predictors for massive model
predictor_names <- df_gmin_fasy |>
  filter(
    !sugar_name |> str_detect("ppm"),
    !is.na(nutrient_conc_scaled),
    !nutrient_name %in%
      c("fe_mg_kg", "mn_mg_kg", "zn_mg_kg", "al_mg_kg", "na_mg_kg"),
    !sugar_name %in% c("sugar_weight_mg")
  ) |>
  select(sugar_name, nutrient_name, meteo_predictor) |>
  unlist(use.names = FALSE) |>
  unique() %>%
  na.omit()

## copy predictors
paste(predictor_names, collapse = " + ")

## lm

mod.all_gmin_fasy <- lm(
  value ~ leaf_age_d +
    fructose_mg_g +
    glucose_mg_g +
    glucose_starch_1 +
    starch_mg_g +
    sucrose_mg_g +
    total_sugars_mg_g +
    starch_sugar_ratio +
    c +
    ca_mg_kg +
    k_mg_kg +
    mg_mg_kg +
    n +
    p_mg_kg +
    s_mg_kg +
    mean_rh_30 +
    mean_et_pet_50 +
    mean_vpd_30 +
    sum_pet_40 +
    mean_ta_10 +
    max_ta_10 +
    sum_ppfd_25 +
    sum_p_15 +
    sum_p_et_15 +
    sum_et_10 +
    mean_rew_10,
  data = df_gmin_fasy_wide,
  na.action = "na.fail"
)

## reduce model

mod.red_gmin_fasy <- mod.all_gmin_fasy |> MASS::stepAIC(direction = "backward")
mod.red_gmin_fasy |> summary()
mod.red_gmin_fasy |> formula()

mod.dredge_gmin_fasy <- mod.red_gmin_fasy |>
  MuMIn::dredge(rank = "AICc", m.max = 7)

avgmod <- model.avg(mod.dredge_gmin_fasy, subset = delta < 2.5)
# confset.95p <- get.models(mod.dredge_gmin_fasy, cumsum(weight) <= .95)
# avgmod.95p <- model.avg(confset.95p)
avgmod %>% summary()

mod.dredge_gmin_fasy[1, ] %>% f.make_formula_dredge()

mod.lme_gmin_fasy <- glmmTMB::glmmTMB(
  value ~
    ## paste
    leaf_age_d +
    glucose_starch_1 +
    mean_vpd_30 +
    total_sugars_mg_g +
    n +
    ## end paste
    (1 | sample_id),
  data = df_gmin_fasy_wide,
  family = Gamma(link = "log"),
  na.action = "na.fail"
)

mod.lme_gmin_fasy |> summary()
mod.lme_gmin_fasy |> r2()
mod.lme_gmin_fasy |> check_collinearity()

mod.lme_dredge_gmin_fasy <- mod.lme_gmin_fasy |>
  MuMIn::dredge(rank = "AICc", m.max = 7)

mod.lme_dredge_gmin_fasy[1, ] %>% f.make_formula_dredge() %>% cat()

mod.lme_dredged_gmin_fasy <- glmmTMB::glmmTMB(
  value ~ leaf_age_d +
    ## paste
    mean_vpd_30 +
    total_sugars_mg_g +
    n +
    ## end paste
    (1 | sample_id),
  data = df_gmin_fasy_wide,
  family = Gamma(link = "log"),
  na.action = "na.fail"
)

mod.lme_dredged_gmin_fasy |> summary()
mod.lme_dredged_gmin_fasy |> r2()
mod.lme_dredged_gmin_fasy |> check_collinearity()

mod.lme_gmin_fasy_for_labels <-
  nlme::lme(
    value ~ leaf_age_d +
      ## paste
      mean_vpd_30 +
      total_sugars_mg_g +
      n,
    ## end paste
    random = ~ 1 | sample_id,
    data = df_gmin_fasy_wide,
    na.action = "na.fail"
  )

### plot effect sizes
model_predictors <- names(fixef(mod.lme_gmin_fasy_for_labels)) # Extract fixed effect names

# Create a named vector for label replacement
df_predictors <- df_predictors %>%
  mutate(base_abbr = extract_base_name(abbr)) # Create a column without numbers

# Generate a named vector for matching
predictor_labels <- setNames(df_predictors$long, df_predictors$base_abbr)

# Apply formatting to all predictors
new_labels <- sapply(model_predictors, format_label)

# Extract fixed effects on link scale
coefs <- summary(mod.lme_dredged_gmin_fasy)$coefficients$cond |>
  as.data.frame()

# Create dataframe for plotting
coef_df <- data.frame(term = rownames(coefs)) |>
  mutate(
    estimate = coefs[, 1],
    lower = coefs[, 1] - 1.96 * coefs[, 2], # Confidence interval
    upper = coefs[, 1] + 1.96 * coefs[, 2],
    est_label = round(estimate, 2),
    p = case_when(
      coefs[, 4] >= 0.05 ~ "",
      coefs[, 4] < 0.05 & coefs[, 4] >= 0.01 ~ "*",
      coefs[, 4] < 0.01 & coefs[, 4] >= 0.001 ~ "**",
      coefs[, 4] < 0.001 ~ "***"
    ),
    labels = paste(est_label, p, sep = " ")
  )

# Define desired order of terms
term_order <- c("n", "total_sugars_mg_g", "mean_vpd_30", "leaf_age_d")

plot.gmin_fasy_coefs.png <- coef_df |>
  filter(term != "(Intercept)") |>
  mutate(
    coef_color = factor(case_when(estimate > 0 ~ "blue", TRUE ~ "red")),
    term = factor(term, levels = term_order) # enforce order
  ) |>
  ggplot(aes(y = term, x = estimate, color = coef_color)) +
  geom_point(size = 1.5, show.legend = F) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0, show.legend = F) +
  geom_text(aes(label = labels), vjust = -1, size = 4, show.legend = F) +
  scale_y_discrete(labels = new_labels) +
  scale_color_manual(values = c("blue" = "#377eb8", "red" = "#e41a1c")) +
  labs(
    y = "",
    x = "Scaled coefficient estimates",
    # title = "Effect sizes",
    # subtitle = "Fraxinus excelsior"
  ) +
  thesis_theme +
  # lims(x = c(-0.05, 0.3)) +
  theme(
    axis.text = element_text(size = 15),
    title = element_text(size = 15),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.margin = margin(t = 0, r = 10, b = 0, l = 0) # add margin on the right
  )
plot.gmin_fasy_coefs.png
ggsave(
  "figures/poster/plot.gmin_fasy_coefs.png",
  width = 10,
  height = 10,
  units = "cm"
)

#### Plot for both species ---------------------------------------------------

# Extract R² (marginal) and format with 2 decimals
r2_fasy <- sprintf("%.2f", r2(mod.lme_dredged_gmin_fasy)$R2_marginal)
r2_frex <- sprintf("%.2f", r2(mod.lme_dredged_gmin_frex)$R2_marginal)

# Custom labels using plotmath (italic species, plain R²)
species_labels <- c(
  bquote(italic("Fagus sylvatica") ~ "(R"^2 * " = " ~ .(r2_fasy) * ")"),
  bquote(italic("Fraxinus excelsior") ~ "(R"^2 * " = " ~ .(r2_frex) * ")")
)

df_gmin_fasy_wide |>
  ggplot() +
  geom_point(
    data = df_gmin_fasy_wide,
    aes(x = leaf_age_d, y = value, shape = "Fagus sylvatica"),
    col = "#dc782a"
  ) +
  geom_point(
    data = df_gmin_frex_wide,
    aes(x = leaf_age_d, y = value, shape = "Fraxinus excelsior"),
    col = "#56b4e9"
  ) +
  geom_line(
    data = df_gmin_fasy_wide,
    aes(
      x = leaf_age_d,
      y = predict(mod.lme_dredged_gmin_fasy, type = "response"),
      group = sample_id,
      linetype = "Fagus sylvatica"
    ),
    col = "#dc782a",
    linewidth = 1,
    alpha = 0.5
  ) +
  geom_line(
    data = df_gmin_frex_wide,
    aes(
      x = leaf_age_d,
      y = predict(mod.lme_dredged_gmin_frex, type = "response"),
      group = sample_id,
      linetype = "Fraxinus excelsior"
    ),
    col = "#56b4e9",
    linewidth = 1,
    alpha = 0.5
  ) +
  facet_wrap(~year) +
  labs(
    y = expression(
      g[min] ~ "[" * mmol ~ m^{
        -2
      } ~ s^{
        -1
      } *
        "]"
    ),
    x = "Leaf age (days since leaf out)"
  ) +
  lims(y = c(0, 18), x = c(10, 170)) +
  theme_bw() +
  guides(
    shape = guide_legend(title = "Species"),
    linetype = guide_legend(title = "Species")
  ) +
  scale_shape_manual(
    values = c("Fagus sylvatica" = 16, "Fraxinus excelsior" = 17),
    labels = species_labels
  ) +
  scale_linetype_manual(
    values = c("Fagus sylvatica" = "solid", "Fraxinus excelsior" = "solid"),
    labels = species_labels
  ) +
  thesis_theme +
  theme(
    strip.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )

ggsave("figures/poster/gmin_model.png", height = 11.5, width = 22, units = "cm")

### 4.3.4 p50  -----------------------------------------------------------

#### FREX ----

df_p50_frex <- df_hydr_traits_best_meteo_scaled |>
  # filter(species == "FREX", trait == "p50") |>
  filter(species == "FREX", trait == "p50") |>
  select(-c(nutrient_conc, sugar_conc, meteo_val)) |>
  distinct()

names(df_p50_frex) = names(df_p50_frex) |>
  str_remove_all("\\[\\, 1\\]")

df_p50_frex

df_p50_frex_wide <- df_p50_frex |>
  filter(!is.na(nutrient_conc_scaled), !is.na(value)) |>
  filter(!sugar_name |> str_detect("ppm")) |>
  pivot_wider(names_from = nutrient_name, values_from = nutrient_conc_scaled) |>
  pivot_wider(names_from = sugar_name, values_from = sugar_conc_scaled) |>
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val_scaled) %>%
  mutate(leaf_age_scale = scale(leaf_age_d))

names(df_p50_frex_wide) = names(df_p50_frex_wide) |>
  str_remove_all("\\[\\, 1\\]")

## get all predictors for massive model
predictor_names <- df_p50_frex |>
  filter(
    !sugar_name |> str_detect("ppm"),
    !is.na(nutrient_conc_scaled),
    !nutrient_name %in%
      c(
        "fe_mg_kg",
        "mn_mg_kg",
        "zn_mg_kg",
        # "na_mg_kg",
        "al_mg_kg"
      )
  ) |>
  select(c(sugar_name, nutrient_name, meteo_predictor)) |>
  as.vector() |>
  unlist() |>
  unique()

## copy predictors
str_c(predictor_names, collapse = " + ") |>
  paste()

mod.all_p50_frex <- lm(
  value ~
    leaf_age_d +
    ## insert copied predictor names
    glucose_starch_1 +
    total_sugars_mg_g +
    starch_sugar_ratio +
    c +
    ca_mg_kg +
    k_mg_kg +
    mg_mg_kg +
    n +
    p_mg_kg +
    s_mg_kg +
    sum_pet_20 +
    sum_ppfd_5 +
    max_ta_15 +
    mean_ta_15 +
    sum_et_1 +
    mean_rh_15 +
    mean_vpd_15 +
    mean_et_pet_20,
  data = df_p50_frex_wide,
  na.action = "na.fail"
)

mod.all_p50_frex |> summary()

## reduce model

mod.red_p50_frex <- mod.all_p50_frex |> MASS::stepAIC(direction = "both")
mod.red_p50_frex |> summary()

## lme

formula.red <- mod.red_p50_frex |> formula()

mod.lme_p50_frex <- nlme::lme(
  formula.red,
  random = ~ 1 | sample_id,
  data = df_p50_frex_wide,
  # correlation = corAR(form = ~ date | sample_id),
  control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 200),
  method = "ML"
)

mod.dregde_p50_frex <- mod.red_p50_frex |>
  MuMIn::dredge(rank = "AICc", m.max = 7)

mod.avg <- model.avg(mod.dregde_p50_frex, delta < 2)
mod.avg %>% summary()

formula.red2 <- mod.lme_p50_frex |> stepAIC(direction = "both") |> formula()

mod.lme_dredged_p50_frex <- nlme::lme(
  value ~ leaf_age_scale +
    max_ta_15 +
    total_sugars_mg_g +
    ca_mg_kg,
  random = ~ 1 | sample_id,
  data = df_p50_frex_wide,
  # correlation = corAR(form = ~ date | sample_id),
  control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 200),
  method = "ML"
)

mod.lme_dredged_p50_frex |> summary()
mod.lme_dredged_p50_frex |> r2()
mod.lme_dredged_p50_frex |> check_model()

### plot effect sizes
model_predictors <- names(fixef(mod.lme_dredged_p50_frex)) # Extract fixed effect names

# Create a named vector for label replacement
df_predictors <- df_predictors %>%
  mutate(base_abbr = extract_base_name(abbr)) # Create a column without numbers

# Generate a named vector for matching
predictor_labels <- setNames(df_predictors$long, df_predictors$base_abbr)

# Apply formatting to all predictors
new_labels <- sapply(model_predictors, format_label)

plot.p50_frex_coefs.png <-
  sjPlot::plot_model(
    mod.lme_dredged_p50_frex,
    show.values = TRUE,
    show.p = T,
    value.offset = 0.3,
    line.size = 0.5,
    dot.size = 1.3
  ) +
  geom_hline(aes(yintercept = 0), alpha = 0.2) +
  scale_x_discrete(labels = new_labels) +
  labs(
    x = "",
    y = "Scaled coefficient estimates",
    title = "",
    # subtitle = "Fraxinus excelsior"
  ) +
  thesis_theme +
  # lims(x = c(-0.05, 0.3)) +
  theme(
    axis.text = element_text(size = 15),
    plot.title = element_blank(),
    title = element_text(size = 15),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.margin = margin(t = 0, r = 10, b = 0, l = 0) # add margin on the right
  )
plot.p50_frex_coefs.png
ggsave(
  "figures/poster/plot.p50_frex_coefs.png",
  width = 10,
  height = 10,
  units = "cm"
)


#### FASY ----

df_p50_fasy <- df_hydr_traits_best_meteo_scaled |>
  # filter(species == "fasy", trait == "p50") |>
  filter(species == "FASY", trait == "p50") |>
  select(-c(nutrient_conc, sugar_conc, meteo_val)) |>
  distinct()

names(df_p50_fasy) = names(df_p50_fasy) |>
  str_remove_all("\\[\\, 1\\]")

df_p50_fasy

df_p50_fasy_wide <- df_p50_fasy |>
  filter(!is.na(nutrient_conc_scaled), !is.na(value)) |>
  filter(!sugar_name |> str_detect("ppm")) |>
  pivot_wider(names_from = nutrient_name, values_from = nutrient_conc_scaled) |>
  pivot_wider(names_from = sugar_name, values_from = sugar_conc_scaled) |>
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val_scaled) %>%
  mutate(leaf_age_scale = scale(leaf_age_d))

names(df_p50_fasy_wide) = names(df_p50_fasy_wide) |>
  str_remove_all("\\[\\, 1\\]")

## get all predictors for massive model
predictor_names <- df_p50_fasy |>
  filter(
    !sugar_name |> str_detect("ppm"),
    !is.na(nutrient_conc_scaled),
    !nutrient_name %in%
      c("fe_mg_kg", "mn_mg_kg", "zn_mg_kg", "al_mg_kg", "na_mg_kg")
  ) |>
  select(c(sugar_name, nutrient_name, meteo_predictor)) |>
  as.vector() |>
  unlist() |>
  unique()

## copy predictors
str_c(predictor_names, collapse = " + ") |>
  paste()

mod.all_p50_fasy <- lm(
  value ~
    leaf_age_d +
    ## insert copied predictor names
    fructose_mg_g +
    glucose_mg_g +
    glucose_starch_1 +
    starch_mg_g +
    sucrose_mg_g +
    sugar_weight_mg +
    total_sugars_mg_g +
    starch_sugar_ratio +
    c +
    ca_mg_kg +
    k_mg_kg +
    mg_mg_kg +
    n +
    p_mg_kg +
    s_mg_kg +
    sum_p_10 +
    sum_p_et_5 +
    max_ta_50 +
    mean_et_pet_40 +
    mean_ta_60,
  data = df_p50_fasy_wide,
  na.action = "na.fail"
)

mod.all_p50_fasy |> summary()

## reduce model

mod.red_p50_fasy <- mod.all_p50_fasy |> MASS::stepAIC(direction = "both")
mod.red_p50_fasy |> summary()

mod.dregde_p50_fasy <- mod.red_p50_fasy |>
  MuMIn::dredge(rank = "AICc", m.max = 5)

mod.avg <- model.avg(mod.dregde_p50_fasy, delta < 2)
mod.avg %>% summary()

## lme

formula.red <- mod.red_p50_fasy |> formula()

mod.lme_p50_fasy <- nlme::lme(
  formula.red,
  random = ~ 1 | sample_id,
  data = df_p50_fasy_wide,
  # correlation = corAR(form = ~ date | sample_id),
  control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 200),
  method = "ML"
)

formula.red2 <- mod.lme_p50_fasy |> stepAIC(direction = "both") |> formula()

mod.lme_p50_fasy |> summary()
mod.lme_p50_fasy |> r2()
mod.lme_p50_fasy |> check_model()

mod.lme_dredge_p50_fasy <- mod.lme_p50_fasy |>
  MuMIn::dredge(rank = "AICc", m.max = 7)
mod.avg <- model.avg(mod.lme_dredge_p50_fasy, delta < 2.5)
mod.avg %>% summary()

mod.lme_dredged_p50_fasy <- nlme::lme(
  value ~ leaf_age_scale +
    k_mg_kg +
    starch_sugar_ratio +
    sum_p_et_5 +
    total_sugars_mg_g,
  random = ~ 1 | sample_id,
  data = df_p50_fasy_wide,
  # correlation = corAR(form = ~ date | sample_id),
  control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 200),
  method = "ML"
)

mod.lme_dredged_p50_fasy |> summary()
mod.lme_dredged_p50_fasy |> r2()
mod.lme_dredged_p50_fasy |> check_model()

### plot effect sizes
model_predictors <- names(fixef(mod.lme_dredged_p50_fasy)) # Extract fixed effect names

# Create a named vector for label replacement
df_predictors <- df_predictors %>%
  mutate(base_abbr = extract_base_name(abbr)) # Create a column without numbers

# Generate a named vector for matching
predictor_labels <- setNames(df_predictors$long, df_predictors$base_abbr)

# Apply formatting to all predictors
new_labels <- sapply(model_predictors, format_label)

plot.p50_fasy_coefs.png <-
  sjPlot::plot_model(
    mod.lme_dredged_p50_fasy,
    show.values = TRUE,
    show.p = T,
    value.offset = 0.3,
    line.size = 0.5,
    dot.size = 1.3
  ) +
  geom_hline(aes(yintercept = 0), alpha = 0.2) +
  scale_x_discrete(labels = new_labels) +
  labs(
    x = "",
    y = "Scaled coefficient estimates",
    # title = "Effect sizes",
    # subtitle = "Fraxinus excelsior"
  ) +
  thesis_theme +
  # lims(x = c(-0.05, 0.3)) +
  theme(
    axis.text = element_text(size = 15),
    title = element_blank(),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.margin = margin(t = 0, r = 10, b = 0, l = 0) # add margin on the right
  )
plot.p50_fasy_coefs.png
ggsave(
  "figures/poster/p50_fasy_effect_sizes.png",
  width = 10,
  height = 10,
  units = "cm"
)


#### Plot for both species ---------------------------------------------------

# Extract R² (marginal) and format with 2 decimals
r2_fasy <- sprintf("%.2f", r2(mod.lme_dredged_p50_fasy)$R2_marginal)
r2_frex <- sprintf("%.2f", r2(mod.lme_dredged_p50_frex)$R2_marginal)

# Custom labels using plotmath (italic species, plain R²)
species_labels <- c(
  bquote(italic("Fagus sylvatica") ~ "(R"^2 * " = " ~ .(r2_fasy) * ")"),
  bquote(italic("Fraxinus excelsior") ~ "(R"^2 * " = " ~ .(r2_frex) * ")")
)

df_p50_fasy_wide |>
  ggplot() +
  geom_point(
    data = df_p50_fasy_wide,
    aes(x = leaf_age_d, y = value, shape = "Fagus sylvatica"),
    col = "#dc782a"
  ) +
  geom_point(
    data = df_p50_frex_wide,
    aes(x = leaf_age_d, y = value, shape = "Fraxinus excelsior"),
    col = "#56b4e9"
  ) +
  geom_line(
    data = df_p50_fasy_wide,
    aes(
      x = leaf_age_d,
      y = predict(mod.lme_dredged_p50_fasy),
      group = sample_id,
      linetype = "Fagus sylvatica"
    ),
    col = "#dc782a",
    linewidth = 1,
    alpha = 0.5
  ) +
  geom_line(
    data = df_p50_frex_wide,
    aes(
      x = leaf_age_d,
      y = predict(mod.lme_dredged_p50_frex),
      group = sample_id,
      linetype = "Fraxinus excelsior"
    ),
    col = "#56b4e9",
    linewidth = 1,
    alpha = 0.5
  ) +
  facet_wrap(~year) +
  labs(y = expression(P[50] ~ "[MPa]"), x = "Leaf age (days since leaf out)") +
  lims(y = c(-6, 0), x = c(10, 170)) +
  theme_bw() +
  guides(
    shape = guide_legend(title = "Species"),
    linetype = guide_legend(title = "Species")
  ) +
  scale_shape_manual(
    values = c("Fagus sylvatica" = 16, "Fraxinus excelsior" = 17),
    labels = species_labels
  ) +
  scale_linetype_manual(
    values = c("Fagus sylvatica" = "solid", "Fraxinus excelsior" = "solid"),
    labels = species_labels
  ) +
  thesis_theme +
  theme(
    strip.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )

ggsave("figures/poster/p50_model.png", height = 11.5, width = 22, units = "cm")


# dump --------------------------------------------------------------------

df_hydr_traits %>%
  filter(
    trait == "psi_midday_mpa",
    sugar_name %>% str_detect("mg\\_g|ratio")
  ) %>%
  group_by(species, campaign, year, sugar_name) %>%
  summarise(sugar_conc = mean(sugar_conc), value = mean(value)) %>%
  ggplot(aes(y = sugar_conc, x = value, col = species)) +
  lims(x = c(0, -3.5)) +
  geom_point() +
  facet_wrap(~sugar_name, scale = "free") +
  labs(x = "Psi midday [MPa]", y = "Sugar concentration") +
  theme_bw()

df_hydr_traits$trait %>% unique()

df_best_meteo_corr |>
  filter(trait == "psi_midday_mpa") |>
  dplyr::select(-data) |>
  write.table("clipboard", sep = "\t", row.names = FALSE)

df_psi_md_meteo <- df_best_meteo_corr |>
  ungroup() |>
  filter(trait == "psi_midday_mpa") |>
  arrange(desc(abs_corr)) |>
  unnest(data) |>
  mutate(
    meteo_predictor = paste(meteo_var, abs(timeframe), sep = "_"),
    pheno_start_doy = case_when(
      species == "FASY" & year == 2023 ~ 121,
      species == "FASY" & year == 2024 ~ 104,
      species == "FREX" & year == 2023 ~ 140,
      species == "FREX" & year == 2024 ~ 138
    ),
    doy = yday(date),
    leaf_age_d = doy - pheno_start_doy
  ) |>
  select(-c(timeframe, meteo_var, corrcoef, pval, abs_corr)) |>
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val)


test_scale <- test |>
  dplyr::select(
    -c(trait, date, year, species, sample_id, campaign, trait_val)
  ) |>
  scale()
test_scale <- test |>
  dplyr::select(c(
    trait,
    date,
    year,
    species,
    sample_id,
    campaign,
    trait_val
  )) |>
  cbind(test_scale)

test <- test |> na.omit()
lm_test <- lm(
  trait_val ~ pheno_start_doy +
    leaf_age_d +
    sum_p_mm_10 +
    sum_p_et_60 +
    min_rew_5 +
    mean_rew_20 +
    max_ta_50 +
    sum_et_35,
  data = test,
  na.action = "na.fail"
)
lm_test |> summary()

library(MuMIn)
model_selection_results <- MuMIn::dredge(lm_test, rank = "AICc")
model_selection_results |> head()

lm_test$residuals |> shapiro.test()

stepAIC(lm_test, direction = "both") |> summary()

lm_test_red <- lm(
  trait_val ~ sum_p_mm_10 +
    sum_p_et_60 +
    sum_et_35 +
    max_ta_50 +
    pheno_start_doy,
  data = test,
  na.action = "na.fail"
)
lm_test_red |> summary()

summary(lm_test_red)$coef |> write.table("clipboard", sep = "\t", row.names = T)


## assess multicollinearity
library(GGally)
test |>
  dplyr::select(
    -c(
      trait,
      date,
      year,
      species,
      sample_id,
      campaign,
      trait_val,
      mean_rew_20,
      min_rew_5,
      doy,
      pheno_start_doy
    )
  ) |>
  ggpairs()

## lme
lme_test <- lmer(
  trait_val ~ sum_p_mm_10 +
    sum_p_et_60 +
    max_ta_50 +
    sum_et_35 +
    leaf_age_d +
    (1 | species / sample_id),
  data = test
)
lme_test |> summary()
lme_test |> performance::r2()

test |>
  modelr::add_predictions(lme_test) |>
  mutate(
    species = recode(
      species,
      "FREX" = "Fraxinus excelsior",
      "FASY" = "Fagus sylvatica"
    )
  ) |>
  ggplot() +
  geom_point(aes(x = doy, y = trait_val, col = species)) +
  geom_line(aes(x = doy, y = pred, col = species, group = sample_id)) +
  facet_wrap(~year) +
  lims(y = c(-4, 0)) +
  scale_color_manual(
    values = c("Fagus sylvatica" = "#410866", "Fraxinus excelsior" = "#D55E00")
  ) +
  labs(
    y = "Midday water potential [MPa]",
    x = "Day of year",
    title = "Midday water potential",
    subtitle = "linear mixed effects results"
  ) +
  thesis_theme


### linear mixed effect models ----------------------------------------------

## cannot keep this structure since dates are across two years

df_pv_params_nest <- df_pv_params_nest |>
  mutate(
    mod = map(data, ~ lmer(value ~ species + date + (1 | sample_id), data = .x))
  )

summary(df_pv_params_nest$mod[[1]])
VarCorr(df_pv_params_nest$mod[[1]])

test <- df_pv_params_nest$data[[1]] |> filter(year == "2023")

mod <- lmer(
  value ~ date + (date | species),
  data = test,
  # control = lmerControl(
  #   optimizer = "nlminb",  # Specify the optimizer directly
  #   You can also specify these if needed:
  #   toler = 1e-6,  # Tolerance for convergence
  #   maxIter = 1000,
  #   msMaxIter = 1000
  # )
)

summary(mod)
ranef(mod)

ggplot(test) +
  geom_point(aes(x = date, y = value, color = species)) +
  # geom_line(aes(x = date, y = value, color = species, group = sample_id), alpha = 0.5) +
  geom_line(aes(
    x = date,
    y = predict(mod),
    color = species,
    group = sample_id
  )) +
  labs(y = "psi_tlp", x = "date") +
  lims(y = c(-3, -2)) +
  theme_bw()

aov(value ~ species + date + species:date, data = test) |> summary()
lm(value ~ species + date + species:date, data = test) |> summary()

### emmeans -----------------------------------------------------------------
library(emmeans)

emm <- emmeans::emmeans(
  mod,
  ~ species * date,
  adjust = "mvt",
  type = "response"
)
emm |> summary()
pairs(emm)
df_emm <- as.data.frame(emm)

test <- test |>
  modelr::add_predictions(mod)

test |>
  ggplot() +
  geom_point(aes(x = date, y = value, color = species)) +
  geom_line(aes(x = date, y = value, group = sample_id), color = "grey") +
  # geom_line(aes(x = date, y = pred, color = species, group = sample_id), shape = 1) +
  geom_line(
    data = df_emm,
    aes(x = date, y = emmean, group = species, color = species)
  )
# +  geom_line(x = date, aes(y = pred))

### simulate data to test lme singularity and re ----------------------------

## dataframe with date, species, sample_id and value

set.seed(123)
n <- 10

## 8 dates, 3 species, 10 samples per species
date <- rep(
  c(
    "2022-01-01",
    "2022-01-02",
    "2022-01-03",
    "2022-01-04",
    "2022-01-05",
    "2022-01-06",
    "2022-01-07",
    "2022-01-08"
  ),
  each = n
) |>
  as.factor()
species <- rep(c("A", "B", "C"), each = n * 8) |> as.factor()
sample_id <- rep(1:n, 8) |> as.factor()
df <- data.frame(species, date)
df <- df |>
  mutate(
    sample_id = paste(species, sample_id, sep = "_"),
    value = case_when(
      species == "A" ~ rnorm(nrow(df), mean = 1, sd = 1),
      species == "B" ~ rnorm(nrow(df), mean = 2, sd = 1),
      species == "C" ~ rnorm(nrow(df), mean = 3, sd = 1)
    )
  )

ggplot(df) +
  geom_point(aes(x = date, y = value, color = species)) +
  geom_line(aes(x = date, y = value, group = sample_id), color = "grey")

mod_sim <- lmer(value ~ date + (1 | species / sample_id), data = df)
mod_sim |> summary()
ranef(mod_sim)

ggplot(df) +
  geom_point(aes(x = date, y = value, color = species)) +
  geom_line(aes(x = date, y = value, group = sample_id), color = "grey") +
  geom_line(
    aes(x = date, y = fitted(mod_sim), color = species, group = sample_id),
    shape = 1
  )


df_sugars_emm |>
  filter(
    sugar_name %in%
      c(
        "total_sugars_mg_g",
        "glucose_mg_g",
        "fructose_mg_g",
        "starch_mg_g",
        "sucrose_mg_g"
      )
  ) |>
  dplyr::select(sample_id, campaign, date, sugar_name, sugar_conc) |>
  pivot_wider(names_from = sugar_name, values_from = sugar_conc) |>
  write.table("clipboard", sep = "\t", row.names = FALSE)

df_sugars_emm |>
  filter(
    sugar_name %in%
      c(
        "total_sugars_mg_g",
        "glucose_mg_g",
        "fructose_mg_g",
        "starch_mg_g",
        "sucrose_mg_g"
      )
  ) |>
  dplyr::select(
    species,
    campaign,
    date,
    sugar_name,
    response,
    SE,
    asymp.LCL,
    asymp.UCL,
    .group
  ) |>
  distinct() |>
  write.table("clipboard", sep = "\t", row.names = FALSE)
