# setup ------------------------------------------------------------------

library(brms)
library(bayesplot)
library(tidybayes)
library(tidyverse)

df_pv_params <- read_csv("data/calculated_parameters/df_pv_params.csv") |>
  mutate(
    date = as.Date(date, format = "%d.%m.%Y"),
    yday = yday(date) |> as.integer(),
    species = as.factor(species),
    sample_id = as.factor(sample_id),
    campaign = as.factor(campaign),
    year = as.factor(year),
    date_fac = as.factor(date)
  )

source("scripts/ggplot_themes.R")

Sys.setlocale("LC_ALL", "English")

n_cpu_cores = parallel::detectCores()

# 1. BLME ----

## 1.1 Full Turgor ----

psi_ft.prior <- default_prior(
  psi_ft ~ species * year * date_fac + (1 | sample_id),
  family = brmsfamily("gaussian"),
  data = df_pv_params
)

blme_psi_ft <- brm(
  psi_ft ~ species * year * date_fac + (1 | sample_id),
  family = brmsfamily("skew_normal"),
  data = df_pv_params,
  prior = psi_ft.prior,
  chains = 8, # number of chains is relative. Default is 4
  cores = n_cpu_cores, # number of cores dependes on your cpu, more cores is faster
  iter = 10000,
  warmup = 2000,
  thin = 5,
  seed = 2300, # seed for reproducibility
  # Tuning parameters for the NUTS algorithm
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  # Saving model parameters and stan code
  save_model = "blme_psi_ft.stan",
  save_pars = save_pars(all = TRUE)
)

blme_psi_ft |>
  tidybayes::summarise_draws(
    "mean",
    ~ quantile(.x, probs = c(.05, .25, .5, .75, .95))
  )
blme_psi_ft |> bayes_R2()
blme_psi_ft |> loo_R2()

p <- mcmc_areas(blme_psi_ft, prob = 0.95, pars = vars(starts_with("b_")))
p +
  geom_vline(xintercept = 0) +
  labs(
    title = "Posterior distribution",
    subtitle = "of coefficients with 95% credible intervals"
  )

## species & years are different

## 1.2 Turgor loss ----
psi_tlp.prior <- default_prior(
  psi_tlp ~ species * campaign * year + (1 | sample_id),
  family = brmsfamily("skew_normal"),
  data = df_pv_params
)

blme_psi_tlp <- brm(
  psi_tlp ~ species * campaign * year + (1 | sample_id),
  family = brmsfamily("skew_normal"),
  data = df_pv_params,
  prior = psi_tlp.prior,
  chains = 8,
  cores = 24,
  iter = 10000,
  warmup = 2000,
  thin = 5,

  # seed for reproducibility
  seed = 2300,

  # Tuning parameters for the NUTS algorithm
  control = list(adapt_delta = 0.95, max_treedepth = 12),

  # Saving model parameters and stan code
  save_model = "blme_psi_tlp.stan",
  save_pars = save_pars(all = TRUE)
)

blme_psi_tlp |>
  tidybayes::summarise_draws(
    "mean",
    ~ quantile(.x, probs = c(.05, .25, .5, .75, .95))
  )
blme_psi_tlp |> bayes_R2()
blme_psi_tlp |> loo_R2()

p <- mcmc_areas(blme_psi_tlp, prob = 0.95, pars = vars(starts_with("b_")))
p +
  geom_vline(xintercept = 0) +
  labs(
    title = "Posterior distribution",
    subtitle = "of coefficients with 95% credible intervals"
  )


## 1.3 Elasticity (total) ----

elast.prior <- default_prior(
  elasticity_tot ~ species * campaign * year + (1 | sample_id),
  family = Gamma(link = "log"),
  data = df_pv_params
)

blme_elast <- brm(
  elasticity_tot ~ species * campaign * year + (1 | sample_id),
  family = Gamma(link = "log"),
  data = df_pv_params,
  prior = elast.prior,
  chains = 8,
  cores = 8,
  iter = 10000,
  warmup = 2000,
  thin = 5,

  # seed for reproducibility
  seed = 2300,

  # Tuning parameters for the NUTS algorithm
  control = list(adapt_delta = 0.95, max_treedepth = 12),

  # Saving model parameters and stan code
  save_model = "blme_elast.stan",
  save_pars = save_pars(all = TRUE)
)
blme_elast |>
  tidybayes::summarise_draws(
    "mean",
    ~ quantile(.x, probs = c(.05, .25, .5, .75, .95))
  )
blme_elast |> bayes_R2()
blme_elast |> loo_R2()

## 1.4 RWC at turgor loss ----
rwc_tlp.prior <- default_prior(
  rwc_tot_tlp ~ species * campaign * year + (1 | sample_id),
  family = weibull(link = "log"),
  data = df_pv_params
)
blme_rwc_tlp <- brm(
  rwc_tot_tlp ~ species * campaign * year + (1 | sample_id),
  family = weibull(link = "log"),
  data = df_pv_params,
  prior = rwc_tlp.prior,
  chains = 8,
  cores = 8,
  iter = 10000,
  warmup = 2000,
  thin = 5,

  # seed for reproducibility
  seed = 2300,

  # Tuning parameters for the NUTS algorithm
  control = list(adapt_delta = 0.95, max_treedepth = 12),

  # Saving model parameters and stan code
  save_model = "blme_rwc_tlp.stan",
  save_pars = save_pars(all = TRUE)
)
blme_rwc_tlp |>
  tidybayes::summarise_draws(
    "mean",
    ~ quantile(.x, probs = c(.05, .25, .5, .75, .95))
  )
blme_rwc_tlp |> bayes_R2()
blme_rwc_tlp |> loo_R2()

# 2. Emmeans post-hoc ----

## 2.1 Full turgor ----

b_emm_psi_ft <- emmeans::emmeans(
  blme_psi_ft,
  ~ species * year * date_fac,
  epred = T,
  adjust = "mvt"
)

df_b_emm_psi_ft <- b_emm_psi_ft |>
  multcomp::cld(Letters = letters) |>
  as.data.frame()

df_b_emm_psi_ft <- df_b_emm_psi_ft |>
  left_join(
    df_pv_params |> dplyr::select(species, date_fac, year, date, psi_ft),
    by = c("species", "date_fac", "year")
  ) |>
  mutate(
    group_y = case_when(species == "FASY" ~ 0, species == "FREX" ~ -4),
    date_plot = date
  )
year(df_b_emm_psi_ft$date_plot) = 2000

# 3. Export values & model files ----

# 4. Plots ----
## 4.1 Full turgor ----

plot.psi_ft.png <- df_b_emm_psi_ft |>
  arrange(species, year, date_fac) |>
  mutate(
    species = recode(
      species,
      "FASY" = "Fagus sylvatica",
      "FREX" = "Fraxinus excelsior"
    ),
    # group = f.reletter_cld(.group)
  ) |>
  ggplot() +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3, # replace lower.CL/upper.CL with asymp.LCL/asymp.UCL
    position = position_dodge(width = 0.5),
    alpha = 0.5,
    show.legend = F
  ) +
  geom_point(
    aes(x = date_plot, y = psi_ft, col = species),
    alpha = 0.5,
    position = position_dodge(width = 0.5),
    show.legend = F
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 0.2,
    position = position_dodge(width = 0.5),
    show.legend = F
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = .group, col = species),
    show.legend = F
  ) +
  see::scale_color_oi(order = c(6, 2)) +
  scale_x_date(
    limits = c(as.Date("2000-05-15"), as.Date("2000-09-30")),
    date_breaks = "1 month",
    date_labels = "%b"
  ) +
  lims(y = c(-4, 0)) +
  labs(
    y = expression(Psi[ft] ~ "[MPa]"),
    x = "Date"
    # , tag = "a)"
  ) +
  guides(color = guide_legend(title = "Species")) +
  facet_wrap(~year) +
  thesis_theme +
  theme(
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot.psi_ft.png
