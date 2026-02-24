library(brms)
library(bayesplot)
library(tidybayes)
library(tidyverse)

# Pressure-Volume ------------------------------------------------------------------------

## 1 prepare -----------------------------------------------------------------------------
## Select your own path fot the .csv
df_pv_params <- read_csv("data/calculated_parameters/df_pv_params.csv") |>
  mutate(
    date = as.Date(date, format = "%d.%m.%Y"),
    species = as.factor(species),
    sample_id = as.factor(sample_id),
    campaign = as.factor(campaign),
    year = as.factor(year)
  )

## 1.2.2 Bayesian lme --------------------------------------------------------------------
#
psi_ft.prior <- default_prior(
  psi_ft ~ species * campaign * year + (1 | sample_id),
  family = brmsfamily("skew_normal"),
  data = df_pv_params
)
blme_psi_ft <- brm(
  psi_ft ~ species * campaign * year + (1 | sample_id),
  family = brmsfamily("skew_normal"),
  data = df_pv_params,
  prior = psi_ft.prior,
  chains = 8, # number of chains is relative. Default is 4
  cores = 8, # number of cores dependes on your cpu, more cores is faster
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

#
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
  cores = 8,
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
#
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
#
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
##

## Both Bayesian Lmer and "normal" lmer just for comparison
ggplot(df_pv_params, aes(x = date, y = psi_ft, color = species)) +
  geom_point() +
  geom_line(aes(x = date, y = predict(blme_psi_ft)[, 1], color = species)) +
  stat_summary(
    aes(y = predict(blme_psi_ft)[, 1]),
    fun = "mean",
    geom = "line"
  ) +
  facet_wrap(~year, scales = "free_x") +
  ggtitle("Bayesian lmer")

lme_psi_ft <- lme4::lmer(
  psi_ft ~ species * campaign * year + (1 | sample_id),
  data = df_pv_params
)
ggplot(df_pv_params) +
  geom_point(aes(x = date, y = psi_ft, color = species)) +
  geom_line(aes(
    x = date,
    y = predict(lme_psi_ft),
    color = species,
    group = sample_id
  )) +
  facet_wrap(~year, scales = "free_x") +
  ggtitle("Normal lmer")

ggplot(df_pv_params) +
  geom_point(aes(x = date, y = psi_ft, color = species)) +
  geom_line(aes(
    x = date,
    y = predict(blme_psi_ft)[, 1],
    color = "bayes",
    group = sample_id
  )) +
  geom_line(aes(
    x = date,
    y = predict(lme_psi_ft),
    color = "glm",
    group = sample_id
  )) +
  facet_wrap(~year, scales = "free_x") +
  ggtitle("Normal lmer")
## Rest of the plots

ggplot(df_pv_params, aes(x = date, y = psi_tlp, color = species)) +
  geom_point() +
  stat_summary(
    aes(y = predict(blme_psi_tlp)[, 1]),
    fun = "mean",
    geom = "line"
  ) +
  facet_wrap(~year, scales = "free_x")

ggplot(df_pv_params, aes(x = date, y = elasticity_tot, color = species)) +
  geom_point() +
  stat_summary(aes(y = predict(blme_elast)[, 1]), fun = "mean", geom = "line") +
  facet_wrap(~year, scales = "free_x")

ggplot(df_pv_params, aes(x = date, y = rwc_tot_tlp, color = species)) +
  geom_point() +
  stat_summary(
    aes(y = predict(blme_rwc_tlp)[, 1]),
    fun = "mean",
    geom = "line"
  ) +
  facet_wrap(~year, scales = "free_x")

## 1.2.3 emmeans ----------------------------------------------------------------

emm_psi_ft <- emmeans::emmeans(
  lme_psi_ft,
  ~ species * campaign * year,
  adjust = "mvt",
)
df_emm_psi_ft <- emm_psi_ft |>
  multcomp::cld(Letters = letters) |>
  as.data.frame()

b_emm_psi_ft <- emmeans::emmeans(
  blme_psi_ft,
  ~ species * campaign * year,
  epred = T,
  adjust = "mvt"
)
df_b_emm_psi_ft <- b_emm_psi_ft |>
  multcomp::cld(Letters = letters) |>
  as.data.frame()
df_b_emm_psi_ft # Notice the different results in the significance
# b_emm_psi_ft <- emmeans::emmeans(blme_psi_ft, ~ species * campaign * year, epred=T, type= "response") |> gather_emmeans_draws() |> median_qi() # not used

#b_emm_psi_ft |> compare_levels(.value, by=campaign, emmeans_comparison("eff")) # doestn work

df_b_emm_psi_ft <- df_b_emm_psi_ft |>
  left_join(
    df_pv_params |> dplyr::select(species, campaign, year, date, psi_ft),
    by = c("species", "campaign", "year")
  ) |>
  mutate(
    group_y = case_when(species == "FASY" ~ 0, species == "FREX" ~ -4),
    date_plot = date
  )
year(df_b_emm_psi_ft$date_plot) = 2000

plot.psi_ft.png <- df_b_emm_psi_ft |>
  arrange(species, campaign, year) |>
  # mutate(species = recode(species, "FASY" = "Fagus sylvatica", "FREX" = "Fraxinus excelsior"),
  #        group = f.reletter_cld(.group)) |>
  ggplot() +
  geom_point(
    aes(x = date_plot, y = psi_ft, col = species),
    alpha = 0.8,
    position = position_dodge(width = 0.2),
    show.legend = F
  ) +
  geom_line(
    aes(x = date_plot, y = emmean, col = species),
    alpha = 0.2,
    position = position_dodge(width = 0.2),
    show.legend = F
  ) +
  geom_errorbar(
    aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, color = species),
    width = 3, # replace lower.CL/upper.CL with asymp.LCL/asymp.UCL
    position = position_dodge(width = 0.2),
    alpha = 0.2,
    show.legend = F
  ) +
  geom_text(
    aes(x = date_plot, y = group_y, label = .group, col = species),
    show.legend = F
  ) +
  see::scale_color_oi(order = c(6, 2)) +
  scale_x_date(limits = c(as.Date("2000-05-15"), as.Date("2000-09-30"))) +
  lims(y = c(-4, 0)) +
  labs(y = expression(Psi[ft] ~ "[MPa]"), x = "Date", tag = "a)") +
  guides(color = guide_legend(title = "Species")) +
  facet_wrap(~year) +
  #thesis_theme +
  theme(strip.background = element_blank())
plot.psi_ft.png # Notice the different results in the significance

# Work In Progress
## Plotting the posterior estimated density of the coefficients for psi_ft model
p <- mcmc_areas(blme_psi_ft, prob = 0.95, pars = vars(starts_with("b_")))
p +
  geom_vline(xintercept = 0) +
  labs(
    title = "Posteriror distribution",
    subtitle = "of coef with 95% credible intervals"
  )
fixef(blme_psi_ft)
ranef(blme_psi_ft)
coef(blme_psi_ft) # fixed efffects + randon effects
