
library(brms)
library(bayesplot)
library(tidybayes)
#library(forcats)
#library(lme4)
library(tidyverse)

# Data load

df_pv_params <- read_csv("data/calculated_parameters/df_pv_params.csv") |> 
  mutate(date = as.Date(date, format = "%d.%m.%Y"), 
         species = as.factor(species),
         sample_id = as.factor(sample_id),
         campaign = as.factor(campaign),
         year = as.factor(year)) 

# Viz

df_pv_params %>%
  #mutate(treat = fct_relevel(campaign, "1", "2", "3", "4")) %>%
  ggplot(., aes(x = date, y = psi_ft, group = species, 
                color = as.factor(species))) +
  geom_point() +
  #geom_line() +
  geom_smooth() +
  theme_bw() +
  labs(col = "Tree id", x = "Date")


# brm model 1: best one from the models tested
psitlp.Model.prior1 <- default_prior(psi_tlp ~ species * campaign * year + (1 | sample_id),
                                    family = student(),
                                    data = df_pv_params)
psitlp.Model.prior2 <- default_prior(psi_tlp ~ species * campaign * year + (1 | sample_id),
                                    family = brmsfamily("skew_normal"),
                                    data = df_pv_params)

## Fitting of the model using the brm pkg. We specify the model the usual way.
## Plus include the prior distribution for each of the parameter, this was done using the default priors. We compute 8 (the more the better, but with diminishing return), and 8 cores (depending on your computer select an appropriate number, usually 0.5 * number of cores on your computer).
## As for the number of iterations (iter), the more the better since we want to reach the stationary distribution (see eg https://arxiv.org/pdf/2311.02726). We burn-up (warup) the first 2000 since we assume they are not from the stationary distribution. We use thinning (thin) of 5 since there was some autocorrelation in the MCMC samples, but it doesnt seem to improve inference.
## We modify the control options. This are parameters for the sampling algorithm used, NUTS. They are a bit technical.
## We save the model and parameters for model evaluation later.
psitlp.Model1 <- brm(psi_ft ~ species * campaign * year + (1 | sample_id),
                    family = student(),
                    data = df_pv_params,
                    prior = psitlp.Model.prior1,
                    chains = 8,
                    cores = 8,
                    iter = 5000,
                    warmup = 2000,
                    thin=5,
                    seed= 2300, # seed for reproducibility
                    control = list(adapt_delta = 0.9, max_treedepth = 10),
                    save_model = "Model1_psitlp",
                    save_pars = save_pars(all = TRUE)
)
psitlp.Model2 <- brm(psi_ft ~ species * campaign * year + (1 | sample_id),
                    family = brmsfamily("skew_normal"),
                    data = df_pv_params,
                    prior = psitlp.Model.prior2,
                    chains = 8,
                    cores = 8,
                    iter = 5000,
                    warmup = 2000,
                    thin=5,
                    seed= 2300, # seed for reproducibility
                    control = list(adapt_delta = 0.9, max_treedepth = 10),
                    save_model = "Model2_psitlp",
                    save_pars = save_pars(all = TRUE)
)

# Model evaluation
summary(psitlp.Model2)
coef(psitlp.Model1) # Get the coef of the model. The interpretation is the same as the lmer version. But now we have CREDIBLE intervals instead of CONFIDENCE intervals. If the CI contain 0 then the corresponding parameters is statistically not different from 0, else at the 95% level they are not 0.

## Simple plot of the posterior samples of the coefficients and the trace plots (caterplliar looking plots)
plot(psitlp.Model2, ask = F)
mcmc_plot(psitlp.Model2, variable = "^b_", regex = T)
mcmc_plot(psitlp.Model1, variable = "^b_", regex = T, type = "acf")



## We want the 2 curves be close to each other-
## loo ribbon (see https://paulbuerkner.com/brms/reference/pp_check.brmsfit.html ) using the number of replications (ndraws) to compute credible bands based on the posterior predictive checks. 
pp_check(psitlp.Model2, ndraws=200, type="loo_ribbon")
## The ppc_loo_pit_qq does a QQ plot of the observation against the posterior predictive check after passing it through the Probability Integral Transformation (PIT) to compare if the 2 distribution match. Its interpreted the same as any QQ plots
pp_check(psitlp.Model2, ndraws=200, type="loo_pit_qq") 
pp_check(psitlp.Model2, ndraws=200, type="scatter_avg") 

## We also compute the posterior distribution of the response and compare it the observed one. We want them to be very similar. In our case there is a little bimodality and skewness that is not capture in the posterior density. Some improvement in the model could help
pp_check(psitlp.Model1, ndraws = 100) 

# plot conditional effects for each predictor
plot(conditional_effects(psitlp.Model2), ask = FALSE)

## Evaluate model fit. loo_R2 and bayes_R2 are variations of the traditional R^2, but Im not familiar with them so I would not repor them in general.
WAIC(psitlp.Model1)
loo_R2(psitlp.Model1)
bayes_R2(psitlp.Model1)


loo_fit <- loo(psitlp.Model1, save_psis = T) # We need the psis for later
loo_fit2 <- loo(psitlp.Model2, save_psis = T) # We need the psis for later

loo_compare(loo_fit, loo_fit2)
loo_compare(WAIC(psitlp.Model1), WAIC(psitlp.Model2))
bayes_factor(psitlp.Model1, psitlp.Model2, log=T) # Model 1 fits better
