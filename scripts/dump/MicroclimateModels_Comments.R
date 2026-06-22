# library(brms)
# library(bayesplot)
# library(tidybayes)
library(tidyverse)
library(utils)

# Put all the user define functions into its own .R file to decluter this script
source("extra_functions.R")


## prefer select dplyr
select <- dplyr::select

## Load raw traits
df_traits <- read_csv("data/calculated_parameters/traits_2023_2024.csv") 

## all meteo and fluxes
df_meteo_flux_30min <- read.csv("data/microclimate/meteo_fluxes_2023_2024_30min.csv", header = TRUE) |> 
  mutate(timestamp = timestamp |> as.POSIXct(format = "%Y-%m-%d %H:%M:%S"),
         pet_mm = case_when(pet_mm < 0 ~ 1e-5, TRUE ~ pet_mm),
         et_mm = case_when(et_mm < 0 ~ 1e-5, TRUE ~ et_mm))


df_predictors = data.frame(
  abbr = c("sum_p", "mean_ta", "max_ta", "mean_rh", "mean_vpd", 
           "sum_pet", "sum_et", "sum_p_et", "mean_et_pet", "mean_rew", "sum_ppfd", "mean_gpp", "leaf_age_d"),
  long = c("P[sum]",
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
           "Leaf age [x]"))


### days --------------------------------------------------------------------
# days? as in what?? Purpose?
f.calculate_meteo_summary(df_meteo_flux_30min, as.Date("2023-05-15"), as.Date("2023-05-16"))


# Why are 2 day_seq. What purpose. Which one to use?
#day_seq <- seq(5, 60, 5)
day_seq <- c(1,2,5,10,15,20,25,30,35,40,45,50,55,60)


# Purpose of this chunk? What is df_traits_nest and df_traits_nest_h?
df_traits_nest <- df_traits |> 
  nest(data = -c(year, date, campaign)) 

df_traits_nest <- df_traits_nest |> 
  mutate(meteo = NA,
         meteo = map(meteo, ~ f.calculate_meteo_summary(
           df_meteo_flux_30min,
           df_traits_nest$date[1] - 1,
           df_traits_nest$date[1]
         )
         ))

df_traits_nest_h <- df_traits |> 
  nest(data = -c(year, date, campaign)) 

for(i in 1:nrow(df_traits_nest)) {
  for(j in 1:length(day_seq)){
    df_traits_nest$meteo[[i]][j, ] = f.calculate_meteo_summary(
      df_meteo_flux_30min,
      df_traits_nest$date[i] - day_seq[j],
      df_traits_nest$date[i]
    )
  }
  print(i)
}
#df_traits_nest$meteo[[1]]

df_traitvals <- df_traits_nest |> 
  select(-meteo) |> 
  unnest(data) |> 
  pivot_longer(cols = -c(year, campaign, date, species, sample_id),
               names_to = "trait",
               values_to = "trait_val")
df_traitvals

df_meteovals <- df_traits_nest|> 
  select(date, meteo) |> 
  unnest(meteo) |> 
  pivot_longer(cols = -c(date, timeframe),
               names_to = "meteo_var",
               values_to = "meteo_val")
df_meteovals

df_meteovals |> 
  ggplot() +
  geom_line(aes(x = timeframe, y = meteo_val, col = as.factor(date))) +
  facet_wrap(~ meteo_var, scales = "free_y") 

df_meteo_corr = left_join(df_traitvals, df_meteovals, by = "date")
# Warning message:
# In left_join(df_traitvals, df_meteovals, by = "date") :
#   Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 1 of `x` matches multiple rows in `y`.
# ℹ Row 1 of `y` matches multiple rows in `x`.
# ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
# Is this warning fine? does the resulting data.frame valid?

# Why are you doing SO MANY test? Is there a purpose? Is bonferroni-type correction sued somewhere?
# This seems to me like p-value fishing. Seems completly unnecesary to me
df_meteo_corr_nest <- df_meteo_corr |> 
  nest(data = -c(timeframe, trait, meteo_var)) |> 
  mutate(cor = map(data, ~ cor.test(y = .x$trait_val, x = .x$meteo_val, method = "spearman")),
         corrcoef = map_dbl(cor, ~ .x$estimate),
         pval = map_dbl(cor, ~ .x$p.value),
         timeframe = as.numeric(timeframe))

df_best_meteo_corr <- df_meteo_corr_nest |>
  filter(pval < 0.1) |>
  select(-cor) |> 
  group_by(meteo_var, trait) |> 
  mutate(abs_corr = abs(corrcoef)) |>
  slice_max(abs_corr, n = 1, with_ties = FALSE) 
df_best_meteo_corr

df_meteo_corr_nest$trait |> unique()
df_meteo_corr_nest$meteo_var |> unique()

df_meteo_corr_nest |>
  filter(trait == "gmin") |> 
  ggplot(aes(x = timeframe, y = corrcoef)) + 
  geom_point(aes(col = ifelse(pval < 0.05, "darkgreen", "grey"))) +  
  scale_color_identity() + 
  facet_wrap(~ meteo_var) +
  labs(y = "correlation coefficient",
       x = "cumulative timeframe [days]",
       title = "gmin") 

df_meteo_corr_nest |>
  filter(trait == "psi_midday_mpa", ) |> 
  ggplot(aes(x = timeframe, y = corrcoef)) + 
  geom_point(aes(col = ifelse(pval < 0.05, "darkgreen", "grey"))) +  
  scale_color_identity() + 
  facet_wrap(~ meteo_var) +
  labs(y = "correlation coefficient",
       x = "cumulative timeframe [days]",
       title = "Midday water potential") 
# This 2 plots just tells me that the model should be functional regression model: 
# as model (1.3) in Greven & Scheipl, 2017

df_test <- df_meteo_corr_nest |>
  filter(trait == "psi_midday_mpa", meteo_var == "mean_vpd") |> 
  unnest(data) 

# Change the smoothing regression line from "lm" to "gam", since there was a clear
# non-linear relationship in some of the plots
df_test |> 
  mutate(species = recode(species, "FREX" = "Fraxinus excelsior", "FASY" = "Fagus sylvatica")) |> 
  ggplot(aes(x = meteo_val, y = trait_val, col = species)) +
  geom_smooth(method = "gam", formula= y ~ s(x, bs= "cs", k= 6)) + #method= "lm"
  geom_point() +
  facet_wrap(~timeframe) +
  scale_color_manual(values = c("Fagus sylvatica" = "#410866", "Fraxinus excelsior" = "#D55E00")) +
  labs(y = "Midday water potential [MPa]", x = "mean relative extractable soil water [%]",
       title = "Midday water potential vs. mean relative extractable soil water",
       subtitle = "Cumulative timeframe [days]") 


### hours -------------------------------------------------------------------

# Same issues with the previous chuck of code apply here
(
f.calculate_meteo_summary_h(df_meteo_flux_30min, as.POSIXct("2023-05-15 12:00:00"), as.POSIXct("2023-05-16 12:00:00"))

hour_seq <- c(1, 2, 3, 4, 5, 6, 8, 10, 15, 20, 25, 30, 35, 40)

df_traits_nest_h <- df_traits_nest |> 
  mutate(date = as.POSIXct(date) + hours(12),
         meteo = NA,
         meteo = map(meteo, ~ f.calculate_meteo_summary(
           df_meteo_flux_30min,
           df_traits_nest$date[1] - 1,
           df_traits_nest$date[1]
         )
         ))

df_traits_nest_h$date[[1]] - hours(1)

for(i in 1:nrow(df_traits_nest_h)) {
  for(j in 1:length(hour_seq)){
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
  pivot_longer(cols = -c(year, campaign, date, species, sample_id),
               names_to = "trait",
               values_to = "trait_val")
df_traitvals_h

df_meteovals_h <- df_traits_nest_h |> 
  select(date, meteo) |> 
  unnest(meteo) |> 
  pivot_longer(cols = -c(date, timeframe),
               names_to = "meteo_var",
               values_to = "meteo_val")

df_meteovals_h

df_meteovals_h |> 
  ggplot() +
  geom_line(aes(x = timeframe, y = meteo_val, col = as.factor(date))) +
  facet_wrap(~ meteo_var, scales = "free_y") 

df_meteo_corr_h = left_join(df_traitvals_h, df_meteovals_h, by = "date")

df_meteo_corr_nest_h <- df_meteo_corr_h |> 
  nest(data = -c(timeframe, trait, meteo_var)) |> 
  mutate(cor = map(data, ~ cor.test(y = .x$trait_val, x = .x$meteo_val, method = "spearman")),
         corrcoef = map_dbl(cor, ~ .x$estimate),
         pval = map_dbl(cor, ~ .x$p.value),
         timeframe = as.numeric(timeframe))
df_meteovals

df_best_meteo_corr_h <- df_meteo_corr_nest_h |>
  filter(pval < 0.1) |>
  select(-cor) |> 
  group_by(meteo_var, trait) |> 
  mutate(abs_corr = abs(corrcoef)) |>
  slice_max(abs_corr, n = 1, with_ties = FALSE) 
df_best_meteo_corr_h

df_meteo_corr_nest_h |>
  filter(trait == "psi_midday_mpa") |> 
  ggplot(aes(x = timeframe * 24 , y = corrcoef)) + 
  geom_point(aes(col = ifelse(pval < 0.05, "darkgreen", "grey"))) +  
  scale_color_identity() + 
  facet_wrap(~ meteo_var) +
  labs(y = "correlation coefficient",
       x = "cumulative timeframe [hours]",
       title = "Midday water potential") 

df_meteo_corr_nest |>
  filter(trait == "psi_midday_mpa", ) |> 
  ggplot(aes(x = timeframe, y = corrcoef)) + 
  geom_point(aes(col = ifelse(pval < 0.05, "darkgreen", "grey"))) +  
  scale_color_identity() + 
  facet_wrap(~ meteo_var) +
  labs(y = "correlation coefficient",
       x = "cumulative timeframe [days]",
       title = "Midday water potential") 

df_test <- df_meteo_corr_nest |>
  filter(trait == "psi_midday_mpa", meteo_var == "mean_vpd") |> 
  unnest(data) 

df_test |> 
  mutate(species = recode(species, "FREX" = "Fraxinus excelsior", "FASY" = "Fagus sylvatica")) |> 
  ggplot(aes(x = meteo_val, y = trait_val, col = species)) +
  geom_smooth(method = "loess") +
  geom_point() +
  facet_wrap(~timeframe) +
  scale_color_manual(values = c("Fagus sylvatica" = "#410866", "Fraxinus excelsior" = "#D55E00")) +
  labs(y = "Midday water potential [MPa]", x = "mean relative extractable soil water [%]",
       title = "Midday water potential vs. mean relative extractable soil water",
       subtitle = "Cumulative timeframe [days]") 
)
# All the opinions from the previous chunk are the same. Many corr test wihtout a clear hypothesis, etc


## 2.2 single models -------------------------------------------------------

alpha <- 0.05
z.value <- qnorm(1 - alpha / 2)  # 1.96 for 95% CI

recode = dplyr::recode

### 2.2.1 midday water potentials ---------------------------------------------

df_psi_md_meteo <- df_best_meteo_corr |> 
  ungroup() |> 
  filter(trait == "psi_midday_mpa") |> 
  arrange(desc(abs_corr)) |> 
  unnest(data) |> 
  mutate(meteo_predictor = paste(meteo_var, abs(timeframe), sep = "_"),
         pheno_start_doy = case_when(species == "FASY" & year == 2023 ~ 121,
                                     species == "FASY" & year == 2024 ~ 104,
                                     species == "FREX" & year == 2023 ~ 140,
                                     species == "FREX" & year == 2024 ~ 138),
         doy = yday(date),
         leaf_age_d = doy - pheno_start_doy) |> 
  select(-c(timeframe, meteo_var, corrcoef, pval, abs_corr, pheno_start_doy)) |>
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val) |> 
  na.omit()
df_psi_md_meteo |> head()

df_psi_md_meteo_scaled <- df_psi_md_meteo |> 
  select(c(trait, date, year, species, sample_id, campaign, trait_val, leaf_age_d)) |> 
  cbind(df_psi_md_meteo |> 
          select(-c(trait, date, year, species, sample_id, campaign, trait_val, leaf_age_d)) |>
          scale())

#f.make_formula_char(df_psi_md_meteo) |> utils::writeClipboard() # Windows function ONLY
model1 <- f.make_formula_char(df_psi_md_meteo)

# The following commented regression model doesnt work since sum_p_et_50 doesnt exist
# lm_psi_md <- lm(
#   trait_val ~ leaf_age_d + sum_p_10 + sum_p_et_50 + mean_rew_1 + max_ta_50 + sum_et_35 + mean_et_pet_1 + mean_ta_60,
#   data = df_psi_md_meteo_scaled,
#   na.action = "na.fail")
lm_psi_md <- lm(trait_val ~ leaf_age_d + sum_p_10 + sum_p_et_60 + mean_rew_1 + max_ta_50 + sum_et_35 + mean_ta_60,
  data = df_psi_md_meteo_scaled)
lm_psi_md |> summary()
lm(model1, df_psi_md_meteo_scaled)

# stepwise model selection is BAD. See eg. https://statmodeling.stat.columbia.edu/2014/06/02/hate-stepwise-regression/
MASS::stepAIC(lm_psi_md, direction = "both") |> summary()
MASS::stepAIC(lme_psi_md, direction = "backward") |> formula()
MuMIn::dredge(lme_psi_md, rank = "AICc") |> head()

require(nlme)

lme_psi_md <- lme(trait_val ~ mean_rew_1 + max_ta_50 + sum_et_35 + sum_p_et_60,
                  random = ~ 1 | species/sample_id,
                  data = df_psi_md_meteo_scaled,
                  control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 200),
                  method = "REML" # Maxmimum Likelihood (ML) method for LMM is not recommended. REML is preferable
)
require(performance)
lme_psi_md |> check_collinearity()
lme_psi_md |> summary()
lme_psi_md |> r2()
lme_psi_md |> check_model()

# This model checks suggest this model, lme_psi_md, is OK but doesnt mean it actually answer the original question, since there are so much double-dipping i.e. using the same data for model selection and stat inference. Bad practice

###


# Since the meteorological variables are measured continuously (eg P, TA, VPD, etc) it makes more sense to use a model that incorporate that into the model.
# For eg. something like this
library(FDboost) #see scalar-on-function vignette

FDboost(trait_val ~ bsignal(sum_p, inS = "linear", knots = 40, df=4) #'knots' and 'df' depends on the covariate and needs to be selected carefully
        + bsignal(mean_ta) + bsignal(mean_rew) + ....,
        timeformula = ~bols(1),
        data= df_meteo_flux_30min)

# That way there is no need to preprocess the data by taking correlation, averaging, max, etc..
# I always like to deal with the data as its measured (within reason), and change the model to meet the structure of the data, and not change the data measure in order to meet the structure of a particular model.


# As stated before, setpwise variable selection is really bad and not recommended at all
# Many different variants for variable selection are out-there. But the gold standard is the LASSO and Ridge, and a little less Elastic Net.

# FOr the code collaboration, here are some "best" practices recommended (at least the one I know): DOI:10.32942/X26S6P , https://bes-guide.github.io/reproducible-code/index.html , 
# And the importance of adding code comments from the start: https://doi.org/10.1111/jeb.14230

# A quote that should be taken to heart: “Programs must be written for people to read, and only incidentally for machines to execute.” 
# I.e. write code that's readable from the start and be central point

# I think the Git repo should REALLY be redone, cleaned up, and reduced in the scope. Too much is put there making it really hard to understand what does what. In the BES guide there is a good template of how to organize the scripts, data, etc






