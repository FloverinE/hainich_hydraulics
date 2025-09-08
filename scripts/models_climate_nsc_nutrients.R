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
library(MuMIn)
  
  
  ## prefer select dplyr
  select <- dplyr::select
}

source("scripts/ggplot_themes.R")

# set locale to US
Sys.setlocale("LC_TIME", "en_US.UTF-8")

plot_position_offset = 5
plot_alpha = 0.5

# 1. define functions ---------------------------------------------------

f.reletter_cld = function(cld_column) {
  cld_column = cld_column |> 
    str_remove_all("\\s")  
  
  cld_letters = cld_column |> 
    paste(collapse = "")  |> 
    strsplit("") |> 
    unlist() |> 
    unique()
  
  reletter_mat = data.frame(
    old = cld_letters, 
    proxy = 1:length(cld_letters) |> as.character(),
    new = letters[1:length(cld_letters)]
  )
  reletter_mat
  
  for(i in 1:nrow(reletter_mat)){
    cld_column = cld_column |> 
      str_replace_all(reletter_mat$old[i], reletter_mat$proxy[i])
  }
  for(i in 1:nrow(reletter_mat)){
    cld_column = cld_column |> 
      str_replace_all(reletter_mat$proxy[i], reletter_mat$new[i])
  }
  cld_column |> map(~ .x |> 
                      str_split("") |> 
                      unlist() |> 
                      sort() |> 
                      paste0(collapse = "")
  ) |> 
    unlist()
}



## traits
df_traits <- read_csv("data/calculated_parameters/traits_2023_2024.csv") 

## all meteo and fluxes
df_meteo_flux_30min <- read.csv("data/microclimate/meteo_fluxes_2023_2024_30min.csv", header = TRUE) |> 
  mutate(timestamp = timestamp |> as.POSIXct(format = "%Y-%m-%d %H:%M:%S"),
         pet_mm = case_when(pet_mm < 0 ~ 1e-5, TRUE ~ pet_mm),
         et_mm = case_when(et_mm < 0 ~ 1e-5, TRUE ~ et_mm))


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
      sum_et  = sum(et_mm, na.rm = TRUE),
      sum_p_et = sum(p_1m - et_mm, na.rm = TRUE),
      mean_et_pet = mean(et_mm / pet_mm, na.rm = TRUE),
      mean_rew = mean(rew, na.rm = TRUE) * 100,
      sum_ppfd = sum(ppfd_in_44m, na.rm = TRUE),
      # mean_gpp = mean(gpp_f, na.rm = TRUE),
      ## add net Radiation / PPFD, GPP, Rn lambdaP-1
      timeframe = start_date - end_date) |> 
    ungroup() 
}

f.make_formula = function(data) {
  paste("trait_val", "~", 
        paste(names(data)[9:ncol(data)], collapse = " + ")) |> 
    as.formula()
}

f.make_formula_char = function(data) {
  paste("trait_val", "~", 
        paste(names(data)[9:ncol(data)], collapse = " + ")) |> 
    as.character()
}

f.make_formula_dredge <- function(dredge_model_line, write_clip = F) {
  ## transpose to get rid of NA predictors (not chosen in dredge selection)
  predictors = dredge_model_line  |> as.data.frame() |> t() |> na.omit() |>
    ## transpose back
    t() |> as.data.frame() |> names()
  ## get column names of predictors
  predictors = predictors[!predictors %in%
                            c("(Intercept)", "df", "logLik", "AICc", "delta", "weight")]
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

match <- str_match(test, "(.*?)\\[(.*?)\\]") # Extract "PET" and "sum"

# Function to format labels
format_label <- function(p) {
  base_name <- extract_base_name(p)  # Remove number suffix
  days <- str_extract(p, "\\d+$")    # Extract the number
  long_name <- predictor_labels[base_name] # Get full name
  
  if (!is.na(long_name)) {
    # Extract variable name and statistic type (if available)
    match <- str_match(long_name, "(.*?)\\[(.*?)\\]") # Extract "PET" and "sum"
    var_name <- match[, 2]  # First part (e.g., "PET")
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
    } 
    else {
      return(paste0(var_name, "\n(", stat_type, ": ", days, " d)"))
    }
  } else {
    return(p)  # Keep original if no match
  }
}

# Function to format labels in export tables
format_label_export <- function(p) {
  base_name <- extract_base_name(p)  # Remove number suffix
  days <- str_extract(p, "\\d+$")    # Extract the number
  long_name <- predictor_labels[base_name] # Get full name
  
  if (!is.na(long_name)) {
    # Extract variable name and statistic type (if available)
    match <- str_match(long_name, "(.*?)\\[(.*?)\\]") # Extract "PET" and "sum"
    var_name <- match[, 2]  # First part (e.g., "PET")
    stat_type <- match[, 3] # Second part (e.g., "sum")
    
    ## for leaf age, type is placeholder "x"
    if (stat_type == "x") {
      return(var_name)
    } else {
      return(paste0(var_name, " (", stat_type, ": ", days, " d)"))
    }
  } else {
    return(p)  # Keep original if no match
  }
}

## for plotting the effect sizes with nice predictor names
df_predictors = data.frame(
  abbr = c("sum_p", "mean_ta", "max_ta", "mean_rh", "mean_vpd", 
           "sum_pet", "sum_et", "sum_p_et", "mean_et_pet", "mean_rew", 
           "sum_ppfd", "mean_gpp", "leaf_age_d", "starch_sugar_ratio", "k_mg_kg",
           "total_sugars_mg_g",
           "p_mg_kg",
           "s_mg_kg",
           "n",
           "glucose_starch_1",
           "sucrose_mg_g",
           "ca_mg_kg",
           "leaf_age_scale"
           ),
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
           ))

# 2. aggregate all traits into one df ----------------------------------------


### days --------------------------------------------------------------------

## 
f.calculate_meteo_summary(df_meteo_flux_30min, as.Date("2023-05-15"), as.Date("2023-05-16"))

## define all possible intervals to be tested
day_seq <- c(1,2,5,10,15,20,25,30,35,40,45,50,55,60)

df_traits_nest <- df_traits |> 
  nest(data = -c(year, date, campaign, species)) 

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
df_traits_nest$meteo[[1]]

df_traitvals <- df_traits_nest |> 
  select(-meteo) |> 
  unnest(data) |> 
  pivot_longer(cols = -c(year, campaign, date, species, sample_id),
               names_to = "trait",
               values_to = "trait_val")
df_traitvals

df_meteovals <- df_traits_nest|> 
  select(date, meteo, species) |> 
  unnest(meteo) |> 
  pivot_longer(cols = -c(date, timeframe, species),
               names_to = "meteo_var",
               values_to = "meteo_val")

df_meteovals

df_meteovals |> 
  filter(species == "FREX") |> 
  ggplot() +
  geom_line(aes(x = timeframe, y = meteo_val, col = as.factor(date))) +
  facet_wrap(~ meteo_var, scales = "free_y") +
  thesis_theme

df_meteo_corr = left_join(df_traitvals, df_meteovals, by = c("date", "species"))

df_meteo_corr_nest <- df_meteo_corr |> 
  nest(data = -c(species, timeframe, trait, meteo_var)) |> 
  mutate(cor = map(data, ~ cor.test(y = .x$trait_val, x = .x$meteo_val, method = "spearman")),
         corrcoef = map_dbl(cor, ~ .x$estimate),
         pval = map_dbl(cor, ~ .x$p.value),
         timeframe = as.numeric(timeframe))

df_best_meteo_corr <- df_meteo_corr_nest |>
  filter(pval < 0.1) |>
  select(-cor) |> 
  group_by(species, meteo_var, trait) |> 
  mutate(abs_corr = abs(corrcoef)) |>
  slice_max(abs_corr, n = 1, with_ties = FALSE) 
df_best_meteo_corr

df_meteo_corr_nest$trait |> unique()
df_meteo_corr_nest$meteo_var |> unique()

## check correlations over timeframes with plots
df_meteo_corr_nest |>
  filter(trait == "psi_midday_mpa") |> 
  ggplot(aes(x = timeframe, y = corrcoef, shape = species)) + 
  geom_point(aes(col = ifelse(pval < 0.05, "darkgreen", "grey"))) +  
  scale_color_identity() + 
  facet_wrap(~ meteo_var) +
  labs(y = "correlation coefficient",
       x = "cumulative timeframe [days]",
       title = "Midday Water Potential") +
  thesis_theme

df_meteo_corr_nest |>
  filter(trait == "psi_tlp") |> 
  ggplot(aes(x = timeframe, y = corrcoef, shape = species)) + 
  geom_point(aes(col = ifelse(pval < 0.05, "darkgreen", "grey"))) +  
  scale_color_identity() + 
  facet_wrap(~ meteo_var) +
  labs(y = "correlation coefficient",
       x = "cumulative timeframe [days]",
       title = "Turgor loss point") +
  thesis_theme

df_meteo_corr_nest |>
  filter(trait == "gmin") |> 
  ggplot(aes(x = timeframe, y = corrcoef, shape = species)) + 
  geom_point(aes(col = ifelse(pval < 0.05, "darkgreen", "grey"))) +  
  scale_color_identity() + 
  facet_wrap(~ meteo_var) +
  labs(y = "correlation coefficient",
       x = "cumulative timeframe [days]",
       title = "Gmin") +
  thesis_theme

df_meteo_corr_nest |>
  filter(trait == "psi_midday_mpa", ) |> 
  ggplot(aes(x = timeframe, y = corrcoef)) + 
  geom_point(aes(col = ifelse(pval < 0.05, "darkgreen", "grey"))) +  
  scale_color_identity() + 
  facet_wrap(~ meteo_var) +
  labs(y = "correlation coefficient",
       x = "cumulative timeframe [days]",
       title = "Midday water potential") +
  thesis_theme

df_test <- df_meteo_corr_nest |>
  filter(trait == "psi_midday_mpa", meteo_var == "mean_vpd") |> 
  unnest(data) 

df_test |> 
  mutate(species = recode(species, "FREX" = "Fraxinus excelsior", "FASY" = "Fagus sylvatica")) |> 
  ggplot(aes(x = meteo_val, y = trait_val, col = species)) +
  geom_smooth(method = "lm") +
  geom_point() +
  facet_wrap(~timeframe) +
  scale_color_manual(values = c("Fagus sylvatica" = "#410866", "Fraxinus excelsior" = "#D55E00")) +
  labs(y = "Midday water potential [MPa]", x = "mean relative extractable soil water [%]",
       title = "Midday water potential vs. mean relative extractable soil water",
       subtitle = "Cumulative timeframe [days]") +
  thesis_theme 

# 3. Microclimate X NSC X Nutrients ---------------------------------------

df_hydr_traits <- read.csv("data/calculated_parameters/df_hydr_traits.csv")

df_hydr_traits <- df_hydr_traits |> 
  select(-c(mean_psi_bar, gmin_max, gmin_min, gmin_mean)) |> 
  pivot_wider(names_from = sugar_name, values_from = sugar_conc) %>% 
  mutate(starch_sugar_ratio = starch_mg_g / (fructose_mg_g + glucose_mg_g + sucrose_mg_g)) %>% 
  pivot_longer(cols = fructose_mg_g:starch_sugar_ratio, names_to = "sugar_name", values_to = "sugar_conc") %>% 
  pivot_longer(cols = -c("date", "species", "sample_id", "campaign", "date_fac", "nutrient_name", "nutrient_conc",
                         "year", "vessel_order", "sugar_name", "sugar_conc", 
                         "doy", "leaf_age_d", "pheno_start_doy"), 
               names_to = "trait", values_to = "value") |> 
  mutate(date_plot = date |> str_replace("2023|2024", "2000") |> as.Date(),
         year = as.factor(year))

## NSC & nutrient PCA
library(FactoMineR)

nutrient_names = df_hydr_traits$nutrient_name |> unique()
sugar_names = df_hydr_traits$sugar_name |> unique()

df_hydr_traits_pca <- df_hydr_traits |> 
  ungroup() |> 
  pivot_wider(names_from = nutrient_name, values_from = nutrient_conc) |> 
  pivot_wider(names_from = sugar_name, values_from = sugar_conc) |>
  pivot_longer(cols = c(nutrient_names, sugar_names), names_to = "sugar_nutrient_name",
               values_to = "sugar_nutrient_conc") |>
  mutate(sugar_nutrient_name = sugar_nutrient_name |> str_remove_all("_con"),
         sugar_nutrient_name = case_when(sugar_nutrient_name == "c" ~ "c_perc",
                                         sugar_nutrient_name == "n" ~ "n_perc",
                                   T ~ sugar_nutrient_name)) |> 
  group_by(year, campaign, date, species, sample_id, sugar_nutrient_name) |> 
  summarise(sugar_nutrient_conc = sugar_nutrient_conc |> as.numeric() |> mean(na.rm = T))
  
df_hydr_traits_pca_rel <- df_hydr_traits_pca |> 
  ungroup() |> 
  # select(c(sugar_nutrient_name, sugar_nutrient_conc)) |>
  pivot_wider(names_from = sugar_nutrient_name, 
              values_from = sugar_nutrient_conc)  |> 
  select(-c(year, campaign, date, species, sample_id))
  
pca_sugar_nutrient <- PCA(df_hydr_traits_pca_rel, scale.unit = T, graph = T, ncp = 5)
pca_sugar_nutrient$ind$coord
pca_sugar_nutrient$eig

df_hydr_traits_pca_res <- df_hydr_traits_pca |> 
  ungroup() |> 
  # select(c(sugar_nutrient_name, sugar_nutrient_conc)) |>
  pivot_wider(names_from = sugar_nutrient_name, 
              values_from = sugar_nutrient_conc) |> 
  cbind(pca_sugar_nutrient$ind$coord) |> 
  mutate(campaign = campaign |> as.factor())

df_hydr_traits_pca_res |> 
  ggplot() +
   geom_point(aes(x = Dim.1, y= Dim.2, col = campaign, shape = species)) +
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
  geom_smooth(aes(x = dim_value, y = gmin,
                  col = year, group = year
                  ), method = "lm") +
  facet_wrap(species~dim, scales = "free") +
  theme_bw() 


## 3.2 join microclimate --------------------------------------------------------

df_best_meteo_corr_all <- df_best_meteo_corr |> 
  filter(!trait %in% c("doy", "mean_psi_bar", "pheno_start_doy", "leaf_age_d")) |> 
  ungroup() |> 
  ## instead of filtering by single trait, now group by!
  # filter(trait == "psi_midday_mpa") |> 
  group_by(trait) |> 
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
  # pivot_wider(names_from = meteo_predictor, values_from = meteo_val) |> 
  mutate(year = year |> as.factor(),
         date = date |> as.character()) |> 
  distinct()

## join with traits dataframe containing NSC and nutrients
df_hydr_traits_best_meteo <- df_hydr_traits |> 
  select(-c(doy, pheno_start_doy, leaf_age_d, campaign)) |> 
  distinct() |> 
  left_join(df_best_meteo_corr_all %>% 
              mutate(trait = trait %>% 
                       str_replace_all("elasticity_tot", "elast_tot") %>% 
                       str_replace_all("elasticity_sym", "elast_sym")
                       )
              , 
            by = c("year", "date", "species", "sample_id", "trait"))

df_hydr_traits_best_meteo_scaled <- df_hydr_traits_best_meteo |>
  mutate(nutrient_name  = nutrient_name |> str_remove("\\_con")) |> 
  ## grouped by species as meteo variables are unique by species
  group_by(species, meteo_predictor)  |> 
  mutate(meteo_val_scaled = scale(meteo_val)) |> 
  ungroup() |> 
  group_by(species, sugar_name)  |> 
  mutate(sugar_conc_scaled = scale(sugar_conc)) |> 
  ungroup() |> 
  group_by(species, nutrient_name)  |> 
  mutate(nutrient_conc_scaled = scale(nutrient_conc)) |> 
  ungroup()

## 3.3 model ---------------------------------------------------------------

### 3.3.1 psi_tlp  -----------------------------------------------------------

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
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val_scaled)  %>% 
  mutate(leaf_age_scale = scale(leaf_age_d))
  
names(df_psi_tlp_frex_wide) = names(df_psi_tlp_frex_wide) |> 
  str_remove_all("\\[\\, 1\\]")

## get all predictors for massive model
predictor_names <- df_psi_tlp_frex |> 
  filter(!sugar_name |> str_detect("ppm"),
         !is.na(nutrient_conc_scaled), ## excluding elements that were only measured in 1 year
         !nutrient_name %in% c("fe_mg_kg",
                               "mn_mg_kg",
                               "zn_mg_kg",
                               # "na_mg_kg",
                               "al_mg_kg"
                               )) |> 
  select(c(sugar_name, nutrient_name, meteo_predictor)) |> 
  as.vector() |> unlist() |> 
  unique() 

## copy predictors 
str_c(predictor_names, collapse = " + ")  |> 
  paste()
  
mod.all_psi_tlp_frex <- lm(
  value ~ 
    leaf_age_d + 
    ## insert copied predictor names
    fructose_mg_g + glucose_mg_g + glucose_starch_1 + starch_mg_g + sucrose_mg_g + sugar_weight_mg + total_sugars_mg_g + starch_sugar_ratio + c + ca_mg_kg + k_mg_kg + mg_mg_kg + n + p_mg_kg + s_mg_kg + mean_ta_55 + max_ta_55 + mean_rew_60 + mean_vpd_45 + sum_et_50 + sum_ppfd_5 + sum_pet_50 + mean_rh_2
  ,
  data = df_psi_tlp_frex_wide,
  na.action = "na.fail"
)

mod.all_psi_tlp_frex |>  summary()

## reduce model 

mod.red_psi_tlp_frex <- mod.all_psi_tlp_frex |> MASS::stepAIC(direction = "both") 
mod.red_psi_tlp_frex |> summary()

## dredging tests all possible predictor combinations up to 7 
mod.dredge_psi_tlp_frex <- mod.red_psi_tlp_frex |> MuMIn::dredge(rank = "AICc", m.lim = c(1,7))

## model averaging takes the top models up to an AIC difference of 2 from the best model
## we then average the coefficient estimate per predictor on these models
## one can also use a weight subset instead of deltaAIC, I think that is a relative metric, 
## i.e. the top 
mod.avg <- model.avg(mod.dredge_psi_tlp_frex, delta < 2)
mod.avg %>% summary()

f.make_formula_dredge(mod.dredge_psi_tlp_frex[1,])

mod.dredged_psi_tlp_frex <- lm(
  value ~
    fructose_mg_g + glucose_mg_g + mean_ta_55 + p_mg_kg + s_mg_kg + sucrose_mg_g + total_sugars_mg_g
  ,
  data = df_psi_tlp_frex_wide,
  na.action = "na.fail"
)

df_psi_tlp_frex_wide |> 
  ggplot() +
  geom_point(aes(x = leaf_age_d, y = value)) +
  geom_line(aes(x = leaf_age_d, y = predict(mod.all_psi_tlp_frex), col = "all predictors", group = sample_id)) +
  geom_line(aes(x = leaf_age_d, y = predict(mod.red_psi_tlp_frex), 
                group = sample_id, col = "reduced with StepAIC")) +
  geom_line(aes(x = leaf_age_d, y = predict(mod.dredged_psi_tlp_frex), 
                group = sample_id, col = "reduced with Dredge")) +
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

mod.lme_dredge_psi_tlp_frex <- mod.lme_psi_tlp_frex |> MuMIn::dredge(rank = "AICc", m.lim = c(1,7))

mod.avg <- model.avg(mod.lme_dredge_psi_tlp_frex, delta < 2)
mod.avg %>% summary()

mod.avg %>% f.make_formula_dredge()

## I used leaf age scale as an extra variable as for plotting, I need the unscaled "leaf_age_d" for the xaxis
mod.lme_dredged_psi_tlp_frex <- nlme::lme(
  value ~  leaf_age_scale + max_ta_55 + p_mg_kg + s_mg_kg + total_sugars_mg_g,
  random = ~ 1 | sample_id,
  data = df_psi_tlp_frex_wide,
  # correlation = corAR(form = ~ date | sample_id),
  control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 200, tolerance = 1e-4),
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
  sjPlot::plot_model(mod.lme_dredged_psi_tlp_frex,
                     show.values = TRUE, show.p = T, value.offset = 0.3, line.size = 0.5, dot.size = 1.3) +
  geom_hline(aes(yintercept = 0), alpha= 0.2)+
  scale_x_discrete(labels = new_labels) + 
  labs(x = "", y = "Scaled coefficient estimates", 
       title = "Effect sizes",
       # subtitle = "Fraxinus excelsior"
  ) +  thesis_theme +
  # lims(x = c(-0.05, 0.3)) +
  theme(axis.text = element_text(size = 15),
        title = element_text(size = 15),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.margin = margin(t = 0, r = 10, b = 0, l = 0)  # add margin on the right
  )
plot.psi_tlp_frex_coefs.png
ggsave("figures/poster/plot.psi_tlp_frex_coefs.png", width = 10, height = 10, units = "cm")


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
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val_scaled)  %>% 
  mutate(leaf_age_scale = scale(leaf_age_d))

names(df_psi_tlp_fasy_wide) = names(df_psi_tlp_fasy_wide) |> 
  str_remove_all("\\[\\, 1\\]")

## get all predictors for massive model
predictor_names <- df_psi_tlp_fasy |> 
  filter(!sugar_name |> str_detect("ppm"),
         !is.na(nutrient_conc_scaled),
         !nutrient_name %in% c("fe_mg_kg",
                               "mn_mg_kg",
                               "zn_mg_kg",
                               "al_mg_kg",
                               "na_mg_kg")) |> 
  select(c(sugar_name, nutrient_name, meteo_predictor)) |> 
  as.vector() |> unlist() |> 
  unique() 

## copy predictors 
str_c(predictor_names, collapse = " + ")  |> 
  paste()

mod.all_psi_tlp_fasy <- lm(
  value ~ 
    leaf_age_d + 
    ## insert copied predictor names
    fructose_mg_g + glucose_mg_g + glucose_starch_1 + starch_mg_g + sucrose_mg_g + sugar_weight_mg + total_sugars_mg_g + starch_sugar_ratio + c + ca_mg_kg + k_mg_kg + mg_mg_kg + n + p_mg_kg + s_mg_kg + mean_rh_40 + mean_et_pet_45 + mean_vpd_30 + sum_pet_40 + mean_rew_15 + mean_ta_10 + sum_p_60 + sum_p_et_45 + max_ta_35 + sum_et_1 + sum_ppfd_40
  ,
  data = df_psi_tlp_fasy_wide,
  na.action = "na.fail"
)

mod.all_psi_tlp_fasy |>  summary()

## reduce model 

mod.red_psi_tlp_fasy <- mod.all_psi_tlp_fasy |> MASS::stepAIC(direction = "both") 
mod.red_psi_tlp_fasy |> summary()

mod.dredge_psi_tlp_fasy <- mod.red_psi_tlp_fasy |> MuMIn::dredge(rank = "AICc")

mod.dredge_psi_tlp_fasy[1,] %>% f.make_formula_dredge() %>% as.formula()

mod.dredged_psi_tlp_fasy <- lm(
  value ~ k_mg_kg + leaf_age_d + mean_rh_40 + mean_ta_10 + mean_vpd_30 + 
    sum_pet_40,
  data = df_psi_tlp_fasy_wide,
  na.action = "na.fail"
)

df_psi_tlp_fasy_wide |> 
  ggplot() +
  geom_point(aes(x = leaf_age_d, y = value)) +
  geom_line(aes(x = leaf_age_d, y = predict(mod.all_psi_tlp_fasy), col = "all predictors", group = sample_id)) +
  geom_line(aes(x = leaf_age_d, y = predict(mod.red_psi_tlp_fasy), 
                group = sample_id, col = "reduced with StepAIC")) +
  geom_line(aes(x = leaf_age_d, y = predict(mod.dredged_psi_tlp_fasy), 
                group = sample_id, col = "reduced with Dredge")) +
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
  value ~ leaf_age_d + fructose_mg_g + glucose_mg_g + starch_mg_g + 
    sucrose_mg_g + total_sugars_mg_g + starch_sugar_ratio + k_mg_kg + 
    mg_mg_kg + n + mean_et_pet_45 + mean_vpd_30 + 
    sum_pet_40 + mean_ta_10,
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

mod.lme_dredge_psi_tlp_fasy <- mod.lme_red_psi_tlp_fasy |> MuMIn::dredge(rank = "AICc", m.lim = c(1,7))

mod.lme_dredge_psi_tlp_fasy[1, ] %>% f.make_formula_dredge() %>% as.formula() 

mod.lme_dredged_psi_tlp_fasy <- nlme::lme(
  value ~ k_mg_kg + leaf_age_d + sum_pet_40 + mean_et_pet_45 + mean_ta_10 + 
    mean_vpd_30 + starch_sugar_ratio 
  ,
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
  value ~ leaf_age_scale + sum_pet_40 + mean_et_pet_45 + mean_ta_10 + k_mg_kg + starch_sugar_ratio 
  ,
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
  sjPlot::plot_model(mod.lme_final_psi_tlp_fasy,
                     show.values = TRUE, show.p = T, value.offset = 0.3, line.size = 0.5, dot.size = 1.3) +
  geom_hline(aes(yintercept = 0), alpha= 0.2)+
  scale_x_discrete(labels = new_labels) + 
  labs(x = "", y = "Scaled coefficient estimates", 
       title = "Effect sizes",
       # subtitle = "Fraxinus excelsior"
  ) +  thesis_theme +
  # lims(x = c(-0.05, 0.3)) +
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 15),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.margin = margin(t = 0, r = 10, b = 0, l = 0)  # add margin on the right
  )
plot.psi_tlp_fasy_coefs.png
ggsave("figures/poster/psi_tlp_fasy_effect_sizes.png", width = 10, height = 10, units = "cm")


#### Plot for both species ---------------------------------------------------

# Extract R² (marginal) and format with 2 decimals
r2_fasy <- sprintf("%.2f", r2(mod.lme_final_psi_tlp_fasy)$R2_marginal)
r2_frex <- sprintf("%.2f", r2(mod.lme_dredged_psi_tlp_frex)$R2_marginal)

# Custom labels using plotmath (italic species, plain R²)
species_labels <- c(
  bquote(italic("Fagus sylvatica") ~ "(R"^2*" = " ~ .(r2_fasy) * ")"),
  bquote(italic("Fraxinus excelsior") ~ "(R"^2*" = " ~ .(r2_frex) * ")")
)

ggplot() +
  geom_point(data = df_psi_tlp_fasy_wide, 
             aes(x = leaf_age_d, y = value, shape = "Fagus sylvatica"), 
             col = "#dc782a") +
  geom_point(data = df_psi_tlp_frex_wide, 
             aes(x = leaf_age_d, y = value, shape = "Fraxinus excelsior"), 
             col = "#56b4e9") +
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
  facet_wrap(~ year) +
  labs(y = "Osm. potential at turgor loss [MPa]", 
       x = "Leaf age (days since leaf out)") +
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
  theme(strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

ggsave("figures/poster/psi_tlp_model.png", height = 11.5, width = 22, units = "cm")


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
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val_scaled)  %>% 
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
  unique() %>% na.omit()

## copy predictors 
paste(predictor_names, collapse = " + ") %>% cat()

## lm

mod.all_elast_frex <- lm(
  value ~ leaf_age_d +
    fructose_mg_g + glucose_mg_g + glucose_starch_1 + starch_mg_g + sucrose_mg_g + total_sugars_mg_g + starch_sugar_ratio + c + ca_mg_kg + k_mg_kg + mg_mg_kg + n + p_mg_kg + s_mg_kg + mean_et_pet_1 + mean_rh_25 + mean_vpd_30 + sum_pet_40  
  ,
  data = df_elast_frex_wide,
  na.action = "na.fail"
)

## reduce model 

mod.red_elast_frex <- mod.all_elast_frex |> MASS::stepAIC(direction = "both") 
mod.red_elast_frex |> summary()
mod.red_elast_frex |> formula() 

mod.dredge_elast_frex <- mod.red_elast_frex |> MuMIn::dredge(rank = "AICc", m.lim = c(1,7))

mod.dredge_elast_frex[1, ] %>% f.make_formula_dredge()

mod.lme_elast_frex <- glmmTMB::glmmTMB(
  value ~ leaf_age_d + 
    ## paste
    leaf_age_d + mean_vpd_30 
    ## end paste
  + (1 | sample_id),
  data = df_elast_frex_wide,
  family = Gamma(link = "log"),
  na.action = "na.fail"
)

mod.lme_elast_frex |> summary()
mod.lme_elast_frex |> r2()
mod.lme_elast_frex |> check_model()

mod.lme_dredge_elast_frex <- mod.lme_elast_frex |> MuMIn::dredge(rank = "AICc", m.lim = c(1,7))

mod.lme_dredge_elast_frex[1, ] %>% f.make_formula_dredge() %>% cat()

mod.lme_dredged_elast_frex <- glmmTMB::glmmTMB(
  value ~ leaf_age_d + 
    ## paste
    mean_vpd_30 
  ## end paste
  + (1 | sample_id),
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
      mean_vpd_30 
    ## end paste
    ,
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
    lower = coefs[, 1] - 1.96 * coefs[, 2],  # Confidence interval
    upper = coefs[, 1] + 1.96 * coefs[, 2],
    est_label = round(estimate, 2),
    p = case_when(coefs[, 4] >= 0.05 ~ "",
                  coefs[, 4] < 0.05 & coefs[, 4] >= 0.01 ~ "*",
                  coefs[, 4] < 0.01 & coefs[, 4] >= 0.001 ~ "**", 
                  coefs[, 4] < 0.001 ~ "***"),
    labels = paste(est_label, p, sep = " ")
  )

plot.elast_frex_coefs.png <- coef_df |> 
  filter(term != "(Intercept)") |>
  mutate(
    coef_color = factor(case_when(estimate > 0 ~ "blue", TRUE ~ "red")),  # Use labels instead of hex
    term = fct_rev(factor(term))
  ) |> 
  ggplot(aes(y = term, x = estimate, color = coef_color)) +
  geom_point(size = 1.5, show.legend = F) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0, show.legend = F) +
  geom_text(aes(label = labels), vjust = -1, size = 4, show.legend = F) +
  # annotate("text", x = -0.45, y = 4.4, label = "b)") +
  scale_y_discrete(labels = new_labels) + 
  # scale_x_continuous(limits = c(0, 2)) +
  scale_color_manual(values = c("blue" = "#377eb8", "red" = "#e41a1c")) +  # Manually define colors
  labs(y = "", x = "Scaled coefficient estimates", 
       # title = "Effect sizes",
       # subtitle = "Fraxinus excelsior"
  ) +  thesis_theme +
  # lims(x = c(-0.05, 0.3)) +
  theme(axis.text = element_text(size = 15),
        title = element_text(size = 15),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.margin = margin(t = 0, r = 10, b = 0, l = 0)  # add margin on the right
  )
plot.elast_frex_coefs.png
ggsave("figures/poster/plot.elast_frex_coefs.png", width = 10, height = 10, units = "cm")

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
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val_scaled)  %>% 
  mutate(leaf_age_scale = scale(leaf_age_d))

names(df_elast_fasy_wide) = names(df_elast_fasy_wide) |> 
  str_remove_all("\\[\\, 1\\]")

## get all predictors for massive model
predictor_names <- df_elast_fasy |> 
  filter(
    !sugar_name |> str_detect("ppm"),
    !is.na(nutrient_conc_scaled),
    !nutrient_name %in% c("fe_mg_kg", "mn_mg_kg", "zn_mg_kg", "al_mg_kg", "na_mg_kg"),
    !sugar_name %in% c("sugar_weight_mg") 
  ) |> 
  select(sugar_name, nutrient_name, meteo_predictor) |> 
  unlist(use.names = FALSE) |> 
  unique() %>% na.omit()

## copy predictors 
paste(predictor_names, collapse = " + ") 

## lm

mod.all_elast_fasy <- lm(
  value ~ leaf_age_d +
fructose_mg_g + glucose_mg_g + glucose_starch_1 + starch_mg_g + sucrose_mg_g + total_sugars_mg_g + starch_sugar_ratio + c + ca_mg_kg + k_mg_kg + mg_mg_kg + n + p_mg_kg + s_mg_kg + mean_rh_45 + mean_et_pet_45 + mean_vpd_35 + mean_rew_60 + sum_et_1 + sum_pet_55 + sum_p_et_2 + sum_ppfd_5 + max_ta_35 + sum_p_2 + mean_ta_10  
  ,
  data = df_elast_fasy_wide,
  na.action = "na.fail"
)

## reduce model 

mod.red_elast_fasy <- mod.all_elast_fasy |> MASS::stepAIC(direction = "both") 
mod.red_elast_fasy |> summary()
mod.red_elast_fasy |> formula() 

mod.dredge_elast_fasy <- mod.red_elast_fasy |> MuMIn::dredge(rank = "AICc", m.lim = c(1,7))

mod.dredge_elast_fasy[1, ] %>% f.make_formula_dredge()

mod.lme_elast_fasy <- glmmTMB::glmmTMB(
  value ~ 
    ## paste
    leaf_age_d + fructose_mg_g + glucose_starch_1 + starch_mg_g + sucrose_mg_g + 
    starch_sugar_ratio +
    c + ca_mg_kg + k_mg_kg + n + s_mg_kg + 
    sum_et_60 + max_ta_55 + mean_et_pet_50
  ## end paste
  + (1 | sample_id),
  data = df_elast_fasy_wide,
  family = Gamma(link = "log"),
  na.action = "na.fail"
)

mod.lme_elast_fasy |> summary()
mod.lme_elast_fasy |> r2()
mod.lme_elast_fasy |> check_collinearity()

mod.lme_dredge_elast_fasy <- mod.lme_dredged_elast_fasy |> MuMIn::dredge(rank = "AICc", m.lim = c(1,7))

mod.lme_dredge_elast_fasy[1, ] %>% f.make_formula_dredge() %>% cat()

mod.lme_dredged_elast_fasy <- glmmTMB::glmmTMB(
  value ~ leaf_age_d + 
    ## paste
    mean_vpd_35 + sum_pet_55
  # + total_sugars_mg_g + sum_et_60 + max_ta_55 + mean_et_pet_50
    ## end paste
  + (1 | sample_id),
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
      mean_vpd_35 + sum_pet_55    
    ## end paste
    ,
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
    lower = coefs[, 1] - 1.96 * coefs[, 2],  # Confidence interval
    upper = coefs[, 1] + 1.96 * coefs[, 2],
    est_label = round(estimate, 2),
    p = case_when(coefs[, 4] >= 0.05 ~ "",
                  coefs[, 4] < 0.05 & coefs[, 4] >= 0.01 ~ "*",
                  coefs[, 4] < 0.01 & coefs[, 4] >= 0.001 ~ "**", 
                  coefs[, 4] < 0.001 ~ "***"),
    labels = paste(est_label, p, sep = " ")
  )

plot.elast_fasy_coefs.png <- coef_df |> 
  filter(term != "(Intercept)") |>
  mutate(
    coef_color = factor(case_when(estimate > 0 ~ "blue", TRUE ~ "red")),  # Use labels instead of hex
    term = fct_rev(factor(term))
  ) |> 
  ggplot(aes(y = term, x = estimate, color = coef_color)) +
  geom_point(size = 1.5, show.legend = F) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0, show.legend = F) +
  geom_text(aes(label = labels), vjust = -1, size = 4, show.legend = F) +
  # annotate("text", x = -0.45, y = 4.4, label = "b)") +
  scale_y_discrete(labels = new_labels) + 
  # scale_x_continuous(limits = c(0, 2)) +
  scale_color_manual(values = c("blue" = "#377eb8", "red" = "#e41a1c")) +  # Manually define colors
  labs(y = "", x = "Scaled coefficient estimates", 
       # title = "Effect sizes",
       # subtitle = "Fraxinus excelsior"
  ) +  thesis_theme +
  # lims(x = c(-0.05, 0.3)) +
  theme(axis.text = element_text(size = 15),
        title = element_text(size = 15),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.margin = margin(t = 0, r = 10, b = 0, l = 0)  # add margin on the right
  )
plot.elast_fasy_coefs.png
ggsave("figures/poster/plot.elast_fasy_coefs.png", width = 10, height = 10, units = "cm")

#### Plot for both species ---------------------------------------------------

# Extract R² (marginal) and format with 2 decimals
r2_fasy <- sprintf("%.2f", r2(mod.lme_dredged_elast_fasy)$R2_marginal)
r2_frex <- sprintf("%.2f", r2(mod.lme_dredged_elast_frex)$R2_marginal)

# Custom labels using plotmath (italic species, plain R²)
species_labels <- c(
  bquote(italic("Fagus sylvatica") ~ "(R"^2*" = " ~ .(r2_fasy) * ")"),
  bquote(italic("Fraxinus excelsior") ~ "(R"^2*" = " ~ .(r2_frex) * ")")
)

df_elast_fasy_wide |>
  ggplot() +
  geom_point(data = df_elast_fasy_wide, 
             aes(x = leaf_age_d, y = value, shape = "Fagus sylvatica"), 
             col = "#dc782a") +
  geom_point(data = df_elast_frex_wide, 
             aes(x = leaf_age_d, y = value, shape = "Fraxinus excelsior"), 
             col = "#56b4e9") +
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
  facet_wrap(~ year) +
  labs(y = "Bulk modulus of elasticity [MPa]", 
       x = "Leaf age (days since leaf out)") +
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
  theme(strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

ggsave("figures/poster/elast_model.png", height = 11.5, width = 22, units = "cm")

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
  unique() %>% na.omit()

## copy predictors 
paste(predictor_names, collapse = " + ") %>% cat()

## lm

mod.all_gmin_frex <- lm(
  value ~ leaf_age_d +
    fructose_mg_g + glucose_mg_g + glucose_starch_1 + starch_mg_g + sucrose_mg_g + total_sugars_mg_g + starch_sugar_ratio + c + ca_mg_kg + k_mg_kg + mg_mg_kg + n + p_mg_kg + s_mg_kg + max_ta_55 + mean_ta_60 + mean_et_pet_50 + sum_ppfd_25 + sum_pet_30 + mean_rh_2 + sum_p_2 + sum_et_5
  ,
  data = df_gmin_frex_wide,
  na.action = "na.fail"
)

## reduce model 

mod.red_gmin_frex <- mod.all_gmin_frex |> MASS::stepAIC(direction = "both") 
mod.red_gmin_frex |> summary()
mod.red_gmin_frex |> formula() 

mod.dredge_gmin_frex <- mod.red_gmin_frex |> MuMIn::dredge(rank = "AICc", m.lim = c(1,7))

mod.dredge_gmin_frex[1, ] %>% f.make_formula_dredge()

mod.lme_gmin_frex <- glmmTMB::glmmTMB(
  value ~  leaf_age_d + 
    ## paste
    max_ta_55 + mean_et_pet_50 + mean_rh_2  
  ## end paste
  + (1 | sample_id),
  data = df_gmin_frex_wide,
  family = Gamma(link = "log"),
  na.action = "na.fail"
)

mod.lme_gmin_frex |> summary()
mod.lme_gmin_frex |> r2()
mod.lme_gmin_frex |> check_model()
mod.lme_gmin_frex |> check_collinearity()

mod.lme_dredge_gmin_frex <- mod.lme_gmin_frex |> MuMIn::dredge(rank = "AICc", m.lim = c(1,7))

mod.lme_dredge_gmin_frex[1, ] %>% f.make_formula_dredge() %>% cat()

mod.lme_dredged_gmin_frex <- glmmTMB::glmmTMB(
  value ~ leaf_age_d + 
    ## paste
    max_ta_55 + mean_et_pet_50 + mean_rh_2  
  ## end paste
  + (1 | sample_id),
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
      max_ta_55 + mean_et_pet_50 + mean_rh_2  
    ## end paste
    ,
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
    lower = coefs[, 1] - 1.96 * coefs[, 2],  # Confidence interval
    upper = coefs[, 1] + 1.96 * coefs[, 2],
    est_label = round(estimate, 2),
    p = case_when(coefs[, 4] >= 0.05 ~ "",
                  coefs[, 4] < 0.05 & coefs[, 4] >= 0.01 ~ "*",
                  coefs[, 4] < 0.01 & coefs[, 4] >= 0.001 ~ "**", 
                  coefs[, 4] < 0.001 ~ "***"),
    labels = paste(est_label, p, sep = " ")
  )

plot.gmin_frex_coefs.png <- coef_df |> 
  filter(term != "(Intercept)") |>
  mutate(
    coef_color = factor(case_when(estimate > 0 ~ "blue", TRUE ~ "red")),  # Use labels instead of hex
    term = fct_rev(factor(term))
  ) |> 
  ggplot(aes(y = term, x = estimate, color = coef_color)) +
  geom_point(size = 1.5, show.legend = F) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0, show.legend = F) +
  geom_text(aes(label = labels), vjust = -1, size = 4, show.legend = F) +
  # annotate("text", x = -0.45, y = 4.4, label = "b)") +
  scale_y_discrete(labels = new_labels) + 
  # scale_x_continuous(limits = c(0, 2)) +
  scale_color_manual(values = c("blue" = "#377eb8", "red" = "#e41a1c")) +  # Manually define colors
  labs(y = "", x = "Scaled coefficient estimates", 
       # title = "Effect sizes",
       # subtitle = "Fraxinus excelsior"
  ) +  thesis_theme +
  # lims(x = c(-0.05, 0.3)) +
  theme(axis.text = element_text(size = 15),
        title = element_text(size = 15),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.margin = margin(t = 0, r = 10, b = 0, l = 0)  # add margin on the right
  )
plot.gmin_frex_coefs.png
ggsave("figures/poster/plot.gmin_frex_coefs.png", width = 10, height = 10, units = "cm")

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
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val_scaled)  %>% 
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
    !nutrient_name %in% c("fe_mg_kg", "mn_mg_kg", "zn_mg_kg", "al_mg_kg", "na_mg_kg"),
    !sugar_name %in% c("sugar_weight_mg") 
  ) |> 
  select(sugar_name, nutrient_name, meteo_predictor) |> 
  unlist(use.names = FALSE) |> 
  unique() %>% na.omit()

## copy predictors 
paste(predictor_names, collapse = " + ") 

## lm

mod.all_gmin_fasy <- lm(
  value ~ leaf_age_d +
    fructose_mg_g + glucose_mg_g + glucose_starch_1 + starch_mg_g + sucrose_mg_g + total_sugars_mg_g + starch_sugar_ratio + c + ca_mg_kg + k_mg_kg + mg_mg_kg + n + p_mg_kg + s_mg_kg + mean_rh_30 + mean_et_pet_50 + mean_vpd_30 + sum_pet_40 + mean_ta_10 + max_ta_10 + sum_ppfd_25 + sum_p_15 + sum_p_et_15 + sum_et_10 + mean_rew_10
  ,
  data = df_gmin_fasy_wide,
  na.action = "na.fail"
)

## reduce model 

mod.red_gmin_fasy <- mod.all_gmin_fasy |> MASS::stepAIC(direction = "backward") 
mod.red_gmin_fasy |> summary()
mod.red_gmin_fasy |> formula() 

mod.dredge_gmin_fasy <- mod.red_gmin_fasy |> MuMIn::dredge(rank = "AICc", m.lim = c(1,7))

avgmod <- model.avg(mod.dredge_gmin_fasy, subset = delta < 2.5)
# confset.95p <- get.models(mod.dredge_gmin_fasy, cumsum(weight) <= .95)
# avgmod.95p <- model.avg(confset.95p)
avgmod %>% summary()

mod.dredge_gmin_fasy[1, ] %>% f.make_formula_dredge()

mod.lme_gmin_fasy <- glmmTMB::glmmTMB(
  value ~ 
    ## paste
    leaf_age_d + glucose_starch_1 + mean_vpd_30 + total_sugars_mg_g  + n
  ## end paste
  + (1 | sample_id),
  data = df_gmin_fasy_wide,
  family = Gamma(link = "log"),
  na.action = "na.fail"
)

mod.lme_gmin_fasy |> summary()
mod.lme_gmin_fasy |> r2()
mod.lme_gmin_fasy |> check_collinearity()

mod.lme_dredge_gmin_fasy <- mod.lme_gmin_fasy |> MuMIn::dredge(rank = "AICc", m.lim = c(1,7))

mod.lme_dredge_gmin_fasy[1, ] %>% f.make_formula_dredge() %>% cat()

mod.lme_dredged_gmin_fasy <- glmmTMB::glmmTMB(
  value ~ leaf_age_d + 
    ## paste
    mean_vpd_30 + total_sugars_mg_g  + n  
  ## end paste
  + (1 | sample_id),
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
      mean_vpd_30 + total_sugars_mg_g + n  
    ## end paste
    ,
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
    lower = coefs[, 1] - 1.96 * coefs[, 2],  # Confidence interval
    upper = coefs[, 1] + 1.96 * coefs[, 2],
    est_label = round(estimate, 2),
    p = case_when(coefs[, 4] >= 0.05 ~ "",
                  coefs[, 4] < 0.05 & coefs[, 4] >= 0.01 ~ "*",
                  coefs[, 4] < 0.01 & coefs[, 4] >= 0.001 ~ "**", 
                  coefs[, 4] < 0.001 ~ "***"),
    labels = paste(est_label, p, sep = " ")
  )

# Define desired order of terms
term_order <- c("n", "total_sugars_mg_g", "mean_vpd_30", "leaf_age_d")

plot.gmin_fasy_coefs.png <- coef_df |> 
  filter(term != "(Intercept)") |>
  mutate(
    coef_color = factor(case_when(estimate > 0 ~ "blue", TRUE ~ "red")),  
    term = factor(term, levels = term_order)   # enforce order
  ) |> 
  ggplot(aes(y = term, x = estimate, color = coef_color)) +
  geom_point(size = 1.5, show.legend = F) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0, show.legend = F) +
  geom_text(aes(label = labels), vjust = -1, size = 4, show.legend = F) +
  scale_y_discrete(labels = new_labels) + 
  scale_color_manual(values = c("blue" = "#377eb8", "red" = "#e41a1c")) +
  labs(y = "", x = "Scaled coefficient estimates", 
       # title = "Effect sizes",
       # subtitle = "Fraxinus excelsior"
  ) +  thesis_theme +
  # lims(x = c(-0.05, 0.3)) +
  theme(axis.text = element_text(size = 15),
        title = element_text(size = 15),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.margin = margin(t = 0, r = 10, b = 0, l = 0)  # add margin on the right
  )
plot.gmin_fasy_coefs.png
ggsave("figures/poster/plot.gmin_fasy_coefs.png", width = 10, height = 10, units = "cm")

#### Plot for both species ---------------------------------------------------

# Extract R² (marginal) and format with 2 decimals
r2_fasy <- sprintf("%.2f", r2(mod.lme_dredged_gmin_fasy)$R2_marginal)
r2_frex <- sprintf("%.2f", r2(mod.lme_dredged_gmin_frex)$R2_marginal)

# Custom labels using plotmath (italic species, plain R²)
species_labels <- c(
  bquote(italic("Fagus sylvatica") ~ "(R"^2*" = " ~ .(r2_fasy) * ")"),
  bquote(italic("Fraxinus excelsior") ~ "(R"^2*" = " ~ .(r2_frex) * ")")
)

df_gmin_fasy_wide |>
  ggplot() +
  geom_point(data = df_gmin_fasy_wide, 
             aes(x = leaf_age_d, y = value, shape = "Fagus sylvatica"), 
             col = "#dc782a") +
  geom_point(data = df_gmin_frex_wide, 
             aes(x = leaf_age_d, y = value, shape = "Fraxinus excelsior"), 
             col = "#56b4e9") +
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
  facet_wrap(~ year) +
  labs(y = expression(g[min] ~ "["*mmol~m^{-2}~s^{-1}*"]"), 
       x = "Leaf age (days since leaf out)") +
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
  theme(strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

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
  filter(!is.na(nutrient_conc_scaled),
         !is.na(value)) |> 
  filter(!sugar_name |> str_detect("ppm")) |> 
  pivot_wider(names_from = nutrient_name, values_from = nutrient_conc_scaled) |> 
  pivot_wider(names_from = sugar_name, values_from = sugar_conc_scaled) |> 
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val_scaled)  %>% 
  mutate(leaf_age_scale = scale(leaf_age_d))

names(df_p50_frex_wide) = names(df_p50_frex_wide) |> 
  str_remove_all("\\[\\, 1\\]")

## get all predictors for massive model
predictor_names <- df_p50_frex |> 
  filter(!sugar_name |> str_detect("ppm"),
         !is.na(nutrient_conc_scaled),
         !nutrient_name %in% c("fe_mg_kg",
                               "mn_mg_kg",
                               "zn_mg_kg",
                               # "na_mg_kg",
                               "al_mg_kg"
         )) |> 
  select(c(sugar_name, nutrient_name, meteo_predictor)) |> 
  as.vector() |> unlist() |> 
  unique() 

## copy predictors 
str_c(predictor_names, collapse = " + ")  |> 
  paste()

mod.all_p50_frex <- lm(
  value ~ 
    leaf_age_d + 
    ## insert copied predictor names
    glucose_starch_1 + total_sugars_mg_g + starch_sugar_ratio + c + ca_mg_kg + k_mg_kg + mg_mg_kg + n + p_mg_kg + s_mg_kg + sum_pet_20 + sum_ppfd_5 + max_ta_15 + mean_ta_15 + sum_et_1 + mean_rh_15 + mean_vpd_15 + mean_et_pet_20
  ,
  data = df_p50_frex_wide,
  na.action = "na.fail"
)

mod.all_p50_frex |>  summary()

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

mod.dredge_p50_frex <- mod.red_p50_frex |> MuMIn::dredge(rank = "AICc", m.lim = c(1,7))

mod.avg <- model.avg(mod.dredge_p50_frex, delta < 2)
mod.avg %>% summary()

formula.red2 <- mod.lme_p50_frex |> stepAIC(direction = "both") |> formula()

mod.lme_dredged_p50_frex <- nlme::lme(
  value ~ leaf_age_scale + 
    max_ta_15 + total_sugars_mg_g  + ca_mg_kg        
    ,
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
  sjPlot::plot_model(mod.lme_dredged_p50_frex,
                     show.values = TRUE, show.p = T, value.offset = 0.3, line.size = 0.5, dot.size = 1.3) +
  geom_hline(aes(yintercept = 0), alpha= 0.2)+
  scale_x_discrete(labels = new_labels) + 
  labs(x = "", y = "Scaled coefficient estimates", 
       title = "",
       # subtitle = "Fraxinus excelsior"
  ) +  thesis_theme +
  # lims(x = c(-0.05, 0.3)) +
  theme(axis.text = element_text(size = 15),
        plot.title = element_blank(),
        title = element_text(size = 15),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.margin = margin(t = 0, r = 10, b = 0, l = 0)  # add margin on the right
  )
plot.p50_frex_coefs.png
ggsave("figures/poster/plot.p50_frex_coefs.png", width = 10, height = 10, units = "cm")


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
  filter(!is.na(nutrient_conc_scaled),
         !is.na(value)) |> 
  filter(!sugar_name |> str_detect("ppm")) |> 
  pivot_wider(names_from = nutrient_name, values_from = nutrient_conc_scaled) |> 
  pivot_wider(names_from = sugar_name, values_from = sugar_conc_scaled) |> 
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val_scaled)  %>% 
  mutate(leaf_age_scale = scale(leaf_age_d))

names(df_p50_fasy_wide) = names(df_p50_fasy_wide) |> 
  str_remove_all("\\[\\, 1\\]")

## get all predictors for massive model
predictor_names <- df_p50_fasy |> 
  filter(!sugar_name |> str_detect("ppm"),
         !is.na(nutrient_conc_scaled),
         !nutrient_name %in% c("fe_mg_kg",
                               "mn_mg_kg",
                               "zn_mg_kg",
                               "al_mg_kg",
                               "na_mg_kg")) |> 
  select(c(sugar_name, nutrient_name, meteo_predictor)) |> 
  as.vector() |> unlist() |> 
  unique() 

## copy predictors 
str_c(predictor_names, collapse = " + ")  |> 
  paste()

mod.all_p50_fasy <- lm(
  value ~ 
    leaf_age_d + 
    ## insert copied predictor names
    fructose_mg_g + glucose_mg_g + glucose_starch_1 + starch_mg_g + sucrose_mg_g + sugar_weight_mg + total_sugars_mg_g + starch_sugar_ratio + c + ca_mg_kg + k_mg_kg + mg_mg_kg + n + p_mg_kg + s_mg_kg + sum_p_10 + sum_p_et_5 + max_ta_50 + mean_et_pet_40 + mean_ta_60
  ,
  data = df_p50_fasy_wide,
  na.action = "na.fail"
)

mod.all_p50_fasy |>  summary()

## reduce model 

mod.red_p50_fasy <- mod.all_p50_fasy |> MASS::stepAIC(direction = "both") 
mod.red_p50_fasy |> summary()

mod.dredge_p50_fasy <- mod.red_p50_fasy |> MuMIn::dredge(rank = "AICc", m.max = 5)

mod.avg <- model.avg(mod.dredge_p50_fasy, delta < 2)
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

mod.lme_dredge_p50_fasy <- mod.lme_p50_fasy |> MuMIn::dredge(rank = "AICc", m.lim = c(1,7))
mod.avg <- model.avg(mod.lme_dredge_p50_fasy, delta < 2.5)
mod.avg %>% summary()

mod.lme_dredged_p50_fasy <- nlme::lme(
  value ~ leaf_age_scale +
    k_mg_kg + starch_sugar_ratio + sum_p_et_5 + total_sugars_mg_g
  ,
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
  sjPlot::plot_model(mod.lme_dredged_p50_fasy,
                     show.values = TRUE, show.p = T, value.offset = 0.3, line.size = 0.5, dot.size = 1.3) +
  geom_hline(aes(yintercept = 0), alpha= 0.2)+
  scale_x_discrete(labels = new_labels) + 
  labs(x = "", y = "Scaled coefficient estimates", 
       # title = "Effect sizes",
       # subtitle = "Fraxinus excelsior"
  ) +  thesis_theme +
  # lims(x = c(-0.05, 0.3)) +
  theme(axis.text = element_text(size = 15),
        title = element_blank(),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.margin = margin(t = 0, r = 10, b = 0, l = 0)  # add margin on the right
  )
plot.p50_fasy_coefs.png
ggsave("figures/poster/p50_fasy_effect_sizes.png", width = 10, height = 10, units = "cm")


#### Plot for both species ---------------------------------------------------

# Extract R² (marginal) and format with 2 decimals
r2_fasy <- sprintf("%.2f", r2(mod.lme_dredged_p50_fasy)$R2_marginal)
r2_frex <- sprintf("%.2f", r2(mod.lme_dredged_p50_frex)$R2_marginal)

# Custom labels using plotmath (italic species, plain R²)
species_labels <- c(
  bquote(italic("Fagus sylvatica") ~ "(R"^2*" = " ~ .(r2_fasy) * ")"),
  bquote(italic("Fraxinus excelsior") ~ "(R"^2*" = " ~ .(r2_frex) * ")")
)

df_p50_fasy_wide |>
  ggplot() +
  geom_point(data = df_p50_fasy_wide, 
             aes(x = leaf_age_d, y = value, shape = "Fagus sylvatica"), 
             col = "#dc782a") +
  geom_point(data = df_p50_frex_wide, 
             aes(x = leaf_age_d, y = value, shape = "Fraxinus excelsior"), 
             col = "#56b4e9") +
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
  facet_wrap(~ year) +
  labs(y = expression(P[50] ~ "[MPa]"), 
       x = "Leaf age (days since leaf out)") +
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
  theme(strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

ggsave("figures/poster/p50_model.png", height = 11.5, width = 22, units = "cm")


# dump --------------------------------------------------------------------

df_hydr_traits %>% 
  filter(trait == "psi_midday_mpa",
         sugar_name %>% str_detect("mg\\_g|ratio")) %>% 
  group_by(species, campaign, year, sugar_name) %>% 
  summarise(sugar_conc = mean(sugar_conc),
            value = mean(value)) %>% 
  ggplot(aes(y = sugar_conc, x = value, col = species)) +
  lims(x = c(0, -3.5)) +
  geom_point() +
  facet_wrap(~ sugar_name, scale = "free") +
  labs(x = "Psi midday [MPa]", y = "Sugar concentration") +
  theme_bw()

df_hydr_traits$trait %>% unique()

df_best_meteo_corr |> 
  filter(trait == "psi_midday_mpa") |> 
  dplyr::select(-data) |> 
  write.table("clipboard", sep="\t", row.names=FALSE)

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
  select(-c(timeframe, meteo_var, corrcoef, pval, abs_corr)) |>
  pivot_wider(names_from = meteo_predictor, values_from = meteo_val)


test_scale <- test |> 
  dplyr::select(-c(trait, date, year, species, sample_id, campaign, trait_val)) |>
  scale()
test_scale <- test |> 
  dplyr::select(c(trait, date, year, species, sample_id, campaign, trait_val)) |> 
  cbind(test_scale)

test <- test |> na.omit()
lm_test <- lm(trait_val ~ pheno_start_doy + leaf_age_d + sum_p_mm_10 + sum_p_et_60 + 
                min_rew_5 + mean_rew_20 + max_ta_50 + sum_et_35,
              data = test,
              na.action = "na.fail")
lm_test |> summary()

library(MuMIn)
model_selection_results <- MuMIn::dredge(lm_test, rank = "AICc")
model_selection_results |> head()

lm_test$residuals |> shapiro.test()

stepAIC(lm_test, direction = "both") |> summary()

lm_test_red <- lm(trait_val ~ sum_p_mm_10 + sum_p_et_60 + 
                    sum_et_35 + max_ta_50 + pheno_start_doy,
                  data = test,
                  na.action = "na.fail")
lm_test_red |> summary()

summary(lm_test_red)$coef |> write.table("clipboard", sep="\t", row.names=T)


## assess multicollinearity
library(GGally)
test |> 
  dplyr::select(-c(trait, date, year, species, sample_id, campaign, trait_val,
                   mean_rew_20, min_rew_5, doy, pheno_start_doy)) |>
  ggpairs()

## lme
lme_test <- lmer(trait_val ~ sum_p_mm_10 + sum_p_et_60 +
                   max_ta_50 + sum_et_35 + leaf_age_d + (1 | species/sample_id),
                 data = test)
lme_test |> summary()
lme_test |> performance::r2()

test |> 
  modelr::add_predictions(lme_test) |>
  mutate(species = recode(species, "FREX" = "Fraxinus excelsior", "FASY" = "Fagus sylvatica")) |>
  ggplot() +
  geom_point(aes(x = doy, y = trait_val, col = species)) +
  geom_line(aes(x = doy, y = pred, col = species, group = sample_id)) +
  facet_wrap(~ year) +
  lims(y = c(-4, 0)) +
  scale_color_manual(values = c("Fagus sylvatica" = "#410866", "Fraxinus excelsior" = "#D55E00")) +
  labs(y = "Midday water potential [MPa]", x = "Day of year",
       title = "Midday water potential",
       subtitle = "linear mixed effects results") +
  thesis_theme 


### linear mixed effect models ----------------------------------------------

## cannot keep this structure since dates are across two years

df_pv_params_nest <- df_pv_params_nest |> 
  mutate(mod = map(data, ~ lmer(value ~ species + date + (1 | sample_id), data = .x)))

summary(df_pv_params_nest$mod[[1]])
VarCorr(df_pv_params_nest$mod[[1]]) 

test <- df_pv_params_nest$data[[1]] |> filter(year == "2023") 

mod <- lmer(value ~  date + (date | species), 
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
  geom_line(aes(x = date, y = predict(mod), color = species, group = sample_id)) +
  labs(y = "psi_tlp", x = "date") + 
  lims(y = c(-3, -2)) +
  theme_bw()

aov(value ~ species + date + species:date, data = test) |> summary()
lm(value ~ species + date + species:date, data = test) |> summary()

### emmeans -----------------------------------------------------------------
library(emmeans)

emm <- emmeans::emmeans(mod, ~ species * date, adjust = "mvt", type = "response")
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
  geom_line(data = df_emm, aes(x = date, y = emmean, group = species, color = species)) 
# +  geom_line(x = date, aes(y = pred))

### simulate data to test lme singularity and re ----------------------------


## dataframe with date, species, sample_id and value

set.seed(123)
n <- 10

## 8 dates, 3 species, 10 samples per species
date <- rep(c("2022-01-01", "2022-01-02", "2022-01-03", "2022-01-04", 
              "2022-01-05", "2022-01-06", "2022-01-07", "2022-01-08"), each = n) |> 
  as.factor()
species <- rep(c("A", "B", "C"), each = n * 8) |> as.factor()
sample_id <- rep(1:n, 8) |> as.factor()
df <- data.frame(species, date)
df <- df |> 
  mutate(
    sample_id = paste(species, sample_id, sep = "_"),
    value = case_when(species == "A" ~ rnorm(nrow(df), mean = 1, sd = 1),
                      species == "B" ~ rnorm(nrow(df), mean = 2, sd = 1),
                      species == "C" ~ rnorm(nrow(df), mean = 3, sd = 1)))

ggplot(df) +
  geom_point(aes(x = date, y = value, color = species)) +
  geom_line(aes(x = date, y = value, group = sample_id), color = "grey")

mod_sim <- lmer(value ~ date + (1 | species/sample_id), data = df)
mod_sim |> summary()
ranef(mod_sim)

ggplot(df) +
  geom_point(aes(x = date, y = value, color = species)) +
  geom_line(aes(x = date, y = value, group = sample_id), color = "grey") +
  geom_line(aes(x = date, y = fitted(mod_sim), color = species, group = sample_id), shape = 1)


df_sugars_emm |> 
  filter(sugar_name %in% c("total_sugars_mg_g", "glucose_mg_g", "fructose_mg_g", "starch_mg_g", "sucrose_mg_g")) |>
  dplyr::select(sample_id, campaign, date, sugar_name, sugar_conc) |>
  pivot_wider(names_from = sugar_name, values_from = sugar_conc) |> 
  write.table("clipboard", sep = "\t", row.names = FALSE)

df_sugars_emm |> 
  filter(sugar_name %in% c("total_sugars_mg_g", "glucose_mg_g", "fructose_mg_g", "starch_mg_g", "sucrose_mg_g")) |>
  dplyr::select(species, campaign, date, sugar_name, response, SE, asymp.LCL, asymp.UCL, .group) |>
  distinct() |> 
  write.table("clipboard", sep = "\t", row.names = FALSE)
