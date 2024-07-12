
# Setup -------------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(scam)


folder <- "Q:/cavicam/"
files <- list.files(folder, pattern = "\\.csv", full.names = T)


# process cavitated area --------------------------------------------------

df_area <- files %>%
  map_df(read_csv, .id = "sample_ID") 

df_area <- files %>%
  set_names(nm = basename(.)) %>%
  map_df(read_csv, .id = "sample_ID") |> 
  rename(id = `...1` ) 


df_area <- df_area |> 
  group_by(sample_ID) |> 
  mutate(area_cav = cumsum(Area),
         perc_area_cav =  area_cav / sum(Area),
         sample_ID = sample_ID |> str_remove_all("\\.csv"),
         minutes = id * 10 - 10) ## 1 picture every 10 minutes processed for now

df_area |> 
  ggplot() +
  geom_line(aes(x = minutes, y = perc_area_cav, group = sample_ID, color = sample_ID))

# process by water potential ----------------------------------------------

           
lst <- lapply(2:9, function(i) readxl::read_excel("cavicam/01_first_round_hainich-cavicam-PSI-(29-05-2024).xlsx", sheet = i))

# coerce list to df
df_cavi <- do.call(rbind.data.frame, lst)
df_cavi <- df_cavi |> 
  mutate(drying_interval = start_measurement - lag(start_measurement,1),
         equilibration_interval =  start_measurement - start_equilibration)

## get elapsed time since start
df_cavi <- df_cavi |> 
  group_by(sample_ID) |> 
  mutate(minutes = difftime(start_measurement, min(start_measurement), units = "mins") |> as.numeric()) |> 
  ungroup()

df_cavi$drying_interval[df_cavi$drying_interval < 1] = NA

df_cavi

df_cavi |>
  ggplot() +
  geom_line(aes(x = minutes, y = psi_scam, group = sample_ID, color = sample_ID)) +
  geom_line(aes(x = minutes, y = psi, group = sample_ID, color = sample_ID))


scamfun <- function(water_potential, area){
  mod <- scam(psi ~ s(minutes, bs = "cr", k = 3), data = water_potential)
  predict(mod, newdata = list(minutes = area$minutes))
}


scam(psi ~ s(minutes, bs = "cr", k = 3), 
     data = df_cavi |> filter(sample_ID == "FASY_04"))

df_area_nest <- df_area |> 
  nest(data = -sample_ID) |> 
  rename("area" = "data")

df_cavi_nest <- df_cavi |> 
  nest(data = -sample_ID) |> 
  rename("water_potential" = "data")

df_all <- left_join(df_area_nest, df_cavi_nest, by = "sample_ID")

df_all <- df_all |> 
  mutate(psi_pred = map2(water_potential, area, scamfun),
         area = map2(area, psi_pred, ~mutate(.x, psi_pred = .y)))

df_area <- df_all |> 
  select(c(sample_ID, area)) |> 
  unnest() |> 
  mutate(psi_pred_MPa = -psi_pred / 10)


df_area |> 
  # filter(sample_ID != "FREX_08") |> 
  ggplot() +
  geom_path(aes(x = psi_pred_MPa, y = perc_area_cav , group = sample_ID, color = sample_ID), linewidth = 0.75) + 
  # xlim(-7, 1) +
  # facet_wrap(~sample_ID) +
  theme_bw()


# fit plc -----------------------------------------------------------------

library(fitplc)


# We use the built-in example dataset 'stemvul' in the examples below. See ?stemvul.
# Most examples will fit the Weibull model (the default); try running some of the examples
# with 'model="sigmoidal"' and compare the results.

# 1. Fit one species (or fit all, see next example)
dfr1 <- subset(stemvul, Species =="dpap")

# Fit Weibull model. Store results in object 'pfit'
# 'varnames' specifies the names of the 'PLC' variable in the dataframe,
# and water potential (WP). 
# In this example, we use only 50 bootstrap replicates but recommend you set this
# to 1000 or so.
pfit <- fitplc(dfr1, varnames=c(PLC="PLC", WP="MPa"), nboot=50)

# Look at fit
pfit
#> Class of object 'plcfit' as returned by 'fitplc'.
#> 
#> Parameters and %s%% confidence interval:
#> 
#>  95%    Estimate Norm - 2.5% Norm - 97.5% Boot - 2.5% Boot - 97.5%
#> SX 27.639042   19.016029    38.636204   16.745048    35.622913
#> PX  2.631328    2.310614     2.955739    2.391684     2.975172
#> 

# Make a standard plot. The default plot is 'relative conductivity',
# (which is 1.0 where PLC = 0). For plotting options, see ?plot.plcfit
plot(pfit)

df_FASY_01 <- df_area |> 
  filter(sample_ID == "FASY_01", psi_pred_MPa > -6) |> 
  mutate(perc_area_cav = perc_area_cav * 100)

pfit_cc <- fitplc(df_FASY_01, varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"), nboot=50, model="sigmoid")
pfit_cc |> plot()



# fit all species ---------------------------------------------------------

df_area2 <- df_area |> 
  mutate(perc_area_cav = perc_area_cav * 100,
         species = sample_ID |> str_extract("FASY|FREX"))

## model separated by sample
all_fit_sample_50 <- fitplcs(df_area2, "sample_ID", varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"), nboot = 1000, model="sigmoid")
plot(all_fit_sample_50, onepanel=TRUE, plotci=FALSE, px_ci="none", pxlinecol="dimgrey")

all_fit_sample_12 <- fitplcs(df_area2, "sample_ID", varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"), nboot = 1000, model="sigmoid", x = 12)
plot(all_fit_sample_12, onepanel=TRUE, plotci=FALSE, px_ci="none", pxlinecol="dimgrey")

all_fit_sample_88 <- fitplcs(df_area2, "sample_ID", varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"), nboot = 1000, model="sigmoid", x = 88)
plot(all_fit_sample_88, onepanel=TRUE, plotci=FALSE, px_ci="none", pxlinecol="dimgrey")


## model separated by species, sample as random effect
all_fit_species <- fitplcs(df_area2, "species", varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"), nboot=50, model="sigmoid", random = "sample_ID")
plot(all_fit_species, onepanel=TRUE, plotci=FALSE, px_ci="none", pxlinecol="dimgrey")
all_fit_species

# test area ---------------------------------------------------------------



df_test <- df_all |> 
  mutate(psi_pred_cavi = map2(water_potential, water_potential, scamfun),
         cavi = map2(water_potential, psi_pred_cavi, ~mutate(.x, psi_pred_cavi = .y)))

df_test2 <- df_test |> 
  select(c(sample_ID, cavi)) |> 
  unnest()

