
### model comparisons -------------------------------------------------------

models <- list(
  nlme_p50_frex_all_vessels,
  nlme_p50_frex_major_vessels,
  nlme_p50_fasy_all_vessels,
  nlme_p50_fasy_major_vessels
)

library(broom.mixed)

df_model_coefs <- models |> map(~coef(.)$PX) |> unlist() |>  matrix(nrow = 4, ncol = 3, byrow = T)

df_model_coefs |> write.excel()

# preprocess images for faster loading into imagej ------------------------
library(magick)
library(tidyverse)

image_path <- "Q:/cavicam/"
samples <- list.dirs(image_path, full.names = T, recursive = F)
frex08 <- samples[1]
new_folder = paste0(frex08, "_resized")
dir.create(new_folder, showWarnings = F)

for(i in 1:length(list.files(frex08, pattern = "\\.png"))) {
  img <- image_read(list.files(frex08, full.names = T)[i])
  img |>
    image_resize("1296x972") |>
    image_write(paste0(new_folder, "/", list.files(frex08)[i]))
  if (i %% 10 == 0) {
    print(i)
  }
}







# test area ---------------------------------------------------------------


df_test <- df_all |> 
  mutate(psi_pred_cavi = map2(water_potential, water_potential, scamfun),
         cavi = map2(water_potential, psi_pred_cavi, ~mutate(.x, psi_pred_cavi = .y)))

df_test2 <- df_test |> 
  select(c(sample_ID, cavi)) |> 
  unnest()


img1 <- image_read("Q:/cavicam/Cav15_FREX_05_FW/20240531-214508.png", depth = 8)
img2 <- image_read("Q:/cavicam/Cav15_FREX_05_FW/20240531-215019.png", depth = 8)


img1 <- img1 |> image_convert(colorspace = "gray") 
img2 <- img2 |> image_convert(colorspace = "gray")

img1data = img1 |> image_data() |> as.data.frame() 
img1data |> col2rgb() 


# dump --------------------------------------------------------------------

fasy011 <- read.csv("cavicam/camp1/frex07_1-800_qnd.csv")
fasy012 <- read.csv("cavicam/camp1/frex07_800-end_qnd.csv")

fasy01 <- rbind.data.frame(fasy011, fasy012) 
fasy01 <- fasy01 |> 
  mutate(sample_ID = "FREX_07",
         campaign = "1",
         year = "2024",
         date = "2024-05-29",
         X = 1:nrow(fasy01))
write.csv(fasy01, "cavicam/camp1/FREX_07.csv", row.names = F)

#### all vessels,  species wise nlme -------------------------------------------

nlme_p50_species_all_vessels <- nlme(
  relK ~ f.weibull_plc(-psi_pred_MPa, SX, PX, X = 50),
  df_area |> filter(psi_pred_MPa > -10,
                    vessel_order == "all") |>
    mutate(
      perc_area_cav = perc_area_cav * 100,
      relK = plc_to_relk(perc_area_cav),
      species = substr(sample_ID, 1, 4)
    ),
  fixed = list(SX ~ 1, PX ~ 1),
  random = SX + PX ~ 1 | species / campaign,
  start = list(fixed = c(SX = 124, PX = 4)),
  control = nlmeControl(maxiter = 1000, msMaxIter = 2000)
)

df_area |> filter(psi_pred_MPa > -10,
                  vessel_order == "all") |>
  mutate(perc_area_cav = perc_area_cav * 100,
         relK = plc_to_relk(perc_area_cav), 
         species = substr(sample_ID, 1, 4)) |>
  modelr::add_predictions(nlme_p50_species_all_vessels, var = "pred_nlme") |>
  ggplot() +
  geom_point(aes(
    x = psi_pred_MPa,
    y = relK_to_plc(relK),
    # col = sample_ID,
    # group = sample_ID,
  ), col = "grey", size = 0.5) +
  geom_line(aes(
    x = psi_pred_MPa, y = relK_to_plc(pred_nlme),
    col = campaign, group = campaign
  ), linewidth = 1) +
  facet_wrap( ~ species) +
  ylab("Relative conductivity") +
  xlab("Water potential (MPa)") +
  theme_minimal() 

#### FREX all vessels nlme -------------------------------------------

df_p50_frex_all_vessels <- df_area |>
  mutate(
    perc_area_cav = perc_area_cav * 100,
    relK = plc_to_relk(perc_area_cav),
    species = substr(sample_ID, 1, 4)
  ) |>
  filter(psi_pred_MPa > -10,
         vessel_order == "all",
         species == "FREX",
         year == "2024")

nls_p50_frex_all_vessels <- nls(relK ~ f.weibull_plc(-psi_pred_MPa, SX, PX, X = 50),
                                data = df_p50_frex_all_vessels,
                                start = c(SX = 100, PX = 5),
                                control = nlmeControl(maxiter = 1000, msMaxIter = 2000)
)

nls_p50_frex_all_vessels |> coef()

nlme_p50_frex_all_vessels <- nlme(
  relK ~ f.weibull_plc(-psi_pred_MPa, SX, PX, X = 50),
  data = df_p50_frex_all_vessels,
  fixed = list(SX ~ 1, PX ~ 1),
  random = SX + PX ~ 1 | campaign,
  # start = coef(nls_p50_frex_all_vessels),
  start = c(SX = 100, PX = 5),
  control = nlmeControl(maxiter = 1000, msMaxIter = 2000)
)

frex_all_vessels_p50_nlme.png <- df_p50_frex_all_vessels |>
  modelr::add_predictions(nlme_p50_frex_all_vessels, var = "pred_nlme") |>
  ggplot() +
  geom_point(aes(
    x = psi_pred_MPa,
    y = relK |> relK_to_plc(),
  ), col = "grey", size = 0.5) +
  geom_line(aes(
    x = psi_pred_MPa, y = pred_nlme |> relK_to_plc(),
    col = campaign, group = campaign
  ), linewidth = 1) +
  geom_vline(xintercept = -coef(nlme_p50_frex_all_vessels)$PX, col = scales::hue_pal()(4), linetype = "dashed", linewidth = 1) +
  ylab("Percent area cavitated") +
  xlab("Water potential (MPa)") +
  plot_theme
frex_all_vessels_p50_nlme.png
ggsave(frex_all_vessels_p50_nlme.png, filename = "plots/frex_all_vessels_p50_nlme.png", width = 8, height = 8)

#### FREX major vessels nlme -------------------------------------------

df_p50_frex_major_vessels <- df_area |>
  mutate(
    perc_area_cav = perc_area_cav * 100,
    relK = plc_to_relk(perc_area_cav),
    species = substr(sample_ID, 1, 4),
    sample_ID = sample_ID |> as.factor(),
    campaign = campaign |> as.factor()
  ) |>
  filter(psi_pred_MPa > -10,
         vessel_order == "major",
         species == "FREX",
         year == "2024")

nls_p50_frex_major_vessels <- nls(relK ~ f.weibull_plc(-psi_pred_MPa, SX, PX, X = 50),
                                  data = df_p50_frex_major_vessels,
                                  start = c(SX = 100, PX = 5),
                                  control = nlmeControl(maxiter = 1000, msMaxIter = 2000)
)

nls_p50_frex_major_vessels |> coef()

nlme_p50_frex_major_vessels <- nlme(
  relK ~ f.weibull_plc(-psi_pred_MPa, SX, PX, X = 50),
  data = df_p50_frex_major_vessels,
  fixed = list(SX ~ 1, PX ~ 1),
  random = SX + PX ~ 1 | campaign,
  # start = coef(nls_p50_frex_major_vessels),
  start = c(SX = 100, PX = 5),
  control = nlmeControl(maxiter = 1000, msMaxIter = 2000)
)

frex_major_vessels_p50_nlme.png <- df_p50_frex_major_vessels |>
  modelr::add_predictions(nlme_p50_frex_major_vessels, var = "pred_nlme") |>
  ggplot() +
  geom_point(aes(
    x = psi_pred_MPa,
    y = relK |> relK_to_plc(),
  ), col = "grey", size = 0.5) +
  geom_line(aes(
    x = psi_pred_MPa, y = pred_nlme |> relK_to_plc(),
    col = campaign, group = campaign
  ), linewidth = 1) +
  geom_vline(xintercept = -coef(nlme_p50_frex_major_vessels)$PX, col = scales::hue_pal()(4), linetype = "dashed", linewidth = 1) +
  ylab("Percent area cavitated") +
  xlab("Water potential (MPa)") +
  plot_theme
frex_major_vessels_p50_nlme.png
ggsave(frex_major_vessels_p50_nlme.png, filename = "plots/frex_major_vessels_p50_nlme.png", width = 8, height = 8)


#### FASY all vessels nlme -------------------------------------------

df_p50_fasy_all_vessels <- df_area |>
  mutate(
    perc_area_cav = perc_area_cav * 100,
    relK = plc_to_relk(perc_area_cav),
    species = substr(sample_ID, 1, 4)
  ) |>
  filter(psi_pred_MPa > -10,
         vessel_order == "all",
         species == "FASY",
         year == "2024")

nls_p50_fasy_all_vessels <- nls(relK ~ f.weibull_plc(-psi_pred_MPa, SX, PX, X = 50),
                                data = df_p50_fasy_all_vessels,
                                start = c(SX = 100, PX = 10),
                                control = nlmeControl(maxiter = 1000, msMaxIter = 2000)
)

nls_p50_fasy_all_vessels |> coef()

nlme_p50_fasy_all_vessels <- nlme(
  relK ~ f.weibull_plc(-psi_pred_MPa, SX, PX, X = 50),
  data = df_p50_fasy_all_vessels,
  fixed = list(SX ~ 1, PX ~ 1),
  random = SX + PX ~ 1 | campaign,
  # start = coef(nls_p50_fasy_all_vessels),
  start = c(SX = 100, PX = 5),
  control = nlmeControl(maxiter = 1000, msMaxIter = 2000)
)

fasy_all_vessels_p50_nlme.png <- df_p50_fasy_all_vessels |>
  modelr::add_predictions(nlme_p50_fasy_all_vessels, var = "pred_nlme") |>
  ggplot() +
  geom_point(aes(
    x = psi_pred_MPa,
    y = relK |> relK_to_plc(),
  ), col = "grey", size = 0.5) +
  geom_line(aes(
    x = psi_pred_MPa, y = pred_nlme |> relK_to_plc(),
    col = campaign, group = campaign
  ), linewidth = 1) +
  geom_vline(xintercept = -coef(nlme_p50_fasy_all_vessels)$PX, col = scales::hue_pal()(4), linetype = "dashed", linewidth = 1) +
  ylab("Percent area cavitated") +
  xlab("Water potential (MPa)") +
  plot_theme
fasy_all_vessels_p50_nlme.png
ggsave(fasy_all_vessels_p50_nlme.png, filename = "plots/fasy_all_vessels_p50_nlme.png", width = 8, height = 8)

#### FASY major vessels nlme -------------------------------------------

df_p50_fasy_major_vessels <- df_area |>
  mutate(
    perc_area_cav = perc_area_cav * 100,
    relK = plc_to_relk(perc_area_cav),
    species = substr(sample_ID, 1, 4)
  ) |>
  filter(
    psi_pred_MPa > -10,
    vessel_order == "major",
    species == "FASY",
    year == "2024")

nls_p50_fasy_major_vessels <- nls(relK ~ f.weibull_plc(-psi_pred_MPa, SX, PX, X = 50),
                                  data = df_p50_fasy_major_vessels,
                                  start = c(SX = 100, PX = 10),
                                  control = nlmeControl(maxiter = 1000, msMaxIter = 2000)
)

nls_p50_fasy_major_vessels |> coef()

nlme_p50_fasy_major_vessels <- nlme(
  relK ~ f.weibull_plc(-psi_pred_MPa, SX, PX, X = 50),
  data = df_p50_fasy_major_vessels,
  fixed = list(SX ~ 1, PX ~ 1),
  random = SX + PX ~ 1 | campaign,
  # start = coef(nls_p50_fasy_major_vessels),
  start = c(SX = 40, PX = 5),
  control = nlmeControl(maxiter = 1000, msMaxIter = 2000)
)

nlme_p50_fasy_major_vessels |> summary()

fasy_major_vessels_p50_nlme.png <- df_p50_fasy_major_vessels |>
  modelr::add_predictions(nlme_p50_fasy_major_vessels, var = "pred_nlme") |>
  ggplot() +
  geom_point(aes(
    x = psi_pred_MPa,
    y = relK |> relK_to_plc(),
  ), col = "grey", size = 0.5) +
  geom_line(aes(
    x = psi_pred_MPa, y = pred_nlme |> relK_to_plc(),
    col = campaign, group = campaign
  ), linewidth = 1) +
  geom_vline(xintercept = -coef(nlme_p50_fasy_major_vessels)$PX, col = scales::hue_pal()(4), linetype = "dashed", linewidth = 1) +
  ylab("Percent area cavitated") +
  xlab("Water potential (MPa)") +
  plot_theme
fasy_major_vessels_p50_nlme.png
ggsave(fasy_major_vessels_p50_nlme.png, filename = "plots/fasy_major_vessels_p50_nlme.png", width = 8, height = 8)

# fit plc tutorial -----------------------------------------------------------------

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

df_test <- df_area |> filter(sample_ID == "FASY_01", 
                             # campaign == "3",
                             psi_pred_MPa > -6) |> 
  mutate(perc_area_cav = perc_area_cav * 100)

pfit_cc <- fitplcs(df_area |> 
                     mutate(species = substr(sample_ID, 1, 4)), 
                   varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"),
                   group = "species",
                   random = "campaign",
                   nboot=100, 
                   model="sigmoid")
pfit_cc |> plot()

pfit_cc |> summary()



# fit all species ---------------------------------------------------------

df_camp1 <- df_area |> 
  filter(campaign == "1")|>
  mutate(perc_area_cav = perc_area_cav * 100,
         species = sample_ID |> str_extract("FASY|FREX"))

## model separated by sample
mod_fix_p50_camp1 <- fitplcs(df_camp1, "sample_ID", varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"), nboot = 1000, model="sigmoid")
mod_fix_p50_camp1 |> coef() |> filter(Parameter == "PX") |> write.excel()

mod_fix_p12_camp1 <- fitplcs(df_camp1, "sample_ID", varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"), nboot = 1000, model="sigmoid", x = 12)
mod_fix_p12_camp1 |> coef() |> filter(Parameter == "PX") |> write.excel()

mod_fix_p88_camp1 <- fitplcs(df_camp1, "sample_ID", varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"), nboot = 1000, model="sigmoid", x = 88)
mod_fix_p88_camp1 |> coef() |> filter(Parameter == "PX") |> write.excel()

df_camp2 <- df_area |> 
  filter(campaign == "2")|>
  mutate(perc_area_cav = perc_area_cav * 100,
         species = sample_ID |> str_extract("FASY|FREX"))

## model separated by sample
mod_fix_p50_camp2 <- fitplcs(df_camp2, "sample_ID", varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"), nboot = 1000, model="sigmoid")
mod_fix_p50_camp2 |> coef() |> filter(Parameter == "PX") |> select(c(Estimate, `Boot - 2.5%`, `Boot - 97.5%`)) |> write.excel()

mod_fix_p12_camp2 <- fitplcs(df_camp2, "sample_ID", varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"), nboot = 1000, model="sigmoid", x = 12)
mod_fix_p12_camp2 |> coef() |> filter(Parameter == "PX") |> select(c(Estimate, `Boot - 2.5%`, `Boot - 97.5%`)) |> write.excel()

mod_fix_p88_camp2 <- fitplcs(df_camp2, "sample_ID", varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"), nboot = 1000, model="sigmoid", x = 88)
mod_fix_p88_camp2 |> coef() |> filter(Parameter == "PX") |> select(c(Estimate, `Boot - 2.5%`, `Boot - 97.5%`)) |> write.excel()




## model separated by sample, species as random effect
all_fit_species <- fitplcs(df_camp1, "sample_ID", varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"), nboot=50, model="sigmoid", random = "species")
plot(all_fit_species, onepanel=TRUE, plotci=FALSE, px_ci="none", pxlinecol="dimgrey")
all_fit_species$FASY_01$fit 



