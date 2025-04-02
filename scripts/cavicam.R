# Setup -------------------------------------------------------------------

library(tidyverse)
library(scam)
library(janitor)
library(modelr)
library(fitplc)
library(nlme)
library(flextable)

write.excel <- function(x,
                        row.names = FALSE,
                        col.names = FALSE,
                        ...) {
  write.table(
    x,
    "clipboard-2048",
    sep = "\t",
    row.names = row.names,
    col.names = col.names,
  )
}

source("scripts/ggplot_themes.R")


# load data ---------------------------------------------------------------

## cavicam embolized area 
df_area <- read.csv("data/cavicam/df_area.csv")

## water potentials 
df_water_potential <- read.csv("data/cavicam/df_water_potential.csv")


# df_area <- read.csv("data/cavicam/df_cavitated_area.csv")


# 0. data preparation --------------------------------------------------------


## 0.0 prepare cavicam starting times --------------------------------------

df_start_times <- readxl::read_excel("data/cavicam/cavicam_start_times.xlsx")

df_start_times <- df_start_times |> 
  mutate(start_time = as.POSIXct(time, format = "%Y%m%d-%H%M%S"),
         year = as.character(year),
         campaign = as.character(campaign),
         sample_id = as.character(sample_id)) |> 
  rename("cavicam_start_time" = "start_time")

## 0.1 prepare cavitated area --------------------------------------------------

cavicam_files <- list.files("data/cavicam/image_areas", pattern = "\\.csv", full.names = T, recursive = T) 

df_area <- cavicam_files %>%
  set_names(nm = paste0(dirname(.), "/", basename(.))) |> 
  map_df(data.table::fread, .id = "all_id") |> 
  clean_names() |> 
  rename("image_nr" = "v1")

df_area <- df_area |>
  dplyr::select(c(all_id, image_nr, area)) |> ## get rid of unnecessary columns
  group_by(all_id) |>
  mutate(
    area_cav = cumsum(area),
    perc_area_cav =  area_cav / sum(area),
    year = all_id |> str_extract("\\d{4}"),
    campaign = all_id |> str_extract("camp\\d{1}") |> str_remove("camp"),
    vessel_order = all_id |> str_extract("all|major"),
    sample_id = all_id |> str_extract("frex\\_\\d{2}|fasy\\_\\d{2}") |> toupper()
  ) |> 
  ungroup() |> 
  select(-all_id) 

df_area <- df_area |> 
  left_join(df_start_times, by = c("year", "campaign", "sample_id")) 

df_area <- df_area |> 
  mutate(real_time = cavicam_start_time + lubridate::minutes(5 * image_nr),
         elapsed_time_min = as.numeric(difftime(real_time, cavicam_start_time, units = "mins")))

## check for completion
xtabs(~campaign + sample_id, data = df_area)

# write.csv(df_area, "data/cavicam/df_cavitated_area.csv", row.names = F)

## 0.2 prepare water potential ----------------------------------------------

water_potential_measurements <- list.files("data/cavicam/water_potentials", pattern = "psi\\.xlsx", full.names = T)

df_water_potential <- as.data.frame(matrix(nrow = 0, ncol = 11, byrow = T))

colnames(df_water_potential) = c(water_potential_measurements[1] |> 
                                   readxl::read_excel(sheet = 2) |> colnames(), 
                                 "year", 
                                 "campaign") 

## read sheets from excel files
for(i in 1:length(water_potential_measurements)){
  n_sheets <-  water_potential_measurements[i] |> readxl::excel_sheets() |> length()
  lst <- lapply(2:n_sheets, function(j) readxl::read_excel(water_potential_measurements[i], sheet = j))
  df_temp <-  do.call(rbind.data.frame, lst)
  df_temp$year =  water_potential_measurements[i] |> str_extract("\\d{4}")
  df_temp$campaign = water_potential_measurements[i] |> str_extract("(?<=camp)\\d{1}")
  df_water_potential <- rbind.data.frame(df_water_potential, df_temp)
}

df_water_potential <- df_water_potential |> 
  clean_names() |> 
  fill(sample_id) |>   # fill missing ids  
  group_by(sample_id, year, campaign) |> 
  mutate(start_measurement = as.POSIXct(format(start_measurement, "%Y-%m-%d %H:%M:%S"), 
                                        tz = "Europe/Berlin"),
         start_equilibration = as.POSIXct(format(start_equilibration, "%Y-%m-%d %H:%M:%S"), 
                                          tz = "Europe/Berlin"),
         drying_interval = start_measurement - lag(start_measurement, 1),       # get drying interval between previous measurement and current one 
         equilibration_interval =  start_measurement - start_equilibration,     # time between taking sample and measurement
         # minutes = difftime(start_measurement, min(start_measurement), units = "mins") |> as.numeric(), # elapsed time since taking the first sample   
         psi = psi |> as.numeric()) |> 
  filter(!is.na(psi)) |>
  ungroup() 

## join start times of the cavicams to the water potential measurements
df_water_potential <- 
  df_water_potential |> 
  left_join(df_area |> select(c("sample_id", "year", "campaign", "cavicam_start_time")) |> 
              distinct(), 
            by = c("sample_id", "year", "campaign")) |> 
  mutate(elapsed_time_min = difftime(start_equilibration, cavicam_start_time, units = "mins") |> 
           as.numeric())

# write.csv(df_water_potential, "data/cavicam/df_water_potential.csv", row.names = F)

# 1. predict psi for cavicams ------------------------------------------------

# ## function
# scamfun <- function(water_potential, area){
#   mod <- scam(psi ~ s(elapsed_time_min, bs = "cr", k = 3), data = water_potential)
#   predict(mod, newdata = list(elapsed_time_min = area$elapsed_time_min))
# }
# 

## 1.1 prediction of water potentials at current percent of embolism ---------

df_area_nest <- df_area |> 
  nest(data = -c(year, campaign, sample_id, vessel_order)) |> 
  rename("area" = "data")

# df_area_nest$area[[63]]

df_water_potential_nest <- df_water_potential |> 
  nest(data = -c(year, campaign, sample_id)) |> 
  rename("water_potential" = "data")

df_wp_area <- left_join(df_area_nest, df_water_potential_nest, 
                        by = c("year", "campaign", "sample_id"))

df_wp_area <- df_wp_area |> 
  mutate(mod = map(water_potential, ~ scam(psi ~ s(elapsed_time_min, bs = "mpi", k = 8), data = .x)),
         water_potential = map2(.x = water_potential, .y = mod, ~ .x |> add_predictions(.y)),
         area = map2(area, mod, ~ mutate(.x, psi_pred = predict(.y, newdata = list(elapsed_time_min = .x$elapsed_time_min)))))

df_area <- df_wp_area |> 
  dplyr::select(c(year, campaign, sample_id, area, vessel_order)) |> 
  unnest(col = c(area)) |> 
  mutate(psi_pred_MPa = -psi_pred / 10)

# write.csv(df_area, "data/cavicam/df_area.csv", row.names = F)

## 1.2 assess scam errors ------------------------------------------------------

df_scam_errors <- df_wp_area |> 
  select(c(year, campaign, sample_id, water_potential)) |> 
  unnest(col = c(water_potential)) |>
  filter(!is.na(psi),
         !is.na(pred)) |> 
  group_by(year, campaign, sample_id) |> 
  summarise(rmse = Metrics::rmse(psi, pred) |> as.numeric(),
            mape = Metrics::mape(psi, pred) |> as.numeric()) |> 
  ungroup() 

df_scam_errors_wide <- df_scam_errors |> 
  pivot_wider(names_from = year, values_from = c(rmse, mape)) 
df_scam_errors_wide <- df_scam_errors_wide[order(df_scam_errors_wide$campaign, df_scam_errors_wide$sample_id), ]

ft.scams <- 
  df_scam_errors_wide |> 
  flextable() |> 
  set_header_df(
    mapping = data.frame(
      col_keys = c("campaign", "sample_id", "rmse_2023", "mape_2023", "rmse_2024", "mape_2024"),
      line1 = c("campaign", "sample_id", "rmse", "mape", "rmse", "mape"),
      line2 = c("campaign", "sample_id", "2023", "2023", "2024", "2024")
    ),
    key = "col_keys"
  ) |>
  merge_h(part = "header") |> # Merge horizontally in header
  merge_v(part = "header", j = c("campaign", "sample_id")) |> # Merge vertically in header
  merge_v(j = c("campaign", "sample_id")) |> # Merge vertically in body
  colformat_double(j = c("rmse_2023", "mape_2023", "rmse_2024", "mape_2024"), digits = 3) |>
  hline(i = c(8, 16, 24, 32)) |> # Horizontal lines at row indices
  # vline(j = c(1, 2, 3, 4, 5, 6)) |>
  ft_theme()
ft.scams

# ft.scams |> save_as_docx(path = "tables/scam_errors.docx")

## 1.3 plot for quality check ----

### assess scam function 

df_wp_area$water_potential[[1]]

df_wp_area |> 
  dplyr::select(c(year, campaign, sample_id, water_potential)) |> 
  unnest(col = c(water_potential)) |>
  # filter(year == "2023") |>
  ggplot(aes(x = psi, y = pred, color = sample_id)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  lims(x = c(0,100), y = c(0, 100)) +
  facet_wrap(campaign ~ year, ncol = 4) +
  plot_theme

## individual predicted ~ observed
df_wp_area |> 
  filter(year == 2023) |> 
  dplyr::select(c(year, campaign, sample_id, water_potential)) |> 
  unnest(col = c(water_potential)) |>
  # filter(year == "2023") |>
  ggplot(aes(x = psi, y = pred, color = sample_id)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  lims(x = c(0,100), y = c(0, 100)) +
  facet_wrap(campaign ~ sample_id, ncol = 8) +
  plot_theme

df_wp_area |> 
  filter(year == 2024) |> 
  dplyr::select(c(year, campaign, sample_id, water_potential)) |> 
  unnest(col = c(water_potential)) |>
  # filter(year == "2023") |>
  ggplot(aes(x = psi, y = pred, color = sample_id)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  lims(x = c(0,70), y = c(0, 70)) +
  facet_wrap(campaign ~ sample_id, ncol = 8) +
  plot_theme

## predictions over elapsed time
plot.ov_wp_preds <- df_wp_area |> 
  dplyr::select(c(year, campaign, sample_id, water_potential)) |> 
  unnest(col = c(water_potential)) |>
  mutate(year = as.factor(year),
         all_id = paste(sample_id, "campaign", campaign)) |> 
  ggplot() +
  geom_point(aes(x = elapsed_time_min, y = - psi / 10, color = year)) +
  geom_line(aes(x = elapsed_time_min, y = - pred / 10, color = year)) +
  labs(x = "Elapsed time [min]", y = "Leaf water potential [MPa]") +
  facet_wrap(~ all_id, ncol = 4) +
  thesis_theme + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 8))
plot.ov_wp_preds
ggsave("figures/cavicam/plot_ov_wp_preds.png", plot.ov_wp_preds, width = 16, height = 24, units = "cm")

plot.ov_cum_emb <- df_wp_area |> 
  dplyr::select(c(year, campaign, sample_id, vessel_order, area)) |> 
  unnest(col = c(area)) |>
  mutate(year = as.factor(year),
         all_id = paste(sample_id, "campaign", campaign)) |> 
  filter(vessel_order == "major") |> 
  ggplot() +
  geom_point(aes(x = elapsed_time_min, y = perc_area_cav, color = year)) +
  geom_line(aes(x = elapsed_time_min, y = perc_area_cav, color = year)) +
  labs(x = "Elapsed time [min]", y = "Leaf water potential [MPa]") +
  facet_wrap(~ all_id, ncol = 4, scales = "free") +
  theme_bw() +
  theme(axis.text = element_text(size = 5),
        legend.position = "bottom")
plot.ov_cum_emb
ggsave("figures/plot_ov_cum_emb", plot.ov_cum_emb,  width = 16, height = 24, units = "cm")

plot.ov_curves <- df_wp_area |> 
  dplyr::select(c(year, campaign, sample_id, vessel_order, area)) |> 
  unnest(col = c(area)) |>
  mutate(year = as.factor(year),
         all_id = paste(sample_id, "campaign", campaign)) |> 
  filter(vessel_order == "major") |> 
  ggplot() +
  geom_point(aes(x = psi_pred / 10, y = perc_area_cav, color = year)) +
  geom_path(aes(x = psi_pred / 10, y = perc_area_cav, color = year)) +
  labs(x = "Leaf water potential [MPa]", y = "Percent area embolised") +
  facet_wrap(~ all_id, ncol = 4, scales = "free") +
  theme_bw() +
  theme(axis.text = element_text(size = 5),
        legend.position = "bottom")
plot.ov_curves

# 2. model per sample -----------------------------------------

# convert PLC to relative conductance/conductivity.
plc_to_relk <- function(plc) (100 - plc)/100
relk_to_plc <- function(relK) 100 - 100*relK

# P is (positive-valued) xylem water potential (i.e. P = −Ψ)
# PX, pressure at X% loss of conductivity
# SX is the slope of the Weibull function at P = 0
# ksat, saturated hydraulic conductivity

# Relative conductance/conductivity to PLC.
# relk_to_plc <- function(relk)100 - 100*relk

## after visual inspection of curves, import lower limits for psi_pred_MPa 
## mostly within -5 to -7 MPa

## 2.1 update with minimum MPa threshold limits ----
df_vc_fit_quality <- readxl::read_xlsx("data/cavicam/vc_fit_quality.xlsx") |>
  as_tibble() |>
  select(c(sample_id, vessel_order, year, campaign, MPa_limit)) |>
  mutate(
    year = as.character(year),
    campaign = as.character(campaign),
    MPa_limit = case_when(is.na(MPa_limit) ~ -10, TRUE ~ as.numeric(MPa_limit))
  )
  
## join limits to df_sample_nest
df_sample_nest <- df_area |> 
  mutate(year = as.character(year),
         campaign = as.character(campaign)) |> 
  left_join(df_vc_fit_quality, by = c("sample_id", "vessel_order", "year", "campaign")) |>
  ## first filter > minimum MPa limit
  filter(psi_pred_MPa >= MPa_limit) |>
  mutate(
    species = substr(sample_id, 1, 4)
  ) |>
  nest(data = -c(year, species, sample_id, campaign, vessel_order))

## 2.2 update and fill ----
## update data to only include values > MPa_limit
## and fill in 0 area along psi up to first psi observation
df_sample_nest <- df_sample_nest |>
  mutate(data = map(data, ~ .x |> 
                      ## filter to only keep values > MPa_limit (lower values are not realistic)
                      filter(psi_pred_MPa > MPa_limit) |> 
                      ## sort by descending MPa
                      arrange(desc(psi_pred_MPa)) |>  
                      ## add 0 area and nd fill psi_predMPa up to first observation
                      bind_rows(data.frame(
                        psi_pred_MPa = seq(0, max(.x$psi_pred_MPa), length = 100), area = 0)) |> 
                    ## sort by descending MPa
                    arrange(desc(psi_pred_MPa))
                    ),
         ## then recalculate cumulative area and relative K
         ## under new psi_pred_MPa limits and less total area
         data = map(
           data, ~ .x |> mutate(
             area_cav = cumsum(area),
             perc_area_cav = area_cav / sum(area, na.rm = T) * 100,
             # relK = plc_to_relk(perc_area_cav),
             psi_pred_MPa = case_when(psi_pred_MPa > 0 ~ 0, psi_pred_MPa <= 0 ~ psi_pred_MPa)
           ))
         )

df_sample_nest$data[[33]] |> View()

## 2.3 model fits

### 2.3.1 fitplc ----

df_sample_nest <- df_sample_nest |>
  mutate(fit_weibull = map(data,
                   ~ try(fitplc( ## try() because some will not fit
                     dfr = .x,
                     varnames = c(PLC = "perc_area_cav", WP = "psi_pred_MPa"),
                     model = "sigmoidal",
                     quiet = FALSE,
                     nboot = 100
                   ))))

## which samples cannot be fit with the Weibull function
which(sapply(df_sample_nest$fit_weibull, typeof) != "list")
failed_Weibull_fits <- which(sapply(df_sample_nest$fit_weibull, typeof) != "list")

## update these with sigmoidal model
for(i in 1:length(failed_Weibull_fits)){
  df_sample_nest$fit_weibull[[failed_Weibull_fits[i]]] <- try(fitplc( ## try() because some will not fit
    dfr = df_sample_nest$data[[failed_Weibull_fits[i]]],
    varnames = c(PLC = "perc_area_cav", WP = "psi_pred_MPa"),
    model = "sigmoid",
    quiet = FALSE,
    nboot = 100
  ))
}

## check again
which(sapply(df_sample_nest$fit_weibull, typeof) != "list")

# => 47 and 48 cannot be fitted

## compute RMSE to assess fit quality
df_sample_nest <- df_sample_nest |>
  mutate(rmse_fitplc = map2(fit_weibull, data, ~ try(
    Metrics::rmse(actual = .y$perc_area_cav, 
                  predicted = .x$fit |> predict() |> relk_to_plc()))),
    rmse_fitplc = rmse_fitplc |> unlist() |> as.numeric())


### 2.3.2 spline model ----

df_sample_nest <- df_sample_nest |>
  mutate(
    data = map(data, ~ .x |> mutate(perc_area_cav_scaled = perc_area_cav / 100)),
    fit_mpd_spline = map(data, ~ try(scam(
        perc_area_cav_scaled ~ s(psi_pred_MPa, bs = "mpd", k = 20),
        family = binomial(),
        data = .x
     )))
  )

df_sample_nest$fit_mpd_spline
## which samples could not be fit with the "mpd" (monothonic decreasing) spline?

which(sapply(df_sample_nest$fit_mpd_spline, typeof) != "list")
failed_spline_fits <- which(sapply(df_sample_nest$fit_mpd_spline, typeof) != "list")

## update these with "cr" cubic regression spline

for(i in 1:length(failed_spline_fits)){
  df_sample_nest$fit_mpd_spline[[failed_spline_fits[i]]] <- try(scam(
    perc_area_cav_scaled ~ s(psi_pred_MPa, bs = "cs", k = 20),
    family = binomial(link = "logit"),
    data = df_sample_nest$data[[failed_spline_fits[i]]]
  ))
}

df_sample_nest$fit_mpd_spline[[52]] 

## check again
which(sapply(df_sample_nest$fit_mpd_spline, typeof) != "list")

## compute RMSE to assess fit quality
df_sample_nest <- df_sample_nest |>
  mutate(rmse_spline = map2(fit_mpd_spline, data, ~ try(
    Metrics::rmse(actual = .y$perc_area_cav, 
                  predicted = .x |> predict(type = "response") * 100))),
    rmse_spline = rmse_spline |> unlist() |> as.numeric())

df_sample_nest$rmse_spline 

## compare model fits
df_sample_nest |>
  select(year, campaign, sample_id, rmse_fitplc, rmse_spline) |>
  pivot_longer(cols = c(rmse_fitplc, rmse_spline), names_to = "model", values_to = "rmse") |>
  ggplot() +
  lims(y = c(0, 5)) +
  geom_boxplot(aes(x = model, y = rmse, fill = model)) +
  facet_wrap(year ~ campaign) +
  plot_theme

for (i in 49:nrow(df_sample_nest)) {

#   PX_test = coef(df_sample_nest$fit_weibull[[i]])[2, 1]
#   SX_test = coef(df_sample_nest$fit_weibull[[i]])[1, 1]
#   
#   
#   df_sample_nest$data[[i]] <- df_sample_nest$data[[i]] |>
#     mutate(plc_pred_spline = predict(df_sample_nest$fit_mpd_spline[[i]], type = "response") * 100,
#            plc_pred_plcfit = relk_to_plc(fweibull(-df_sample_nest$data[[i]]$psi_pred_MPa, SX_test, PX_test, X = 50)))
#   print(i)
# }

## 2.4 extract parameters ----

### 2.4.1 fitplc ----
## save coefficitents for P12, P50, P88
df_sample_nest_params <- df_sample_nest |>
  filter(year != 2024 | campaign != 1 | sample_id != "FREX_05") |>
  mutate(fitplc_params = map(fit_weibull,
                     ~ try(getPx(object = .x , x=c(12, 50, 88)))
  ))


## can't fit model for FREX_05_2024_1
## set to empty dataframe
# df_sample_nest$fitplc_params[[47]] = data.frame()
# df_sample_nest$fitplc_params[[48]] = data.frame()

## extract coefficients and confidence intervals
df_sample_nest_params <- df_sample_nest_params |>
  mutate(fitplc_params = map(fitplc_params, ~ .x |> 
                               pivot_wider(names_from = "x", 
                                           values_from = c("Px","2.5%", "97.5%")) |> 
                               rename("p12_fitplc" = "Px_12",
                                      "p50_fitplc" = "Px_50",
                                      "p88_fitplc" = "Px_88",
                                      "p12_lwr" = "2.5%_12",
                                      "p12_upr" = "97.5%_12",
                                      "p50_lwr" = "2.5%_50",
                                      "p50_upr" = "97.5%_50",
                                      "p88_lwr" = "2.5%_88",
                                      "p88_upr" = "97.5%_88"
                               )
                             ))

df_sample_nest_params |> names()

### 2.4.2 spline model ----

## get spline model p12, p50 and p88
f.find_px_spline <- function(psi_vec, model_preds, X, spline_model) {
  # Use uniroot to find the x-value (psi_pred_MPa) where y equals newy
  root_function <- function(psi_vec, new_y) {
    predict(spline_model, newdata = data.frame(psi_pred_MPa = psi_vec), type = "response") - X
  }
  # Provide the interval explicitly within the observed data range
  interval <- range(psi_vec)
  
  new_x <- uniroot(root_function, interval = interval, new_y = X)$root
  return(new_x)
}

df_sample_nest_params <- df_sample_nest_params |> 
  mutate(p12_spline = map2(data, fit_mpd_spline, ~ 
                      f.find_px_spline(psi_vec = .x$psi_pred_MPa, model_preds = predict(.y),
                                       X = 0.12, spline_model = .y)) |> unlist() |> as.numeric(),
    p50_spline = map2(data, fit_mpd_spline, ~ 
                 f.find_px_spline(psi_vec = .x$psi_pred_MPa, model_preds = predict(.y),
                                  X = 0.50, spline_model = .y)) |> unlist() |> as.numeric(),
    p88_spline = map2(data, fit_mpd_spline, ~ 
      f.find_px_spline(psi_vec = .x$psi_pred_MPa, model_preds = predict(.y),
                       X = 0.88, spline_model = .y)) |> unlist() |> as.numeric()
  )

df_ov_params <- df_sample_nest_params |> 
  select(-c(data, fit_weibull, fit_mpd_spline)) |>
  mutate(fitplc_params = map(fitplc_params, ~ .x |> 
                               mutate(across(everything(), ~ -1 * .)))) |>
  unnest(cols = c(fitplc_params)) 

View(df_ov_params)

df_ov_params |> 
  ggplot() +
  geom_density(aes(x = p50_fitplc, fill = "spline"), alpha = 0.5, binwidth = 0.25) +
  geom_density(aes(x = p50_spline, fill = "weibull"), alpha = 0.5, binwidth = 0.25) +
  ggtitle("P50 fit comparison") +
  thesis_theme

df_ov_params |> 
  ggplot() +
  geom_density(aes(x = p12_fitplc, fill = "spline"), alpha = 0.5, binwidth = 0.25) +
  geom_density(aes(x = p12_spline, fill = "weibull"), alpha = 0.5, binwidth = 0.25) +
  ggtitle("P12 fit comparison") +
  thesis_theme

df_ov_params |>
  ggplot() +
  geom_density(aes(x = p88_fitplc, fill = "spline"), alpha = 0.5, binwidth = 0.25) +
  geom_density(aes(x = p88_spline, fill = "weibull"), alpha = 0.5, binwidth = 0.25) +
  ggtitle("P88 fit comparison") +
  thesis_theme


## plot and save individual fits for visual analysis ---- 
i = 73
for (i in 69:nrow(df_sample_nest)) {
  title = paste(df_sample_nest$sample_id[[i]],
                df_sample_nest$vessel_order[[i]],
                df_sample_nest$year[[i]],
                df_sample_nest$campaign[[i]],
                sep = "_")
  
  PX_test = coef(df_sample_nest$fit_weibull[[i]])[2, 1]
  SX_test = coef(df_sample_nest$fit_weibull[[i]])[1, 1]
  
  df_sample_nest$data[[i]] <- df_sample_nest$data[[i]] |>
    mutate(plc_pred_spline = predict(df_sample_nest$fit_mpd_spline[[i]], type = "response") * 100,
           plc_pred_plcfit = relk_to_plc(fweibull(-df_sample_nest$data[[i]]$psi_pred_MPa, SX_test, PX_test, X = 50)))
  
  df_sample_nest$data[[i]] |>
    ggplot() +
    geom_point(aes(x = psi_pred_MPa, y = perc_area_cav)) +
    geom_line(aes(x = psi_pred_MPa, y = plc_pred_spline, col = "spline"), lwd = 1) +
    geom_line(aes(x = psi_pred_MPa, y = plc_pred_plcfit, col = "sigmoidal"), lwd = 1) +
    lims(x = c(0, -10))+
    ## print RMSE
    annotate("text", x = -1, y = 100, size = 2, label = 
               paste("RMSE spline: ", 
                     round(Metrics::rmse(
                       df_sample_nest$data[[i]]$perc_area_cav, 
                       df_sample_nest$data[[i]]$plc_pred_spline), 2))) +
    annotate("text", x = -1, y = 90, size = 2, label = 
               paste("RMSE plcfit: ", 
                     round(Metrics::rmse(
                       df_sample_nest$data[[i]]$perc_area_cav, 
                       df_sample_nest$data[[i]]$plc_pred_plcfit), 2))) +
    guides(colour = guide_legend(title = "model")) +
    labs(y = "Embolised area [%]", x = "Predicted water potential [MPa]") +
    # ggtitle(title) +
    thesis_theme + 
    theme(legend.text = element_text(size = 8),
          legend.title = element_text(size = 8),
          axis.title.x =  element_text(size = 8),
          axis.title.y =  element_text(size = 8))
  ggsave(paste0("data/cavicam/plots/spline_fits/", title, ".png"), width = 8, height = 6, dpi = 150, units = "cm")
  print(i)
}

write.excel(paste(df_sample_nest$sample_id,
                  df_sample_nest$vessel_order,
                  df_sample_nest$year,
                  df_sample_nest$campaign,
                  sep = "_"))

df_sample_nest$data[[71]] |> 
  ggplot() +
  geom_point(aes(x = psi_pred_MPa, y = perc_area_cav))

## solve spline for 50% embolism
df_sample_nest <- df_sample_nest |>
  mutate(p50_spline = map(data, ~ predict(df_sample_nest$fit_mpd_spline[[1]], newdata = .x, type = "response") * 100))



df_sample_nest2 = df_sample_nest |> 
  dplyr::select(sample_id, vessel_order, year, campaign, data) |> 
  filter(vessel_order == "major") |> 
  unnest(data)


plot.ov_curves <- df_sample_nest2 |> 
  mutate(year = as.factor(year),
         all_id = paste(sample_id, "campaign", campaign)) |> 
  ggplot() +
  geom_point(aes(x = psi_pred_MPa, y = perc_area_cav, col = year), size = 0.5, alpha = 0.3) +
  geom_line(aes(x = psi_pred_MPa, y = plc_pred_spline, col = year)) +
  lims(y = c(0, 100), x = c(0, -10)) +
  scale_x_continuous(breaks = c(0, -2, -4, -6, -8, -10)) +
  labs(y = "Embolised area [%]", x = "Predicted water potential [MPa]") +
  facet_wrap(~ all_id, ncol = 4) +
  # see::scale_color_oi(order = c(1,2,3,6)) +
  thesis_theme +
  theme(strip.background = element_blank())
plot.ov_curves
ggsave("figures/cavicam/spline_fits_major_vessels.png", plot.ov_curves,
       width = 15.5, height = 20, dpi = 150, units = "cm")


# 3. export ---------------------------------------------------------------
df_ov_params <- df_ov_params |> 
  mutate(date = case_when(campaign == "1" & year == "2023" ~ "2023-06-13",
                          campaign == "1" & year == "2024" ~ "2024-05-28",
                          campaign == "2" & year == "2023" ~ "2023-07-17",
                          campaign == "2" & year == "2024" ~ "2024-07-09",
                          campaign == "3" & year == "2023" ~ "2023-08-09",
                          campaign == "3" & year == "2024" ~ "2024-08-13",
                          campaign == "4" & year == "2023" ~ "2023-09-18",
                          campaign == "4" & year == "2024" ~ "2024-09-23"),
         date_plot = date |> str_replace_all("2023|2024", "2000") |> as.Date("%Y-%m-%d"))

write.csv(df_ov_params, "data/calculated_parameters/df_ov_params.csv", row.names = F)


## fit splines to vulnerability curves -------------------------------------

df_sample_nest <- df_sample_nest |>
  mutate(
    data = map(data, ~ .x |> mutate(perc_area_cav_scale = perc_area_cav / 100)),
    mod_splinefit = try(map(data, ~ scam(
      perc_area_cav_scale ~ s(psi_pred_MPa, bs = "mpd", k = 10), data = .x,
    family = binomial(link = "logit"))
  ))) 


## test area for single curve

## weibull function from fitplc package code
fweibull <- function(P, SX, PX, X=50){
  X <- X[1] # when fitting; vector may be passed but X cannot actually vary.
  V <- (X-100)*log(1-X/100)
  p <- (P/PX)^((PX*SX)/V)
  relk <- (1-X/100)^p
  return(relk)
}

df_sample_nest$data[[curve]] |> View()

curve = 18
test <- df_sample_nest$data[[curve]] 
test_curve_id <- paste(df_sample_nest$sample_id[[curve]],
                       df_sample_nest$vessel_order[[curve]],
                       df_sample_nest$year[[curve]],
                       df_sample_nest$campaign[[curve]],
                       sep = "_")
test_curve_id
test |> 
  ggplot() +
  geom_line(aes(x = psi_pred_MPa, y = perc_area_cav)) +
  ggtitle(test_curve_id)

# Ensure the response is between 0 and 1
test <- test |> 
  mutate(perc_area_cav_scaled = perc_area_cav / 100)

# Fit the SCAM model using the logit link for a binomial response 
test_mod <- scam(perc_area_cav_scaled ~ s(psi_pred_MPa, bs = "mpd", k = 20),
                 family = binomial(), data = test)


## get predictions from plcfit
plc_fit_test <- fitplc(
  dfr = test,
  varnames = c(PLC = "perc_area_cav", WP = "psi_pred_MPa"),
  model = "sigmoid",
  nboot = 100,
  msMaxIter = 500,
  maxIter = 100,
  control = nls.control(maxiter = 500, tol = 1e-6)
)

PX_test = coef(plc_fit_test)[2, 1]
SX_test = coef(plc_fit_test)[1, 1]

# Add predictions to the dataset
test <- test |> 
  mutate(plc_pred_spline = predict(test_mod, newdata = test, type = "response") * 100,
         plc_pred_plcfit = relk_to_plc(fweibull(-psi_pred_MPa, SX_test, PX_test, X = 50)))

test |> 
  ggplot() +
  geom_point(aes(x = psi_pred_MPa, y = perc_area_cav)) +
  geom_line(aes(x = psi_pred_MPa, y = plc_pred_spline, col = "Spline"), lwd = 1) +
  geom_line(aes(x = psi_pred_MPa, y = plc_pred_plcfit, col = "PLCfit"), lwd = 1) +
  # geom_line(aes(x = psi_pred_MPa, y = plc_pred_cobs, col = "COBS"), lwd = 1) +
  lims(x = c(0, -10)) +
  ## print RMSE
  annotate("text", x = -3, y = 100, label = 
             paste("RMSE spline: ", 
                   round(Metrics::rmse(test$perc_area_cav, test$plc_pred_spline), 2))) +
  annotate("text", x = -3, y = 90, label =
             paste("RMSE plcfit: ", 
                   round(Metrics::rmse(test$perc_area_cav, test$plc_pred_plcfit), 2))) +
  ggtitle(test_curve_id) +
  thesis_theme

# for(i in 1:nrow(df_sample_nest)){
  test <- df_sample_nest$data[[i]] 
  test_curve_id <- paste(df_sample_nest$sample_id[[i]],
                         df_sample_nest$vessel_order[[i]],
                         df_sample_nest$year[[i]],
                         df_sample_nest$campaign[[i]],
                         sep = "_")
  test |> 
    ggplot() +
    geom_line(aes(x = psi_pred_MPa, y = perc_area_cav)) +
    ggtitle(test_curve_id)
  # Ensure the response is between 0 and 1
  test <- test |> 
    mutate(perc_area_cav_scaled = perc_area_cav / 100)
  # Fit the SCAM model using the logit link for a binomial response 
  test_mod <- scam(perc_area_cav_scaled ~ s(psi_pred_MPa, bs = "mpd", k = 10),
                   family = binomial(), data = test,
                   control = gam.control(trace = TRUE))
  ## get predictions from plcfit
  plc_fit_test <- df_sample_nest$mod[[i]]
  
  PX_test = coef(plc_fit_test)[2, 1]
  SX_test = coef(plc_fit_test)[1, 1]
  
  # Add predictions to the dataset
  test <- test |> 
    mutate(plc_pred_spline = predict(test_mod, newdata = test, type = "response") * 100,
           plc_pred_plcfit = relk_to_plc(fweibull(-psi_pred_MPa, SX_test, PX_test, X = 50)))
  
  test |> 
    ggplot() +
    geom_point(aes(x = psi_pred_MPa, y = perc_area_cav)) +
    geom_line(aes(x = psi_pred_MPa, y = plc_pred_spline, col = "Spline"), lwd = 1) +
    geom_line(aes(x = psi_pred_MPa, y = plc_pred_plcfit, col = "PLCfit"), lwd = 1) +
    # geom_line(aes(x = psi_pred_MPa, y = plc_pred_cobs, col = "COBS"), lwd = 1) +
    lims(x = c(0, -10)) +
    ## print RMSE
    annotate("text", x = -1, y = 100, label = 
               paste("RMSE spline: ", 
                     round(Metrics::rmse(test$perc_area_cav, test$plc_pred_spline), 2))) +
    annotate("text", x = -1, y = 90, label =
               paste("RMSE plcfit: ", 
                     round(Metrics::rmse(test$perc_area_cav, test$plc_pred_plcfit), 2))) +
    ggtitle(test_curve_id) +
    thesis_theme
  ggsave(paste0("data/cavicam/plots/spline_fits/", test_curve_id, ".png"))
  print(i)
}


## assess fit quality again
 
# library(cobs)
# 
# # Fit a monotonic decreasing constrained spline
# cobs_mod <- cobs(x = test$psi_pred_MPa, y = test$perc_area_cav_scaled, 
#                  constraint = "decrease", degree = 2)

## vulnerability curves to fix ---------------------------------------------

data.frame(df_sample_nest$sample_id,
     df_sample_nest$vessel_order,
     df_sample_nest$year,
     df_sample_nest$campaign) |> write.excel()

i = 47
df_sample_nest$data[[i]] |>
  bind_rows(data.frame(psi_pred_MPa = seq(0, min(df_sample_nest$data[[i]]$psi_pred_MPa), length = 100), area = 0)) |>   
  filter(psi_pred_MPa > -6) |>
  arrange(desc(psi_pred_MPa)) |> ## order by psi_pred_MPa
  mutate(
    # area = case_when(psi_pred_MPa > - 1 ~ 0,
    #                  psi_pred_MPa <= - 1 ~ area),
    area_cav = cumsum(area),
    perc_area_cav = area_cav / sum(area) * 100,
    relK = plc_to_relk(perc_area_cav),
    psi_pred_MPa = case_when(psi_pred_MPa > 0 ~ 0,
                             psi_pred_MPa <= 0 ~ psi_pred_MPa)
  ) |> 
fitplc(
  varnames = c(PLC = "perc_area_cav", WP = "psi_pred_MPa"),
  # x = 50,
  model = "sigmoid",
  nboot = 100
) |> plot(main = paste(df_sample_nest$sample_id[[i]],
                        df_sample_nest$vessel_order[[i]],
                        df_sample_nest$year[[i]],
                        df_sample_nest$campaign[[i]],
                        sep = "_"))

df_sample_nest$data[[i]] |>
  # filter(psi_pred_MPa > -7) |>
  mutate(
    area_cav = cumsum(area),
    perc_area_cav =  area_cav / sum(area),
    relK = plc_to_relk(perc_area_cav),
    psi_pred_MPa = case_when(psi_pred_MPa > 0 ~ 0,
                             psi_pred_MPa <= 0 ~ psi_pred_MPa)
  ) |> 
  ggplot(aes(x = psi_pred_MPa, y = perc_area_cav)) +
  geom_point()
df_sample_nest$data[[i]] |> View()

df_sample_nest <- df_sample_nest |>
  mutate(mod_nls = map(data,
                       nlme_p50 = map2(data, nls_p50, ~ try(nlme(
                         relK ~ f.weibull_plc(-psi_pred_MPa, SX, PX, X = 50),
                         data = .x,
                         fixed = list(SX ~ 1, PX ~ 1),
                         random = SX + PX ~ 1 | campaign,
                         # start = coef(.y),
                         start = c(SX = 40, PX = 4),
                         control = nlmeControl(maxiter = 1000, msMaxIter = 2000)
                       ), silent = TRUE))))

df_sample_nest$mod[[1]]

f.weibull_plc


# old ---------------------------------------------------------------------

df_sample_nest <- df_area |> 
  mutate(
    perc_area_cav = perc_area_cav * 100,
    # relK = plc_to_relk(perc_area_cav),
    species = substr(sample_id, 1, 4),
    ## set psi_pred_MPa to 0 for erroneous values > 0 
    psi_pred_MPa = case_when(psi_pred_MPa > 0 ~ 0,
                             psi_pred_MPa <= 0 ~ psi_pred_MPa)
  ) |>  
  # filter(psi_pred_MPa > -10,
  #        # sample_id != "FREX_05" | campaign != "1" | year != "2024"
  # ) |>
  nest(data = -c(year, species, sample_id, campaign, vessel_order))


## individual models per sample_id per campaign
df_sample_nest <- df_sample_nest |>
  mutate(mod = map(data,
                   ~ try(fitplc(
                     dfr = .x,
                     varnames = c(PLC = "perc_area_cav", WP = "psi_pred_MPa"),
                     # x = 50,
                     model = "sigmoid",
                     nboot = 100
                   ))))



# drm fits ----------------------------------------------------------------

plc_to_relk <- function(plc) (100 - plc)/100

test_data <- df_area |> 
  filter(year == "2024", sample_id == "FASY_04", vessel_order == "all") |> 
  mutate(relk = plc_to_relk(perc_area_cav*100),
         curve_id = paste(sample_id, campaign, sep = "_"))

ggplot(test_data, aes(x = abs(psi_pred_MPa), y = relk)) +
  geom_point() 

library(drc)
mod1 <- drm(relk ~ abs(psi_pred_MPa), curve_id, data = test_data, fct = W1.4())
plot(mod1)

ED(mod1, c(50, 12, 88), interval = "delta")

all_data <- df_area |> 
  mutate(species = substr(sample_id, 1, 4)) |> 
  filter(year == "2024", vessel_order == "all", species == "FASY") |> 
  mutate(relk = plc_to_relk(perc_area_cav*100),
         curve_id = paste(sample_id, campaign, sep = "_"))

mod2 <- drm(relk ~ abs(psi_pred_MPa), curve_id, data = all_data, fct = W1.4())

ED(mod2, c(50, 12, 88), interval = "delta")


# nlme for plc fit --------------------------------------------------------

library(nlme)

f.weibull_plc <- function (P, SX, PX, X = 50) 
{
  X <- X[1]
  V <- (X - 100) * log(1 - X/100)
  p <- (P/PX)^((PX * SX)/V)
  relk <- (1 - X/100)^p
  return(relk)
}

# convert PLC to relative conductance/conductivity.
plc_to_relk <- function(plc) (100 - plc)/100
relK_to_plc <- function(relK) 100 - 100*relK

# P is (positive-valued) xylem water potential (i.e. P = −Ψ)
# PX, pressure at X% loss of conductivity
# SX is the slope of the Weibull function at P = 0
# ksat, saturated hydraulic conductivity

# Relative conductance/conductivity to PLC.
# relk_to_plc <- function(relk)100 - 100*relk

# nested nlme per species -------------------------------------------

df_mod_nest <- df_area |>
  mutate(
    perc_area_cav = perc_area_cav * 100,
    relK = plc_to_relk(perc_area_cav),
    species = substr(sample_id, 1, 4),
    psi_pred_MPa = case_when(psi_pred_MPa > 0 ~ 0,
                             psi_pred_MPa <= 0 ~ psi_pred_MPa)
  ) |>  
  filter(psi_pred_MPa > -8) |>
  nest(data = -c(year, species, vessel_order))

df_mod_nest <- df_sample_nest |> 
  mutate(nls_p50 = map(data, ~ try(nls(relK ~ f.weibull_plc(-psi_pred_MPa, SX, PX, X = 50),
                                       data = .x,
                                       start = c(SX = 40, PX = 5),
                                       control = nlmeControl(maxiter = 1000, msMaxIter = 2000)))),
         nlme_p50 = map2(data, nls_p50, ~ try(nlme(
           relK ~ f.weibull_plc(-psi_pred_MPa, SX, PX, X = 50),
           data = .x,
           fixed = list(SX ~ 1, PX ~ 1),
           random = SX + PX ~ 1 | campaign,
           # start = coef(.y),
           start = c(SX = 40, PX = 4),
           control = nlmeControl(maxiter = 1000, msMaxIter = 2000)
         ), silent = TRUE)))

df_mod_nest$nls_p50[[1]]
df_mod_nest$nlme_p50[[1]]

df_sample_nest$mod[[1]] |> coef()


## extract random effects to display as +- sd() around P50
ranef(df_mod_nest$nlme_p50[[6]])
VarCorr(df_mod_nest$nlme_p50[[6]])
df_mod_nest$nlme_p50[[6]] |> coef() |> pull("PX")


df_mod_nest <- df_mod_nest |> 
  mutate(p50 = purrr::map(nlme_p50, ~ coef(.x)$PX),
         ranef_sd_p50 = map(nlme_p50, ~ ranef(.x) |> pull(PX) |> unlist() |> sd())|> unlist() |> as.numeric())

df_p50 <- df_mod_nest |> 
  mutate(p50 = map(nlme_p50, ~ coef(.x)$PX)) |> 
  dplyr::select(-c(data, nls_p50, nlme_p50)) |> 
  unnest(cols = p50) |> 
  mutate(campaign = c(1:4, 1:4, 3:4, 3:4, 1:4, 1:4, 1:4, 1:4))

df_p50 |> write.csv("data/cavicam/df_p50.csv", row.names = F)
df_p50 |> write.excel()

df_p50_summ <- df_mod_nest |> 
  mutate(p50 = map(nlme_p50, ~ - coef(.x)$PX)) |> 
  dplyr::select(-c(data, nls_p50, nlme_p50)) |> 
  unnest(cols = c(p50, ranef_sd_p50)) |> 
  mutate(campaign = c(1:4, 1:4, 3:4, 3:4, 1:4, 1:4, 1:4, 1:4)) |> 
  mutate(date = case_when(campaign == "1" & year == "2023" ~ "2023-06-13",
                          campaign == "1" & year == "2024" ~ "2024-05-28",
                          campaign == "2" & year == "2023" ~ "2023-07-19",
                          campaign == "2" & year == "2024" ~ "2024-07-09",
                          campaign == "3" & year == "2023" ~ "2023-08-11",
                          campaign == "3" & year == "2024" ~ "2024-08-13",
                          campaign == "4" & year == "2023" ~ "2023-09-18",
                          campaign == "4" & year == "2024" ~ "2024-09-23"))

df_p50_summ$date = df_p50_summ$date |> str_replace_a




