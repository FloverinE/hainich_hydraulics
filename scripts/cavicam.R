# Setup -------------------------------------------------------------------
{
  library(tidyverse)
  # library(openxlsx)
  library(scam)
  library(janitor)
  
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
  plot_theme <- theme_bw() +
    theme(
      legend.position = "bottom",
      legend.spacing.x = unit(0.7, 'cm'),
      aspect.ratio = 1,
      axis.text = element_text(size = 18),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 13),
      legend.key.width = unit(1.5, "cm", data = NULL),
      # panel.grid = element_line(
      #   color = "grey",
      #   size = 0.25,
      #   linetype = 1
      # )
    )
}

# load data ---------------------------------------------------------------
{
  df_water_potential <- read.csv("data/cavicam/df_water_potential.csv")
  
  df_area <- read.csv("data/cavicam/df_cavitated_area.csv")
}


# 0. data preparation --------------------------------------------------------


## 0.0 prepare cavicam starting times --------------------------------------

df_start_times <- readxl::read_excel("data/cavicam/cavicam_start_times.xlsx")

df_start_times <- df_start_times |> 
  mutate(start_time = as.POSIXct(time, format = "%Y%m%d-%H%M%S"),
         year = as.character(year),
         campaign = as.character(campaign),
         sample_id = as.character(sample_id))

## 0.1 prepare cavitated area --------------------------------------------------

files <- list.files("data/cavicam/image_areas", pattern = "\\.csv", full.names = T, recursive = T) 

df_area <- files %>%
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
  ) 

df_area <- df_area |> 
  left_join(df_start_times, by = c("year", "campaign", "sample_id")) 

df_area <- df_area|> 
  mutate(time = start_time + lubridate::minutes(5 * image_nr),
         elapsed_time_min = as.numeric(difftime(time, start_time, units = "mins")))



## 2023
df_camp1_2023 <- df_area |>
  mutate(campaign = "1",
         year = 2023,
         date = "2023-06-14")

df_camp2_2023 <- df_area |>
  mutate(campaign = "2",
         year = 2023,
         date = "2023-07-19")

df_camp3_2023 <- df_area |>
  mutate(campaign = "3",
         year = 2023,
         date = "2023-08-10")

df_camp4_2023 <- df_area |>
  mutate(campaign = "4",
         year = 2023,
         date = "2023-08-10")

## 2024

df_camp1_2024 <- df_area |>
  mutate(campaign = "1",
         year = 2024,
         date = "2024-05-19")

df_camp2_2024 <- df_area |>
  mutate(campaign = "2",
         year = 2024,
         date = "2024-07-10")

df_camp3_2024 <- df_area |>
  mutate(campaign = "3",
         year = 2024,
         date = "2024-08-13")

df_camp4_2024 <- df_area |>
  mutate(campaign = "4",
         year = 2024,
         date = "2024-09-23")

df_area_all <- rbind(
  df_camp1_2023,
  df_camp2_2023,
  df_camp3_2023,
  df_camp4_2023,
  df_camp1_2024,
  df_camp2_2024,
  df_camp3_2024,
  df_camp4_2024
) |> 
  mutate(year = as.factor(year),
         campaign = as.factor(campaign),
         vessel_order = as.factor(vessel_order),
         sample_ID = as.factor(sample_ID))


df_area_all  |> 
  filter(vessel_order == "all") |> 
  ggplot() +
  geom_path(aes(x = minutes, y = perc_area_cav, group = sample_ID, color = sample_ID)) +
  theme_bw() +
  facet_wrap(~ year)


write.csv(df_area_all, "data/cavicam/df_cavitated_area.csv", row.names = F)

df_area <- df_area |>
  group_by(all_id, vessel_order) |>
  mutate(id = row_number(),
         minutes =  id * 5 - 5) |>
  ungroup() 

write.csv(df_area, "data/cavicam/df_cavitated_area.csv", row.names = F)


# df_area <- df_area |> 
#   filter(year != "2024" & campaign != "4")
# 
# df_area <- rbind.data.frame(df_area, df_camp4_2024)

## 0.2 prepare water potential ----------------------------------------------

water_potential_measurements <- list.files("data/cavicam/water_potentials", pattern = "psi\\.xlsx", full.names = T)

df_water_potential <- as.data.frame(matrix(nrow = 0, ncol = 11, byrow = T))
colnames(df_water_potential) = c(water_potential_measurements[1] |> readxl::read_excel(sheet = 2) |> colnames(), 
                                 "year", 
                                 "campaign")

for(i in 1:length(water_potential_measurements)){
  n_sheets <-  water_potential_measurements[i] |> readxl::excel_sheets() |> length()
  lst <- lapply(2:n_sheets, function(j) readxl::read_excel(water_potential_measurements[i], sheet = j))
  df_temp <-  do.call(rbind.data.frame, lst)
  df_temp$year =  water_potential_measurements[i] |> str_extract("\\d{4}")
  df_temp$campaign = water_potential_measurements[i] |> str_extract("(?<=camp)\\d{1}")
  df_water_potential <- rbind.data.frame(df_water_potential, df_temp)
}


df_water_potential <- df_water_potential |> 
  fill(sample_ID) |>                                                            # fill missing ids
  group_by(sample_ID, year, campaign) |> 
  mutate(drying_interval = start_measurement - lag(start_measurement, 1),       # get drying interval between previous measurement and current one 
         equilibration_interval =  start_measurement - start_equilibration,     # time between taking sample and measurement
         minutes = difftime(start_measurement, min(start_measurement), units = "mins") |> as.numeric(), # elapsed time since taking the first sample   
         psi = psi |> as.numeric()) |> 
  ungroup()

write.csv(df_water_potential, "data/cavicam/df_water_potential.csv", row.names = F)


# Predict psi for cavicams ------------------------------------------------

## function
scamfun <- function(water_potential, area){
  mod <- scam(psi ~ s(minutes, bs = "cr", k = 3), data = water_potential)
  predict(mod, newdata = list(minutes = area$minutes))
}

df_area_nest <- df_area |> 
  mutate(all_id = paste(year, campaign, sample_ID, sep = "_")) |> 
  nest(data = -c(all_id, vessel_order)) |> 
  rename("area" = "data")

df_water_potential_nest <- df_water_potential |> 
  mutate(all_id = paste(year, campaign, sample_ID, sep = "_")) |> 
  nest(data = -c(all_id, campaign)) |> 
  rename("water_potential" = "data")

df_all <- left_join(df_area_nest, df_water_potential_nest, by = "all_id")

df_all <- df_all |> 
  mutate(psi_pred = map2(water_potential, area, scamfun),
         area = map2(area, psi_pred, ~mutate(.x, psi_pred = .y)))

df_area <- df_all |> 
  dplyr::select(c(all_id, area, vessel_order)) |> 
  unnest() |> 
  mutate(psi_pred_MPa = -psi_pred / 10,
         campaign = str_extract(all_id, "(?<=\\_)\\d"))

## all vessels
cavi_psi_pred_all_vessels.png <- df_area |>
  filter(vessel_order == "all",
         year == "2024") |> 
  ggplot() +
  geom_line(aes(
    x = psi_pred_MPa,
    y = perc_area_cav,
    group = all_id,
    color = campaign),
    linewidth = 0.75) +
  xlim(-7, 0) +
  facet_wrap(~ sample_ID, ncol = 4) +
  ylab("Percent area cavitated") +
  xlab("Water potential (MPa)") +
  plot_theme
cavi_psi_pred_all_vessels.png
ggsave("plots/cavi_psi_pred_all_vessels.png", width = 8, height = 6)

plot(perc_area_cav ~ psi_pred_MPa,
     data = df_area |> filter(vessel_order == "all", all_id == "2024_4_FREX_06", year == "2024"), type = "l")

ggplot(df_area_clean ) +
  geom_line(aes(x = psi_pred_MPa, y = perc_area_cav, group = all_id, colour = all_id))

## major vessels only
cavi_psi_pred_major_vessels.png <- df_area |>
  filter(vessel_order == "major",
         year == "2024") |> 
  ggplot() +
  geom_path(aes(
    x = psi_pred_MPa,
    y = perc_area_cav,
    group = all_id,
    color = campaign),
    linewidth = 0.75) +
  # xlim(-7, 0) +
  facet_wrap(~ sample_ID, ncol = 4) +
  ylab("Percent area cavitated") +
  xlab("Water potential (MPa)") +
  plot_theme
cavi_psi_pred_major_vessels.png
ggsave("plots/cavi_psi_pred_major_vessels.png", width = 8, height = 6)

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
    species = substr(sample_ID, 1, 4),
    psi_pred_MPa = case_when(psi_pred_MPa > 0 ~ 0,
                             psi_pred_MPa <= 0 ~ psi_pred_MPa)
  ) |>  
  filter(psi_pred_MPa > -8) |>
  nest(data = -c(year, species, vessel_order))

df_mod_nest <- df_mod_nest |> 
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

df_p50_summ$date = df_p50_summ$date |> str_replace_all("2023|2024", "2000") |> 
  as.Date("%Y-%m-%d")

## plot --------------------------------------------------------------------

df_p50_summ |> 
  ggplot() +
  geom_point(aes(x = date, y = p50, 
                 group = interaction(year, species), color = as.factor(year)), 
             position = position_dodge(width = 0.2), size = 2) +
  geom_errorbar(aes(x = date, ymin = p50 - ranef_sd_p50, ymax = p50 + ranef_sd_p50, color = as.factor(year)), 
                position = position_dodge(width = 0.2), linewidth = 1, width = 0) +
  scale_color_discrete("Year") +
  facet_wrap(species ~ vessel_order) +
  ylab("P50 [MPa]") +
  xlab("Date") +
  ggtitle("mean P50 per species and vessel order per campaign",
          subtitle = "mean and standard deviation of random effects on campaign") +
  plot_theme +
  theme(aspect.ratio = 0.5)

# nested nlme per sample/campaign -----------------------------------------

df_sample_nest <- df_area |> 
  mutate(
    perc_area_cav = perc_area_cav * 100,
    relK = plc_to_relk(perc_area_cav),
    species = substr(sample_ID, 1, 4),
    psi_pred_MPa = case_when(psi_pred_MPa > 0 ~ 0,
                             psi_pred_MPa <= 0 ~ psi_pred_MPa)
  ) |>  
  filter(psi_pred_MPa > -8) |>
  nest(data = -c(year, species, sample_ID, campaign, vessel_order))

library(fitplc)

## individual models per sample_id per campaign

df_sample_nest <- df_sample_nest |>
  mutate(mod_p50 = map(data,
                       ~ fitplc(
                         dfr = .x,
                         varnames = c(PLC = "perc_area_cav", WP = "psi_pred_MPa"),
                         x = 50
                         model = "sigmoid",
                         nboot = 100,
                         start = c(SX = 40, PX = 5),
                         control = nls.control(maxiter = 1000)
                       )
  ))

## P12
df_sample_nest <- df_sample_nest |>
  mutate(mod_p12 = map(data,
                       ~ fitplc(
                         dfr = .x,
                         varnames = c(PLC = "perc_area_cav", WP = "psi_pred_MPa"),
                         x = 12,
                         model = "sigmoid",
                         nboot = 100,
                         start = c(SX = 40, PX = 5),
                         control = nls.control(maxiter = 1000)
                       )
  ))

## P50
df_sample_nest <- df_sample_nest |>
  mutate(mod_p88 = map(data,
                       ~ fitplc(
                         dfr = .x,
                         varnames = c(PLC = "perc_area_cav", WP = "psi_pred_MPa"),
                         x = 88,
                         model = "sigmoid",
                         nboot = 100,
                         start = c(SX = 40, PX = 5),
                         control = nls.control(maxiter = 1000)
                       )
  ))

## extract coefficients and confidence intervals

df_sample_nest <- df_sample_nest |>
  mutate(p50 = map(mod_p50, ~ coef(.x)[2, 1]),
         p50_lwr = map(mod_p50, ~ coef(.x)[2, 2]),
         p50_upr = map(mod_p50, ~ coef(.x)[2, 3]),
         p12 = map(mod_p12, ~ coef(.x)[2, 1]),       
         p12_lwr = map(mod_p12, ~ coef(.x)[2, 2]),
         p12_upr = map(mod_p12, ~ coef(.x)[2, 3]),
         p88 = map(mod_p88, ~ coef(.x)[2, 1]),
         p88_lwr = map(mod_p88, ~ coef(.x)[2, 2]),
         p88_upr = map(mod_p88, ~ coef(.x)[2, 3])
  )

coef(df_sample_nest$mod_p50[[1]])

## unnest

df_ov_params <- df_sample_nest |> 
  select(-c(mod_p50, mod_p12, mod_p88, data)) |>
  unnest(cols = c(p50, p50_lwr, p50_upr, p12, p12_lwr, p12_upr, p88, p88_lwr, p88_upr)) 

# names(df_ov_params) <- tolower(names(df_ov_params))
write.csv(df_ov_params, "data/cavicam/df_ov_params.csv", row.names = F)

# models ------------------------------------------------------------------

library(emmeans)
library(lme4)

## summarise --------------------------------------------------------------------

df_ov_params <- read.csv("data/cavicam/df_ov_params.csv") 

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

df_ov_params_summ <- df_ov_params |> 
  group_by(year, species, vessel_order, date) |>
  summarise(p50 = mean(p50, na.rm = T),
            p50_lwr = mean(p50_lwr, na.rm = T),
            p50_upr = mean(p50_upr, na.rm = T),
            p12 = mean(p12, na.rm = T),
            p12_lwr = mean(p12_lwr, na.rm = T),
            p12_upr = mean(p12_upr, na.rm = T),
            p88 = mean(p88, na.rm = T),
            p88_lwr = mean(p88_lwr, na.rm = T),
            p88_upr = mean(p88_upr, na.rm = T)) |> 
  mutate(date_plot = date |> str_replace_all("2023|2024", "2000") |> as.Date("%Y-%m-%d")) |> 
  ungroup()


df_ov_params_summ <- cbind.data.frame(
  df_ov_params_summ |> 
  pivot_longer(cols = c(p50, p12, p88), names_to = "param", values_to = "mean"),
df_ov_params_summ |> 
  pivot_longer(cols = c(p50_lwr, p12_lwr, p88_lwr), names_to = "ci_low", values_to = "lower") |> 
  select(c(lower, ci_low)),
df_ov_params_summ |> 
  pivot_longer(cols = c(p50_upr, p12_upr, p88_upr), names_to = "ci_upp", values_to = "upper") |> 
  select(c(upper, ci_upp))
) 

write.csv(df_ov_params_summ, "data/cavicam/df_ov_params_summ.csv", row.names = F)

plot_vc_params <- df_ov_params_summ |> 
  filter(vessel_order == "major") |>
  mutate(date_plot = date_plot |> as.Date("%Y-%m-%d"),
        species = recode(species, "FASY" = "beech", "FREX" = "ash"),
         param_labels = case_when(param == "p12" ~ "P12",
                                    param == "p50" ~ "P50",
                                    param == "p88" ~ "P88") |> as.factor(),
         year = year(date) |> as.factor()
         ) |> 
  ggplot(aes(x = date_plot, y = -mean, color = species)) +
  geom_point() +
  geom_line(aes(group = interaction(species, year), lty = year)) +
  geom_errorbar(aes(ymin = -lower, ymax = -upper), width = 0) +
  labs(x = "Date", y = "Leaf water potential [MPa]", 
       title = "Leaf water potentials at 12%, 50%, 88% embolism",
       subtitle = "errorbars represent bootstrap confidence intervals") +
  guides(color = guide_legend(title = "Species", ncol = 1),
         lty = guide_legend(title = "Year", ncol = 1))+
  facet_wrap(~ param_labels)  +
  scale_color_viridis_d(option = "inferno", begin = 0.2, end = 0.8) +
  presentation_theme 

plot_vc_params
ggsave("figures/plot_vc_params.png", plot_vc_params, width = 10, height = 6, dpi = 300)


## lmer --------------------------------------------------------------------

df_ov_params_nest <- df_ov_params |> 
  filter(vessel_order == "major",
         # species != "FREX" | campaign != "3" | year != "2023",
         # species != "FREX" | campaign != "2" | year != "2023",
         # species != "FREX" | campaign != "1" | year != "2023",
         # year != "2023"
         ) |> 
  mutate(date = as.factor(date), 
         species = as.factor(species),
         sample_id = as.factor(sample_id)) |>
  pivot_longer(cols = c(p50, p12, p88), names_to = "param", values_to = "value") |>
  nest(data = -param)

df_ov_params_nest <- df_ov_params_nest |> 
  mutate(mod = map(data, ~ lmer(value ~ species * date + (1 | species/sample_id), data = .x)
  ))

df_ov_params_nest$mod[[1]]

## emmeans -----------------------------------------------------------------

df_ov_params_nest <- df_ov_params_nest |> 
  mutate(est_mean = map(mod, ~ emmeans(.x,  ~ species * date, 
                                         adjust = "mvt",
                                         type = "response",
                                       cov.reduce = TRUE,
                                       na.rm = T) 
  ))

df_ov_params_nest <- df_ov_params_nest |>
  mutate(est_mean_df = map(est_mean, ~ as.data.frame(.x)))

df_sample_est_means <- df_ov_params_nest |> 
  select(-est_mean) |>
  unnest(cols = c(est_mean_df)) |>
  mutate(date_plot = date |> str_replace_all("2023|2024", "2000") |> as.Date("%Y-%m-%d"))


df_sample_est_means |> 
  mutate(year = year(date) |> as.factor()) |>
  ggplot() +
  geom_line(aes(x = date_plot, y = -emmean, group = interaction(year, species), lty = year), size = 1, col = "grey") +
  geom_point(aes(x = date_plot, y = -emmean, col = species, group = interaction(year, species)), size = 3,
              position = position_dodge(0.75)) +
  geom_errorbar(aes(x = date_plot, ymin = -lower.CL, ymax = -upper.CL, col = species), 
                width = 1, linewidth = 1, position = position_dodge(0.75)) +
  scale_color_discrete(name = "Species") +
  ylab("") +
  xlab("Date") +
  ggtitle("Gmin per species and date",
          subtitle = "contrasts and 95% CI of Gmin between 70% and 90% residual water content") +
  facet_wrap(~param) +
  plot_theme +
  theme(aspect.ratio = 1)


