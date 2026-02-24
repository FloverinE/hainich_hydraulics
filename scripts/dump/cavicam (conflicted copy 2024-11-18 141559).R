# Setup -------------------------------------------------------------------
{
  library(tidyverse)
  # library(openxlsx)
  library(scam)
  
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

## 0.1 prepare cavitated area --------------------------------------------------

files <- list.files("data/cavicam/2024/camp4/", pattern = "\\.csv", full.names = T)

df_area <- files %>%
  set_names(nm = basename(.)) %>%
  map_df(read_csv, .id = "sample_ID") |> 
  rename(id = "...1" ) 

df_area <- df_area |>
  dplyr::select(c(sample_ID, id, Area)) |> ## get rid of unnecessary columns
  group_by(sample_ID) |>
  mutate(
    area_cav = cumsum(Area),
    perc_area_cav =  area_cav / sum(Area),
    vessel_order = sample_ID |> str_extract("all|major"),
    sample_ID = sample_ID |> str_extract("frex\\_\\d{2}|fasy\\_\\d{2}") |> toupper(),
    minutes = id * 5 - 5
  ) ## 1 picture every 5 minutes


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


# 1. predict psi for cavicams ------------------------------------------------

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
    species = substr(sample_ID, 1, 4)
  ) |>  
  filter(psi_pred_MPa > -8) |>
  nest(-c(year, species, vessel_order))

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

df_sample_nest <- df_sample_nest |> 
  mutate(nls_p50 = map(data, ~ try(nls(relK ~ f.weibull_plc(-psi_pred_MPa, SX, PX, X = 50),
                                       data = .x,
                                       start = list(SX = 40, PX = 5),
                                       # start = expand.grid(SX = seq(30,50, 5), 
                                       #                     PX = seq(1, 10, 1)),
                                       control = nls.control(maxiter = 1000)))))


df_sample_nest$nls_p50[[5]]
df_sample_nest$sample_ID[[5]]
df_sample_nest$vessel_order[[5]]
df_sample_nest$year[[5]]
df_sample_nest$campaign[[5]]


nls2::nls2(relK ~ f.weibull_plc(-psi_pred_MPa, SX, PX, X = 50),
     data = df_sample_nest$data[[5]],
     start = expand.grid(SX = seq(1,100, 5), 
                         PX = seq(3, 6, 0.5)),
     control = nls.control(maxiter = 1000))

nls(relK ~ f.weibull_plc(-psi_pred_MPa, SX, PX, X = 50),
           data = df_sample_nest$data[[5]][2:254,],
           start = list(SX = 40, PX = 5),
           control = nls.control(maxiter = 1000),
    trace = TRUE)


plot(relK ~ psi_pred_MPa, data = df_sample_nest$data[[4]], col = "red")
lines(df_sample_nest$data[[4]]$psi_pred_MPa, predict(df_sample_nest$nls_p50[[4]]), col = "red")

test_data = df_sample_nest$data[[32]]
View(test_data)

plot(relK ~ psi_pred_MPa, data = df_sample_nest$data[[32]], type = "l")
plot(relK ~ minutes, data = df_sample_nest$data[[32]], type = "l")


is.na(test_data) |> sum()

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

##### model comparisons -------------------------------------------------------

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



