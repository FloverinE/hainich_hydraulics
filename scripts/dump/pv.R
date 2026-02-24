# setup -------------------------------------------------------------------

library(tidyverse)
library(janitor)

source("scripts/ggplot_themes.R")

# load data ---------------------------------------------------------------

pv_2023 <- read.csv("data/PV/pv_2023.csv")
pv_2024 <- read.csv("data/PV/pv_2024.csv")

df_pv <- bind_rows(pv_2023, pv_2024) |> 
  mutate(date = case_when(campaign == "1" & year == "2023" ~ "2023-06-13",
                          campaign == "1" & year == "2024" ~ "2024-05-28",
                          campaign == "2" & year == "2023" ~ "2023-07-19",
                          campaign == "2" & year == "2024" ~ "2024-07-09",
                          campaign == "3" & year == "2023" ~ "2023-08-11",
                          campaign == "3" & year == "2024" ~ "2024-08-13",
                          campaign == "4" & year == "2023" ~ "2023-09-18",
                          campaign == "4" & year == "2024" ~ "2024-09-23"))

table(df_pv$sample_id)
table(df_pv$campaign)

# plot --------------------------------------------------------------------

df_pv |> 
  ggplot() +
  geom_line(aes(x = mass_cum_g, y = p_inv, group = as.factor(campaign),
                col = as.factor(campaign)), linewidth = 1) +
  facet_wrap(year ~ sample_id, ncol = 4) +
  theme_bw()

# analyze -----------------------------------------------------------------

# 1. get missing end weights from max cumulative_mass ---------------------
df_pv[is.na(df_pv$end_weight), ]

df_pv <- df_pv |> 
  group_by(sample_id, year, campaign) |> 
  mutate(max_cum_mass_g = max(mass_cum_g, na.rm = T),
         end_weight = if_else(is.na(end_weight), 
                              initial_weight_g - max_cum_mass_g, end_weight),
         current_weight_g = initial_weight_g - mass_cum_g,
         rwc = 100 * ((current_weight_g - dry_weight_g) / ((initial_weight_g) - dry_weight_g)),
         rwc_tot = 100 - rwc
  ) |> 
  ungroup()

df_pv |> 
  ggplot() +
  geom_point(aes(x = rwc_tot, y = p_inv, group = interaction(sample_id, year),
                 col = as.factor(sample_id))) +
  facet_wrap(campaign~species, ncol = 4) +
  theme_bw()

# 2. prepare window for each sample/campaign/year ----
## => select TLP for each single curve and note in excel


## 2.1 prepare plot for visual analysis ------------------------------------

df_pv |> 
  filter(year == "2023", campaign == "2") |> 
  group_by(sample_id, year, campaign) |>
  mutate(n = 1:n()) |>
  ggplot() +
  geom_point(aes(x = rwc_tot, y = p_inv,
                 col = as.factor(sample_id))) +
  geom_text(aes(x = rwc_tot, y = p_inv, label = n), nudge_y = -0.1) +
  facet_wrap(sample_id ~ campaign, ncol = 4) 


## 2.2 write to excel ------------------------------------------------------

df_pv |> 
  group_by(sample_id, year, campaign) |>
  mutate(n = 1:n()) |>
  filter(n == 1) |> 
  write.csv("data/PV/pv_window.csv", row.names = F)

df_pv <- df_pv |> 
  mutate(curve_id = paste0(sample_id, "_", year, "_", campaign))

# for(curve in unique(df_pv$curve_id)){
#   data = df_pv |> filter(curve_id == curve)
#   plot(data |> select(rwc_tot, p_inv), 
#        main = curve)
#   text(data$rwc_tot, data$p_inv,
#        labels = 1:nrow(data), pos = 3)
#   print(curve)
# }

## 2.3 join tlp locations  -------------------------------------------------

pv_window <- readxl::read_excel("data/PV/pv_window.xlsx") |> 
  mutate(curve_id = paste0(sample_id, "_", year, "_", campaign)) |> 
  select(c(curve_id, tlp_loc))

df_pv <- df_pv |> 
  mutate(curve_id = paste0(sample_id, "_", year, "_", campaign))

df_pv <- df_pv |> 
  left_join(pv_window, by = "curve_id")

## 2.4 SWC and post-TLP calculations ---------------------------------------------

df_pv_mod <- df_pv |>
  group_by(year, campaign, sample_id) |>
  mutate(
    meas_no = 1:n(),
    pre_tlp = case_when(meas_no <= tlp_loc ~ 1, meas_no > tlp_loc ~ 0),
    post_tlp = case_when(meas_no < tlp_loc ~ 0, meas_no >= tlp_loc ~ 1),
    leaf_water_g = initial_weight_g - mass_cum_g - dry_weight_g,
    psi_MPa = - psi_bar / 10,
    ## saturated water content = above and including TLP
    swc_slope = sd(leaf_water_g[pre_tlp == 1]) / sd(psi_MPa[pre_tlp == 1]),
    ## swc intercept = swc [g]
    swc_intercept = mean(leaf_water_g[pre_tlp == 1]) - swc_slope * mean(psi_MPa[pre_tlp == 1]),
    swc = swc_intercept / dry_weight_g,
    ## relative water content
    rwc_tot = leaf_water_g / swc_intercept,
    rwc_tot_per = rwc_tot * 100,
    rwc_tot_per_100 = 100 - rwc_tot_per,
    ## tlp slope and intercept = below  and including TLP
    tlp_slope = -sd(p_inv[post_tlp == 1]) / sd(rwc_tot_per_100[post_tlp == 1]) , 
    tlp_intercept = mean(p_inv[post_tlp == 1]) - (tlp_slope * mean(rwc_tot_per_100[post_tlp == 1])), 
    ## osmotic potential
    psi_osmotic = -1 / (tlp_intercept + tlp_slope * rwc_tot_per_100), 
    ## TLP: take osmotic potential at TLP (from visual estimation)
    psi_tlp = case_when(meas_no == tlp_loc ~ psi_osmotic),
    ## psi at full turgor
    psi_ft = -(1 / tlp_intercept), 
    ## turgor pressure
    psi_turgor = psi_MPa - psi_osmotic,
    ## apoplastic water fraction
    apoplastic_water = 100 + (tlp_intercept / tlp_slope), 
    ## symplastic water content = below TLP
    rwc_sym = (rwc_tot - apoplastic_water / 100) / (1 - apoplastic_water / 100), 
    rwc_sym_per = rwc_sym * 100, rwc_sym_per_100 = 100 - rwc_sym_per, ## osmotic potential
    rwc_sym_per_100 = 100 - rwc_sym_per,
    ## rwc at tlp
    rwc_tot_tlp = case_when(meas_no == tlp_loc ~ rwc_tot_per),
    rwc_sym_tlp = case_when(meas_no == tlp_loc ~ rwc_sym_per),
    ## elasticity
    elast_tot = sd(psi_turgor[pre_tlp == 1]) / sd(rwc_tot[pre_tlp == 1]),
    elast_sym = sd(psi_turgor[pre_tlp == 1]) / sd(rwc_sym[pre_tlp == 1]),
    ## capacitance
    capacitance_ft_tot = sd(rwc_tot[pre_tlp == 1]) / sd(psi_MPa[pre_tlp == 1]),
    capacitance_ft_sym = sd(rwc_sym[pre_tlp == 1]) / sd(psi_MPa[pre_tlp == 1]),
    capacitance_tlp_tot = sd(rwc_tot[post_tlp == 1]) / sd(psi_MPa[post_tlp == 1]),
    capacitance_tlp_sym = sd(rwc_sym[post_tlp == 1]) / sd(psi_MPa[post_tlp == 1])
  ) |> 
  ## fill partly empty columns
  fill(swc_intercept:capacitance_tlp_sym, .direction = "downup")

write.csv(df_pv_mod, "data/PV/pv_data_calc.csv", row.names = F)



df_pv_pre_tlp <- df_pv_mod |>
  filter(pre_tlp == "1") |> 
  group_by(year, campaign, sample_id) |> 
  mutate(
    swc_slope = -sd(leaf_water_g) / sd(psi_bar),
    swc_intercept = mean(leaf_water_g) - (swc_slope * mean(psi_bar)),
    swc = swc_intercept / dry_weight_g,
    rwc_tot = leaf_water_g / swc_intercept,
    rwc_tot_per = rwc_tot * 100,
    rwc_tot_per_100 = 100 - rwc_tot_per)

df_pv_mod <- df_pv_mod |> 
  left_join(df_pv_pre_tlp |> dplyr::select(year, campaign, sample_id, meas_no, swc_intercept), 
            by = c("year", "campaign", "sample_id", "meas_no", "swc_intercept")) |> 
  group_by(year, campaign, sample_id) |>
  fill("swc_intercept", .direction = "downup") |>
  mutate( 
    # rwc = 100 * ((current_weight_g - dry_weight_g) / ((initial_weight_g) - dry_weight_g)),
    rwc_tot = leaf_water_g / swc_intercept,
    rwc_tot_per = rwc_tot * 100,
    rwc_tot_per_100 = 100 - rwc_tot_per)

df_pv_post_tlp <- df_pv_mod |> 
  filter(post_tlp == "1") |>
  group_by(year, campaign, sample_id) |>
  mutate(
    tlp_slope = -sd(p_inv, na.rm = T) / sd(rwc_tot_per_100, na.rm = T),
    tlp_intercept = mean(p_inv, na.rm = T) - (tlp_slope * mean(rwc_tot_per_100, na.rm = T)),
  )

df_pv_mod <- rbind(df_pv_pre_tlp, df_pv_post_tlp) |> 
  group_by(year, campaign, sample_id) |> 
  mutate(    
    psi_ft = -(1 / tlp_intercept),
    apoplastic_water = 100 + (tlp_intercept / tlp_slope),
    rwc_sym = (rwc_tot - apoplastic_water / 100) / (1 - apoplastic_water / 100),
    rwc_sym_per = rwc_sym * 100,
    #  solute/osmotic potential
    psi_osmotic = (-1 / (tlp_intercept + tlp_slope * rwc_tot_per_100)),
    #  turgor/pressure potential
    # psi_turgor = psi_bar - psi_osmotic,
    psi_turgor = (psi_bar / 10) - psi_osmotic, ### divide to get MPa
    #      elasticity stuff      #
    capacitance_ft_tot = sd(rwc_tot, na.rm = T) / sd(psi_bar, na.rm = T),
    capacitance_ft_sym = sd(rwc_sym, na.rm = T) / sd(psi_bar, na.rm = T),
    capacitance_tlp_tot = sd(rwc_tot, na.rm = T) / sd(psi_bar, na.rm = T),
    capacitance_tlp_sym = sd(rwc_sym, na.rm = T) / sd(psi_bar, na.rm = T),
    elasticity_tot =  sd(psi_turgor, na.rm = T) / sd(rwc_tot, na.rm = T),
    elasticity_sym =  sd(psi_turgor, na.rm = T) / sd(rwc_sym, na.rm = T),
    #  turgor loss point
    psi_tlp = case_when(meas_no == tlp_loc ~ psi_osmotic),
    rwc_tot_tlp = case_when(meas_no == tlp_loc ~ rwc_tot_per),
    # there seems to be some issues here, double check this point 
    rwc_sym_tlp = case_when(meas_no == tlp_loc ~ rwc_sym_per)) |> 
  fill(capacitance_ft_tot:rwc_sym_tlp, .direction = "downup")


df_pv_mod
## 2.5 plot -----------------------------------------------------------------

df_pv_params <- read.csv("data/PV/pv_data_calc.csv")
df_pv_params_summ <- read.csv("data/calculated_parameters/df_pv_params_summ.csv")

df_pv_params <- df_pv_params |> 
  mutate(species = recode(species, "FASY" = "beech", "FREX" = "ash")) 

## single curves with highlighted lines post tlp
df_pv_params %>%
  filter(species == "beech") %>%
  ggplot(aes(y = p_inv, x = mass_cum_g, group = interaction(curve_id))) +
  geom_point(col = "#58508d", shape = 21,
             alpha = .6, pch = 20) +
  geom_path(col = "#58508d") +
  geom_smooth(col = "#ffa600", method = "lm",  size = 0.5,
              data = ~filter(.x, phase == "post_tlp")) +
  labs(x = "Cumulative mass of water loss",
       y =  "Psi Inverse") +
  facet_wrap(~curve_id, scales = "free") +
  theme_bw()

## check -------------------------------------------------------------------

df_pv_params |> 
  filter(year == "2024", campaign == "3", sample_id == "FREX_08")

## save --------------------------------------------------------------------

df_pv_params_summ <- df_pv_params |> 
  group_by(year, campaign, date, species, sample_id) |>
  summarise(psi_tlp = mean(psi_tlp, na.rm = T),
            psi_ft = mean(psi_ft, na.rm = T),
            elast_sym = mean(elast_sym, na.rm = T),
            elast_tot = mean(elast_tot, na.rm = T),
            capacitance_tlp_sym = mean(capacitance_tlp_sym, na.rm = T),
            capacitance_tlp_tot = mean(capacitance_tlp_tot, na.rm = T),
            capacitance_ft_sym = mean(capacitance_ft_sym, na.rm = T),
            capacitance_ft_tot = mean(capacitance_ft_tot, na.rm = T),
            rwc_tot_tlp = mean(rwc_tot_tlp, na.rm = T),
            rwc_sym_tlp = mean(rwc_sym_tlp, na.rm = T))

df_pv_params_summ <- df_pv_params_summ |> 
  mutate(date = date |> str_replace("2023-07-19", "2023-07-17"),
         date = date |> str_replace("2023-08-11", "2023-08-09"))

write.csv(df_pv_params_summ, "data/calculated_parameters/df_pv_params_summ.csv", row.names = F) 

# fit tlp  ----------------------------------------------------------------

sample_data = df_pv |> 
  filter(year == 2023, sample_id == "FASY_02", campaign == "2", )

## p_inv ~ rwc
ggplot(sample_data, aes(x = rwc_tot, y = p_inv)) +
  geom_point() +
  ggtitle("FASY_02, 2023, campaign 2") +
  theme_bw()

mod_nls = nls(p_inv ~ a * rwc_tot^b, data = sample_data, start = list(a = -1, b = -1))
summary(mod_nls)
mod_switch = nls(p_inv ~ a * rwc_tot^b, data = sample_data, start = list(a = -1, b = -1))

mod_nls$m$formula()

D(mod_nls$call)

ggplot(sample_data, aes(x = rwc_tot, y = p_inv)) +
  geom_point() +
  geom_line(aes(y = predict(mod_nls)), col = "red") +
  ggtitle("FASY_02, 2023, campaign 2") +
  theme_bw()

## find 1st derivative
f=expression(x^2+3*x)
D(f,'x')

f.nls <- expression(a * rwc_tot^b)
(d1.nls <- D(f.nls, 'rwc_tot'))

coef.a = coef(mod_nls)["a"]
coef.b = coef(mod_nls)["b"]

f_d1_nls = function(mod, rwc_tot){
  coef.a = coef(mod_nls)["a"]
  coef.b = coef(mod_nls)["b"]
  return(coef.a * (rwc_tot^(coef.b - 1) * coef.b))
}
f_d1_nls(mod_nls, sample_data$rwc_tot)

## find 2nd derivative
d.nls
d2.nls <- D(d1.nls, "rwc_tot")
f_d2_nls = function(mod, rwc_tot){
  a = coef(mod_nls)["a"]
  b = coef(mod_nls)["b"]
  return(a * (rwc_tot^((b - 1) - 1) * (b - 1) * b))
}
f_d2_nls(mod_nls, sample_data$rwc_tot)

rwc_int = seq(min(sample_data$rwc_tot), max(sample_data$rwc_tot), length.out = 100)

## plot derivatives (=> slope and change of slope of the nls)
ggplot() +
  geom_point(data = sample_data, aes(x = rwc_tot, y = p_inv)) +
  geom_line(aes(x = rwc_int, y = predict(mod_nls, newdata = list(rwc_tot = rwc_int)), col = "nonlinear model"), linewidth = 1) +
  geom_line(aes(x = rwc_int, y = f_d1_nls(mod_nls, rwc_int), col = "1st derivative"), linewidth = 1) +
  geom_line(aes(x = rwc_int, y = f_d2_nls(mod_nls, rwc_int), col = "2nd derivative"), linewidth = 1) +
  geom_hline(yintercept = 0, col = "grey") +
  scale_color_manual(name = "Legend", 
                     values = c("nonlinear model" = "red", 
                                "1st derivative" = "blue", 
                                "2nd derivative" = "green")) +
  ylim(-1, 10) +
  # ggtitle("FASY_02, 2023, campaign 2") +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot() +
  # geom_line(aes(x = rwc_int, y = f_d1_nls(mod_nls, rwc_int), col = "1st derivative"), linewidth = 1) +
  geom_line(aes(x = rwc_int, y = f_d2_nls(mod_nls, rwc_int), col = "2nd derivative"), linewidth = 1) +
  geom_hline(yintercept = mean(f_d2_nls(mod_nls, rwc_int)), col = "grey") 



# solve for y = mean of 2nd derivative
f_get_rwc_from_2nd_derivative = function(mod, y){
  a = coef(mod_nls)["a"]
  b = coef(mod_nls)["b"]
  rwc = (y / (a * (b-1) * b)) ** (1 / (b-2))
  return(rwc)
}

log10(f_d2_nls(mod_nls, rwc_int)) |> boxplot()
ggplot() +
  geom_density(aes(x = log10(f_d2_nls(mod_nls, rwc_int)))) +
  geom_vline(xintercept = mean(log10(f_d2_nls(mod_nls, rwc_int))), col = "red") +
  geom_vline(xintercept = median(log10(f_d2_nls(mod_nls, rwc_int))), col = "blue") 


median(log10(f_d2_nls(mod_nls, rwc_int)))
mean(log10(f_d2_nls(mod_nls, rwc_int)))
max(log10(f_d2_nls(mod_nls, rwc_int)))

median(f_d2_nls(mod_nls, rwc_int))

y = median(f_d2_nls(mod_nls, rwc_int))
y = 10 ** mean(log10(f_d2_nls(mod_nls, rwc_int)))

tlp_2nd_deriv <- f_get_rwc_from_2nd_derivative(mod_nls, y)

ggplot() +
  geom_point(data = sample_data, aes(x = rwc_tot, y = p_inv)) +
  geom_line(aes(x = rwc_int, y = predict(mod_nls, newdata = list(rwc_tot = rwc_int)), col = "nonlinear model"), linewidth = 1) +
  geom_point(aes(x = tlp_2nd_deriv, y = predict(mod_nls, newdata = list(rwc_tot = tlp_2nd_deriv)), col = "2nd derivative median"), pch = 19, size = 2) +
  geom_hline(yintercept = 0, col = "grey") +
  scale_color_manual(name = "Legend", 
                     values = c("nonlinear model" = "red", 
                                "2nd derivative median" = "blue")) +
  theme_bw() +
  theme(legend.position = "bottom")


# clean data --------------------------------------------------------------
library(readxl)

## 2023 --------------------------------------------------------------------

pv_2023 <- read_csv("data/PV/2023/pv_data_clean.csv") |> 
  clean_names() |> 
  select(-c(files, id, x6, parafilm_weight)) |>
  rename(sample_id = sheets,
         e_cup_initial_g = e_cup_initial,
         e_cup_final_g = e_cup_final,
         mass_diff_g = mass_diff,
         mass_cum_g = mass_cum,
         initial_weight_g = initial_weight,
         dry_weight_g = dry_weight,
         psi_bar = psi,
         psi_initial_bar = initial_psi) |> 
  mutate(species = str_sub(sample_id, 1, 4)) |> 
  filter(species %in% c("FREX", "FASY")) |> 
  mutate(campaign = case_when(date %in% c("2023-06-14", "2023-06-15", "2023-06-16") ~ "1",
                              date %in% c("2023-07-18", "2023-07-19") ~ "2",
                              date %in% c("2023-08-12", "2023-08-10") ~ "3",
                              date %in% c("2023-09-19", "2023-09-20", "2023-09-21") ~ "4"),
         year = "2023",#
         species = str_sub(sample_id, 1, 4)) |> 
  filter(!is.na(campaign))
pv_2023$sample_id <- pv_2023$sample_id |> 
  str_replace_all("\\_try2", "")

write.csv(pv_2023, "data/PV/pv_2023.csv", row.names = F)

## 2024 --------------------------------------------------------------------

pv_2024_files <- list.files("data/PV/2024", full.names = T, recursive = F, pattern = "xlsx")
pv_2024_sheets <- map(pv_2024_files, excel_sheets)

pv_2024_temp <- tibble(files = pv_2024_files, 
                       sheets = pv_2024_sheets) |> 
  unnest_longer(col = sheets) |> 
  mutate(data_meas =  map2(files, sheets, ~ read_excel(.x, .y, 
                                                       progress = readxl_progress(),
                                                       skip = 3)),
         data_meta = map2(files, sheets, ~ read_excel(.x, .y, 
                                                      progress = readxl_progress(),
                                                      range = cell_rows(c(1:2)))),
         data = map2(data_meas, data_meta, ~ bind_cols(.x, .y)))

pv_2024 <- pv_2024_temp |> 
  unnest(data) |> 
  clean_names() |> 
  select(-c(files, sheets, data_meas, data_meta)) |> 
  select(-c(sample_1, parafilm_weight)) |> 
  rename(sample_id = sample_13,
         e_cup_final_g = e_cup_final,
         e_cup_initial_g = e_cup,
         initial_weight_g = initial_weight,
         dry_weight_g = dry_weight,
         psi_initial_bar = initial_psi,
         mass_diff_g = weight_difference,
         psi_bar = water_potential_10_min,
         p_inv = x1_p_1_m_pa,
         mass_cum_g = curve) |> 
  mutate(campaign = case_when(date %in% c("2024-05-29", "2024-05-31") ~ "1",
                              date %in% c("2024-07-10", "2024-07-11") ~ "2",
                              date %in% c("2024-08-14", "2024-08-15") ~ "3",
                              date %in% c("2024-09-25") ~ "4"),
         year = "2024",
         species = str_sub(sample_id, 1, 4))

table(pv_2024$date)
table(pv_2024$campaign)

names(pv_2024)
names(pv_2023)
setdiff(names(pv_2023), names(pv_2024))

write.csv(pv_2024, "data/PV/pv_2024.csv", row.names = F)

