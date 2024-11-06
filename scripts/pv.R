# setup -------------------------------------------------------------------

library(tidyverse)
library(janitor)

plot_theme <- theme_bw() +
  theme(
    legend.position = "bottom",
    legend.spacing.x = unit(0.7, 'cm'),
    aspect.ratio = 0.5,
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
  geom_point(aes(x = mass_cum_g, y = p_inv, group = interaction(sample_id, year),
              col = as.factor(sample_id))) +
  facet_wrap(campaign~species, ncol = 4) +
  theme_minimal()



# analyse -----------------------------------------------------------------

## 1. get missing end weights from max cumulative_mass
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

## 2. get lm per sample/campaign/year from the last 5 values p_inv ~ rwc_tot

df_pv_nest <- df_pv |> 
  nest(data = -c(year, campaign, sample_id)) |>
  mutate(data_tlp = map(data, ~ tail(.x, 5)),
         model = map(data_tlp, ~ lm(p_inv ~ rwc_tot, data = .x)))

## 3. get intercept and transform into tlp
df_pv_nest <- df_pv_nest |> 
  mutate(intercept = map(model, ~ coef(.x)[1]) |> unlist() |> as.numeric(),
         slope = map(model, ~ coef(.x)[2]) |> unlist() |> as.numeric(),
         psi_ft_bar = map(intercept, ~ 1/.x*10) |> unlist() |> as.numeric(), ## transform intercept for psi at full turgor
         tlp = map(data_tlp, ~ .x |> slice_min(rwc_tot, n = 1) |> as.data.frame() |> pull(rwc_tot)) ,
         psi_tlp_inv = map2(model, tlp, ~ predict(.x, newdata = list("rwc_tot" = .y)) |> as.numeric()) |> unlist() |> as.numeric(),
         psi_tlp_bar = map(psi_tlp_inv, ~ 1/.x*10) |> as.numeric()
         ) 

df_pv_nest$psi_tlp_bar

ggplot(df_pv |> filter(year == 2023, sample_id == "FASY_01")) +
  geom_point(aes(x = rwc_tot, y = p_inv, group = interaction(sample_id, year),
              col = as.factor(sample_id))) +
  scale_color_discrete("Sample") +
  geom_abline(aes(intercept = intercept, slope = slope), 
              data = df_pv_nest |> filter(year == 2023, sample_id == "FASY_01"), col = "red") +
  ylab("Inverse water potential") +
  xlab("Relative water content [%]") +
  ggtitle("PV curves for FASY_01 for four campaigns 2023") +
  facet_wrap(campaign~species, ncol = 4) +
  theme_bw() +
  theme(legend.position = "bottom")
  

ggplot(df_pv |> filter(year == 2024, sample_id == "FASY_01")) +
  geom_point(aes(x = rwc_tot, y = p_inv, group = interaction(sample_id, year),
                 col = as.factor(sample_id))) +
  scale_color_discrete("Sample") +
  geom_abline(aes(intercept = intercept, slope = slope), 
              data = df_pv_nest |> filter(year == 2024, sample_id == "FASY_01"), col = "red") +
  ylab("Inverse water potential") +
  xlab("Relative water content [%]") +
  ggtitle("PV curves for FASY_01 for four campaigns 2024") +
  facet_wrap(campaign~species, ncol = 4) +
  theme_bw() +
  theme(legend.position = "bottom")
## summary -----------------------------------------------------------------

df_pv_nest$psi_tlp_bar

df_pv_tlp_summ <- df_pv_nest |> 
  unnest(data) |> 
  group_by(year, date, species) |> 
  summarise(mean_tlp_MPa = - mean(psi_tlp_bar) / 10,
            sd_tlp_MPa = - sd(psi_tlp_bar) / 10,
            mean_ft_MPa = - mean(psi_ft_bar) / 10,
            sd_ft_MPa = - sd(psi_ft_bar) / 10)

df_pv_tlp_summ$date = df_pv_tlp_summ$date |> str_replace_all("2023|2024", "2000") |> 
  as.Date("%Y-%m-%d")

df_pv_tlp_summ |> 
  ggplot() +
  geom_point(aes(x = date, y = mean_tlp_MPa, col = as.factor(year)), position = position_dodge(width = 0.2), size = 2) +
  geom_errorbar(
    aes(x = date, 
        ymin = mean_tlp_MPa - sd_tlp_MPa, 
        ymax = mean_tlp_MPa + sd_tlp_MPa,
        col = as.factor(year)),
                width = 0.2, position = position_dodge(), size = 1) +
  scale_color_discrete("Year") +
  ylab("Turgor loss point [MPa]") +
  ggtitle("Water potentials at Turgor Loss Point [MPa]",
          subtitle = "mean and standard deviation, n = 4 samples per species") + 
  facet_wrap(~species, ncol = 2) +
  plot_theme +
  theme(aspect.ratio = 1)

df_pv_tlp_summ |> 
  ggplot() +
  geom_point(aes(x = campaign, y = mean_ft_MPa, col = as.factor(year))) +
  geom_errorbar(
    aes(x = campaign, 
        ymin = mean_ft_MPa - sd_ft_MPa, 
        ymax = mean_ft_MPa + sd_ft_MPa,
        col = as.factor(year)),
    width = 0.2) +
  scale_color_discrete("Year") +
  ylab("Water potential at full turgor [MPa]") +
  ggtitle("Water potential at full turgor") +
  facet_wrap(~species, ncol = 2) +
  plot_theme

# clean data --------------------------------------------------------------
library(readxl)

# 2023 --------------------------------------------------------------------

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

write.csv(pv_2023, "data/PV/pv_2023.csv", row.names = F)

# 2024 --------------------------------------------------------------------

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

pv_2023$sample_id <- pv_2023$sample_id |> 
  str_replace_all("\\_try2", "")

write.csv(pv_2024, "data/PV/pv_2024.csv", row.names = F)

