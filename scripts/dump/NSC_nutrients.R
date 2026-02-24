# setup -------------------------------------------------------------------

library(tidyverse)
library(janitor)

source("scripts/ggplot_themes.R")

df_sugars_summ <- read.csv("data/calculated_parameters/df_sugars_2023.csv") |> 
  mutate(date = as.Date(date),
         species = as.factor(species),
         sample_id = as.factor(sample_id),
         campaign = as.factor(campaign))

df_nutrients_summ <- read.csv("data/calculated_parameters/df_nutrients_2023.csv") |> 
  mutate(date = as.Date(date),
         species = as.factor(species),
         species = relevel(species, ref = "FASY"),
         sample_id = as.factor(sample_id),
         campaign = as.factor(campaign))

# set locale to US
Sys.setlocale("LC_TIME", "en_US.UTF-8")

plot_position_offset = 1.7
plot_alpha = 0.5


# data preparation --------------------------------------------------------


# 2023 --------------------------------------------------------------------

sugar_nutrient_2023 <- read.csv("data/sugars/sugars_nutrients_2023.csv") |>
  clean_names() 

unique(sugar_nutrient_2023$species)

sugar_nutrient_2023 |>
  names()

table(sugar_nutrient_2023$date)

sugars_2023 <- sugar_nutrient_2023 |> 
  dplyr::select(site, id_field, species, campaign, date, tree, 
                canopy_position, sugar_weight_mg:starch_mg_g) |>
  mutate(sample_id = str_extract(id_field, "FREX\\_\\d+|FASY\\_\\d+")) |> 
  pivot_longer(names_to = "sugar_name",
               values_to = "sugar_conc",
               cols = c(sugar_weight_mg:starch_mg_g)) |> 
  ungroup()

df_sugars_2023_summ <- sugars_2023 |>
  filter(canopy_position == "Top", !sugar_name %in% c("dilution", "starch_dilution")) |>
  mutate(species = recode(species, "Fagus_sylvatica" = "FASY",
                          "Fraxinus_excelsior" = "FREX"),
         year = "2023",
         date = as.Date(date, format = "%d.%m.%Y")) |> 
  select(-c(site, id_field, tree, canopy_position)) |> 
  group_by(campaign, species, sample_id, year, sugar_name, date) |>
  summarise(sugar_conc = mean(sugar_conc, na.rm = TRUE)) 

write.csv(df_sugars_2023_summ, file = "data/calculated_parameters/df_sugars_2023.csv", row.names = FALSE)

nutrients_2023 <- sugar_nutrient_2023 %>% 
  dplyr::select(site, id_field, species, campaign, date, tree, canopy_position, c_con:s_con_mg_kg) %>% 
  pivot_longer(names_to = "nutrient_name",
               values_to = "nutrient_conc",
               cols = -c(site:canopy_position))

df_nutrients_2023_summ <- nutrients_2023 |> 
  filter(canopy_position == "Top") |>
  mutate(sample_id = str_extract(id_field, "FREX\\_\\d+|FASY\\_\\d+"),
         species = recode(species, "Fagus_sylvatica" = "FASY",
                          "Fraxinus_excelsior" = "FREX"),
         year = 2023,
         date = as.Date(date, format = "%d.%m.%Y")) |> 
  group_by(campaign, species, sample_id, year, nutrient_name, date) |>
  summarise(nutrient_conc = mean(nutrient_conc, na.rm = TRUE)) |>
  dplyr::select(campaign, species, sample_id, year, nutrient_name, nutrient_conc, date)

write.csv(df_nutrients_2023_summ, file = "data/calculated_parameters/df_nutrients_2023.csv", row.names = FALSE)

# 2024 --------------------------------------------------------------------

sugar_nutrient_2024 <- read.csv("data/sugars/sugars_nutrients_2024.csv") |>
  clean_names() %>% 
  mutate(date = as.Date(date, format = "%d.%m.%Y"))

unique(sugar_nutrient_2024$species)

sugar_nutrient_2024 

sugars_2024 <- sugar_nutrient_2024 |> 
  dplyr::select(site, id_field, species, campaign, date, tree, sugar_weight_mg:starch_mg_g) |>
  mutate(sample_id = str_extract(id_field, "FREX\\_\\d+|FASY\\_\\d+")) |> 
  pivot_longer(names_to = "sugar_name",
               values_to = "sugar_conc",
               cols = c(sugar_weight_mg:starch_mg_g)) |> 
  ungroup()

df_sugars_2024_summ <- sugars_2024 |>
  filter(!sugar_name %in% c("dilution", "starch_dilution"), !sugar_name |> str_detect("ppm")) |>
  mutate(species = recode(species, "Fagus_sylvatica" = "FASY",
                          "Fraxinus_excelsior" = "FREX"),
         year = "2024",
         date = as.Date(date, format = "%d.%m.%Y")
         ) |> 
  dplyr::select(-c(site, id_field, tree)) |>
  group_by(campaign, species, sample_id, year, sugar_name, date) |>
  summarise(sugar_conc = mean(sugar_conc, na.rm = TRUE)) 

write.csv(df_sugars_2024_summ, file = "data/calculated_parameters/df_sugars_2024.csv", row.names = FALSE)

nutrients_2024 <- sugar_nutrient_2024 %>% 
  dplyr::select(site, id_field, species, campaign, date, tree, canopy_position, c_con:s_con_mg_kg) %>% 
  pivot_longer(names_to = "nutrient_name",
               values_to = "nutrient_conc",
               cols = -c(site:canopy_position)) %>% 
  na.omit()

df_nutrients_2024_summ <- nutrients_2024 |> 
  filter(canopy_position == "Top") |>
  mutate(sample_id = str_extract(id_field, "FREX\\_\\d+|FASY\\_\\d+"),
         species = recode(species, "Fagus_sylvatica" = "FASY",
                          "Fraxinus_excelsior" = "FREX"),
         year = 2024,
         date = as.Date(date, format = "%d.%m.%y")) |> 
  group_by(campaign, species, sample_id, year, nutrient_name, date) |>
  summarise(nutrient_conc = mean(nutrient_conc, na.rm = TRUE)) |>
  dplyr::select(campaign, species, sample_id, year, nutrient_name, nutrient_conc, date)

write.csv(df_nutrients_2024_summ, file = "data/calculated_parameters/df_nutrients_2024.csv", row.names = FALSE)


### join sugars and nutrients

df_sugars_2023_2024 <- rbind.data.frame(df_sugars_2023_summ, df_sugars_2024_summ)

df_nutrients_2023_2024 <- rbind.data.frame(df_nutrients_2023_summ, df_nutrients_2024_summ)


# 2024 new data -----------------------------------------------------------

data_2024_sugars <- read.csv("data/sugars/Hainich_Data_2024_for Sharath.csv")

data_2024_nutrients <- read.csv("data/sugars/Jianbei_Huang_250425_LS_1646.csv", skip = 11)
data_2024_nutrients <- data_2024_nutrients %>% 
  select(c(Probenname, Nummer, Parameter, Ergebnis, Fehler)) %>% 
  rename("id.labo" = "Probenname",
         "nutrient_name" = "Parameter",
         "nutrient_conc" = "Ergebnis",
         "nutrient_conc_error" = "Fehler") %>% 
  mutate(nutrient_name = nutrient_name |> tolower() |> paste0("_mg_kg")) |> 
  left_join(data_2024_sugars %>% select(id.labo, Species, Campaign, Date, Tree)) %>% 
  janitor::clean_names() %>% 
  mutate(date = date %>% as.Date(format = "%d.%m.%Y")) %>% 
  rename("sample_id" = "tree") %>% 
  select(-c(id_labo, nummer))

# write.csv(data_2024_nutrients, "data/calculated_parameters/df_elements_2024.csv", row.names = F)

data_2024_nutrients <- data_2024_nutrients|> 
  mutate(nutrient_conc = nutrient_conc |> as.numeric(),
         campaign = campaign - 4) |> 
  select(-nutrient_conc_error) |> 
  filter(!is.na(nutrient_conc)) |> 
  mutate(species = sample_id |> str_sub(1, 4))
  
df_nutrients_2023_2024 <- df_nutrients_2023_2024 |> 
  ungroup() |> 
  select(campaign, species, sample_id, nutrient_name, nutrient_conc, date)

data_2024_nutrients |> 
rbind.data.frame(df_nutrients_2023_2024)

write.csv(df_sugars_2023_2024, "data/calculated_parameters/df_sugars_2023_2024.csv", row.names = F)
write.csv(df_nutrients_2023_2024, "data/calculated_parameters/df_nutrients_2023_2024.csv", row.names = F)


