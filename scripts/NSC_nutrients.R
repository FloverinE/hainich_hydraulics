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


sugar_nutrient <- readxl::read_excel("data/sugars/sugars_nutrients_2023.xlsx") |>
  clean_names() 

unique(sugar_nutrient$species)

sugar_nutrient |>
  names()

sugars <- sugar_nutrient |> 
  dplyr::select(site, id_field, species, campaign, date, tree, 
                canopy_position, sugar_weight_mg:starch_mg_g) |>
  mutate(sample_id = str_extract(id_field, "FREX\\_\\d+|FASY\\_\\d+")) |> 
  pivot_longer(names_to = "sugar_name",
               values_to = "sugar_conc",
               cols = c(sugar_weight_mg:starch_mg_g)) |> 
  ungroup()

df_sugars_summ <- sugars |>
  filter(canopy_position == "Top", !sugar_name %in% c("dilution", "starch_dilution")) |>
  mutate(species = recode(species, "Fagus_sylvatica" = "FASY",
                          "Fraxinus_excelsior" = "FREX"),
         year = "2023",
         date = as.Date(date, format = "%d.%m.%y")) |> 
  select(-c(site, id_field, tree, canopy_position)) |> 
  group_by(campaign, species, sample_id, year, sugar_name, date) |>
  summarise(sugar_conc = mean(sugar_conc, na.rm = TRUE)) 

write.csv(df_sugars_summ, file = "data/calculated_parameters/df_sugars_2023.csv", row.names = FALSE)

nutrients <- sugar_nutrient %>% 
  dplyr::select(site, id_field, species, campaign, date, tree, canopy_position, c_con_percent:s_con_mg_kg) %>% 
  pivot_longer(names_to = "nutrient_name",
               values_to = "nutrient_conc",
               cols = -c(site:canopy_position))

df_nutrients_summ <- nutrients |> 
  filter(canopy_position == "Top") |>
  mutate(sample_id = str_extract(id_field, "FREX\\_\\d+|FASY\\_\\d+"),
         species = recode(species, "Fagus_sylvatica" = "FASY",
                          "Fraxinus_excelsior" = "FREX"),
         year = 2023,
         date = as.Date(date, format = "%d.%m.%y")) |> 
  group_by(campaign, species, sample_id, year, nutrient_name, date) |>
  summarise(nutrient_conc = mean(nutrient_conc, na.rm = TRUE)) |>
  dplyr::select(campaign, species, sample_id, year, nutrient_name, nutrient_conc, date)

write.csv(df_nutrients_summ, file = "data/calculated_parameters/df_nutrients_2023.csv", row.names = FALSE)


# 2024 --------------------------------------------------------------------

sugar_nutrient_2024 <- readxl::read_excel("data/sugars/sugars_nutrients_2024.xlsx") |>
  clean_names() 

unique(sugar_nutrient_2024$species)

sugar_nutrient_2024 |>
  names()

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
  dplyr::select(site, id_field, species, campaign, date, tree, canopy_position, c_con_percent:s_con_mg_kg) %>% 
  pivot_longer(names_to = "nutrient_name",
               values_to = "nutrient_conc",
               cols = -c(site:canopy_position)) %>% 
  na.omit()

df_nutrients_summ <- nutrients_2024 |> 
  filter(canopy_position == "Top") |>
  mutate(sample_id = str_extract(id_field, "FREX\\_\\d+|FASY\\_\\d+"),
         species = recode(species, "Fagus_sylvatica" = "FASY",
                          "Fraxinus_excelsior" = "FREX"),
         year = 2023,
         date = as.Date(date, format = "%d.%m.%y")) |> 
  group_by(campaign, species, sample_id, year, nutrient_name, date) |>
  summarise(nutrient_conc = mean(nutrient_conc, na.rm = TRUE)) |>
  dplyr::select(campaign, species, sample_id, year, nutrient_name, nutrient_conc, date)

write.csv(df_nutrients_summ, file = "data/calculated_parameters/df_nutrients_2024.csv", row.names = FALSE)

# 2025 new data -----------------------------------------------------------

data_2025_sugars <- readxl::read_excel("data/sugars/Hainich_Data_2024_for Sharath.xlsx")

data_2025_nutrients <- readxl::read_excel("data/sugars/Jianbei_Huang_250425_LS_1646.xls", skip = 10)
data_2025_nutrients <- data_2025_nutrients %>% 
  select(c(Probenname, Nummer, Parameter, Ergebnis, Fehler)) %>% 
  rename("id.labo" = "Probenname",
         "element_name" = "Parameter",
         "element_conc_mg_kg" = "Ergebnis",
         "element_conc_error_mg_kg" = "Fehler") %>% 
  left_join(data_2025_sugars %>% select(id.labo, Species, Campaign, Date, Tree)) %>% 
  janitor::clean_names() %>% 
  mutate(date = date %>% as.Date(format = "%d.%m.%Y")) %>% 
  rename("tree_id" = "tree") %>% 
  select(-c(id_labo, nummer))

write.csv(data_2025_nutrients, "data/calculated_parameters/df_elements_2025.csv", row.names = F)














