


pacman::p_load(tidyverse, janitor)


sugar_nutrient <- readxl::read_excel("data/sugars/sugars_nutrients_2023.xlsx") %>% 
  clean_names() %>% 
  mutate(species = recode(species, "Fagus_sylvatica" = "beech", 
                          "Fraxinus_excelsior" = "ash"))

unique(sugar_nutrient$species)

sugar_nutrient %>% 
  names()


sugars <- sugar_nutrient %>% 
  dplyr::select(site, id_field, species, campaign, date, tree, canopy_position, sugar_weight_mg:starch_mg_g) %>% 
  pivot_longer(names_to = "sugar_name",
               values_to = "sugar_conc",
               cols = -c(site:canopy_position))

sugars %>% 
  filter(canopy_position == "Top") %>% 
  ggplot(aes(species, sugar_conc, group = interaction(campaign, species), col = as.factor(campaign))) +
  geom_point(position = position_dodge(width = 0.75)) +
  geom_boxplot(alpha = 0.5, 
               position = position_dodge2( preserve = c("total", "single"))) +
  facet_wrap(~sugar_name, scales = "free_y") +
  theme_bw()

nutrients <- sugar_nutrient %>% 
  dplyr::select(site, id_field, species, campaign, date, tree, canopy_position, c_con_percent:s_con_mg_kg) %>% 
  pivot_longer(names_to = "nutrient_name",
               values_to = "nutrient_conc",
               cols = -c(site:canopy_position))

nutrients %>% 
  filter(canopy_position == "Top") %>% 
  ggplot(aes(species, nutrient_conc, group = interaction(campaign, species), 
             col = as.factor(campaign))) +
  geom_point(position = position_dodge(width = 0.75)) +
  geom_boxplot(alpha = 0.5, 
               position = position_dodge2( preserve = c("total", "single"))) +
  facet_wrap(~nutrient_name, scales = "free_y") +
  theme_bw()

