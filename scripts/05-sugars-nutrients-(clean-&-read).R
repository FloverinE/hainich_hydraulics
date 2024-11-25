
pacman::p_load(tidyverse, janitor)


sugar_nutrient <- readxl::read_excel("data/sugars/sugars_nutrients_2023.xlsx") %>% 
  clean_names() %>% 
  mutate(species = dplyr::recode(species, "Fagus_sylvatica" = "beech", 
                                 "Fraxinus_excelsior" = "ash"))

unique(sugar_nutrient$species)

sugar_nutrient %>% 
  names()


sugars <- sugar_nutrient %>% 
  dplyr::select(site, id_field, species, campaign, date, tree, canopy_position, sugar_weight_mg:starch_mg_g) %>% 
  pivot_longer(names_to = "sugar_name",
               values_to = "sugar_conc",
               cols = -c(site:canopy_position))

df_sugars_summ <- sugars %>% 
  filter(canopy_position == "Top", !sugar_name %in% c("dilution", "starch_dilution")) %>% 
  group_by(species, campaign, sugar_name) %>% 
  summarise(mean_conc = mean(sugar_conc, na.rm = T),
            sd_conc = sd(sugar_conc, na.rm = T))

df_sugars_summ %>% 
  ggplot(aes(species, mean_conc, group = interaction(campaign, species), col = as.factor(campaign))) +
  geom_point(position = position_dodge(width = 0.75)) +
  # geom_boxplot(alpha = 0.5, 
  #              position = position_dodge2( preserve = c("total", "single"))) +
  geom_pointrange(aes(x = species, 
                      ymin = mean_conc - sd_conc, 
                      ymax = mean_conc + sd_conc),
                  position = position_dodge(width = 0.75)) +
  facet_wrap(~sugar_name, scales = "free_y", ncol = 5) +
  ylab("Concentration") +
  scale_color_discrete("Campaign") +
  theme_bw() +
  theme(legend.position = "bottom")

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
  facet_wrap(~nutrient_name, scales = "free_y", ncol = 4) +
  scale_color_discrete("Campaign") +
  theme_bw() +
  theme(legend.position = "bottom")

df_nutrients_summ <- nutrients %>% 
  filter(canopy_position == "Top") %>% 
  group_by(species, campaign, nutrient_name) %>% 
  summarise(mean_conc = mean(nutrient_conc, na.rm = T),
            sd_conc = sd(nutrient_conc, na.rm = T),
            nutrient_conc = nutrient_conc)

df_nutrients_summ %>% 
  ggplot(aes(species, mean_conc, group = interaction(campaign, species), col = as.factor(campaign))) +
  geom_point(position = position_dodge(width = 0.75)) +
  # geom_boxplot(alpha = 0.5, 
  #              position = position_dodge2( preserve = c("total", "single"))) +
  geom_pointrange(aes(x = species, 
                      ymin = mean_conc - sd_conc, 
                      ymax = mean_conc + sd_conc),
                  position = position_dodge(width = 0.75)) +
  facet_wrap(~nutrient_name, scales = "free_y", ncol = 4) +
  ylab("Concentration") +
  scale_color_discrete("Campaign") +
  theme_bw() +
  theme(legend.position = "bottom")

# combine with gmin -------------------------------------------------------

df_gmin_2023 = df_gmin_rwc7090 |> 
  filter(year == "2023") |> 
  mutate(date = date |> as.character() |> str_replace_all("2000", "2023") |> as.Date("%Y-%m-%d"),
         species = recode(species, "FASY" = "beech", "FREX" = "ash"))
df_gmin_2023$campaign = rep(1:4, each = 2)

sugars <- sugars |> 
  left_join(df_gmin_2023, by = c("species", "campaign"))

sugars |> 
  filter(canopy_position == "Top", !sugar_name %in% c("dilution", "starch_dilution")) %>% 
  ggplot(aes(x = mean_gmin, y = sugar_conc, group = interaction(campaign, species), col = as.factor(campaign))) +
  geom_point() +
  geom_boxplot(alpha = 0.5, 
               position = position_dodge2( preserve = c("total", "single"))) +
  facet_wrap(species~sugar_name, scales = "free_y", ncol = 5) +
  scale_color_discrete("Campaign") +
  theme_bw() +
  theme(legend.position = "bottom")

nutrients <- nutrients |> 
  left_join(df_gmin_2023, by = c("species", "campaign"))

nutrients |> 
  filter(canopy_position == "Top") %>% 
  ggplot(aes(x = mean_gmin, y = nutrient_conc, group = interaction(campaign, species), col = as.factor(campaign))) +
  geom_point() +
  geom_smooth(aes(x = mean_gmin, y = nutrient_conc, group = interaction(campaign, species), col = as.factor(campaign))) +
  geom_boxplot(alpha = 0.5, 
               position = position_dodge2( preserve = c("total", "single"))) +
  facet_wrap(species~nutrient_name, scales = "free_y", ncol = 5) +
  scale_color_discrete("Campaign") +
  theme_bw() +
  theme(legend.position = "bottom")
