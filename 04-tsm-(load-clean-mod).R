


pacman::p_load(tidyverse, janitor, drc)

flu_path <- here::here("data/flurometer-heat/")

flu_files <- list.files(flu_path, recursive = T,
                        pattern = ".xlsx", full.names = T)

campaign1 <- tibble(files = flu_files) %>% 
  filter(str_detect(files, "darkfl_hainich_rebeccatest") | str_detect(files, "Hainich_TSM_Sharath")) %>% 
mutate(contents = map(files, ~readxl::read_excel(., skip = 1))) %>% 
  unnest(contents) %>% 
  clean_names()


flu_data <- tibble(files = flu_files) %>% 
  filter(str_detect(files, "darkfl_hainich_rebeccatest") | 
           str_detect(files, "Hainich_TSM_Sharath")) %>% 
  mutate(contents = map(files, ~readxl::read_excel(., skip = 1))) %>% 
  unnest(contents) %>% 
  clean_names()

flu_data %>% 
  names()

unique(flu_data_tidy$campaign)

flu_data_tidy <- flu_data %>% 
  dplyr::select(time, date, tree_number, wb_temp, fv_fm, treatment_time) %>% 
  filter(!is.na(tree_number)) %>% 
  group_by(treatment_time, tree_number, wb_temp) %>% 
  mutate(fv_fm = as.double(fv_fm),
         date = ymd(date),
         fv_fm = case_when(fv_fm < -0.00001 ~ 0, T ~ fv_fm),
         wb_temp = case_when(wb_temp == "525" ~ "52.5",
                             wb_temp == "475" ~ "047", T ~ wb_temp),
         wb_temp = as.double(wb_temp),
         campaign = case_when(date < as_date("2024-06-20") ~ "First",
                              date > 2024-07-01 ~ "Second", T ~ "Third"),
         treatment_time = case_when(campaign == "First" ~ "15min", T ~ treatment_time),
         species = case_when(tree_number %in% c("001", "002", "003", "004")  ~ "FASY",
                             T ~ "FREX")) %>% 
  group_by(species, campaign, treatment_time, tree_number, wb_temp) %>% 
    filter(treatment_time == "15min") %>% 
  filter(!wb_temp < 20) %>% 
  summarise(fv_fm = mean(fv_fm)) %>% 
  mutate(drm_fct = paste0(campaign, "_", tree_number))


flu_data_tidy %>% 
  ggplot(aes(wb_temp, fv_fm, group = interaction(tree_number),
             col = tree_number)) +
  geom_point(position = position_dodge(preserve = "total", width = 0.5)) +
  geom_smooth(aes(group = tree_number),
              linewidth = 0.5,
              method = "drm", 
              method.args = list(
                fct = W1.4()),se = FALSE) +
  facet_wrap(campaign~species)


ggsave("figures/heat-1st-campaign.png", width = 170, height = 100, units = "mm", dpi = 300)


tsm_01 <- drm(fv_fm ~ wb_temp, drm_fct, data = flu_data_tidy, fct = W1.4())


tsm_mod_tidy <- data.frame(ED(tsm_01, c(5,50,95),interval="delta", 
              display=FALSE),ll="Log-logistic") %>% 
  rownames_to_column("tree_trait") %>% 
  as_tibble() %>% 
  separate(tree_trait, c("e", "tree_campaign", "trait"), sep = ":") %>% 
  separate(tree_campaign, c("campaign", "tree_number")) %>% 
  dplyr::select(!e) %>% 
  mutate(species = case_when(tree_number %in% c("001", "002", "003", "004")  ~ "beech",
                      T ~ "ash"),
         trait = paste0("T", trait)) %>% 
  clean_names()

tsm_mod_tidy %>% 
  ggplot(aes(species, estimate, col = campaign)) +
  geom_point(position = position_dodge(width = 0.75)) +
  geom_boxplot(alpha = 0.5) +
  scale_color_viridis_d(aesthetics = c("fill", "col")) +
  labs(x = "Species", y = "Temperature") +
  facet_wrap(~trait) +
  theme_bw()

ggsave("figures/tsm.png", width = 150, height = 70, units = "mm", dpi = 300)

