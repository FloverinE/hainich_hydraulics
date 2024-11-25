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
         campaign = case_when(date < as_date("2024-06-20") ~ "1",
                              date > as_date("2024-07-01") &  date < as_date("2024-08-01") ~ "2", 
                              date > as_date("2024-08-01") &  date < as_date("2024-09-01") ~ "3", 
                              date > as_date("2024-09-01") ~ "4"),
         treatment_time = replace_na(treatment_time, "15min"),
         species = case_when(tree_number %in% c("001", "002", "003", "004")  ~ "FASY",
                             T ~ "FREX")) %>% 
  group_by(species, campaign, treatment_time, tree_number, wb_temp) %>% 
    filter(treatment_time == "15min") %>% 
  filter(!wb_temp < 20 & wb_temp < 100 & fv_fm < 1) %>% 
  summarise(fv_fm = mean(fv_fm)) %>% 
  mutate(drm_fct = paste0(campaign, "_", tree_number))

write.csv(flu_data_tidy, "data/flurometer-heat/flu_data_tidy.csv")

flu_data_tidy %>% 
  ggplot(aes(wb_temp, fv_fm, group = interaction(tree_number),
             col = tree_number)) +
  geom_point(position = position_dodge(preserve = "total", width = 0.5)) +
  geom_smooth(aes(group = tree_number),
              linewidth = 0.5,
              method = "drm", 
              method.args = list(
                fct = W1.4()),se = FALSE) +
  labs(y = "Quantum yield Fv/Fm", x = "Temperature",
       title = "Thermal sensitivity curves of 2024")  +
  facet_wrap(campaign~species, ncol = 4) +
  presentation_theme +
    theme(aspect.ratio = 0.6)


ggsave("figures/tsm_curves.png", width = 10, height = 6)


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
  ggtitle("Loss of 5, 50, 95 % of quantum yield (Fv/Fm); 2024 only") +
  facet_wrap(~trait) +
  theme_bw() +
  theme(legend.position = "bottom")

tsm_mod_tidy_summ <- tsm_mod_tidy |> 
  group_by(species, campaign, trait) |>
  summarise(mean_est = mean(estimate), 
            sd_est = sd(estimate)) |> 
  ungroup()

## plot ----

presentation_theme <- 
  plot_theme +
  theme(aspect.ratio = 1,
        plot.background = element_rect(fill = "transparent"),
        plot.title = element_text(size = 19),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 15, hjust = 0),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"))

tsm_mod_tidy_summ |> 
  mutate(date = case_when(campaign == "1" ~ "2020-05-28",
                          campaign == "2" ~ "2020-07-09",
                          campaign == "3" ~ "2020-08-14",
                          campaign == "4" ~ "2020-09-23") |> as.Date("%Y-%m-%d"),
         species = recode(species, "FASY" = "beech", "FREX" = "ash")) |>
  ggplot(aes(x = date, y = mean_est, col = species)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_line(aes(x = date, y = mean_est, col = species), position = position_dodge(width = 0.2)) +
  geom_pointrange(aes(
                      ymin = mean_est - sd_est,
                      ymax = mean_est + sd_est), position = position_dodge(width = 0.2)) +
  ylim(30, 60) +
  labs(x = "Date", y = "Temperature") +
  ggtitle("Loss of 5, 50, 95 % of quantum yield (Fv/Fm); 2024 only") +
  facet_wrap(~trait) +
  scale_color_viridis_d(option = "inferno", begin = 0.2, end = 0.8) +
  presentation_theme +
  theme(aspect.ratio = 1)

ggsave("figures/tsm.png", width = 10, height = 6)

# model ----
## lmer ----

df_tsm_nest <- tsm_mod_tidy |> 
  mutate(species = case_when(tree_number %in% c("001", "002", "003", "004")  ~ "FASY",
                             T ~ "FREX") |> as.factor(),
         sample_id = paste0(species, "_", tree_number |> str_sub(2,3)) |> as.factor()) |>
  nest(data = -c(trait))

df_tsm_nest <- df_tsm_nest |> 
  mutate(mod = map(data, ~ lmer(estimate ~ species * campaign + (1|sample_id), data = .x)))

## emmeans ----

df_tsm_nest <- df_tsm_nest |> 
  mutate(emm = map(mod, ~emmeans::emmeans(.x, ~species)))

df_tsm_nest <- df_tsm_nest |>
  mutate(emm_grid = map(emm, ~as.data.frame(.x)))

df_tsm_nest <- df_tsm_nest |>
  unnest(emm_grid)
