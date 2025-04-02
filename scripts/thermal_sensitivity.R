# setup -------------------------------------------------------------------

pacman::p_load(tidyverse, janitor, drc)

source("scripts/ggplot_themes.R")


# preparation -------------------------------------------------------------


flu_path <- here::here("data/flurometer-heat/")

flu_files <- list.files(flu_path, recursive = T,
                        pattern = ".xlsx", full.names = T)

flu_data <- tibble(files = flu_files) %>% 
  filter(str_detect(files, "darkfl_hainich_rebeccatest") | 
           str_detect(files, "Hainich_TSM_Sharath")) %>% 
  mutate(contents = map(files, ~readxl::read_excel(., skip = 1))) %>% 
  unnest(contents) %>% 
  clean_names()

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
                             T ~ "FREX"),
         sample_id = paste(species, tree_number |> str_sub(2,3), sep = "_")) %>% 
  dplyr::select(-c(tree_number)) |> 
  group_by(species, campaign, treatment_time, sample_id, wb_temp) %>% 
  filter(treatment_time == "15min") %>% 
  filter(!wb_temp < 20 & wb_temp < 100 & fv_fm < 1) %>% 
  summarise(fv_fm = mean(fv_fm)) %>% 
  mutate(drm_fct = paste0(campaign, "_", sample_id),
         campaign = campaign |> as.factor())

# write.csv(flu_data_tidy, "data/flurometer-heat/flu_data_tidy.csv")

plot.tsm_curves <- flu_data_tidy %>% 
  ggplot(aes(wb_temp, fv_fm, group = interaction(sample_id),
             col = campaign)) +
  geom_point(position = position_dodge(preserve = "total", width = 0.5)) +
  geom_smooth(aes(col = campaign, group = campaign),
              linewidth = 0.5,
              method = "drm", 
              method.args = list(
                fct = W1.4()),se = FALSE) +
  labs(y = expression("Quantum yield"~F[v]/F[m]), x = "Water bath temperature [°C]") +
  see::scale_color_oi(order = c(1,2,3,6)) +
  facet_wrap(~ sample_id, ncol = 4) +
  thesis_theme +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 8))
plot.tsm_curves

ggsave("figures/tsm/tsm_curves.png",plot.tsm_curves, width = 15.5, height = 10, dpi = 150, units = "cm")


# model for T5, T50, T95 -------------------------------------------------------------------

flu_data_tidy <- read.csv("data/flurometer-heat/flu_data_tidy.csv")

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



df_tsm_params <- read.csv("data/flurometer-heat/tsm_mod_tidy.csv")

df_tsm_params <- df_tsm_params |> 
  mutate(tree_number = paste0("0", tree_number),
         sample_id = case_when(tree_number %in% c("1", "2", "3", "4")  ~ "FASY",
                               T ~ "FREX") |> 
           paste0("_", tree_number) |> as.factor(),,
         campaign = campaign |> as.factor(),
         date = case_when(campaign == "1" ~ "2020-05-28",
                          campaign == "2" ~ "2020-07-09",
                          campaign == "3" ~ "2020-08-14",
                          campaign == "4" ~ "2020-09-23") |> as.factor(),
         year = "2024") |> 
  dplyr::select(-c(tree_number, std_error, lower, upper, ll))

write.csv(df_tsm_params, "data/calculated_parameters/df_tsm_params.csv", row.names = F)

df_tsm_params_wide = df_tsm_params |> 
  pivot_wider(names_from = trait, values_from = estimate) |> 
  mutate(species = dplyr::recode(species, "beech" = "Fagus sylvatica", "ash" = "Fraxinus excelsior")) |> 
  dplyr::select(-year)
df_tsm_params_wide
write.csv(df_tsm_params_wide, "data/calculated_parameters/df_tsm_params_wide.csv", row.names = F)


# plc ---------------------------------------------------------------------

df_tsm_nest <- flu_data_tidy |> 
  dplyr::select(-c(X, drm_fct, treatment_time)) |> 
  group_by(species, campaign, sample_id) |>
  mutate(qy_max = max(fv_fm),
         rel_qy = 100 - (fv_fm / qy_max * 100)) |>
  na.omit() |> 
  nest(data = -c(species, campaign, sample_id)) |> 
  mutate(plc_fit = map(data, ~fitplc(dfr = .x,
                                     varnames = c(PLC = "rel_qy", WP = "wb_temp"),
                                     model = "sigmoidal",
                                     nboot = 1
  )))


df_tsm_nest$plc_fit[[1]]
plot(df_tsm_nest$plc_fit[[1]])


df_tsm_nest <- df_tsm_nest |> 
  mutate(data = map2(data, plc_fit, .x |> mutate(pred_qy = predict(.y) |> as.numeric)))

