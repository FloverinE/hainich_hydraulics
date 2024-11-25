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
  geom_line(aes(x = mass_cum_g, y = p_inv, group = as.factor(campaign),
                col = as.factor(campaign)), linewidth = 1) +
  facet_wrap(year ~ sample_id, ncol = 4) +
  theme_bw()

# analyse -----------------------------------------------------------------

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

## 2.4 SWC and post-TLP models ---------------------------------------------

df_pv_mod <- df_pv |> 
  group_by(year, campaign, sample_id) |> 
  mutate(meas_no = 1:n(),
         phase = case_when(meas_no < tlp_loc ~ "pre_tlp",
                           meas_no >= tlp_loc ~ "post_tlp"),
         leaf_water_g = initial_weight_g - mass_cum_g - dry_weight_g)

df_pv_pre_tlp <- df_pv_mod |>
  filter(phase == "pre_tlp") |> 
  group_by(year, campaign, sample_id) |> 
  mutate(
    swc_slope = - sd(leaf_water_g) / sd(psi_bar),
    swc_intercept = mean(leaf_water_g) - (swc_slope * mean(psi_bar)),
    # swc_slope = coef(lm(leaf_water_g ~ psi_bar))[2],
    # swc_intercept = coef(lm(leaf_water_g ~ psi_bar))[1],
    swc = swc_intercept / dry_weight_g,
    # rwc = 100 * ((current_weight_g - dry_weight_g) / ((initial_weight_g) - dry_weight_g)),
    rwc_tot = leaf_water_g / swc_intercept,
    rwc_tot_per = rwc_tot * 100,
    rwc_tot_per_100 = 100 - rwc_tot_per)

df_pv_mod <- df_pv_mod |> 
  left_join(df_pv_pre_tlp |> select(year, campaign, sample_id, meas_no, swc_intercept), 
            by = c("year", "campaign", "sample_id", "meas_no")) |> 
  group_by(year, campaign, sample_id) |>
  fill(swc_intercept, .direction = "downup") |>
  mutate( 
    # rwc = 100 * ((current_weight_g - dry_weight_g) / ((initial_weight_g) - dry_weight_g)),
    rwc_tot = leaf_water_g / swc_intercept,
    rwc_tot_per = rwc_tot * 100,
    rwc_tot_per_100 = 100 - rwc_tot_per)

df_pv_post_tlp <- df_pv_mod |> 
  filter(phase == "post_tlp") |>
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

write.csv(df_pv_mod, "data/PV/pv_data_calc.csv", row.names = F)


## 2.5 plot -----------------------------------------------------------------

presentation_theme <- 
  plot_theme +
  theme(aspect.ratio = 1,
        plot.background = element_rect(fill = "transparent"),
        plot.title = element_text(size = 19),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 15, hjust = 0),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"))

df_pv_params <- read.csv("data/PV/pv_data_calc.csv")
df_pv_params_summ <- read.csv("data/PV/pv_data_calc_summ.csv")

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

### tlp ####
plot_psi_tlp <- df_pv_params |> 
  group_by(year, campaign, species) |>
  mutate(date = date |> as.character() |> 
           str_replace_all("2023|2024", "2000") |> as.Date("%Y-%m-%d"),
         year = as.factor(year)) |>
  summarise(date = unique(date),
            mean_psi_tlp = mean(psi_tlp, na.rm = T),
            sd_psi_tlp = sd(psi_tlp, na.rm = T)) |> 
  ungroup() |> 
  ggplot(aes(col = species, lty = year)) +
  geom_point(aes(x = date, y = mean_psi_tlp, 
                 col = species, lty = year, group = interaction(species,campaign)),
             size = 2, position = position_dodge(0.5)) +
  geom_errorbar(
    aes(x = date, 
        ymin = mean_psi_tlp - sd_psi_tlp, ymax = mean_psi_tlp + sd_psi_tlp,
        ), size = 0.5, position = position_dodge(0.5),
    width = 0.2) +
  geom_line(aes(x = date, y = mean_psi_tlp), size = 0.5) +
  scale_color_discrete("Year") +
  labs(y = "Turgor loss point [MPa]",
       x = "Date",
       title = "Water potentials at Turgor Loss Point [MPa]",
       subtitle = "mean and standard deviation, n = 4 samples per species") +
  guides(col = guide_legend("Species", ncol = 1),
         linetype = guide_legend("Year", ncol = 1)) +
  scale_color_viridis_d(option = "inferno", begin = 0.2, end = 0.8) +
  presentation_theme

plot_psi_tlp
ggsave("figures/pv_psi_tlp.png", plot_psi_tlp, width = 6, height = 6)

### elasticity ----
#### symplastic ----
plot_sym_elasticity <- df_pv_params |> 
  group_by(year, campaign, species) |>
  mutate(date = date |> as.character() |> 
           str_replace_all("2023|2024", "2000") |> as.Date("%Y-%m-%d"),
         year = as.factor(year)) |>
  summarise(date = unique(date),
            mean_elasticity = mean(elasticity_sym, na.rm = T),
            sd_elasticity = sd(elasticity_sym, na.rm = T)) |> 
  ggplot() +
  geom_point(aes(x = date, y = mean_elasticity, 
                 col = species, lty = year, group = interaction(species,campaign)),
             size = 2, position = position_dodge(0.5)) +
  geom_errorbar(
    aes(x = date, 
        ymin = mean_elasticity - sd_elasticity, ymax = mean_elasticity + sd_elasticity,
        col = species, lty = year ), size = 0.5, position = position_dodge(0.5),
    width = 0.2) +
  geom_line(aes(x = date, y = mean_elasticity, col = species, lty = year), size = 0.5) +
  scale_color_discrete("Year") +
  labs(y = "Elasticity [MPa]", 
       title = "Bulk modulus of symplastic elasticity [MPa]",
       subtitle = "mean and standard deviation, n = 4 samples per species") +
  guides(col = guide_legend("Species", ncol = 1),
         linetype = guide_legend("Year", ncol = 1)) +
  scale_color_viridis_d(option = "inferno", begin = 0.2, end = 0.8) +
  presentation_theme

plot_sym_elasticity
ggsave("figures/plot_sym_elasticity.png", plot_sym_elasticity, width = 6, height = 6)

#### total ----

plot_total_elasticity <- df_pv_params |> 
  group_by(year, campaign, species) |>
  mutate(date = date |> as.character() |> 
           str_replace_all("2023|2024", "2000") |> as.Date("%Y-%m-%d"),
         year = as.factor(year)) |>
  summarise(date = unique(date),
            mean_elasticity = mean(elasticity_tot, na.rm = T),
            sd_elasticity = sd(elasticity_tot, na.rm = T)) |> 
  ggplot() +
  geom_point(aes(x = date, y = mean_elasticity, 
                 col = species, lty = year, group = interaction(species,campaign)),
             size = 2, position = position_dodge(0.5)) +
  geom_errorbar(
    aes(x = date, 
        ymin = mean_elasticity - sd_elasticity, ymax = mean_elasticity + sd_elasticity,
        col = species, lty = year ), size = 0.5, position = position_dodge(0.5),
    width = 0.2) +
  geom_line(aes(x = date, y = mean_elasticity, col = species, lty = year), size = 0.5) +
  scale_color_discrete("Year") +
  labs(y = "Elasticity",
       title = "Bulk modulus of total elasticity [MPa]",
       subtitle = "mean and standard deviation, n = 4 samples per species") +
  scale_color_viridis_d(option = "inferno", begin = 0.2, end = 0.8) +
  presentation_theme
  
plot_total_elasticity
ggsave("figures/plot_total_elasticity.png", plot_total_elasticity, width = 6, height = 6)

### capacitance ----
plot_capacitance_tlp <- df_pv_params |> 
  group_by(year, campaign, species) |>
  mutate(date = date |> as.character() |> 
           str_replace_all("2023|2024", "2000") |> as.Date("%Y-%m-%d"),
         year = as.factor(year)) |>
  summarise(date = unique(date),
            mean_cap = mean(capacitance_tlp_sym, na.rm = T),
            sd_cap = sd(capacitance_tlp_sym, na.rm = T)) |> 
  ggplot(aes(col = species, lty = year)) +
  geom_point(aes(x = date, y = mean_cap, 
                 group = interaction(species,campaign)),
             size = 2, position = position_dodge(0.5)) +
  geom_errorbar(
    aes(x = date, 
        ymin = mean_cap - sd_cap, ymax = mean_cap + sd_cap), 
    size = 0.5, position = position_dodge(0.5),
    width = 0.2) +
  geom_line(aes(x = date, y = mean_cap), size = 0.5) +
  scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  labs(y = "Capacitance",
       x = "Date",
       title = "Symplastic capacitance at Turgor Loss Point",
       subtitle = "mean and standard deviation, n = 4 samples per species") +
  guides(col = guide_legend("Species", ncol = 1),
         linetype = guide_legend("Year", ncol = 1)) +
  scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  presentation_theme

plot_capacitance_tlp
ggsave("figures/plot_capacitance_tlp.png", plot_capacitance_tlp, width = 6, height = 6)

### RWC at tlp ----

plot_rwc_tlp <- df_pv_params |> 
  group_by(year, campaign, species) |>
  mutate(date = date |> as.character() |> 
           str_replace_all("2023|2024", "2000") |> as.Date("%Y-%m-%d"),
         year = as.factor(year)) |>
  summarise(date = unique(date),
            mean_rwc = mean(rwc_tot_tlp, na.rm = T),
            sd_rwc = sd(rwc_tot_tlp, na.rm = T)) |> 
  ggplot(aes(col = species, lty = year)) +
  geom_point(aes(x = date, y = mean_rwc, 
                 group = interaction(species,campaign)),
             size = 2, position = position_dodge(0.5)) +
  geom_errorbar(
    aes(x = date, 
        ymin = mean_rwc - sd_rwc, ymax = mean_rwc + sd_rwc), 
    size = 0.5, position = position_dodge(0.5),
    width = 0.2) +
  geom_line(aes(x = date, y = mean_rwc), size = 0.5) +
  ylim(70, 100) +
  scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  labs(y = "RWC at TLP [%]",
       x = "Date",
       title = "Relative water content at Turgor Loss Point",
       subtitle = "mean and standard deviation, n = 4 samples per species") +
  guides(col = guide_legend("Species", ncol = 1),
         linetype = guide_legend("Year", ncol = 1)) +
  scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  presentation_theme

plot_rwc_tlp
ggsave("figures/plot_rwc_tlp.png", plot_rwc_tlp, width = 6, height = 6)

## check -------------------------------------------------------------------

df_pv_params |> 
  filter(year == "2024", campaign == "3", sample_id == "FREX_08")

## save --------------------------------------------------------------------

df_pv_params_summ <- df_pv_params |> 
  group_by(year, campaign, date, species, sample_id) |>
  summarise(psi_tlp = mean(psi_tlp, na.rm = T),
            psi_ft = mean(psi_ft, na.rm = T),
            elasticity_sym = mean(elasticity_sym, na.rm = T),
            elasticity_tot = mean(elasticity_tot, na.rm = T),
            capacitance_tlp_sym = mean(capacitance_tlp_sym, na.rm = T),
            capacitance_tlp_tot = mean(capacitance_tlp_tot, na.rm = T),
            capacitance_ft_sym = mean(capacitance_ft_sym, na.rm = T),
            capacitance_ft_tot = mean(capacitance_ft_tot, na.rm = T),
            rwc_tot_tlp = mean(rwc_tot_tlp, na.rm = T),
            rwc_sym_tlp = mean(rwc_sym_tlp, na.rm = T))

df_pv_params_summ <- df_pv_params_summ |> 
  mutate(date = date |> str_replace("2023-07-19", "2023-07-17"),
         date = date |> str_replace("2023-08-11", "2023-08-09"))

write.csv(df_pv_params_summ, "data/PV/pv_data_calc_summ.csv", row.names = F) 

# simple analysis ---------------------------------------------------------


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
pv_2023$sample_id <- pv_2023$sample_id |> 
  str_replace_all("\\_try2", "")

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

write.csv(pv_2024, "data/PV/pv_2024.csv", row.names = F)

