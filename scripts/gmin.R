# Setup -------------------------------------------------------------------

library(tidyverse)
library(scam)

plot_theme <- theme_bw() +
  theme(
    legend.position = "bottom",
    legend.spacing.x = unit(0.7, 'cm'),
    aspect.ratio = 1,
    axis.text = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13),
    legend.key.width = unit(1.5, "cm", data = NULL)
    # panel.grid = element_line(
    #   color = "grey",
    #   size = 0.25,
    #   linetype = 1
    # )
  )

# load data ---------------------------------------------------------------

df_gmin <- read.csv("data/gmin/all_years_gmin_data.csv")
df_gmin

# process data ------------------------------------------------------------

## calculate VPD, gmin 
# set constants used in this script
eu_cons <- 2.71828 # euler's constant 
temp <- 25         # temperature was set to 25 degC       
rh <- 50           # relative humidity was set to 50 %
atm_p <- 101.4     # constant atmospheric pressure
vp_sat = 610.78 * eu_cons^(temp / (temp + 237.3) * 17.2694) / 1000
mf_vpd = (1 - (rh / 100)) * vp_sat / atm_p

df_gmin <- df_gmin |> 
  group_by(sample_id, campaign, year) |> 
  mutate(
    species = str_sub(sample_id, 1, 4) |> as.factor(),
    # elapsed_time_min = difftime(real_time, min(real_time), units = "mins") |> as.numeric(),
    wax_mass_g = max(leaf_mass_g) - leaf_mass_no_wax_g,
    leaf_mass_g = leaf_mass_g - wax_mass_g - petri_dish_mass_g,
    start_leaf_mass_g = max(leaf_mass_g),
    leaf_mass_diff_g = leaf_mass_g - lag(leaf_mass_g),
    time_diff = elapsed_time_min - lag(elapsed_time_min),
    
    
    gmin = -(leaf_mass_diff_g / 18 * 1000) / (time_diff * 60) / mf_vpd / (leaf_area_cm2 * 2 / 10000),
    rwc = 100 * ((leaf_mass_g - dry_weight_g) / ((start_leaf_mass_g) - dry_weight_g)),
    rwd = 1 - ((leaf_mass_g - dry_weight_g) / ((start_leaf_mass_g) - dry_weight_g))) |> 
  ungroup()

# plot --------------------------------------------------------------------
ggplot(df_gmin |> filter(elapsed_time_min < 750)) + 
  geom_line(aes(x = elapsed_time_min, y = leaf_mass_g / start_leaf_mass_g * 100, group = campaign, col = as.factor(campaign)), linewidth = 1) +
  scale_color_discrete(name = "Campaign") +
  ylab("Relative leaf mass (%)") +
  xlab("Elapsed time (min)") +
  ggtitle("Mass loss of detached leaves") +
  facet_wrap(sample_id~year, ncol = 4) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(df_gmin |> filter(elapsed_time_min < 400)) + 
  geom_line(aes(x = elapsed_time_min, y = rwc, group = interaction(sample_id, year), col = as.factor(year)), linewidth = 1) +
  scale_color_discrete(name = "Year") +
  ylab("Relative water content (%)") +
  xlab("Elapsed time (min)") +
  ggtitle("Relative water content over time") +
  facet_wrap(campaign~species, ncol = 4) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(df_gmin |> filter(elapsed_time_min < 750 & rwc > 65, year == "2023")) + 
  geom_path(aes(x = elapsed_time_min, y = gmin, group = campaign, col = as.factor(campaign)), linewidth = 1) +
  facet_wrap(~ sample_id) +
  theme_bw()

ggplot(df_gmin |> filter(elapsed_time_min < 750 & rwc > 65, year == "2023")) + 
  geom_line(aes(x = elapsed_time_min, y = rwc, group = campaign, col = as.factor(campaign)), linewidth = 1) +
  facet_wrap(~ sample_id) +
  theme_bw()

ggplot(df_gmin |> filter(elapsed_time_min < 750 & rwc > 65, year == "2024")) + 
  geom_line(aes(x = elapsed_time_min, y = rwc, group = campaign, col = as.factor(campaign)), linewidth = 1) +
  facet_wrap(~ sample_id) +
  theme_bw()

# interpolate --------------------------------------------------------

## 1. model leaf mass ~ elapsed time

df_gmin_nest <- df_gmin |> 
  nest(data = -c(year, campaign, sample_id, date))

elapsed_time_seq_min <- seq(0, 750, 1)

df_gmin_nest <- df_gmin_nest |>
  mutate(
    mod_leaf_mass = map(data, ~ 
                          scam::scam(leaf_mass_g ~ s(elapsed_time_min, bs = "mpd"), data = .x)))
df_gmin_nest <- df_gmin_nest |>
  mutate(data_interpolated = map2(data, mod_leaf_mass, ~ 
                                    data.frame(elapsed_time_min = elapsed_time_seq_min,
                                               leaf_mass_g = predict(.y, newdata = data.frame(elapsed_time_min = elapsed_time_seq_min)),
                                               start_leaf_mass_g = .x$start_leaf_mass_g[1],
                                               leaf_area_cm2 = .x$leaf_area_cm2[1],
                                               dry_weight_g = .x$dry_weight_g[1])))

df_gmin_nest <- df_gmin_nest |>
  mutate(data_interpolated = map(data_interpolated, ~ 
                                   mutate(.x, 
                                          leaf_mass_diff_g = leaf_mass_g - lag(leaf_mass_g),
                                          time_diff = elapsed_time_min - lag(elapsed_time_min),
                                          gmin = -(leaf_mass_diff_g / 18 * 1000) / (time_diff * 60) / mf_vpd / (leaf_area_cm2 * 2 / 10000),
                                          rwc = 100 * ((leaf_mass_g - dry_weight_g) / ((start_leaf_mass_g) - dry_weight_g)),
                                          rwd = 1 - ((leaf_mass_g - dry_weight_g) / ((start_leaf_mass_g) - dry_weight_g)))))

df_gmin_interpolated <- df_gmin_nest |>
  select(c(year, campaign, date, sample_id, data_interpolated)) |>
  unnest(data_interpolated)

df_gmin_interpolated |> write.csv("data/gmin_interpolated.csv", row.names = FALSE)

# analyse interpolated ----------------------------------------------------

df_gmin_interpolated <- read_csv("data/gmin_interpolated.csv")

df_gmin_interpolated <- df_gmin_interpolated |> 
  mutate(species = str_sub(sample_id, 1, 4) |> as.factor())

## relative leaf mass
ggplot(df_gmin_interpolated) + 
  geom_line(aes(x = elapsed_time_min, y = leaf_mass_g / start_leaf_mass_g * 100, group = campaign, col = as.factor(campaign)), linewidth = 1) +
  scale_color_discrete(name = "Campaign") +
  ylab("Relative leaf mass (%)") +
  xlab("Elapsed time (min)") +
  ggtitle("Mass loss of detached leaves") +
  facet_wrap(sample_id~year, ncol = 4) +
  theme_bw() +
  theme(legend.position = "bottom")

## absolute leaf mass
ggplot(df_gmin_interpolated) + 
  geom_line(aes(x = elapsed_time_min, y = leaf_mass_g, group = campaign, col = as.factor(campaign)), linewidth = 1) +
  scale_color_discrete(name = "Campaign") +
  ylab("Absolute leaf mass [g]") +
  xlab("Elapsed time (min)") +
  ggtitle("Mass loss of detached leaves") +
  facet_wrap(sample_id~year, ncol = 4) +
  theme_bw() +
  theme(legend.position = "bottom")

## relative water content
ggplot(df_gmin_interpolated) + 
  geom_line(aes(x = elapsed_time_min, y = rwc, group = campaign, col = as.factor(campaign)), linewidth = 1) +
  scale_color_discrete(name = "Campaign") +
  ylab("Relative water content (%)") +
  xlab("Elapsed time (min)") +
  ggtitle("Mass loss of detached leaves") +
  facet_wrap(sample_id~year, ncol = 4) +
  theme_bw() +
  theme(legend.position = "bottom")

## gmin
ggplot(df_gmin_interpolated) + 
  geom_line(aes(x = elapsed_time_min, y = gmin, group = campaign, col = as.factor(campaign)), linewidth = 1) +
  scale_color_discrete(name = "Campaign") +
  ylab("Gmin [mmol m-2 s-1]") +
  xlab("Elapsed time (min)") +
  ggtitle("Gmin (interpolated values)") +
  facet_wrap(sample_id~year, ncol = 4) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(df_gmin_interpolated) + 
  geom_line(aes(x = elapsed_time_min, y = gmin, group = interaction(sample_id, year),
                col = as.factor(year)), linewidth = 1) +
  scale_color_discrete(name = "Year") +
  ylab("Gmin [mmol m-2 s-1]") +
  xlab("Elapsed time (min)") +
  ggtitle("Gmin (interpolated values)") +
  facet_wrap(campaign~species, ncol = 4) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(df_gmin_interpolated) + 
  geom_line(aes(x = rwc, y = gmin, group = interaction(sample_id, year),
                col = as.factor(year)), linewidth = 1) +
  scale_color_discrete(name = "Year") +
  ylab("Gmin [mmol m-2 s-1]") +
  xlab("Residual water content [%]") +
  ggtitle("Gmin (interpolated values)") +
  facet_wrap(campaign~species, ncol = 4) +
  theme_bw() +
  theme(legend.position = "bottom")


## filter for rwc between 70 and 90 %
df_gmin_interpolated |> 
  filter(rwc >= 70, rwc < 90, year == 2023) |> 
  ggplot() +
  geom_density(aes(x = gmin, fill = as.factor(sample_id)), alpha = 0.2) +
  scale_color_discrete(name = "Sample") +
  ylab("density") +
  xlab("Gmin [mmol m-2 s-1]") +
  ggtitle("Gmin (interpolated values)") +
  facet_wrap(campaign ~ species, ncol = 4) +
  theme_bw() +
  theme(legend.position = "bottom")

df_gmin_interpolated |> 
  filter(rwc >= 70, rwc < 90) |> 
  mutate(campaign = as.factor(campaign),
         species = as.factor(species)) |>
  ggplot() +
  geom_line(aes(x = rwc, y = gmin, group = interaction(sample_id, year),
                col = as.factor(year)), linewidth = 1) +
  scale_color_discrete(name = "Year") +
  ylab("Gmin [mmol m-2 s-1]") +
  xlab("Residual water content [%]") +
  xlim(90, 70) + ## mirror x-axis to get high -> low RWC values
  ggtitle("Gmin (interpolated values)") +
  facet_wrap(campaign~species, ncol = 4) +
  theme_bw() +
  theme(legend.position = "bottom")

df_gmin_rwc7090 <- df_gmin_interpolated |> 
  mutate(year = as.factor(year),
         species = str_sub(sample_id, 1, 4) |> as.factor()) |> 
  group_by(year, date, species) |> 
  filter(rwc >= 70, rwc < 90) |> 
  summarise(mean_rwc = mean(rwc),
            mean_gmin = mean(gmin),
            sd_gmin = sd(gmin)) |> 
  mutate(date = date |> str_replace_all("2023|2024", "2000") |> 
           as.Date("%Y-%m-%d"))

df_gmin_rwc7090$date

df_gmin_rwc7090 |> 
  ggplot() +
  geom_point(aes(x = date, y = mean_gmin, col = year), position = position_dodge(width = 0.2), size = 2) +
  geom_pointrange(aes(x = date, y = mean_gmin, ymin = mean_gmin - sd_gmin, ymax = mean_gmin + sd_gmin, col = year), 
                  position = position_dodge(width = 0.2), linewidth = 1) +
  scale_color_discrete(name = "Year") +
  ylab("Gmin [mmol m-2 s-1]") +
  xlab("Date") +
  ggtitle("Gmin per species and year",
          subtitle = "mean and sd of Gmin between 70% and 90% residual water content") +
  facet_wrap(~species) +
  plot_theme +
  theme(aspect.ratio = 1)

df_gmin_rwc7090 |> 
  ggplot() +
  geom_boxplot(aes(x = campaign, y = mean_gmin, fill = species), alpha = 0.5) +
  # geom_jitter(aes(x = campaign, y = mean_gmin, col = species), width = 0.1) +
  facet_wrap(~year)

### normalize for percent of dry weight

df_gmin_pdw <- df_gmin_interpolated |> 
  group_by(year, campaign, sample_id) |> 
  mutate(perc_dry_weight = leaf_mass_g / dry_weight_g * 100) |> 
  filter(perc_dry_weight <= 300, perc_dry_weight > 200) |> 
  summarise(mean_gmin = mean(gmin),
            sd_gmin = sd(gmin)) |> 
  mutate(campaign = as.factor(campaign),
         year = as.factor(year),
         sample_id = as.factor(sample_id),
         species = str_sub(sample_id, 1, 4) |> as.factor())

ggplot(df_gmin_interpolated) + 
  geom_line(aes(x = elapsed_time_min, y = perc_dry_weight, group = campaign, col = as.factor(campaign)), linewidth = 1) +
  scale_color_discrete(name = "Campaign") +
  ylab("gmin [mmol m-2 s-1]") +
  xlab("Elapsed time (min)") +
  ggtitle("Gmin (interpolated values)") +
  facet_wrap(sample_id~year, ncol = 4) +
  theme_bw() +
  theme(legend.position = "bottom")

df_gmin_pdw |> 
  ggplot() +
  geom_boxplot(aes(x = campaign, y = mean_gmin, fill = species), alpha = 0.5) +
  # geom_jitter(aes(x = campaign, y = mean_gmin, col = species), width = 0.1) +
  facet_wrap(~year)


# models ------------------------------------------------------------------

library(emmeans)
library(lme4)

## lmer --------------------------------------------------------------------

df_gmin_interpolated <- read_csv("data/gmin_interpolated.csv")

df_gmin_models <- df_gmin_interpolated |> 
  mutate(year = as.factor(year),
         campaign = as.factor(campaign),
         species = str_sub(sample_id, 1, 4) |> as.factor(),
         date = date |> as.factor(),
         sample_id = as.factor(sample_id),
         date_plot = date |> str_replace_all("2023|2024", "2000") |> as.Date("%Y-%m-%d")) |> 
  select(c(year, campaign, species, date, sample_id, gmin, rwc, date_plot))

mod_gmin_all <- lmer(gmin ~ species * date + (1 | sample_id), 
                     data = df_gmin_models |> filter(rwc >= 70, rwc < 90))

mod_gmin_all |> summary()

## nested df for 5% RWC classes
df_gmin_models_nest <- df_gmin_models |>
  mutate(rwc_class = case_when(rwc < 60 ~ "rwc60-",
                               rwc >= 60 & rwc < 70 ~ "rwc70",
                               rwc >= 70 & rwc < 80 ~ "rwc80",
                               rwc >= 80 & rwc < 90 ~ "rwc90",
                               rwc >= 90 ~ "rwc90+") |> as.factor()) |> 
  nest(data = -rwc_class)

df_gmin_models_nest <- df_gmin_models_nest |> 
  mutate(mod_gmin = map(data, ~ lmer(gmin ~ species * date + (1|species/sample_id), data = .x)))


## emmeans  -----------------------------------------------

gmin_emmean <- emmeans(mod_gmin_all,  ~ species * date, 
                       adjust = "mvt",
                       type = "response")

gmin_emmean |> summary()



df_gmin_models_nest <- df_gmin_models_nest |> 
  mutate(est_mean = map(mod_gmin, ~ emmeans(.x,  ~ species * date, 
                                            adjust = "mvt",
                                            type = "response")))

df_gmin_rwc_class_emmeans <- df_gmin_models_nest |> 
  mutate(est_mean = map(est_mean, ~ as.data.frame(.x))) |>
  select(c(rwc_class, est_mean)) |>
  unnest(cols = c(est_mean))

df_gmin_models_nest$est_mean[[1]] |> as.data.frame()

# write.csv(df_gmin_rwc_class_emmeans, "data/gmin/df_gmin_rwc_class_emmeans.csv", row.names = FALSE)
# df_gmin_rwc_class_emmeans <- read.csv("data/gmin/df_gmin_rwc_class_emmeans.csv")

df_gmin_rwc_class_emmeans |> 
  mutate(date_plot = date |> str_replace_all("2023|2024", "2000") |> as.Date("%Y-%m-%d")) |> 
  ggplot() +
  geom_point(aes(x = date_plot, y = emmean, col = species), size = 3,
             position = position_dodge(0.75)) +
  geom_errorbar(aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL, col = species), 
                width = 1, linewidth = 1, position = position_dodge(0.75)) +
  scale_color_discrete(name = "Species") +
  ylab("Gmin [mmol m-2 s-1]") +
  xlab("Date") +
  ggtitle("Gmin per species and date",
          subtitle = "contrasts and 95% CI of Gmin between 70% and 90% residual water content") +
  facet_wrap(~rwc_class, ncol = 5) +
  plot_theme +
  theme(aspect.ratio = 1)


## plot --------------------------------------------------------------------

presentation_theme <- 
  plot_theme +
  theme(aspect.ratio = 1,
        plot.background = element_rect(fill = "transparent"),
        plot.title = element_text(size = 19),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 15, hjust = 0),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"))


plot_gmin_emmean <- gmin_emmean |> 
  as.data.frame() |> 
  mutate(date_plot = date |> str_replace_all("2023|2024", "2000") |> as.Date("%Y-%m-%d"),
         year = date |> str_extract_all("2023|2024") |> as.character(),
         species = recode(species, "FREX" = "ash", "FASY" = "beech"),
         species = factor(species, levels = c("ash", "beech"))) |>
  ggplot(aes(linetype = year, col = species,)) +
  geom_line(aes(x = date_plot, y = emmean, 
                group = interaction(year, species)), size = 1) +
  geom_point(aes(x = date_plot, y = emmean), size = 3,  position = position_dodge(0.75)) +
  geom_errorbar(aes(x = date_plot, ymin = asymp.LCL, ymax = asymp.UCL), 
                width = 1, linewidth = 1, position = position_dodge(0.75)) +
  scale_color_discrete(name = "Species") +
  labs(y = "Gmin [mmol m-2 s-1]",
       x = "Date",
       title = "Gmin per species and date",
       subtitle = "contrasts and 95% CI of Gmin between 70% and 90% residual water content") +
  guides(linetype = guide_legend(title = "Year", ncol = 1),
         col = guide_legend(title = "Species", ncol = 1)) +
  scale_color_viridis_d(option = "inferno", begin = 0.2, end = 0.8) +
  presentation_theme +
  theme(aspect.ratio = 0.5)

plot_gmin_emmean
ggsave("figures/gmin_emmean.png", plot_gmin_emmean, width = 10, height = 6)


plot_gmin_rwc_class_emmeans <- df_gmin_rwc_class_emmeans |> 
  mutate(date_plot = date |> str_replace_all("2023|2024", "2000") |> as.Date("%Y-%m-%d"),
         rwc_class_labels = case_when(rwc_class == "rwc60-" ~ "60",
                                      rwc_class == "rwc70" ~ "70",
                                      rwc_class == "rwc80" ~ "80",
                                      rwc_class == "rwc90" ~ "90",
                                      rwc_class == "rwc90+" ~ "90+") |> as.factor(),
         species = recode(species, "FREX" = "ash", "FASY" = "beech"),
         species = factor(species, levels = c("ash", "beech")),
         ## 1 for 60, 2 for 70, 3 for 80, 4 for 90, 5 for 90+
         rwc_class_locs = recode(rwc_class_labels, `60` = 1, `70` = 2, `80` = 3, `90` = 4, `90+` = 5)) |>
  ggplot() +
  geom_point(aes(x = rwc_class_labels, y = emmean, col = species), size = 2) +
  scale_x_discrete(labels = c("<60", "65", "75", "85", ">90")) +
  geom_errorbar(aes(x = rwc_class_locs, ymin = asymp.LCL, ymax = asymp.UCL, col = species), 
                width = 0.2, linewidth = 0.5) +
  geom_line(aes(x = rwc_class_locs, y = emmean, col = species), alpha = 0.5, linewidth = 0.5)  +
  scale_color_viridis_d(option = "inferno", begin = 0.2, end = 0.8) +
  labs(y = "Gmin [mmol m-2 s-1]",
       x = "relative water content class [%]",
       title = "Gmin per species and RWC class",
       subtitle = "contrasts and 95% CI of Gmin for 10% bins of residual water content") +
  facet_wrap(~date, ncol = 4) +
  presentation_theme +
  theme(aspect.ratio = 0.6)

plot_gmin_rwc_class_emmeans
ggsave("figures/gmin_rwc_class_emmeans.png", plot_gmin_rwc_class_emmeans, width = 10, height = 6)

pairs(df_gmin_models_nest$est_mean[[1]])
## assign groups for significant pairs

library(multcompView)
multcomp::cld(gmin_emmean)


# clean data --------------------------------------------------------------

library(janitor)
library(readxl)


gmin_2023 <- read.csv("data/gmin/2023_gmin_all.csv")

gmin_2024 <- read.csv("data/gmin/2024_gmin_all.csv")

df_gmin <- rbind(gmin_2023, gmin_2024) |> 
  select(-real_time)

write.csv(df_gmin, file = "data/gmin/all_years_gmin_data.csv", row.names = F)

df_gmin <- df_gmin |> 
  mutate(date = case_when(campaign == "1" & year == "2023" ~ "2023-06-13",
                          campaign == "1" & year == "2024" ~ "2024-05-28",
                          campaign == "2" & year == "2023" ~ "2023-07-19",
                          campaign == "2" & year == "2024" ~ "2024-07-09",
                          campaign == "3" & year == "2023" ~ "2023-08-11",
                          campaign == "3" & year == "2024" ~ "2024-08-13",
                          campaign == "4" & year == "2023" ~ "2023-09-18",
                          campaign == "4" & year == "2024" ~ "2024-09-23"))

# 2024 ---------------------------------------------------------------

camp1_gmin <-
  lapply(2:9, function(x) readxl::read_excel("data/gmin/01_gmin.xlsx", sheet = x))

df_gmin_camp1 <- do.call(rbind.data.frame, camp1_gmin) |>
  fill(
    c(Sample_ID,
      Date,
      Start_Time,
      Real_Time,
      Leaf_Mass_No_Wax ,
      Petri_Dish_Mass),
    .direction = "down"
  )


df_gmin_camp1$Date[df_gmin_camp1$Day == 1] = df_gmin_camp1$Date + days(1)
df_gmin_camp1$Date[df_gmin_camp1$Day == 2] = df_gmin_camp1$Date + days(2)

df_gmin_camp1 <- df_gmin_camp1 |> 
  mutate(Date = format(Date, "%Y-%m-%d"),
         Start_Time = paste(Date, format(Start_Time, "%H:%M:%S")) |> as.POSIXct(),
         Real_Time = paste(Date, format(Real_Time, "%H:%M:%S")) |> as.POSIXct(),
         campaign = "1")

camp1_leaf_area <- readxl::read_excel("data/gmin/01_gmin.xlsx", sheet = 1)
df_gmin_camp1 <- df_gmin_camp1 |> left_join(camp1_leaf_area, by = "Sample_ID")

## campaign 2
camp2_gmin <-
  lapply(2:9, function(x) readxl::read_excel("data/gmin/02_gmin.xlsx", sheet = x))

df_gmin_camp2 <- do.call(rbind.data.frame, camp2_gmin) |>
  fill(
    c(Sample_ID,
      Date,
      Start_Time,
      Real_Time,
      Leaf_Mass_No_Wax ,
      Petri_Dish_Mass),
    .direction = "down"
  )

df_gmin_camp2$Date[df_gmin_camp2$Day == 1] = df_gmin_camp2$Date[df_gmin_camp2$Day == 1] + days(1)
df_gmin_camp2$Date[df_gmin_camp2$Day == 2] = df_gmin_camp2$Date[df_gmin_camp2$Day == 2] + days(2)

df_gmin_camp2 <- df_gmin_camp2 |> 
  mutate(Date = format(Date, "%Y-%m-%d"),
         Start_Time = paste(Date, format(Start_Time, "%H:%M:%S")) |> as.POSIXct(),
         Real_Time = paste(Date, format(Real_Time, "%H:%M:%S")) |> as.POSIXct(),
         campaign = "2")

camp2_leaf_area <- readxl::read_excel("data/gmin/02_gmin.xlsx", sheet = 1)

df_gmin_camp2 <- df_gmin_camp2 |> left_join(camp2_leaf_area, by = "Sample_ID")


## campaign 3
camp3_gmin <-
  lapply(2:9, function(x) readxl::read_excel("data/gmin/03_gmin.xlsx", sheet = x))

df_gmin_camp3 <- do.call(rbind.data.frame, camp3_gmin) |>
  fill(
    c(Sample_ID,
      Date,
      Start_Time,
      Real_Time,
      Leaf_Mass_No_Wax ,
      Petri_Dish_Mass),
    .direction = "down"
  )


df_gmin_camp3 <- df_gmin_camp3 |> 
  mutate(Date = format(Date, "%Y-%m-%d"),
         Start_Time = paste(Date, format(Start_Time, "%H:%M:%S")) |> as.POSIXct(),
         Real_Time = paste(Date, format(Real_Time, "%H:%M:%S")) |> as.POSIXct(),
         campaign = "3")

camp3_leaf_area <- readxl::read_excel("data/gmin/03_gmin.xlsx", sheet = 1)

df_gmin_camp3<- df_gmin_camp3 |> left_join(camp3_leaf_area, by = "Sample_ID")

## campaign 4
camp4_gmin <-
  lapply(2:9, function(x) readxl::read_excel("data/gmin/04_gmin.xlsx", sheet = x))

df_gmin_camp4 <- do.call(rbind.data.frame, camp4_gmin) |>
  fill(
    c(Sample_ID,
      Date,
      Start_Time,
      Real_Time,
      Leaf_Mass_No_Wax ,
      Petri_Dish_Mass),
    .direction = "down"
  )


df_gmin_camp4 <- df_gmin_camp4 |> 
  mutate(Date = format(Date, "%Y-%m-%d"),
         Start_Time = paste(Date, format(Start_Time, "%H:%M:%S")) |> as.POSIXct(),
         Real_Time = paste(Date, format(Real_Time, "%H:%M:%S")) |> as.POSIXct(),
         campaign = "4")

camp4_leaf_area <- readxl::read_excel("data/gmin/04_gmin.xlsx", sheet = 1)

df_gmin_camp4 <- df_gmin_camp4 |> left_join(camp4_leaf_area, by = "Sample_ID")

## bind all 
df_gmin <- rbind(df_gmin_camp1, df_gmin_camp2, df_gmin_camp3, df_gmin_camp4)


df_gmin <- df_gmin |> 
  rename(Temp = `T`) 

colnames(df_gmin) = tolower(colnames(df_gmin))


## join dry weight
dry_weights <- readxl::read_excel("data/gmin/2024_gmin_pv_dry_weights.xlsx", sheet = "gmin")
colnames(dry_weights) = tolower(colnames(dry_weights))

dry_weights <- dry_weights |> 
  mutate(campaign = as.character(campaign)) |> 
  select(c(campaign, sample_id, dry_weight_g))

df_gmin <- df_gmin |> left_join(dry_weights, by = c("campaign", "sample_id"))

# write.csv(df_gmin, file = "data/gmin/2024_gmin_all.csv", row.names = F)
# 
# table(df_gmin$campaign)

df_gmin_data_2024_export <- df_gmin |> 
  select(
    c(
      sample_id,
      campaign,
      date,
      leaf_area_cm2,
      leaf_mass_no_wax_g = leaf_mass_no_wax,
      leaf_mass_g = leaf_mass,
      petri_dish_mass_g = petri_dish_mass,
      dry_weight_g,
      real_time
    )
  ) |> 
  group_by(sample_id, campaign) |> 
  mutate(elapsed_time_min = difftime(real_time, min(real_time), units = "mins") |> as.numeric(),
         year = "2024")
write.csv(file = "data/gmin/2024_gmin_all.csv", df_gmin_data_2024_export, row.names = FALSE)

# 2023 ---------------------------------------------------------

gmin_files_path <- "data/gmin/2023"

# list all the measured curves
gmin_curve_files <- list.files(gmin_files_path, full.names = F, recursive = T, pattern = "curve")

# list all the sheets with the pv data
gmin_curve_sheets <- map(here::here(gmin_files_path, gmin_curve_files), excel_sheets)

dry_weights_2023 <- list.files(path_gmin_2023, full.names = T, recursive = T, pattern = "dry")
leaf_area_2023 <- list.files(path_gmin_2023, full.names = T, recursive = T, pattern = "Area")

df_gmin_data_2023 <-
  tibble(files = here::here(gmin_files_path, gmin_curve_files),
         sheets = gmin_curve_sheets) |>
  unnest_longer(col = sheets) |>
  mutate(data = map2(files, sheets, ~ read_excel(.x, .y,
                                                 progress = readxl_progress()))) |>
  unnest(data) |>
  clean_names() |>
  mutate(
    campaign = files |> str_extract("\\dst|\\dnd|\\drd|\\dth") |> str_remove("[A-Za-z].") |> as.factor(),
    elapsed_time_min = real_time,
    year = "2023"
  ) |>
  fill(
    c(
      sample_id,
      date,
      start_time,
      real_time,
      leaf_mass_no_wax,
      petri_dish_mass
    ),
    .direction = "down"
  ) |>
  rename(vp_sat = v_psat) |>
  select(-c(files))

df_dry_weights_2023 <- tibble(files = dry_weights_2023) |> 
  mutate(data = map(files, ~ read_excel(.x))) |> 
  unnest(data) |> 
  janitor::clean_names() |> 
  mutate(campaign = files |> str_extract("\\dst|\\dnd|\\drd|\\dth") |> str_remove_all("[A-Za-z].") |> as.factor())

df_leaf_area_2023 <- tibble(files = leaf_area_2023) %>% 
  mutate(data = map(files, ~ read_excel(.x, skip = 9))) %>%
  unnest(data) %>% 
  select(files, sample_id = SampleId, area = `...13`, timestamp = `ClassSpecs/BgGrp`) |>  
  janitor::clean_names() |> 
  filter(!sample_id == "SampleId") 

df_leaf_area_2023 <- df_leaf_area_2023 |> 
  mutate(campaign = rep(1:4, each = 8) |> as.factor())

df_gmin_data_2023 <- left_join(df_gmin_data_2023, df_dry_weights_2023, by = c("sample_id", "campaign"))
df_gmin_data_2023 <- left_join(df_gmin_data_2023, df_leaf_area_2023, by = c("sample_id", "campaign"))                    

df_gmin_data_2023_export <- df_gmin_data_2023 |>
  select(
    c(
      sample_id,
      campaign,
      date,
      leaf_area_cm2 = area,
      leaf_mass_no_wax_g = leaf_mass_no_wax,
      leaf_mass_g = leaf_mass,
      petri_dish_mass_g = petri_dish_mass,
      dry_weight_g = dry_weight,
      real_time,
      elapsed_time_min,
      year
    )
  )

write.csv(file = "data/gmin/2023_gmin_all.csv", df_gmin_data_2023_export, row.names = FALSE)
