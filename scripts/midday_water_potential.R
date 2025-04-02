# setup -------------------------------------------------------------------

library(tidyverse)

source("scripts/ggplot_themes.R")

# load data ---------------------------------------------------------------

df_psi_md <- read.csv("data/midday_water_potential/df_md.csv")

df_psi_md <- df_psi_md |> 
  mutate(psi_midday_mpa = mean_psi_bar / 10)


# df_pv_params <- read.csv("data/PV/pv_data_calc_summ.csv")
# df_ov_params <- read.csv("data/cavicam/df_ov_params.csv")
# df_ov_params_summ <- read.csv("data/cavicam/df_ov_params_summ.csv")
# 
# df_psi_md <- left_join(df_pv_params, psi_midday, by = c("year", "date", "campaign", "sample_id", "species")) 
# df_psi_md <- left_join(df_psi_md, df_ov_params |> filter(vessel_order == "major"), by = c("year", "campaign", "sample_id", "species")) 

# write.csv(df_psi_md, "data/midday_water_potential/df_psi_md.csv", row.names = FALSE)
write.csv(df_psi_md, "data/calculated_parameters/df_psi_midday.csv", row.names = FALSE)

# plot --------------------------------------------------------------------

plot_psi_midday <- df_psi_md |> 
  mutate(year = year |> as.factor(),
         # date = date |> as.Date("%Y-%m-%d"),
         date_plot = date |> str_replace_all("2023|2024", "2000") |> as.Date("%Y-%m-%d"),
         species = case_when(
           species == "FASY" ~ "Beech",
           species == "FREX" ~ "Ash")) |>
  group_by(date_plot, species, year) |> 
  summarise(mean_psi_MPa = mean(mean_psi_bar, na.rm = TRUE) / 10,
            sd_psi_MPa = sd(mean_psi_bar, na.rm = TRUE) / 10) |>
  ungroup() |> 
  ggplot() +
  geom_point(aes(x = date_plot, y = mean_psi_MPa, color = species), size = 3, 
             position = position_dodge(width = 0.5)) +
  geom_pointrange(aes(x = date_plot, 
                      y = mean_psi_MPa, 
                      ymin = mean_psi_MPa - sd_psi_MPa, 
                      ymax = mean_psi_MPa + sd_psi_MPa, 
                      col = species, lty = year), linewidth = 1, 
                  position = position_dodge(width = 0.5)) +
  geom_line(aes(x = date_plot, y = mean_psi_MPa, color = species, linetype = year), size = 1) +
  labs(y = expression(paste("Midday water potential [MPa]")),
       x = "Date") +
  guides(color = guide_legend(title = "Species", ncol = 1),
         linetype = guide_legend(title = "Year", ncol = 1)) +
  scale_color_viridis_d(option = "inferno", begin = 0.2, end = 0.8) +
  presentation_theme +
  theme(aspect.ratio = 0.6)

plot_psi_midday
ggsave("figures/psi_midday.png", plot_psi_midday, width = 10, height = 6)


# data --------------------------------------------------------------------

psi_midday <- readxl::read_xlsx("data/midday_water_potential/psi_midday_all.xlsx")

psi_midday <- psi_midday |>
  mutate(
    year = year(date) |> as_factor(),
    campaign = case_when(
      date %in% c("2023-06-13") ~ "1",
      date %in% c("2023-07-17") ~ "2",
      date %in% c("2023-08-09") ~ "3",
      date %in% c("2023-09-18") ~ "4",
      date %in% c("2024-05-28") ~ "1",
      date %in% c("2024-07-09") ~ "2",
      date %in% c("2024-08-13") ~ "3",
      date %in% c("2024-09-23") ~ "4"
    ) |> as.factor(),
    species = sample_id |> str_sub(1, 4) |> as.factor()) |> 
  select(-time)

psi_midday$mean_psi_bar = - rowMeans(psi_midday[, 3:4], na.rm = TRUE)

write.csv(psi_midday, "data/midday_water_potential/psi_midday.csv", row.names = FALSE)

df_psi_md$date <- as.Date(df_psi_md$date, "%Y-%m-%d") 

df_psi_md$date - df_psi_md$date[-1]
