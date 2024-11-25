# setup -------------------------------------------------------------------
library(tidyverse)

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
presentation_theme <- 
  plot_theme +
  theme(aspect.ratio = 1,
        plot.background = element_rect(fill = "transparent"),
        plot.title = element_text(size = 19),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 15, hjust = 0),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"))

# load data ---------------------------------------------------------------
psi_midday <- read.csv("data/midday_water_potential/psi_midday.csv")

df_pv_params <- read.csv("data/PV/pv_data_calc_summ.csv")
df_ov_params <- read.csv("data/cavicam/df_ov_params.csv")
df_ov_params_summ <- read.csv("data/cavicam/df_ov_params_summ.csv")

df_midday <- left_join(df_pv_params, psi_midday, by = c("year", "date", "campaign", "sample_id", "species")) 
df_midday <- left_join(df_midday, df_ov_params |> filter(vessel_order == "major"), by = c("year", "date", "campaign", "sample_id", "species")) 

# plot --------------------------------------------------------------------
plot_psi_midday <- psi_midday |> 
  mutate(year = year |> as.factor(),
         # date = date |> as.Date("%Y-%m-%d"),
         date_plot = date |> str_replace_all("2023|2024", "2000") |> as.Date("%Y-%m-%d"),
         species = recode(species, "FASY" = "beech", "FREX" = "ash")) |>
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

# hydraulic safety margins ------------------------------------------------

df_midday_summ <- df_midday |> 
  group_by(date, species) |>
  summarise(mean_psi_md = mean(mean_psi_bar, na.rm = TRUE) / 10,
            mean_p50 = -mean(p50, na.rm = TRUE),
            mean_psi_tlp = mean(psi_tlp, na.rm = TRUE)) |> 
  mutate(hsm_p50 = mean_psi_md - mean_p50,
         hsm_psi_tlp = mean_psi_md - mean_psi_tlp,
         date_plot = date |> str_replace_all("2023|2024", "2000") |> as.Date("%Y-%m-%d")) |> 
  pivot_longer(cols = c("mean_psi_md", "mean_p50", "mean_psi_tlp"), names_to = "psi", values_to = "value") 

df_midday_summ |> 
  # filter(!is.na(mean_psi_md)) |>
  ggplot() +
  geom_point(aes(x = date_plot, y = value, color = species, pch = psi), size = 3,
             position = position_dodge(width = 0.5)) +
  geom_line(aes(x = date_plot, y = value, color = species, linetype = psi), size = 1)


df_midday_summ <- df_midday |> 
  mutate(mean_psi_md = mean_psi_bar / 10,
         p50 = -p50,
         hsm_p50 = mean_psi_md - p50,
         hsm_psi_tlp = mean_psi_md - psi_tlp) |> 
  group_by(date, species) |>
  summarise(mean_hsm_p50 = mean(hsm_p50, na.rm = TRUE),
            sd_hsm_p50 = sd(hsm_p50, na.rm = TRUE),
            mean_hsm_psi_tlp = mean(hsm_psi_tlp, na.rm = TRUE),
            sd_hsm_psi_tlp = sd(hsm_psi_tlp, na.rm = TRUE)) |> 
  ungroup()

df_midday_summ <- cbind.data.frame(
  df_midday_summ |> 
    select(-c(sd_hsm_p50, sd_hsm_psi_tlp)) |> 
    pivot_longer(cols = c(mean_hsm_p50, mean_hsm_psi_tlp), names_to = "mean", values_to = "mean_hsm"),
  df_midday_summ |> 
    select(c(sd_hsm_p50, sd_hsm_psi_tlp)) |> 
    pivot_longer(cols = c(sd_hsm_p50, sd_hsm_psi_tlp), names_to = "sd", values_to = "sd_hsm")
) |> 
  mutate(date_plot = date |> str_replace_all("2023|2024", "2000") |> as.Date("%Y-%m-%d"),
         year = year(date) |> as.factor())

symbol_psi_md = expression("psi_{md}")

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

df_midday_summ_plot <- df_midday_summ |> 
  mutate(species = recode(species, "FASY" = "beech", "FREX" = "ash") |> as.character(),
         mean = recode(mean, "mean_hsm_p50" = "P50", "mean_hsm_psi_tlp" = "TLP") |> as.character()) 

plot_hsm <- 
  df_midday_summ_plot |>
  ggplot() +
  geom_point(aes(x = date_plot, y = mean_hsm, color = species), 
             size = 3, position = position_dodge(width = 0.5)) +
  geom_pointrange(aes(x = date_plot, 
                      y = mean_hsm, 
                      ymin = mean_hsm - sd_hsm, 
                      ymax = mean_hsm + sd_hsm,
                      col = species, linetype = year), 
                  linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_line(aes(x = date_plot, y = mean_hsm, color = species, linetype = year), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(x = "Date", y = "Margin (MPa)",
       title = "Hydraulic safety margins",
       # subtitle = expression(paste("HSM = psi - ", psi, "_{md}"))
       ) +
  guides(color = guide_legend(title = "Species", ncol = 1),
         linetype = guide_legend(title = "Year", ncol = 1)) +
  facet_wrap(~ mean) +
  scale_color_viridis_d(option = "inferno", begin = 0.2, end = 0.8) +
  presentation_theme 

plot_hsm
ggsave("figures/hsm.png", plot_hsm, width = 10, height = 6, dpi = 300)

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


