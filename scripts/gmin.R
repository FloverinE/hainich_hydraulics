# Setup -------------------------------------------------------------------

library(tidyverse)
library(scam)
library(janitor)
library(readxl)


# load data ---------------------------------------------------------------

df_gmin <- read.csv("data/gmin/all_years_gmin_data.csv")

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
  geom_line(aes(x = elapsed_time_min, y = rwc, group = campaign, col = as.factor(campaign)), linewidth = 1) +
  scale_color_discrete(name = "Campaign") +
  ylab("Relative water content (%)") +
  xlab("Elapsed time (min)") +
  ggtitle("Relative water content over time") +
  facet_wrap(sample_id~year, ncol = 4) +
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
  nest(-c(year, campaign, sample_id))

elapsed_time_seq_min <- seq(0, 750, 5)

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
  select(c(year, campaign, sample_id, data_interpolated)) |>
  unnest(data_interpolated)

df_gmin_interpolated |> write_csv("data/gmin_interpolated.csv")


ggplot(df_gmin_interpolated) + 
  geom_line(aes(x = elapsed_time_min, y = leaf_mass_g / start_leaf_mass_g * 100, group = campaign, col = as.factor(campaign)), linewidth = 1) +
  scale_color_discrete(name = "Campaign") +
  ylab("Relative leaf mass (%)") +
  xlab("Elapsed time (min)") +
  ggtitle("Mass loss of detached leaves") +
  facet_wrap(sample_id~year, ncol = 4) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(df_gmin_interpolated) + 
  geom_line(aes(x = elapsed_time_min, y = gmin, group = campaign, col = as.factor(campaign)), linewidth = 1) +
  scale_color_discrete(name = "Campaign") +
  ylab("Relative leaf mass (%)") +
  xlab("Elapsed time (min)") +
  ggtitle("Mass loss of detached leaves") +
  facet_wrap(sample_id~year, ncol = 4) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(df_gmin_interpolated) + 
  geom_line(aes(x = elapsed_time_min, y = gmin, group = campaign, col = as.factor(campaign)), linewidth = 1) +
  scale_color_discrete(name = "Campaign") +
  ylab("gmin [mmol m-2 s-1]") +
  xlab("Elapsed time (min)") +
  ggtitle("Gmin (interpolated values)") +
  facet_wrap(sample_id~year, ncol = 4) +
  theme_bw() +
  theme(legend.position = "bottom")

df_gmin_rwc7090 <- df_gmin_interpolated |> 
  group_by(year, campaign, sample_id) |> 
  filter(rwc >= 70, rwc < 90) |> 
  summarise(mean_rwc = mean(rwc),
            mean_gmin = mean(gmin),
            sd_gmin = sd(gmin)) |> 
  mutate(campaign = as.factor(campaign),
         year = as.factor(year),
         sample_id = as.factor(sample_id),
         species = str_sub(sample_id, 1, 4) |> as.factor())

df_gmin_rwc7090 |> 
  ggplot() +
  geom_point(aes(x = campaign, y = mean_gmin, fill = species, alpha = 0.2)) +
  geom_pointrange(aes(x = campaign, y = mean_gmin, ymin = mean_gmin - sd_gmin, ymax = mean_gmin + sd_gmin, col = species), width = 0.1) +
  # geom_errorbar(aes()) +
  facet_wrap(~year)

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

ggplot(df_gmin_pdw) + 
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

# clean data 2024 ---------------------------------------------------------------

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

write.csv(df_gmin, file = "data/gmin/2024_gmin_all.csv", row.names = F)

table(df_gmin$campaign)

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

# clean data 2023 ---------------------------------------------------------

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

gmin_2023 <- read.csv("data/gmin/2023_gmin_all.csv")

gmin_2024 <- read.csv("data/gmin/2024_gmin_all.csv")

df_gmin <- rbind(gmin_2023, gmin_2024) |> 
  select(-real_time)

write.csv(df_gmin, file = "data/gmin/all_years_gmin_data.csv", row.names = F)
