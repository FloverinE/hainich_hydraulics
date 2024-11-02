# Setup -------------------------------------------------------------------

library(tidyverse)
library(scam)


# load data ---------------------------------------------------------------

df_gmin <- read.csv("data/gmin/2024_gmin_all.csv")

## calculate VPD, gmin 
# set constants used in this script
eu_cons <- 2.71828 # euler's constant 
temp <- 25         # temperature was set to 25 degC       
rh <- 50           # relative humidity was set to 50 %
atm_p <- 101.4     # constant atmospheric pressure

df_gmin <- df_gmin |> 
  group_by(sample_id, campaign) |> 
  mutate(elapsed_time_min = difftime(real_time, min(real_time), units = "mins") |> as.numeric(),
         
         wax_mass = max(leaf_mass) - leaf_mass_no_wax,
         leaf_mass = leaf_mass - wax_mass - petri_dish_mass,
         start_leaf_mass = max(leaf_mass),
         leaf_mass_diff = leaf_mass - lag(leaf_mass),
         time_diff = elapsed_time_min - lag(elapsed_time_min),
         
         vp_sat = 610.78 * eu_cons^(temp / (temp + 237.3) * 17.2694) / 1000,
         mf_vpd = (1 - (rh / 100)) * vp_sat / atm_p,
         
         gmin = -(leaf_mass_diff / 18 * 1000) / (time_diff * 60) / mf_vpd / (leaf_area_cm2 * 2 / 10000),
         rwc = 100 * ((leaf_mass - dry_weight_g) / ((start_leaf_mass) - dry_weight_g)),
         rwd = 1 - ((leaf_mass - dry_weight_g) / ((start_leaf_mass) - dry_weight_g))) |> 
  ungroup()

# plot --------------------------------------------------------------------
ggplot(df_gmin |> filter(elapsed_time_min < 750)) + 
  geom_line(aes(x = elapsed_time_min, y = leaf_mass / start_leaf_mass * 100, group = campaign, col = as.factor(campaign)), linewidth = 1) +
  scale_color_discrete(name = "Campaign") +
  ylab("Relative leaf mass (%)") +
  xlab("Elapsed time (min)") +
  ggtitle("Mass loss of detached leaves 2024 campaigns") +
  facet_wrap(~ sample_id, ncol = 4) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(df_gmin |> filter(elapsed_time_min < 750)) + 
  geom_line(aes(x = elapsed_time_min, y = leaf_mass / start_leaf_mass * 100, group = campaign, col = as.factor(campaign)), linewidth = 1) +
  facet_wrap(~ sample_id) +
  theme_bw()

ggplot(df_gmin |> filter(elapsed_time_min < 750 & rwc > 65)) + 
  geom_path(aes(x = elapsed_time_min, y = gmin, group = campaign, col = as.factor(campaign)), linewidth = 1) +
  facet_wrap(~ sample_id) +
  theme_bw()

ggplot(df_gmin |> filter(elapsed_time_min < 750 & rwc > 65)) + 
  geom_line(aes(x = elapsed_time_min, y = rwc, group = campaign, col = as.factor(campaign)), linewidth = 1) +
  facet_wrap(~ sample_id) +
  theme_bw()

# Gmin calculation --------------------------------------------------------





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


# clean data 2023 ---------------------------------------------------------

path_gmin_2023 <- "data/gmin/2023"

curves_2023 <- list.files(path_gmin_2023, full.names = T, recursive = T, pattern = "curve")
dry_weights_2023 <- list.files(path_gmin_2023, full.names = T, recursive = T, pattern = "dry")
leaf_area_2023 <- list.files(path_gmin_2023, full.names = T, recursive = T, pattern = "Area")

df_gmin_data_2023 <- tibble(files = curves_2023) |> 
  mutate(data = map(files, ~ read_excel(.x))) |> 
  unnest(data) |> 
  clean_names() |> 
  mutate(campaign = files |> str_extract("\\dst|\\dnd|\\drd|\\dth") |> str_remove("[A-Za-z].") |> as.factor()) |> 
  fill(c(sample_id,
         date,
         start_time,
         real_time,
         leaf_mass_no_wax ,
         petri_dish_mass), .direction = "down") |> 
  rename(vp_sat = v_psat) |> 
  select(-c(files))

df_dry_weights_2023 <- tibble(files = dry_weights_2023) |> 
  mutate(data = map(files, ~ read_excel(.x))) |> 
  unnest(data) |> 
  clean_names() |> 
  mutate(campaign = files |> str_extract("\\dst|\\dnd|\\drd|\\dth") |> str_remove_all("[A-Za-z].") |> as.factor())

df_leaf_area_2023 <- tibble(files = leaf_area_2023) %>% 
  mutate(data = map(files, ~ read_excel(.x))) %>%
  unnest(data) %>% 
  select(files, sample_id = `WinFOLIA Reg 2014a`, area = TotLeafArea, timestamp = 'Operator Date Time') %>% 
  clean_names() %>% 
  filter(!sample_id == "SampleId") 
 