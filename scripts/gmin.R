# Setup -------------------------------------------------------------------

library(tidyverse)
library(scam)


# load data ---------------------------------------------------------------

df_gmin <- read.csv("data/gmin/2024_gmin_all.csv")

## calculate VPD, gmin 
# set constants used in this script
eu_cons <- 2.71828 # euler's constant 

df_gmin <- df_gmin |> 
  group_by(sample_id, campaign) |> 
  mutate(elapsed_time_min = difftime(real_time, min(real_time), units = "mins") |> as.numeric(),
         temp = 25,
         rh = 50,
         atm_p = 101.4,
         
         wax_mass = max(leaf_mass) - leaf_mass_no_wax,
         leaf_mass = leaf_mass - wax_mass - petri_dish_mass,
         start_leaf_mass = max(leaf_mass),
         leaf_mass_diff = leaf_mass - lag(leaf_mass),
         time_diff = elapsed_time_min - lag(elapsed_time_min),
         
         vp_sat = 610.78 * eu_cons^(temp / (temp + 237.3) * 17.2694) / 1000,
         mf_vpd = (1 - (rh / 100)) * vp_sat / atm_p,
         
         gmin = -(leaf_mass_diff / 18 * 1000) / (time_diff * 60) / mf_vpd / (leaf_area_cm2 * 2 / 10000),
         rwc = 100*((leaf_mass - dry_weight_g)/((max(leaf_mass)) - dry_weight_g)),
         rwd = 1 - ((leaf_mass - dry_weight_g)/((max(leaf_mass)) - dry_weight_g))) |> 
  ungroup()

# plot --------------------------------------------------------------------
ggplot(df_gmin) + 
  geom_line(aes(x = elapsed_time_min, y = leaf_mass / start_leaf_mass, group = campaign, col = as.factor(campaign)), linewidth = 1) +
  facet_wrap(~ sample_id) +
  theme_bw()

ggplot(df_gmin) + 
  geom_line(aes(x = elapsed_time_min, y = gmin, group = campaign, col = as.factor(campaign)), linewidth = 1) +
  facet_wrap(~ sample_id) +
  ylim(0, 3) +
  theme_bw()

# Gmin calculation --------------------------------------------------------





# clean data ---------------------------------------------------------------

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

df_gmin <- rbind(df_gmin_camp1, df_gmin_camp2)


df_gmin <- df_gmin |> 
  rename(Temp = `T`) 

colnames(df_gmin) = tolower(colnames(df_gmin))


## join dry weight
dry_weights <- readxl::read_excel("data/gmin/2024-06-03_gmin_pv_dry_weights.xlsx", sheet = "gmin")
colnames(dry_weights) = tolower(colnames(dry_weights))

dry_weights <- dry_weights |> 
  mutate(campaign = as.character(campaign)) |> 
  select(c(campaign, sample_id, dry_weight_g))

df_gmin <- df_gmin |> left_join(dry_weights, by = c("campaign", "sample_id"))

write.csv(df_gmin, file = "data/gmin/2024_gmin_all.csv", row.names = F)
