library(tidyverse)
library(vroom)

folder <- "C:/Users/Win10 Pro x64/ownCloud - florian.wilms01@stud.uni-goettingen.de@owncloud.gwdg.de/Project_CanopyHydraulics/Data/UniJena_Ruth/"



# water potential ---------------------------------------------------------


soil_psi_files <- list.files(paste0(folder, "FloraPulse_SoilWaterPotential/Rawdata/"), full.names = T)
soil_psi_names <- list.files(paste0(folder, "FloraPulse_SoilWaterPotential/Rawdata/"), full.names = F)

soil_psi <- map(soil_psi_files, ~ vroom(.x, skip = 3)) |> 
  bind_rows() 

soil_psi_colnames <- read.csv(soil_psi_files[1], skip = 1, nrows = 1, header = F) |> 
  as.character() |> 
  str_replace_all(" ", "_") |> 
  str_replace_all("\\.", "") |> 
  str_replace_all("-", "_") |> 
  str_replace_all("___", "_") |> 
  str_replace_all("__", "_") |> 
  tolower()

soil_psi_units <- read.csv(soil_psi_files[1], skip = 2, nrows = 1, header = F) |> 
  as.character() |> 
  str_replace_all(" ", "_") |> 
  str_replace_all("\\.", "") |> 
  str_replace_all("-", "_") |> 
  str_replace_all("___", "_") |> 
  str_replace_all("__", "_") |> 
  tolower()

soil_psi_colnames <- paste(soil_psi_colnames, soil_psi_units, sep = "_") |> 
  str_replace_all("\\(", "_") |> 
  str_replace_all("\\)", "_") |> 
  str_replace_all("_+", "_")

colnames(soil_psi) <- soil_psi_colnames

soil_psi_avg <- soil_psi |> 
  select(timestamp_ts, 
         pressure_avg_1_bar, pressure_avg_2_bar, pressure_avg_3_bar, pressure_avg_4_bar,
         pressure_avg_5_bar, pressure_avg_6_bar, pressure_avg_7_bar, pressure_avg_8_bar) |> 
  pivot_longer(cols = -timestamp_ts, names_to = "sensor", values_to = "pressure_avg")

soil_psi_avg |> 
  # filter(sensor == "pressure_avg_4_bar") |>
ggplot() +
  geom_line(aes(x = timestamp_ts, y = pressure_avg, col = sensor, group = sensor)) +
  labs(title = "Soil Water Potential",
       x = "Date",
       y = "Soil Water Potential (bar)") +
  theme_minimal() +
  # facet_wrap(~sensor) +
  theme(plot.title = element_text(hjust = 0.5)
  )

write.csv(soil_psi, "data/2023_jul_oct_soil_psi.csv")

max(soil_psi$timestamp_ts)


# soil moisture -----------------------------------------------------------

soil_moisture_files <- list.files(paste0(folder, "/SoilMoisture"), full.names = T)
soil_moisture_names <- list.files(paste0(folder, "/SoilMoisture"), full.names = F)

soil_moisture <- map(soil_moisture_files, ~ read.csv(.x, skip = 0, sep = "", dec = ",", header = F)) |> 
  bind_rows()

colnames(soil_moisture)  = c("date", "time", paste0("sens_", 1:20))
soil_moisture <- soil_moisture |> 
  mutate(timestamp = paste(date, time) |> as.POSIXct(format = "%d.%m.%Y %H:%M:%S"))

soil_moisture_long <- soil_moisture |> 
  select(-c(date, time)) |> 
  pivot_longer(cols = -timestamp, names_to = "sensor", values_to = "vwc_perc")

soil_moisture_long |> 
  ggplot() +
  geom_line(aes(x = timestamp, y = vwc_perc, col = sensor, group = sensor))
