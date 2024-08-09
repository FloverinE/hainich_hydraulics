library(tidyverse)
library(vroom)

folder <- "C:/Users/Win10 Pro x64/ownCloud - florian.wilms01@stud.uni-goettingen.de@owncloud.gwdg.de/Project_CanopyHydraulics/Data/UniJena_Ruth/"

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

