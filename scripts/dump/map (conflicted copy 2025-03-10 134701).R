# setup -------------------------------------------------------------------

library(sf)
library(patchwork)
library(tidyverse)

source("scripts/ggplot_themes.R")

df_coords <- read.csv("map/CanopyHydraulicsTrees_ClimbingCampaigns_corr.csv",
                      header = T) |>
  dplyr::select(c(decimal.degree.lon, decimal.degree.lat, tree)) |>
  st_as_sf(coords = c("decimal.degree.lon", "decimal.degree.lat"),
           crs = 4326) |> 
  st_transform(crs = 32632) |> 
  mutate(species = str_sub(tree, 1, 4))

germany_gadm <- st_read("map/gadm_ger.gpkg") |>
  st_transform(crs = 32632) 
thuringia_gadm <- st_read("map/gadm_th.gpkg") |>
  st_transform(crs = 32632) 


main_map <- ggplot() +
  geom_sf(data = df_coords, aes(shape = species, col = tree)) +
  coord_sf(datum = 32632) +
  scale_y_continuous(
    breaks = seq(5659540, 5659640, 20), limits = c(5659540, 5659640),
    labels = scales::label_number(accuracy = 1, big.mark = ",")) +
  scale_x_continuous(labels = scales::label_number(accuracy = 1, big.mark = ",")) +
  labs(x = "Easting", y = "Northing") +
  thesis_theme
main_map

inset_map = ggplot() +
  geom_sf(data = germany_gadm, fill = "grey") +
  geom_sf(data = thuringia_gadm, fill = "white") +
  geom_sf(data = df_coords[1,], aes(color = tree), size = 2, col = "red") +
  coord_sf(datum = 32632) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1, big.mark = ",")) +
  scale_x_continuous(labels = scales::label_number(accuracy = 1, big.mark = ",")) +
  labs(x = "Easting", y = "Northing") +
  thesis_theme
inset_map

main_map + inset_map



germany_gadm$geom
