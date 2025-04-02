# setup -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(patchwork)
library(cowplot)
library(terra)
library(tidyterra)

source("scripts/ggplot_themes.R")


# shapefiles --------------------------------------------------------------

df_coords <- read.csv("map/CanopyHydraulicsTrees_ClimbingCampaigns_corr.csv",
                      header = T) |>
  dplyr::select(c(decimal.degree.lon, decimal.degree.lat, tree)) |>
  st_as_sf(coords = c("decimal.degree.lon", "decimal.degree.lat"),
           crs = 4326) |> 
  st_transform(crs = 32632) |> 
  mutate(species = str_sub(tree, 1, 4),
         species = case_when(species == "FREX" ~ "Fraxinus excelsior",
                             species == "FASY" ~ "Fagus sylvatica"))

germany_gadm <- st_read("map/gadm_ger.gpkg") |>
  st_transform(crs = 32632) 
thuringia_gadm <- st_read("map/gadm_th.gpkg") |>
  st_transform(crs = 32632) 

# background raster ----
## load processed background raster (orthomosaic) ----
background <- terra::rast("map/bg_resample.tif", lyrs = c(1, 2, 3))

## raw file versions
# background <- terra::rast("map/Hainich_1608_RGB_flight2_transparent_mosaic_group1.tif", lyrs = c(1, 2, 3))
background <- terra::rast("map/bg_uncropped.tif", lyrs = c(1, 2, 3))

background <- background |> project("EPSG:32632")

## process background raster ----
# writeRaster(background, "map/bg_uncropped.tif")

ext(vect(df_coords))$xmax - ext(vect(df_coords))$xmin 
ext(vect(df_coords))$ymax - ext(vect(df_coords))$ymin 

aoi <- df_coords[2:9, ] |> st_buffer(10) |> vect() |> ext()

aoi_rast <- terra::rast(
  xmin = 601590, xmax = 601640,  # X extent
  ymin = 5659540, ymax = 5659580,  # Y extent
  resolution = 0.5,
  nrows = 380, ncols = 480,
  crs = "EPSG:32632")

# newgrid <- terra::rast(aoi_rast, nrows = 380, ncols = 480)
values(aoi_rast) = 0

aoi_rast <- buffer(aoi_rast, width = 10)

background_resample <- background |> terra::resample(aoi_rast)

plotRGB(background_resample, r=1, g=2, b=3)  # Linear stretch for better visualization

# writeRaster(background_resample, "map/bg_resample.tif")


# plot GER and main maps ---------------------------------------------------------------

main_map <- ggplot() +
  geom_spatraster_rgb(data = background_resample) +
  geom_sf(data = df_coords[2:9, ], aes(shape = species), col = "black", size = 3, show.legend = F) +
  geom_sf(data = df_coords[2:9, ], aes(col = tree, shape = species), size = 2) +
  see::scale_color_oi() +
  scale_shape_manual(values = c(16, 17, 18), 
                     guide = guide_legend(
                       override.aes = list(label.theme = element_text(face = "italic"))
                     )) +
  scale_y_continuous(breaks = seq(5659540, 5659580, 10),
                     labels = scales::label_number(accuracy = 1, big.mark = ",")) +
  scale_x_continuous(breaks = seq(601590, 601640, 10),
                     labels = scales::label_number(accuracy = 1, big.mark = ",")) +
  coord_sf(datum = 32632, expand = T) +
  labs(x = "Easting [m]", y = "Northing [m]", tag = "a)") +
  guides(shape = guide_legend(title = "Species", ncol = 1, label.theme = element_text(angle = 0, face = "italic")),
         col = guide_legend(title = "Tree", ncol = 4)) +
  thesis_theme +
  theme(axis.text = element_text(size = 8),
        text = element_text(size = 6),
        plot.tag.position = c(0.05, 0.95),
        legend.justification = c(0.3, 0),
        legend.spacing.x = unit(0.0, 'cm'),
        legend.text = element_text(size = 8),
        plot.margin = margin(0, 0, 0, -2, "cm"),
        legend.key.width = unit(0.5, "cm", data = NULL))
main_map

inset_map = ggplot() +
  geom_sf(data = germany_gadm, fill = "grey") +
  geom_sf(data = thuringia_gadm, fill = "white") +
  geom_sf(data = df_coords[1,], aes(color = tree), size = 2, col = "red") +
  coord_sf(expand = F) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1, big.mark = ",")) +
  scale_x_continuous(labels = scales::label_number(accuracy = 1, big.mark = ",")) +
  labs(x = "Longitude [°E]", y = "Latitude [°N]",tag = "b)") +
  thesis_theme +
  theme(axis.text = element_text(size = 8),
        text = element_text(size = 6),
        legend.text = element_text(size = 8),
        plot.tag.position = c(0, 0.95),
        legend.position = "none",
        plot.margin = unit(rep(0, 4), "cm"),
        legend.key.width = unit(1.0, "cm", data = NULL)) 
inset_map

maps_combined.png <- main_map + plot_spacer() + inset_map +
  plot_layout(widths = c(10, 0, 5))
maps_combined.png

ggsave(maps_combined.png, filename = "figures/map/maps_combined.png", width = 16, height = 10, dpi = 150, units = "cm")

