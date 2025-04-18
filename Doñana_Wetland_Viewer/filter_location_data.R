library(sf)
library(dplyr)
library(terra)

donana <- read_sf('Doñana_Wetland_Viewer/donana_geometry/Donana.shp')
donana <- st_transform(donana,'EPSG:4326')

locations <- readRDS('Doñana_Wetland_Viewer/allLocations.rds')
locations <- locations %>%
  mutate(year = format(timestamp, "%Y")) %>%
  filter(year >= "2021")

# Convert locations to sf object (assuming lon and lat columns)
locations_sf <- st_as_sf(locations, coords = c("location_long", "location_lat"), crs = 4326)

# Clip locations using the bounding box of landCover
locations_clipped <- locations_sf %>%
  st_crop(st_bbox(donana))

locations_clipped <- st_intersection(locations_clipped,st_union(donana))

saveRDS(locations_clipped,'Doñana_Wetland_Viewer/locations_donana.rds')
