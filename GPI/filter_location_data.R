library(sf)
library(dplyr)

swf <- read_sf('GPI/parcel_geometry/LUI_SWF_2016_WGS84.shp')
locations <- readRDS('GPI/allLocations.rds')
locations <- locations %>%
  mutate(year = format(timestamp, "%Y")) %>%
  filter(year >= "2021")

# Convert locations to sf object (assuming lon and lat columns)
locations_sf <- st_as_sf(locations, coords = c("location_long", "location_lat"), crs = 4326)

# Clip locations using the bounding box of landCover
locations_clipped <- locations_sf %>%
  st_crop(st_bbox(swf))

locations_clipped <- st_intersection(locations_clipped,st_union(swf))

saveRDS(locations_clipped,'GPI/locations_swf.rds')
