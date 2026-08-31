library(sf)
library(dplyr)

swf <- read_sf('Friesland_GPI_App/parcel_geometry/LUI_SWF_2016_WGS84.shp')
locations <- readRDS('Friesland_GPI_App/allLocations.rds')
locations <- locations %>%
  mutate(year = format(timestamp, "%Y")) %>%
  filter(year >= "2021")

# Convert locations to sf object (assuming lon and lat columns)
locations_sf <- st_as_sf(locations, coords = c("location_long", "location_lat"), crs = 4326)

# Crop locations using the bounding box of swf before final clip
locations_clipped <- locations_sf %>%
  st_crop(st_bbox(swf))

sf_use_s2(FALSE)
locations_clipped <- st_intersection(locations_clipped,st_make_valid(st_union(swf)))

saveRDS(locations_clipped,'Friesland_GPI_App/locations_swf.rds')
