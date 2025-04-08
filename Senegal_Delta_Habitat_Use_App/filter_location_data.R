library(sf)
library(terra)
library(dplyr)
library(lubridate)

# load study area boundary for clipping locations
SRD_boundary <- read_sf('Senegal_delta/SRD_boundary/transboundary-biosphere-reserve.shp')

# load locations
locations <- readRDS('Senegal_delta/allLocations.rds')
locations <- locations %>%
  mutate(year = format(timestamp, "%Y")) %>%
  filter(year >= "2021")

# convert locations to sf object
locations_sf <- st_as_sf(locations, coords = c("location_long", "location_lat"), crs = 4326)

# clip locations
locations_clipped <- locations_sf %>%
  st_crop(st_bbox(SRD_boundary))

# function to compute season
compute_season <- function(ts) {
  m <- month(ts)
  y <- year(ts)
  ifelse(m >= 7,
         paste0(y, "-", y + 1),
         paste0(y - 1, "-", y))
}

# apply function
locations_clipped$season <- compute_season(locations_clipped$timestamp)

# load land cover raster to extract land cover class for each location
land_cover_raster <- rast("Senegal_delta/landCover.tif")

# extract the land cover
lc <- terra::extract(land_cover_raster, vect(locations_clipped))
locations_clipped$land_cover <- lc[, 2]

# save
saveRDS(locations_clipped, "Senegal_delta/locations_senegal_delta.rds")


