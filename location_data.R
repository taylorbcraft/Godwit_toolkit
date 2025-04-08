library(move)
library(dplyr)
library(purrr)
library(data.table)
library(zoo)
library(lubridate)

# movebank login
login <- movebankLogin(username = "t.b.craft", password = "GodwitSnl24!!")

# study IDs and names
study_ids <- list(
  ib19 = 652989041, microwave2021 = 1498143083, extremadura2022 = 1923591036, 
  extremadura2023 = 2638950465, southholland2021 = 1145538280, BtgTagus2021 = 1693518103, 
  ad_dum2018 = 484019425, ad_dum2019 = 831990025, ad_dum2020 = 1105026166, ad_dum2021 = 1482506572, 
  ad_dum2022 = 1751337831, ad_dum2023 = 2635621808, ad_dum2024 = 3626635334, ch_dum2018 = 500187586, 
  ch_dum2019 = 878914763, ch_dum2020 = 1183466126, ch_dum2021 = 1482505185, ch_dum2022 = 2098519852, 
  ch_dum2023 = 2791727214, ch_dum2024 = 3864855585, hmadults = 69402287, hrjuv = 76429224, 
  hrjuv2016 = 175328223, hrjuv2017 = 293970900, HQXS_Black_tailed_godwits = 1658294759, 
  iberiaBlackwits = 49547785, icarus = 1487044886, polish = 163516781, teamPiersmaHQXS = 1563249841, 
  teamPiersmaHQXS2022 = 2083443328, teamPiersmaHQXS2023 = 2654984909, teamPiersmaHQXS2024 = 3395897563, 
  teamPiersmaInterrex2023 = 2621200322, wildjuv = 75360602, wildjuv2016 = 170829089, wildjuv2017 = 282596404, 
  VeenVitaal2023 = 2749104371
)

# function to get location data
get_data <- function(study_id) {
  location_data <- as.data.frame(getMovebankData(study = study_id, login = login, removeDuplicatedTimestamps = TRUE))
}

all_study_data <- map(unname(study_ids), get_data)

# change cols to characters so they can merge
all_study_data <- lapply(all_study_data, function(df) {
  df$ring_id <- as.character(df$ring_id)
  return(df)
})

all_study_data <- lapply(all_study_data, function(df) {
  df$tag_local_identifier <- as.character(df$tag_local_identifier)
  return(df)
})

all_study_data <- lapply(all_study_data, function(df) {
  df$comments <- as.character(df$comments)
  return(df)
})

all_study_data <- lapply(all_study_data, function(df) {
  # check if `mortality_status` exists; if not, create it as a character column
  if (!"mortality_status" %in% names(df)) {
    df$mortality_status <- NA_character_  # create as NA if not present
  } else {
    df$mortality_status <- as.character(df$mortality_status) 
  }
  return(df)
})

# combine the data frames into one
combined_data <- bind_rows(all_study_data)


# keep only limosa limosa subspecies
unique(combined_data$taxon_detail)
combined_data <- combined_data[combined_data$taxon_detail %in% c("Limosa limosa limosa", NA, "Limosa limosa limos", "ssp. lmosa"), ]


# remove outliers
combined_data_filter <- combined_data %>%
  group_by(trackId) %>%
  arrange(trackId, timestamp) %>%
  mutate(across(c(location_lat, location_long), 
                list(mean_5d = ~ zoo::rollapply(.x, width = 5, FUN = mean, fill = NA),
                     sd_5d = ~ zoo::rollapply(.x, width = 5, FUN = sd, fill = NA)))) %>%
  mutate(lat.dev.to.roll = abs(location_lat - location_lat_mean_5d)^2 / location_lat_sd_5d,
         lon.dev.to.roll = abs(location_long - location_long_mean_5d)^2 / location_long_sd_5d,
         outlier = if_else(lat.dev.to.roll > 10 | lon.dev.to.roll > 10, "outlier", "normal")) %>%
  filter(outlier == "normal") %>%
  ungroup() %>%
  mutate(year = year(timestamp)) %>%
  distinct()

combined_data_filter <- combined_data_filter %>%
  filter(is.na(argos_lc) | !(argos_lc %in% c("A", "B", "C", "Z"))) # remove low quality Argos locations

combined_data_filter <- combined_data_filter %>%
  filter(is.na(argos_lc) | (argos_lc %in% c("1","2","3"))) # remove low quality Argos locations

combined_data_filter <- combined_data_filter %>%
  filter(is.na(ground_speed) | ground_speed < 1) # remove in flight locs

combined_data_filter <- combined_data_filter %>%
  filter(is.na(argos_altitude) | argos_altitude < 100) # remove in flight locs

# round to nearest hour and remove duplicates
combined_data_filter <- combined_data_filter %>%
  mutate(timestamp = round_date(timestamp, unit = "hour"))

# remove duplicate timestamps per trackId
combined_data_filter <- combined_data_filter %>%
  distinct(trackId, timestamp, .keep_all = TRUE)

# keep only 6 locs/day
combined_data_filter_6 <- combined_data_filter %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(trackId, date) %>%
  slice_head(n = 6) %>%
  ungroup() %>%
  dplyr::select(-date)

#### get tagging site ####
library(dplyr)
library(geosphere)
library(rnaturalearth)
library(sf)

# convert lat/lon to sf points
combined_data_sf <- combined_data_filter_6 %>%
  st_as_sf(coords = c("location_long", "location_lat"), crs = 4326, remove = FALSE)

# get country polygons
world <- st_make_valid(ne_countries(scale = "medium", returnclass = "sf"))

# find country per point
combined_data_sf <- st_join(combined_data_sf, world["iso_a2"])

# extract country of first location per trackId
tag_site_lookup <- combined_data_sf %>%
  arrange(trackId, timestamp) %>%
  group_by(trackId) %>%
  slice(1) %>%
  ungroup() %>%
  select(trackId, tag_site = iso_a2)

tag_site_lookup <- tag_site_lookup %>%
  mutate(tag_site = ifelse(tag_site %in% c("NL", "DE", "PT", "ES", "PL"), tag_site, NA))

# join back to full dataset
combined_data_with_tag_site <- combined_data_filter_6 %>%
  left_join(tag_site_lookup, by = "trackId")

combined_data_with_tag_site$tag_site <- as.factor(combined_data_with_tag_site$tag_site)

# keep only desired columns
allLocations <- combined_data_with_tag_site %>% 
  dplyr::select(trackId, timestamp, location_lat, location_long, ring_id, sex, sensor,tag_site)

#### export to respective directory #### 
saveRDS(allLocations, "Donana_flooding/allLocations.rds")
saveRDS(allLocations, "GPI/allLocations.rds")
saveRDS(allLocations, "Senegal_delta/allLocations.rds")
saveRDS(allLocations, "Summary_stats/allLocations.rds")




