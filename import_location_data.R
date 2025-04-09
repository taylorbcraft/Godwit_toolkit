library(dplyr)
library(move)
library(data.table)
library(zoo)
library(lubridate)
library(rnaturalearth)
library(sf)
cat("Starting import script\n")



# movebank login
login <- movebankLogin(username = "t.b.craft", password = "GodwitSnl24!!")
cat("Login successful\n")

# load each study
ib19 <- as.data.table(as.data.frame(getMovebankData(study = 652989041, login = login, removeDuplicatedTimestamps = TRUE)))
cat("First study downloaded\n")
microwave2021 <- as.data.table(as.data.frame(getMovebankData(study = 1498143083, login = login, removeDuplicatedTimestamps = TRUE)))
extremadura2022 <- as.data.table(as.data.frame(getMovebankData(study = 1923591036, login = login, removeDuplicatedTimestamps = TRUE)))
extremadura2023 <- as.data.table(as.data.frame(getMovebankData(study = 2638950465, login = login, removeDuplicatedTimestamps = TRUE)))
southholland2021 <- as.data.table(as.data.frame(getMovebankData(study = 1145538280, login = login, removeDuplicatedTimestamps = TRUE)))
BtgTagus2021 <- as.data.table(as.data.frame(getMovebankData(study = 1693518103, login = login, removeDuplicatedTimestamps = TRUE)))
ad_dum2018 <- as.data.table(as.data.frame(getMovebankData(study = 484019425, login = login, removeDuplicatedTimestamps = TRUE)))
ad_dum2019 <- as.data.table(as.data.frame(getMovebankData(study = 831990025, login = login, removeDuplicatedTimestamps = TRUE)))
ad_dum2020 <- as.data.table(as.data.frame(getMovebankData(study = 1105026166, login = login, removeDuplicatedTimestamps = TRUE)))
ad_dum2021 <- as.data.table(as.data.frame(getMovebankData(study = 1482506572, login = login, removeDuplicatedTimestamps = TRUE)))
ad_dum2022 <- as.data.table(as.data.frame(getMovebankData(study = 1751337831, login = login, removeDuplicatedTimestamps = TRUE)))
ad_dum2023 <- as.data.table(as.data.frame(getMovebankData(study = 2635621808, login = login, removeDuplicatedTimestamps = TRUE)))
ad_dum2024 <- as.data.table(as.data.frame(getMovebankData(study = 3626635334, login = login, removeDuplicatedTimestamps = TRUE)))
ch_dum2018 <- as.data.table(as.data.frame(getMovebankData(study = 500187586, login = login, removeDuplicatedTimestamps = TRUE)))
ch_dum2019 <- as.data.table(as.data.frame(getMovebankData(study = 878914763, login = login, removeDuplicatedTimestamps = TRUE)))
ch_dum2020 <- as.data.table(as.data.frame(getMovebankData(study = 1183466126, login = login, removeDuplicatedTimestamps = TRUE)))
ch_dum2021 <- as.data.table(as.data.frame(getMovebankData(study = 1482505185, login = login, removeDuplicatedTimestamps = TRUE)))
ch_dum2022 <- as.data.table(as.data.frame(getMovebankData(study = 2098519852, login = login, removeDuplicatedTimestamps = TRUE)))
ch_dum2023 <- as.data.table(as.data.frame(getMovebankData(study = 2791727214, login = login, removeDuplicatedTimestamps = TRUE)))
ch_dum2024 <- as.data.table(as.data.frame(getMovebankData(study = 3864855585, login = login, removeDuplicatedTimestamps = TRUE)))
hmadults <- as.data.table(as.data.frame(getMovebankData(study = 69402287, login = login, removeDuplicatedTimestamps = TRUE)))
hrjuv <- as.data.table(as.data.frame(getMovebankData(study = 76429224, login = login, removeDuplicatedTimestamps = TRUE)))
hrjuv2016 <- as.data.table(as.data.frame(getMovebankData(study = 175328223, login = login, removeDuplicatedTimestamps = TRUE)))
hrjuv2017 <- as.data.table(as.data.frame(getMovebankData(study = 293970900, login = login, removeDuplicatedTimestamps = TRUE)))
HQXS_Black_tailed_godwits <- as.data.table(as.data.frame(getMovebankData(study = 1658294759, login = login, removeDuplicatedTimestamps = TRUE)))
iberiaBlackwits <- as.data.table(as.data.frame(getMovebankData(study = 49547785, login = login, removeDuplicatedTimestamps = TRUE)))
icarus <- as.data.table(as.data.frame(getMovebankData(study = 1487044886, login = login, removeDuplicatedTimestamps = TRUE)))
polish <- as.data.table(as.data.frame(getMovebankData(study = 163516781, login = login, removeDuplicatedTimestamps = TRUE)))
teamPiersmaHQXS <- as.data.table(as.data.frame(getMovebankData(study = 1563249841, login = login, removeDuplicatedTimestamps = TRUE)))
teamPiersmaHQXS2022 <- as.data.table(as.data.frame(getMovebankData(study = 2083443328, login = login, removeDuplicatedTimestamps = TRUE)))
teamPiersmaHQXS2023 <- as.data.table(as.data.frame(getMovebankData(study = 2654984909, login = login, removeDuplicatedTimestamps = TRUE)))
teamPiersmaHQXS2024 <- as.data.table(as.data.frame(getMovebankData(study = 3395897563, login = login, removeDuplicatedTimestamps = TRUE)))
teamPiersmaInterrex2023 <- as.data.table(as.data.frame(getMovebankData(study = 2621200322, login = login, removeDuplicatedTimestamps = TRUE)))
wildjuv <- as.data.table(as.data.frame(getMovebankData(study = 75360602, login = login, removeDuplicatedTimestamps = TRUE)))
wildjuv2016 <- as.data.table(as.data.frame(getMovebankData(study = 170829089, login = login, removeDuplicatedTimestamps = TRUE)))
wildjuv2017 <- as.data.table(as.data.frame(getMovebankData(study = 282596404, login = login, removeDuplicatedTimestamps = TRUE)))
VeenVitaal2023 <- as.data.table(as.data.frame(getMovebankData(study = 2749104371, login = login, removeDuplicatedTimestamps = TRUE)))
cat("All studies downloaded\n")

# merge everything
all_study_data <- rbindlist(list(
  ib19, microwave2021, extremadura2022, extremadura2023, southholland2021, BtgTagus2021,
  ad_dum2018, ad_dum2019, ad_dum2020, ad_dum2021, ad_dum2022, ad_dum2023, ad_dum2024,
  ch_dum2018, ch_dum2019, ch_dum2020, ch_dum2021, ch_dum2022, ch_dum2023, ch_dum2024,
  hmadults, hrjuv, hrjuv2016, hrjuv2017,
  HQXS_Black_tailed_godwits, iberiaBlackwits, icarus, polish,
  teamPiersmaHQXS, teamPiersmaHQXS2022, teamPiersmaHQXS2023, teamPiersmaHQXS2024,
  teamPiersmaInterrex2023, wildjuv, wildjuv2016, wildjuv2017, VeenVitaal2023
), use.names = TRUE, fill = TRUE)

# filter to limosa limosa
combined_data <- all_study_data[taxon_detail %in% c("Limosa limosa limosa", NA, "Limosa limosa limos", "ssp. lmosa")]

# remove outliers
setorder(combined_data, trackId, timestamp)
combined_data[, `:=`(
  location_lat_mean_5d = zoo::rollapply(location_lat, 5, mean, fill = NA),
  location_lat_sd_5d   = zoo::rollapply(location_lat, 5, sd, fill = NA),
  location_long_mean_5d = zoo::rollapply(location_long, 5, mean, fill = NA),
  location_long_sd_5d   = zoo::rollapply(location_long, 5, sd, fill = NA)
), by = trackId]

combined_data[, lat.dev.to.roll := (abs(location_lat - location_lat_mean_5d)^2) / location_lat_sd_5d]
combined_data[, lon.dev.to.roll := (abs(location_long - location_long_mean_5d)^2) / location_long_sd_5d]
combined_data[, outlier := ifelse(lat.dev.to.roll > 10 | lon.dev.to.roll > 10, "outlier", "normal")]
combined_data_filter <- combined_data[outlier == "normal"]
combined_data_filter[, year := year(timestamp)]
combined_data_filter <- unique(combined_data_filter)

# remove low quality and in-flight
combined_data_filter <- combined_data_filter[
  (is.na(argos_lc) | !(argos_lc %in% c("A", "B", "C", "Z"))) &
    (is.na(argos_lc) | (argos_lc %in% c("1", "2", "3"))) &
    (is.na(ground_speed) | ground_speed < 1) &
    (is.na(argos_altitude) | argos_altitude < 100)
]

# round timestamps and deduplicate
combined_data_filter[, timestamp := round_date(timestamp, unit = "hour")]
combined_data_filter <- unique(combined_data_filter, by = c("trackId", "timestamp"))

# keep 6 per day
combined_data_filter_6 <- combined_data_filter %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(trackId, date) %>%
  slice_head(n = 6) %>%
  ungroup() %>%
  dplyr::select(-date)

# tagging site
combined_data_sf <- st_as_sf(combined_data_filter_6, coords = c("location_long", "location_lat"), crs = 4326, remove = FALSE)
world <- st_make_valid(ne_countries(scale = "medium", returnclass = "sf"))
combined_data_sf <- st_join(combined_data_sf, world["iso_a2"])

tag_site_lookup <- combined_data_sf %>%
  arrange(trackId, timestamp) %>%
  group_by(trackId) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(trackId, tag_site = iso_a2) %>%
  mutate(tag_site = ifelse(tag_site %in% c("NL", "DE", "PT", "ES", "PL"), tag_site, NA))

combined_data_with_tag_site <- as.data.table(combined_data_filter_6) %>%
  merge(as.data.table(tag_site_lookup), by = "trackId", all.x = TRUE)

combined_data_with_tag_site[, tag_site := as.factor(tag_site)]

# final export object
allLocations <- combined_data_with_tag_site[, .(trackId, timestamp, location_lat, location_long, ring_id, sex, sensor, tag_site)]

# export
saveRDS(allLocations, "Doñana_Wetland_Viewer/allLocations.rds")
saveRDS(allLocations, "Friesland_GPI_App/allLocations.rds")
saveRDS(allLocations, "Senegal_Delta_Habitat_Use_App/allLocations.rds")
saveRDS(allLocations, "Flyway_Tracking_Summary_App/allLocations.rds")
