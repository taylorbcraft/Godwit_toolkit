library(dplyr)
library(move)
library(data.table)
library(zoo)
library(lubridate)
library(rnaturalearth)
library(sf)

cat("Starting import script\n")

# Single auth path: read from env vars set by GitHub Actions secrets
u <- Sys.getenv("MOVEBANK_USER", "")
p <- Sys.getenv("MOVEBANK_PASSWORD", "")
if (!nzchar(u) || !nzchar(p)) {
  stop("Movebank credentials not found. Set MOVEBANK_USER and MOVEBANK_PASSWORD in GitHub Actions secrets or your local .Renviron.")
}

# Create Movebank login using those env vars
login <- move::movebankLogin(username = u, password = p)


# Helper to load data and attach study name
load_study_with_name <- function(study_id, login) {
  data <- as.data.table(as.data.frame(getMovebankData(study = study_id, login = login, removeDuplicatedTimestamps = TRUE)))
  study_info <- getMovebankStudy(study_id, login = login)
  data[, `:=`(
    study_name = study_info$name,
    study_id = study_id  # capture the numeric ID
  )]
  return(data)
}


# Load all studies
ib19 <- load_study_with_name(652989041, login)
cat("First study downloaded\n")
microwave2021 <- load_study_with_name(1498143083, login)
extremadura2022 <- load_study_with_name(1923591036, login)
extremadura2023 <- load_study_with_name(2638950465, login)
southholland2021 <- load_study_with_name(1145538280, login)
BtgTagus2021 <- load_study_with_name(1693518103, login)
ad_dum2018 <- load_study_with_name(484019425, login)
ad_dum2019 <- load_study_with_name(831990025, login)
ad_dum2020 <- load_study_with_name(1105026166, login)
ad_dum2021 <- load_study_with_name(1482506572, login)
ad_dum2022 <- load_study_with_name(1751337831, login)
ad_dum2023 <- load_study_with_name(2635621808, login)
ad_dum2024 <- load_study_with_name(3626635334, login)
ad_dum2025 <- load_study_with_name(5867603206, login)
ch_dum2018 <- load_study_with_name(500187586, login)
ch_dum2019 <- load_study_with_name(878914763, login)
ch_dum2020 <- load_study_with_name(1183466126, login)
ch_dum2021 <- load_study_with_name(1482505185, login)
ch_dum2022 <- load_study_with_name(2098519852, login)
ch_dum2023 <- load_study_with_name(2791727214, login)
ch_dum2024 <- load_study_with_name(3864855585, login)
ch_dum2025 <- load_study_with_name(6401201060, login)
hmadults <- load_study_with_name(69402287, login)
hrjuv <- load_study_with_name(76429224, login)
hrjuv2016 <- load_study_with_name(175328223, login)
hrjuv2017 <- load_study_with_name(293970900, login)
HQXS_Black_tailed_godwits <- load_study_with_name(1658294759, login)
iberiaBlackwits <- load_study_with_name(49547785, login)
icarus <- load_study_with_name(1487044886, login)
polish <- load_study_with_name(163516781, login)
teamPiersmaHQXS <- load_study_with_name(1563249841, login)
teamPiersmaHQXS2022 <- load_study_with_name(2083443328, login)
teamPiersmaHQXS2023 <- load_study_with_name(2654984909, login)
teamPiersmaHQXS2024 <- load_study_with_name(3395897563, login)
teamPiersmaHQXS2025 <- load_study_with_name(3395897563, login)
teamPiersmaInterrex2023 <- load_study_with_name(6288645742, login)
wildjuv <- load_study_with_name(75360602, login)
wildjuv2016 <- load_study_with_name(170829089, login)
wildjuv2017 <- load_study_with_name(282596404, login)
VeenVitaal2023 <- load_study_with_name(2749104371, login)
cat("All studies downloaded\n")

# Merge all studies
all_study_data <- rbindlist(list(
  ib19, microwave2021, extremadura2022, extremadura2023, southholland2021, BtgTagus2021,
  ad_dum2018, ad_dum2019, ad_dum2020, ad_dum2021, ad_dum2022, ad_dum2023, ad_dum2024,
  ch_dum2018, ch_dum2019, ch_dum2020, ch_dum2021, ch_dum2022, ch_dum2023, ch_dum2024,
  hmadults, hrjuv, hrjuv2016, hrjuv2017,
  HQXS_Black_tailed_godwits, iberiaBlackwits, icarus, polish,
  teamPiersmaHQXS, teamPiersmaHQXS2022, teamPiersmaHQXS2023, teamPiersmaHQXS2024,
  teamPiersmaInterrex2023, wildjuv, wildjuv2016, wildjuv2017, VeenVitaal2023
), use.names = TRUE, fill = TRUE)

# Filter to limosa limosa
combined_data <- all_study_data[taxon_detail %in% c("Limosa limosa limosa", NA, "Limosa limosa limos", "ssp. lmosa")]

# Remove outliers
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

# Filter low quality / in-flight
combined_data_filter <- combined_data_filter[
  (is.na(argos_lc) | !(argos_lc %in% c("A", "B", "C", "Z"))) &
    (is.na(argos_lc) | (argos_lc %in% c("1", "2", "3"))) &
    (is.na(ground_speed) | ground_speed < 1) &
    (is.na(argos_altitude) | argos_altitude < 100)
]

# Round timestamp, deduplicate
combined_data_filter[, timestamp := round_date(timestamp, unit = "hour")]
combined_data_filter <- unique(combined_data_filter, by = c("trackId", "timestamp"))

# Keep 6 per day
combined_data_filter_6 <- combined_data_filter %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(trackId, date) %>%
  slice_head(n = 6) %>%
  ungroup() %>%
  dplyr::select(-date)

combined_data_filter_6 <- combined_data_filter_6 %>%
  dplyr::select(trackId, timestamp, location_lat, location_long, ring_id, sex, sensor, study_name, study_id)

# Add tagging site
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

# Final export object
allLocations <- combined_data_with_tag_site[, .(trackId, timestamp, location_lat, location_long, ring_id, sex, sensor, tag_site, study_name, study_id)]
allLocations[, sex := toupper(trimws(sex))]
allLocations[sex == "" | sex == "U" | is.na(sex), sex := NA]
allLocations[, sex := factor(sex, levels = c("M", "F"))]

# Export
saveRDS(allLocations, "Doñana_Wetland_Viewer/allLocations.rds")
saveRDS(allLocations, "Friesland_GPI_App/allLocations.rds")
saveRDS(allLocations, "Senegal_Delta_Habitat_Use_App/allLocations.rds")
saveRDS(allLocations, "Flyway_Tracking_Summary_App/allLocations.rds")
