library(move)
library(dplyr)
library(purrr)
library(data.table)
library(zoo)
library(lubridate)

# Movebank login
login <- movebankLogin(username = "t.b.craft", password = "GodwitSnl24!!")

# Study IDs and names
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

# Function to get location data
get_data <- function(study_id) {
  location_data <- as.data.frame(getMovebankData(study = study_id, login = login, removeDuplicatedTimestamps = TRUE))
}

all_study_data <- map(study_ids, get_data)


# Ensure all `ring_id` columns are of the same type (character)
all_study_data <- lapply(all_study_data, function(df) {
  df$ring_id <- as.character(df$ring_id)
  return(df)
})

# Ensure all `tag_local_identifier` columns are of the same type (character)
all_study_data <- lapply(all_study_data, function(df) {
  df$tag_local_identifier <- as.character(df$tag_local_identifier)
  return(df)
})

# Ensure all `comments` columns are of the same type (character)
all_study_data <- lapply(all_study_data, function(df) {
  df$comments <- as.character(df$comments)
  return(df)
})

# Ensure all `mortality_status` columns exist and are of the same type (character)
all_study_data <- lapply(all_study_data, function(df) {
  # Check if `mortality_status` exists; if not, create it as a character column
  if (!"mortality_status" %in% names(df)) {
    df$mortality_status <- NA_character_  # Create as NA if not present
  } else {
    df$mortality_status <- as.character(df$mortality_status)  # Convert to character
  }
  return(df)
})

# Now combine the data frames into one
combined_data <- bind_rows(all_study_data)


# Keep only limosa limosa subspecies
unique(combined_data$taxon_detail)
combined_data <- combined_data[combined_data$taxon_detail %in% c("Limosa limosa limosa", NA, "Limosa limosa limos", "ssp. lmosa"), ]


# Remove outliers
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
  filter(is.na(argos_lc) | !(argos_lc %in% c("A", "B", "C", "Z"))) # Remove low quality Argos locations

combined_data_filter <- combined_data_filter %>%
  filter(is.na(argos_lc) | (argos_lc %in% c("1","2","3"))) # Remove low quality Argos locations

combined_data_filter <- combined_data_filter %>%
  filter(is.na(ground_speed) | ground_speed < 1)

combined_data_filter <- combined_data_filter %>%
  filter(is.na(argos_altitude) | argos_altitude < 100)

combined_data_filter_5 <- combined_data_filter %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(trackId, date) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  dplyr::select(-date)

# Keep only desired columns
allLocations <- combined_data_filter_5 %>% 
  dplyr::select(trackId, timestamp, location_lat, location_long, ring_id, sex, sensor)

# Save
saveRDS(allLocations, "Donana_flooding/allLocations.rds")
saveRDS(allLocations, "GPI/allLocations.rds")
saveRDS(allLocations, "Senegal_delta/allLocations.rds")
saveRDS(allLocations, "Summary_stats/allLocations.rds")




