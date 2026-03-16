### travel_time.R
### run after merge_and_finalize.R
### adds driving and transit travel times to Pierce County Superior Courthouse
### requires Google Maps Distance Matrix API key in .env

#load packages
library(tidyverse)
library(httr)

#load environment variables
readRenviron(".env")
api_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")

#load data
final_merged_data <- readRDS("data/final_merged_data.rds")

#courthouse destination (Pierce County Superior Court)
courthouse_lat <- 47.2529
courthouse_lon <- -122.4443

#departure time: 1:00pm on a representative weekday (Wednesday)
#using a fixed date ensures reproducibility — transit schedules are time-sensitive
departure_time <- as.integer(as.POSIXct("2024-01-17 13:00:00", tz = "America/Los_Angeles"))

#check coordinate coverage
cat("Cases with coordinates:", sum(!is.na(final_merged_data$lat)), "\n")
cat("Cases missing coordinates:", sum(is.na(final_merged_data$lat)), "\n")

#distance matrix API function — one origin, one destination, one mode
get_travel_time <- function(origin_lat, origin_lon, dest_lat, dest_lon,
                            mode, departure_time, key) {
  resp <- GET(
    "https://maps.googleapis.com/maps/api/distancematrix/json",
    query = list(
      origins        = paste0(origin_lat, ",", origin_lon),
      destinations   = paste0(dest_lat, ",", dest_lon),
      mode           = mode,
      departure_time = departure_time,
      key            = key
    )
  )
  result <- content(resp, as = "parsed")
  
  element <- result$rows[[1]]$elements[[1]]
  
  if (element$status == "OK") {
    tibble(
      duration_secs = element$duration$value,
      duration_text = element$duration$text,
      status        = "OK"
    )
  } else {
    tibble(
      duration_secs = NA_real_,
      duration_text = NA_character_,
      status        = element$status
    )
  }
}

#deduplicate to unique coordinates — many cases share the same building address
unique_coords <- final_merged_data %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  distinct(lat, lon)

cat("Unique coordinate pairs:", nrow(unique_coords), "\n")

#run driving times
driving_times <- unique_coords %>%
  mutate(row = row_number()) %>%
  group_by(row) %>%
  group_modify(~{
    Sys.sleep(0.05)
    bind_cols(
      .x,
      get_travel_time(
        .x$lat, .x$lon,
        courthouse_lat, courthouse_lon,
        mode           = "driving",
        departure_time = departure_time,
        key            = api_key
      ) %>% rename(
        drive_time_secs = duration_secs,
        drive_time_text = duration_text,
        drive_status    = status
      )
    )
  }) %>%
  ungroup() %>%
  select(-row)

#run transit times
transit_times <- unique_coords %>%
  mutate(row = row_number()) %>%
  group_by(row) %>%
  group_modify(~{
    Sys.sleep(0.05)
    bind_cols(
      .x,
      get_travel_time(
        .x$lat, .x$lon,
        courthouse_lat, courthouse_lon,
        mode           = "transit",
        departure_time = departure_time,
        key            = api_key
      ) %>% rename(
        transit_time_secs = duration_secs,
        transit_time_text = duration_text,
        transit_status    = status
      )
    )
  }) %>%
  ungroup() %>%
  select(-row)

#join driving and transit results
travel_times <- driving_times %>%
  left_join(transit_times, by = c("lat", "lon"))

#save raw results before patching — avoids re-running API calls if needed
saveRDS(travel_times, "data/travel_times_raw.rds")

#qc: check status distribution
cat("\nDriving status:\n")
print(count(travel_times, drive_status))
cat("\nTransit status:\n")
print(count(travel_times, transit_status))

#convert seconds to minutes for interpretability
travel_times <- travel_times %>%
  mutate(
    drive_time_mins   = round(drive_time_secs / 60, 1),
    transit_time_mins = round(transit_time_secs / 60, 1)
  )

#summary statistics
cat("\nDriving time (minutes):\n")
print(summary(travel_times$drive_time_mins))
cat("\nTransit time (minutes):\n")
print(summary(travel_times$transit_time_mins))

#join travel times back onto final_merged_data by coordinates
final_merged_data <- final_merged_data %>%
  left_join(
    travel_times %>% select(lat, lon, drive_time_mins, transit_time_mins),
    by = c("lat", "lon")
  )

#final coverage check
cat("\nCases with driving time:", sum(!is.na(final_merged_data$drive_time_mins)), "\n")
cat("Cases with transit time:", sum(!is.na(final_merged_data$transit_time_mins)), "\n")

#overwrite final_merged_data with travel time variables added
saveRDS(final_merged_data, "data/final_merged_data.rds")