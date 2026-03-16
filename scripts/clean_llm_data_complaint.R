#load packages
library(tidyverse)
library(sf)
library(tigris)
library(httr)
library(readr)
library(lubridate)

#load environment variables
readRenviron(".env")
api_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")

#read in complaint llm data
llm_data_complaint <- read_rds("data/llm_data_complaint.rds")

### clean case numbers ###

#look for possible OCR errors in case numbers, fix if paths and case numbers are misaligned
complaint_mismatch <- llm_data_complaint %>%
  mutate(
    direc_case = basename(direc),
    match_direc_case = direc_case == case_number
  ) %>%
  filter(is.na(match_direc_case) | !match_direc_case) %>%
  select(row_id, case_number, direc, direc_case)

source("scripts/fix_by_path.R")
llm_data_complaint_clean <- fix_by_path(llm_data_complaint)

#set case_type to commercial if defendant names include LLC or INC
llm_data_complaint_clean <- llm_data_complaint_clean %>%
  mutate(
    #check if ANY defendant name contains LLC, Inc or Rentals (case insensitive)
    is_commercial = map_lgl(defendant_names, function(names) {
      if (is.null(names) || length(names) == 0) return(FALSE)
      any(str_detect(names, regex("\\bLLC\\b|\\bINC\\b|\\bRENTALS\\b", ignore_case = TRUE)))
    }),
    
    #set case_type
    case_type = if_else(is_commercial, "commercial", "residential")
  )

### clean address data ###

#check for missing data in address columns, between 1-4% for each column
cols <- c("address", "address_street",
          "address_city", "address_state", "address_zip")
count_empty <- function(x) sum(!is.na(x) & x == "")
missing_summary <- tibble(
  column = cols,
  n_na = map_int(cols, ~ sum(is.na(llm_data_complaint_clean[[.x]]))),
  n_empty = map_int(cols, ~ count_empty(llm_data_complaint_clean[[.x]])),
  n_total_missing = n_na + n_empty,
  pct_missing = round(n_total_missing / nrow(llm_data_complaint_clean) * 100, 2)
)
missing_summary

#check for address columns with leading or trailing whitespace
llm_data_complaint_clean %>%
  summarise(across(starts_with("address"), ~ sum(str_detect(., "^\\s|\\s$"), na.rm = TRUE)))

#trim whitespace for a few address entries
llm_data_complaint_clean <- llm_data_complaint_clean %>%
  mutate(address = if_else(is.na(address), NA_character_, str_squish(address)))

#clean address_city manually
llm_data_complaint_clean <- llm_data_complaint_clean %>%
  mutate(address_city = case_when(
    address_city == "Bonney" ~ "Bonney Lake",
    address_city == "Bonneylake" ~ "Bonney Lake",
    address_city == "DuPont" ~ "Dupont",
    address_city == "Gerei Harbor" ~ "Gig Harbor",
    address_city == "Lake Tipps" ~ "Lake Tapps",
    address_city == "Purellup" ~ "Puyallup",
    address_city == "Puyalllup" ~ "Puyallup",
    address_city == "Tacuman" ~ "Tacoma",
    address_city == "Jacana" ~ "Tacoma",
    address_city == "Miljonua" ~ "Milton",
    address_city == "Summer" ~ "Sumner",
    address_city == "Serving" ~ "Spanaway",
    address_city == "Shake Bary" ~ "Lakebay",
    TRUE ~ address_city
  ))

### add census block groups ###

#reconstruct full addresses
llm_data_complaint_clean <- llm_data_complaint_clean %>%
  mutate(
    full_address = paste(address_street, address_city, address_state, address_zip, sep = ", "),
    has_complete_address = !if_any(
      c(address_street, address_city, address_state, address_zip),
      ~ is.na(.) | . == ""
    )
  )

#geocode using Census batch geocoder
addresses_to_geocode <- llm_data_complaint_clean %>%
  filter(has_complete_address) %>%
  distinct(full_address) %>%
  rowid_to_column("id") %>%
  separate(full_address,
           into = c("street", "city", "state", "zip"),
           sep = ", ",
           remove = FALSE)

batch_data <- addresses_to_geocode %>%
  select(id, street, city, state, zip) %>%
  mutate(across(c(street, city, state, zip), ~ replace_na(., "")))

temp_file <- tempfile(fileext = ".csv")
write_csv(batch_data, temp_file, col_names = FALSE)

response <- POST(
  "https://geocoding.geo.census.gov/geocoder/locations/addressbatch",
  body = list(addressFile = upload_file(temp_file), benchmark = "Public_AR_Current"),
  encode = "multipart"
)

geocoded_results <- content(response, as = "text") %>%
  read_csv(col_names = c("id", "input", "match", "match_type", "lon", "lat", "tigerline_id", "side"),
           show_col_types = FALSE) %>%
  filter(match == "Match") %>%
  separate(lat, into = c("lon", "lat"), sep = ",", convert = TRUE) %>%
  select(id, lon, lat)

address_coords <- addresses_to_geocode %>%
  select(id, full_address) %>%
  left_join(geocoded_results, by = "id") %>%
  select(full_address, lon, lat) %>%
  distinct()

llm_data_complaint_clean <- llm_data_complaint_clean %>%
  left_join(address_coords, by = "full_address")

#get census block groups
complaint_sf <- llm_data_complaint_clean %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

cbg_wa <- block_groups(state = "WA", year = 2020) %>%
  st_transform(st_crs(complaint_sf))

result <- st_join(complaint_sf, cbg_wa, join = st_within)

cbg_lookup <- result %>%
  st_drop_geometry() %>%
  select(row_id, GEOID) %>%
  rename(census_block_group = GEOID)

llm_data_complaint_clean <- llm_data_complaint_clean %>%
  left_join(cbg_lookup, by = "row_id")

#identify cases with multiple block groups
cases_multi_cbg <- llm_data_complaint_clean %>%
  filter(!is.na(census_block_group)) %>%
  group_by(case_number) %>%
  summarize(
    n_unique_cbg = n_distinct(census_block_group),
    n_records = n(),
    .groups = "drop"
  ) %>%
  filter(n_unique_cbg > 1)

#identify cases where geocoding was not successful
geocoding_summary <- tibble(
  n_total = nrow(llm_data_complaint_clean),
  n_complete_address = sum(llm_data_complaint_clean$has_complete_address),
  n_geocoded = sum(!is.na(llm_data_complaint_clean$lon)),
  n_with_cbg = sum(!is.na(llm_data_complaint_clean$census_block_group)),
  pct_geocoded = round(n_geocoded / n_total * 100, 2),
  pct_with_cbg = round(n_with_cbg / n_total * 100, 2),
  n_cases_multi_cbg = nrow(cases_multi_cbg)
)

print(geocoding_summary)

### google maps geocoding fallback ###

#define known out-of-county cases to exclude from geocoding
out_of_county_cases <- c(
  "22-2-07206-2",  # Auburn
  "22-2-09620-4",  # Seattle
  "23-2-05570-1",  # Yelm
  "23-2-05739-8",  # Clinton
  "23-2-07405-5",  # Tumwater
  "24-2-05674-8",  # Auburn
  "24-2-07646-3",  # Kent
  "24-2-09155-1"   # Arlington
)

#identify cases not geocoded by Census geocoder that have recoverable addresses
to_geocode_google <- llm_data_complaint_clean %>%
  filter(
    is.na(census_block_group),
    !is.na(address_street), address_street != "",
    !case_number %in% out_of_county_cases
  ) %>%
  mutate(
    address_zip_clean = str_extract(address_zip, "^\\d{5}"),
    #strip unit number — geocoders match to building; unit irrelevant for lat/lon
    geocode_input = str_squish(paste0(
      address_street, ", ",
      address_city, ", ",
      "Pierce County, WA",
      if_else(!is.na(address_zip_clean) & address_zip_clean != "",
              paste0(" ", address_zip_clean), "")
    ))
  )

#geocoder function
geocode_google <- function(address, key) {
  resp <- GET(
    "https://maps.googleapis.com/maps/api/geocode/json",
    query = list(address = address, key = key)
  )
  result <- content(resp, as = "parsed")
  
  if (result$status == "OK") {
    loc <- result$results[[1]]$geometry$location
    tibble(
      lat_new           = loc$lat,
      lon_new           = loc$lng,
      geocode_status    = "OK",
      geocode_type      = result$results[[1]]$types[[1]],
      formatted_address = result$results[[1]]$formatted_address
    )
  } else {
    tibble(
      lat_new           = NA_real_,
      lon_new           = NA_real_,
      geocode_status    = result$status,
      geocode_type      = NA_character_,
      formatted_address = NA_character_
    )
  }
}

#run google geocoder with rate limiting
geocode_results_google <- to_geocode_google %>%
  mutate(row = row_number()) %>%
  group_by(row) %>%
  group_modify(~{
    Sys.sleep(0.05)
    bind_cols(.x, geocode_google(.x$geocode_input, api_key))
  }) %>%
  ungroup()

#save raw results before patching — avoids re-running API calls if needed
saveRDS(geocode_results_google, "data/geocode_google_fallback.rds")

#qc: review match status and type distribution
print(count(geocode_results_google, geocode_status))
print(count(geocode_results_google, geocode_type))

#keep only street-level matches — coarser types unreliable for block group assignment
geocode_usable <- geocode_results_google %>%
  filter(
    geocode_status == "OK",
    geocode_type %in% c("street_address", "premise", "subpremise")
  )

#spatial join to Pierce County block groups (year = 2020 to match Census geocoder above)
pierce_bg <- block_groups(state = "WA", county = "Pierce", year = 2020) %>%
  st_transform(4326)

geocode_sf <- geocode_usable %>%
  st_as_sf(coords = c("lon_new", "lat_new"), crs = 4326)

geocode_with_cbg <- st_join(geocode_sf, pierce_bg["GEOID"], join = st_within) %>%
  st_drop_geometry() %>%
  select(row_id, census_block_group_google = GEOID)

#carry lat/lon separately from the pre-geometry dataframe
geocode_coords <- geocode_usable %>%
  select(row_id, lat_new, lon_new)

geocode_with_cbg <- geocode_with_cbg %>%
  left_join(geocode_coords, by = "row_id")

#patch google results into complaint data, preserving Census geocoder where it succeeded
llm_data_complaint_clean <- llm_data_complaint_clean %>%
  left_join(geocode_with_cbg, by = "row_id") %>%
  mutate(
    lon = coalesce(lon, lon_new),
    lat = coalesce(lat, lat_new),
    census_block_group = coalesce(census_block_group, census_block_group_google)
  ) %>%
  select(-lon_new, -lat_new, -census_block_group_google)

#final geocoding coverage summary
geocoding_summary_final <- tibble(
  n_total      = nrow(llm_data_complaint_clean),
  n_geocoded   = sum(!is.na(llm_data_complaint_clean$lon)),
  n_with_cbg   = sum(!is.na(llm_data_complaint_clean$census_block_group)),
  pct_geocoded = round(n_geocoded / n_total * 100, 2),
  pct_with_cbg = round(n_with_cbg / n_total * 100, 2)
)

print(geocoding_summary_final)

### clean amount owed data ###

#convert amount_owed variable
llm_data_complaint_clean <- llm_data_complaint_clean %>%
  mutate(
    amount_owed = as.numeric(
      na_if(
        str_replace_all(str_trim(as.character(amount_owed)), "[$,]", ""),
        ""
      )
    )
  )

#check for outliers
amount_summary <- llm_data_complaint_clean %>%
  filter(!is.na(amount_owed), amount_owed >= 10000) %>%
  mutate(ends_in_00 = amount_owed %% 100 == 0) %>%
  summarise(
    n_above_9999 = n(),
    n_ends_in_00 = sum(ends_in_00),
    pct_ends_in_00 = round(100 * mean(ends_in_00), 2)
  )

saveRDS(llm_data_complaint_clean, "data/llm_data_complaint_clean.rds")
