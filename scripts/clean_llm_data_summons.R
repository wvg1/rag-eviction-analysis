### this cleaning script should be run after complaint data has been cleaned ###

#load packages
library(tidyverse)
library(sf)
library(httr)
library(readr)
library(tigris)
library(lubridate)

#load environment variables
readRenviron(".env")
api_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")

#load summons llm data
llm_data_summons <- read_rds("data/llm_data_summons.rds")

### clean case numbers ###

#look for possible OCR errors in case numbers, fix if paths and case numbers are misaligned
summons_mismatch <- llm_data_summons %>%
  mutate(
    direc_case = basename(direc),
    match_direc_case = direc_case == case_number
  ) %>%
  filter(is.na(match_direc_case) | !match_direc_case) %>%
  select(row_id, case_number, direc, direc_case)

source("scripts/fix_by_path.R")
llm_data_summons_clean <- fix_by_path(llm_data_summons)

### update case_type ###

#set case_type to commercial if defendant names include LLC or INC
llm_data_summons_clean <- llm_data_summons_clean %>%
  mutate(
    #check if ANY defendant name contains LLC or Inc (case insensitive)
    is_commercial = map_lgl(defendant_names, function(names) {
      if (is.null(names) || length(names) == 0) return(FALSE)
      any(str_detect(names, regex("\\bLLC\\b|\\bINC\\b", ignore_case = TRUE)))
    }),
    
    #set case_type
    case_type = if_else(is_commercial, "commercial", "residential")
  )

### clean address data ###

#check for address columns with leading or trailing whitespace
llm_data_summons_clean %>%
  summarise(across(starts_with("address"), ~ sum(str_detect(., "^\\s|\\s$"), na.rm = TRUE)))

#trim whitespace for a few address entries
llm_data_summons_clean <- llm_data_summons_clean %>%
  mutate(address = if_else(is.na(address), NA_character_, str_squish(address)))

#fix a few addresses manually
manual_fixes_street <- tibble(
  address_street_old = c(
    "18025 Woodland Gelen Dr",
    "12104 2415 Ave Ct E",
    "4317-A South Puget Ave",
    "10520 140th Street Court East",
    "County Estates 150th St. SW",
    "2511 Malik, Ja. Way",
    "11527 20th Ave. Ct. E",
    "3815 10014 St SW",
    "#5 Lakeside Club SW",
    "4027 S Puget Dr Ave",
    "5709 151st Ave NW",
    "4508 175th Street Court East",
    "10301 Kendrick Drive Southwest",
    "114 129 H St S",
    "1501 SM St",
    "8016 185th St Court E",
    "33521 Mountain Highway E.",
    "20515 85th Avenue Court East",
    "8626 5 Asatin St",
    "1912 105th Street Court South",
    "922 73rd Street Court East",
    "10423 Rainier Ridge Boulevard East",
    "18025 Woodland Glen Dr",
    "11112 10th Avenue Court East",
    "15612 1164 Street E",
    "194913 166th St E",
    "11063 188th St. Ct. E.",
    "12608 81st Avenue Court E",
    "1512 5 Spraque"
  ),
  address_street_new = c(
    "1802 S Woodland Glen Dr",
    "12104 241st Ave Ct E",
    "4317 South Puget Ave",
    "10520 140th St Ct E",
    "6900 150th St Ct E",
    "2511 Martin Luther King Jr Way",
    "11527 240th Ave. Ct. E",
    "3815 100th St SW",
    "5 Lakeside Country Club SW",
    "4027 S Puget Sound Ave",
    "5709 151st Ave Ct NW",
    "4508 175th St Ct E",
    "10301 Kendrick St SW",
    "114 129th St S",
    "1501 S M St",
    "8016 185th St Ct E",
    "33521 Mountain Highway E",
    "20515 85th Avenue Ct E",
    "8626 S Asotin St",
    "1912 105th St Ct S",
    "922 73rd St Ct E",
    "10423 Rainier Ridge Blvd E",
    "1802 S Woodland Glen Dr",
    "11112 10th Avenue Ct East",
    "15612 116th Street E",
    "19413 166th St E",
    "11063 188th St Ct E",
    "12608 81st Avenue Ct E",
    "1512 S Sprague"
  )
)

llm_data_summons_clean <- llm_data_summons_clean %>%
  left_join(manual_fixes_street, by = c("address_street" = "address_street_old")) %>%
  mutate(address_street = coalesce(address_street_new, address_street)) %>%
  select(-address_street_new)

#clean address_city manually (common OCR errors)
llm_data_summons_clean <- llm_data_summons_clean %>%
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

### google maps geocoding — cases with no complaint address ###

#define known out-of-county cases to exclude
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

#load complaint clean data to identify cases with no geocoded address
llm_data_complaint_clean <- read_rds("data/llm_data_complaint_clean.rds")

complaint_no_address <- llm_data_complaint_clean %>%
  filter(is.na(address_street) | address_street == "") %>%
  pull(case_number) %>%
  unique()

#target: cases with no complaint address, valid summons address, not out-of-county
to_geocode_summons <- llm_data_summons_clean %>%
  filter(
    case_number %in% complaint_no_address,
    !is.na(address_street), address_street != "",
    !case_number %in% out_of_county_cases
  ) %>%
  group_by(case_number) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    address_zip_clean = str_extract(address_zip, "^\\d{5}"),
    geocode_input = str_squish(paste0(
      address_street, ", ",
      address_city, ", ",
      "Pierce County, WA",
      if_else(!is.na(address_zip_clean) & address_zip_clean != "",
              paste0(" ", address_zip_clean), "")
    ))
  )

nrow(to_geocode_summons)  # check how many cases to geocode

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
geocode_results_summons <- to_geocode_summons %>%
  mutate(row = row_number()) %>%
  group_by(row) %>%
  group_modify(~{
    Sys.sleep(0.05)
    bind_cols(.x, geocode_google(.x$geocode_input, api_key))
  }) %>%
  ungroup()

#save raw results
saveRDS(geocode_results_summons, "data/geocode_google_summons_fallback.rds")

#qc: review match status and type distribution
print(count(geocode_results_summons, geocode_status))
print(count(geocode_results_summons, geocode_type))

#keep only street-level matches
geocode_usable_summons <- geocode_results_summons %>%
  filter(
    geocode_status == "OK",
    geocode_type %in% c("street_address", "premise", "subpremise")
  )

#spatial join to Pierce County block groups (year = 2020 to match complaint pipeline)
pierce_bg <- block_groups(state = "WA", county = "Pierce", year = 2020) %>%
  st_transform(4326)

geocode_sf_summons <- geocode_usable_summons %>%
  st_as_sf(coords = c("lon_new", "lat_new"), crs = 4326)

geocode_with_cbg_summons <- st_join(geocode_sf_summons, pierce_bg["GEOID"], join = st_within) %>%
  st_drop_geometry() %>%
  select(row_id, census_block_group_summons = GEOID)

#carry lat/lon separately — lost when st_as_sf converts coords to geometry
geocode_coords_summons <- geocode_usable_summons %>%
  select(row_id, lat_new, lon_new)

geocode_with_cbg_summons <- geocode_with_cbg_summons %>%
  left_join(geocode_coords_summons, by = "row_id")

#attach geocoding results to summons data
llm_data_summons_clean <- llm_data_summons_clean %>%
  left_join(geocode_with_cbg_summons, by = "row_id") %>%
  mutate(
    lon = coalesce(lon_new, NA_real_),
    lat = coalesce(lat_new, NA_real_),
    census_block_group = census_block_group_summons
  ) %>%
  select(-lon_new, -lat_new, -census_block_group_summons)

#coverage summary
geocoding_summary_summons <- tibble(
  n_targeted   = nrow(to_geocode_summons),
  n_geocoded   = sum(!is.na(llm_data_summons_clean$lon)),
  n_with_cbg   = sum(!is.na(llm_data_summons_clean$census_block_group)),
  pct_geocoded = round(n_geocoded / n_targeted * 100, 2)
)

print(geocoding_summary_summons)

#save data
saveRDS(llm_data_summons_clean, "data/llm_data_summons_clean.rds")