#load packages
library(tidyverse)
library(sf)
library(httr)
library(readr)
library(tigris)
library(lubridate)

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

### clean name data ###
#surname extraction (defendants only)
extract_surnames <- function(names) {
  if (is.null(names) || length(names) == 0 || all(is.na(names))) {
    return(NA_character_)
  }
  
  surnames <- map_chr(names, function(full_name) {
    full_name <- str_trim(full_name)
    
    #flag non-person entries
    if (str_detect(full_name, regex("occupant|doe|unknown|premises|possession|property", ignore_case = TRUE))) {
      return(NA_character_)
    }
    
    #remove suffixes
    full_name <- str_remove(full_name, "\\s*,?\\s*(Jr\\.?|Sr\\.?|III?|IV|V)$")
    
    #split into words
    parts <- str_split(full_name, "\\s+")[[1]]
    
    if (length(parts) <= 1) return(NA_character_)
    
    #extract surname based on word count
    if (length(parts) == 2) {
      surname <- parts[2]
    } else if (length(parts) == 3) {
      if (str_to_lower(parts[2]) %in% c("de", "del", "van", "von", "da", "di", "le", "la")) {
        surname <- paste(parts[2:3], collapse = " ")
      } else {
        surname <- parts[3]
      }
    } else if (length(parts) >= 4) {
      if (str_to_lower(parts[length(parts)-1]) %in% c("de", "del", "van", "von", "da", "di", "le", "la")) {
        surname <- paste(parts[(length(parts)-1):length(parts)], collapse = " ")
      } else {
        surname <- parts[length(parts)]
      }
    }
    
    #clean for Census matching (uppercase, letters only)
    surname <- str_to_upper(surname) %>%
      str_remove_all("[^A-Z\\s]") %>%
      str_squish()
    
    if (surname == "" || nchar(surname) <= 1) return(NA_character_)
    return(surname)
  })
  
  #return only valid surnames
  valid_surnames <- surnames[!is.na(surnames) & surnames != ""]
  if (length(valid_surnames) == 0) return(NA_character_)
  return(valid_surnames)
}

#name cleaning for matching across datasets
clean_name_for_matching <- function(name) {
  if (is.na(name) || name == "") return(NA_character_)
  
  name <- str_trim(name)
  name_clean <- str_to_upper(name)
  
  #standardize suffixes (Jr, Sr, II, III, etc.)
  name_clean <- str_replace_all(name_clean, "\\s+(JR\\.?|SR\\.?|I{2,3}|IV|V)$", "")
  
  #standardize initials
  name_clean <- str_replace_all(name_clean, "\\b([A-Z])\\.\\s*", "\\1 ")
  name_clean <- str_replace_all(name_clean, "\\s+", " ")
  
  #remove procedural language
  if (str_detect(name_clean, regex("ALL OTHER|UNKNOWN|ANY AND ALL|OCCUPANTS|PREMISES|POSSESSION", ignore_case = FALSE))) {
    return(NA_character_)
  }
  
  name_clean <- str_squish(name_clean)
  if (name_clean == "") return(NA_character_)
  
  return(name_clean)
}

#apply to name lists
clean_name_list <- function(name_list) {
  if (is.null(name_list) || all(is.na(name_list))) {
    return(NA)
  }
  
  cleaned <- map_chr(name_list, clean_name_for_matching)
  cleaned <- cleaned[!is.na(cleaned)]
  cleaned <- unique(cleaned)
  
  if (length(cleaned) == 0) return(NA)
  return(cleaned)
}

#apply surname extraction and name standardization

llm_data_summons_clean <- llm_data_summons_clean %>%
  mutate(
    # BISG surname extraction (defendants only)
    defendant_surnames = map(defendant_names, extract_surnames),
    n_valid_defendants = map_int(defendant_surnames, function(snames) {
      if (all(is.na(snames))) return(0L)
      sum(!is.na(snames) & snames != "")
    }),
    
    #name standardization (all parties)
    plaintiff_names_clean = map(plaintiff_names, clean_name_list),
    defendant_names_clean = map(defendant_names, clean_name_list),
    plaintiff_attorneys_clean = map(plaintiff_attorneys, clean_name_list)
  )

#check results
llm_data_summons_clean %>%
  count(case_type, n_valid_defendants) %>%
  arrange(case_type, n_valid_defendants) %>%
  print(n = 20)

llm_data_summons_clean %>%
  filter(case_type == "residential", n_valid_defendants > 0) %>%
  mutate(
    defendant_names_str = sapply(defendant_names, paste, collapse = ", "),
    defendant_surnames_str = sapply(defendant_surnames, paste, collapse = ", ")
  ) %>%
  select(defendant_names_str, defendant_surnames_str, n_valid_defendants) %>%
  head(15) %>%
  print()

all_surnames <- llm_data_summons_clean %>%
  filter(case_type == "residential") %>%
  select(defendant_surnames) %>%
  unnest(defendant_surnames) %>%
  filter(!is.na(defendant_surnames))

all_surnames %>%
  count(defendant_surnames, sort = TRUE) %>%
  head(30) %>%
  print()

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
    "12608 81st Avenue Court E"
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
    "12608 81st Avenue Ct E"
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

### add census block groups ###

#reconstruct full addresses
llm_data_summons_clean <- llm_data_summons_clean %>%
  mutate(
    full_address = paste(address_street, address_city, address_state, address_zip, sep = ", "),
    has_complete_address = !if_any(
      c(address_street, address_city, address_state, address_zip),
      ~ is.na(.) | . == ""
    )
  )

#geocode using Census batch geocoder
addresses_to_geocode <- llm_data_summons_clean %>%
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

llm_data_summons_clean <- llm_data_summons_clean %>%
  left_join(address_coords, by = "full_address")

#get census block groups
summons_sf <- llm_data_summons_clean %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

cbg_wa <- block_groups(state = "WA", year = 2020) %>%
  st_transform(st_crs(summons_sf))

result <- st_join(summons_sf, cbg_wa, join = st_within)

cbg_lookup <- result %>%
  st_drop_geometry() %>%
  select(row_id, GEOID) %>%
  rename(census_block_group = GEOID)

llm_data_summons_clean <- llm_data_summons_clean %>%
  left_join(cbg_lookup, by = "row_id")

#identify conflicts in census block groups within cases
cases_multi_cbg <- llm_data_summons_clean %>%
  filter(!is.na(census_block_group)) %>%
  group_by(case_number) %>%
  summarize(
    n_unique_cbg = n_distinct(census_block_group),
    n_records = n(),
    .groups = "drop"
  ) %>%
  filter(n_unique_cbg > 1)

#identify where geocoding failed
geocoding_summary <- tibble(
  n_total = nrow(llm_data_summons_clean),
  n_complete_address = sum(llm_data_summons_clean$has_complete_address),
  n_geocoded = sum(!is.na(llm_data_summons_clean$lon)),
  n_with_cbg = sum(!is.na(llm_data_summons_clean$census_block_group)),
  pct_geocoded = round(n_geocoded / n_total * 100, 2),
  pct_with_cbg = round(n_with_cbg / n_total * 100, 2),
  n_cases_multi_cbg = nrow(cases_multi_cbg)
)

print(geocoding_summary)

llm_data_summons_clean %>%
  filter(!is.na(plaintiff_names)) %>%
  select(plaintiff_names, plaintiff_names_clean) %>%
  mutate(
    original = map_chr(plaintiff_names, ~paste(.x, collapse = " | ")),
    cleaned = map_chr(plaintiff_names_clean, ~paste(.x, collapse = " | "))
  ) %>%
  select(original, cleaned) %>%
  head(10) %>%
  print()

#save data
saveRDS(llm_data_summons_clean, "data/llm_data_summons_clean.rds")