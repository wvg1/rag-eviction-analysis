#load packages
library(tidyverse)
library(lubridate)

#load data
llm_data_agreement <- readRDS("data/llm_data_agreement.rds")

###look for possible OCR errors in case number, fix if paths and case numbers are misaligned
agreement_mismatch <- llm_data_agreement %>%
  mutate(
    direc_case = basename(direc),
    match_direc_case = direc_case == case_number
  ) %>%
  filter(is.na(match_direc_case) | !match_direc_case) %>%
  select(row_id, case_number, direc, direc_case)

source("scripts/fix_by_path.R")
llm_data_agreement_clean <- fix_by_path(llm_data_agreement)

###clean name data ###

#extract surnames from defendant names for BISG
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
    
    #handle edge cases with zero or one words
    if (length(parts) <= 1) return(NA_character_)
    
    #extract surname based on word count
    if (length(parts) == 2) {
      # For (first last)
      surname <- parts[2]
    } else if (length(parts) == 3) {
      #for first middle last or compound surname
      #check if middle word is lowercase particle
      if (str_to_lower(parts[2]) %in% c("de", "del", "van", "von", "da", "di", "le", "la")) {
        surname <- paste(parts[2:3], collapse = " ")
      } else {
        #assume first middle last if there's no particle
        surname <- parts[3]
      }
    } else if (length(parts) >= 4) {
      #check last 2 words for particles
      if (str_to_lower(parts[length(parts)-1]) %in% c("de", "del", "van", "von", "da", "di", "le", "la")) {
        surname <- paste(parts[(length(parts)-1):length(parts)], collapse = " ")
      } else {
        #otherwise, take last word only
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

#standardize formatting for document matching
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

#apply both surname extraction AND name cleaning
llm_data_agreement <- llm_data_agreement %>%
  mutate(
    # BISG surname extraction (defendants)
    defendant_surnames = map(defendant_names, extract_surnames),
    n_valid_defendants = map_int(defendant_surnames, function(snames) {
      if (all(is.na(snames))) return(0L)
      sum(!is.na(snames) & snames != "")
    }),
    
    #cleaned names for matching across datasets
    plaintiff_names_clean = map(plaintiff_names, clean_name_list),
    defendant_names_clean = map(defendant_names, clean_name_list),
    plaintiff_attorneys_clean = map(plaintiff_attorneys, clean_name_list),
    defendant_attorneys_clean = map(defendant_attorneys, clean_name_list)
  )
#results
all_surnames <- llm_data_agreement %>%
  select(defendant_surnames) %>%
  unnest(defendant_surnames) %>%
  filter(!is.na(defendant_surnames))

all_surnames %>%
  count(defendant_surnames, sort = TRUE) %>%
  head(30) %>%
  print()

#cleaned plaintiff name examples
llm_data_agreement %>%
  filter(!is.na(plaintiff_names)) %>%
  select(plaintiff_names, plaintiff_names_clean) %>%
  mutate(
    original = map_chr(plaintiff_names, ~paste(.x, collapse = " | ")),
    cleaned = map_chr(plaintiff_names_clean, ~paste(.x, collapse = " | "))
  ) %>%
  select(original, cleaned) %>%
  head(10) %>%
  print()

###clean defendant attorney names ###
#cleaning function
clean_attorney_names <- function(name_list) {
  # Handle NULL, NA, empty list, or character(0)
  if (is.null(name_list) || length(name_list) == 0) {
    return(list())  # Return empty list for consistency
  }
  
  # If it's a single NA value
  if (length(name_list) == 1 && is.na(name_list)) {
    return(list())
  }
  
  # Flatten if nested list
  name_list <- unlist(name_list)
  
  # After unlisting, check again if empty
  if (length(name_list) == 0) {
    return(list())
  }
  
  # Clean each name
  cleaned <- map_chr(name_list, function(name) {
    if (is.na(name) || name == "" || name == "character(0)") {
      return(NA_character_)
    }
    
    # Remove c() notation artifacts
    name <- str_remove_all(name, 'c\\(|\\)|"')
    
    # Trim whitespace
    name <- str_trim(name)
    name_clean <- str_to_upper(name)
    
    # Standardize suffixes (Jr, Sr, II, III, etc.)
    name_clean <- str_replace_all(name_clean, "\\s+(JR\\.?|SR\\.?|I{2,3}|IV|V)$", "")
    
    # Remove periods after initials/middle names
    name_clean <- str_replace_all(name_clean, "\\b([A-Z])\\.\\s*", "\\1 ")
    
    # Standardize spacing
    name_clean <- str_replace_all(name_clean, "\\s+", " ")
    name_clean <- str_squish(name_clean)
    
    if (name_clean == "") return(NA_character_)
    
    return(name_clean)
  })
  
  # Remove NAs and empty strings
  cleaned <- cleaned[!is.na(cleaned) & cleaned != ""]
  
  # Remove exact duplicates
  cleaned <- unique(cleaned)
  
  # Return empty list if nothing valid remains
  if (length(cleaned) == 0) return(list())
  
  return(cleaned)
}

#apply function
llm_data_agreement_clean <- llm_data_agreement_clean %>%
  mutate(
    defendant_attorneys_clean = map(defendant_attorneys, clean_attorney_names),
    plaintiff_attorneys_clean = map(plaintiff_attorneys, clean_attorney_names)
  )

### other variable mutations ###
#convert data formats
llm_data_agreement_clean <- llm_data_agreement_clean %>%
  mutate(
    document_file_date = ymd(document_file_date)
  )

#create agreement_to_move_date
llm_data_agreement_clean <- llm_data_agreement_clean %>%
  group_by(case_number) %>%
  mutate(
    agreement_to_move_date = {
      rows <- agreement == "Yes" & tenant_move == "Yes"
      if (any(rows, na.rm = TRUE)) {
        max(document_file_date[rows], na.rm = TRUE)
      } else {
        as.Date(NA)
      }
    },
    #capture both confidences for the composite measure
    agreement_to_move_conf_agreement = {
      rows <- agreement == "Yes" & tenant_move == "Yes"
      if (any(rows, na.rm = TRUE)) {
        max(conf_agreement[rows], na.rm = TRUE)
      } else {
        NA_real_
      }
    },
    agreement_to_move_conf_tenant_move = {
      rows <- agreement == "Yes" & tenant_move == "Yes"
      if (any(rows, na.rm = TRUE)) {
        max(conf_tenant_move[rows], na.rm = TRUE)
      } else {
        NA_real_
      }
    }
  ) %>%
  ungroup()

#double check for conflicting agreement_to_move_date values, shouldn't print anything
duplicate_agreement_to_move <- llm_data_agreement_clean %>%
  filter(!is.na(agreement_to_move_date)) %>%
  group_by(case_number) %>%
  summarise(
    n_distinct_dates = n_distinct(agreement_to_move_date),
    .groups = "drop"
  ) %>%
  filter(n_distinct_dates > 1)

if (nrow(duplicate_agreement_to_move) > 0) {
  cat("cases with conflicting agreement_to_move dates:\n")
  print(duplicate_agreement_to_move)
}

saveRDS(llm_data_agreement_clean, "data/llm_data_agreement_clean.rds")