#load packages
library(tidyverse)

#read in data
llm_data_minute_entry <- readRDS("data/llm_data_minute_entry.rds")

###look for possible OCR errors, fix if paths and case numbers are misaligned
minute_entry_mismatch <- llm_data_minute_entry %>%
  mutate(
    direc_case = basename(direc),
    match_direc_case = direc_case == case_number
  ) %>%
  filter(is.na(match_direc_case) | !match_direc_case) %>%
  select(row_id, case_number, direc, direc_case)

source("scripts/fix_by_path.R")
llm_data_minute_entry_clean <- fix_by_path(llm_data_minute_entry)

### clean name data ###
#surname extraction for BISG (defendants only)
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

#clean names for matching
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
llm_data_minute_entry_clean <- llm_data_minute_entry_clean %>%
  mutate(
    # BISG surname extraction (defendants)
    defendant_surnames = map(defendant_names, extract_surnames),
    n_valid_defendants = map_int(defendant_surnames, function(snames) {
      if (all(is.na(snames))) return(0L)
      sum(!is.na(snames) & snames != "")
    }),
    
    #cleaned names for matching across datasets (all parties for conflict resolution)
    plaintiff_names_clean = map(plaintiff_names, clean_name_list),
    defendant_names_clean = map(defendant_names, clean_name_list),
    plaintiff_attorneys_clean = map(plaintiff_attorneys, clean_name_list),
    defendant_attorneys_clean = map(defendant_attorneys, clean_name_list),
    
    #also clean the "at hearing" name lists for more granular analysis
    plaintiffs_at_hearing_clean = map(plaintiffs_at_hearing, clean_name_list),
    plaintiff_attorneys_at_hearing_clean = map(plaintiff_attorneys_at_hearing, clean_name_list),
    defendants_at_hearing_clean = map(defendants_at_hearing, clean_name_list),
    defendant_attorneys_at_hearing_clean = map(defendant_attorneys_at_hearing, clean_name_list)
  )

#surname extraction results
llm_data_minute_entry_clean %>%
  count(n_valid_defendants) %>%
  print()

#cleaned plaintiff name examples
llm_data_minute_entry_clean %>%
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
  # handle NULL, NA, empty list, or character(0)
  if (is.null(name_list) || length(name_list) == 0) {
    return(list())  # Return empty list for consistency
  }
  
  # if it's a single NA value
  if (length(name_list) == 1 && is.na(name_list)) {
    return(list())
  }
  
  # flatten if nested list
  name_list <- unlist(name_list)
  
  # after unlisting, check again if empty
  if (length(name_list) == 0) {
    return(list())
  }
  
  # clean each name
  cleaned <- map_chr(name_list, function(name) {
    if (is.na(name) || name == "" || name == "character(0)") {
      return(NA_character_)
    }
    
    # remove c() notation artifacts
    name <- str_remove_all(name, 'c\\(|\\)|"')
    
    # trim whitespace
    name <- str_trim(name)
    name_clean <- str_to_upper(name)
    
    # standardize suffixes (Jr, Sr, II, III, etc.)
    name_clean <- str_replace_all(name_clean, "\\s+(JR\\.?|SR\\.?|I{2,3}|IV|V)$", "")
    
    # remove periods after initials/middle names
    name_clean <- str_replace_all(name_clean, "\\b([A-Z])\\.\\s*", "\\1 ")
    
    # standardize spacing
    name_clean <- str_replace_all(name_clean, "\\s+", " ")
    name_clean <- str_squish(name_clean)
    
    if (name_clean == "") return(NA_character_)
    
    return(name_clean)
  })
  
  # remove NAs and empty strings
  cleaned <- cleaned[!is.na(cleaned) & cleaned != ""]
  
  # remove exact duplicates
  cleaned <- unique(cleaned)
  
  # return empty list if nothing valid remains
  if (length(cleaned) == 0) return(list())
  
  return(cleaned)
}

#apply function
llm_data_minute_entry_clean <- llm_data_minute_entry_clean %>%
  mutate(
    defendant_attorneys_clean = map(defendant_attorneys, clean_attorney_names),
    plaintiff_attorneys_clean = map(plaintiff_attorneys, clean_attorney_names),
    defendant_attorneys_at_hearing_clean = map(defendant_attorneys_at_hearing, clean_attorney_names),
    plaintiff_attorneys_at_hearing_clean = map(plaintiff_attorneys_at_hearing, clean_attorney_names)
  )

#convert data formats
llm_data_minute_entry_clean <- llm_data_minute_entry_clean %>%
  mutate(
    hearing_date = ymd(hearing_date)
    )

#create first_hearing_date variable
first_hearing_by_case <- llm_data_minute_entry_clean %>%
  group_by(case_number) %>%
  summarise(
    first_hearing_date = if (all(is.na(hearing_date))) as.Date(NA) else min(hearing_date, na.rm = TRUE),
    .groups = "drop"
  )

#join back to the row-level dataframe
llm_data_minute_entry_clean <- llm_data_minute_entry_clean %>%
  left_join(first_hearing_by_case, by = "case_number")

saveRDS(llm_data_minute_entry_clean, "data/llm_data_minute_entry_clean.rds")
