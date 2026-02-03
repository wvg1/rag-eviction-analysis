#load packages
library(tidyverse)
library(stringdist)

#read in data
llm_data_agreement_clean <- readRDS("data/llm_data_agreement_clean.rds")
llm_data_appearance_clean <- readRDS("data/llm_data_appearance_clean.rds")
llm_data_complaint_clean <- readRDS("data/llm_data_complaint_clean.rds")
llm_data_dismissal_clean <- readRDS("data/llm_data_dismissal_clean.rds")
llm_data_minute_entry_clean <- readRDS("data/llm_data_minute_entry_clean.rds")
llm_data_stay_vacate_clean <- readRDS("data/llm_data_stay_vacate_clean.rds")
llm_data_summons_clean <- readRDS("data/llm_data_summons_clean.rds")
llm_data_writ_clean <- readRDS("data/llm_data_writ_clean.rds")

#bind dataframes, add source identifier
llm_data_combined <- bind_rows(
  agreement    = llm_data_agreement_clean,
  appearance   = llm_data_appearance_clean,
  complaint    = llm_data_complaint_clean,
  dismissal    = llm_data_dismissal_clean,
  minute_entry = llm_data_minute_entry_clean,
  stay_vacate  = llm_data_stay_vacate_clean,
  summons      = llm_data_summons_clean,
  writ         = llm_data_writ_clean,
  .id = "source"
)

### resolve case_type ###
#show conflicts
case_type_conflicts <- llm_data_combined %>%
  group_by(case_number) %>%
  summarize(case_types = n_distinct(case_type, na.rm = TRUE), .groups = "drop") %>%
  filter(case_types > 1)

llm_data_combined %>%
  filter(!is.na(case_type)) %>%
  semi_join(case_type_conflicts, by = "case_number") %>%
  distinct(case_number, case_type, source) %>%
  arrange(case_number, case_type, source) %>%
  print(n = Inf)

#after manual check, adopt complaint result for case_type
llm_data_combined <- llm_data_combined %>%
  group_by(case_number) %>%
  mutate(case_type = case_when(
    #if there's a complaint value for this case, use it (and use first one if multiple complaints exist)
    any(source == "complaint" & !is.na(case_type)) ~ 
      case_type[source == "complaint" & !is.na(case_type)][1],
    TRUE ~ case_type
  )) %>%
  ungroup()

### resolve address data (keep most common valid Pierce County zip) ###
llm_data_combined <- llm_data_combined %>%
  group_by(case_number) %>%
  mutate(
    valid_zips = list(address_zip[!is.na(address_zip) & address_zip != ""]),
    address_zip = if_else(
      length(valid_zips[[1]]) > 0,
      names(sort(table(valid_zips[[1]]), decreasing = TRUE))[1] %||% NA_character_,
      NA_character_
    )
  ) %>%
  select(-valid_zips) %>%
  ungroup()

### check for conflicting name data and resolve conflicts ###
source_priority <- c("complaint" = 1, "summons" = 2, "appearance" = 3, 
                     "minute_entry" = 4, "agreement" = 5, "dismissal" = 6, "writ" = 7, "stay_vacate" = 8)

resolve_field <- function(field_data, source_data, source_priority_data, fuzzy_threshold = 0.85) {
  non_blank_idx <- !sapply(field_data, function(x) all(is.na(x)) || (is.character(x) && all(x == "")))
  
  if (!any(non_blank_idx)) {
    return(list(NA))
  }
  
  variants <- field_data[non_blank_idx]
  variant_strings <- sapply(variants, paste, collapse = "|")
  unique_variants <- unique(variant_strings)
  
  if (length(unique_variants) == 1) {
    return(list(variants[[1]]))
  }
  
  variant_groups <- list()
  used <- rep(FALSE, length(unique_variants))
  
  for (i in seq_along(unique_variants)) {
    if (used[i]) next
    
    group <- unique_variants[i]
    used[i] <- TRUE
    
    for (j in seq_along(unique_variants)) {
      if (i != j && !used[j]) {
        similarity <- stringsim(unique_variants[i], unique_variants[j], method = "jw")
        if (similarity >= fuzzy_threshold) {
          group <- c(group, unique_variants[j])
          used[j] <- TRUE
        }
      }
    }
    
    variant_groups[[length(variant_groups) + 1]] <- group
  }
  
  best_variant <- NA
  best_count <- 0
  best_priority <- Inf
  
  for (group in variant_groups) {
    count <- sum(variant_strings %in% group)
    
    if (count > best_count) {
      group_counts <- table(variant_strings[variant_strings %in% group])
      best_variant <- names(sort(group_counts, decreasing = TRUE))[1]
      best_count <- count
      best_priority <- min(source_priority_data[variant_strings == best_variant], na.rm = TRUE)
    } else if (count == best_count && count > 0) {
      group_indices <- which(variant_strings %in% group)
      group_priorities <- source_priority_data[group_indices]
      min_priority_idx <- which.min(group_priorities)
      this_priority <- group_priorities[min_priority_idx]
      
      if (this_priority < best_priority) {
        best_variant <- variant_strings[group_indices[min_priority_idx]]
        best_priority <- this_priority
      }
    }
  }
  
  if (!is.na(best_variant)) {
    match_idx <- which(variant_strings == best_variant)[1]
    return(list(variants[[match_idx]]))
  } else {
    priority_idx <- which(non_blank_idx)
    best_idx <- priority_idx[which.min(source_priority_data[priority_idx])]
    return(list(field_data[[best_idx]]))
  }
}

llm_data_combined <- llm_data_combined %>%
  mutate(source_priority = source_priority[source]) %>%
  group_by(case_number) %>%
  mutate(
    case_plaintiff_names = resolve_field(plaintiff_names, source, source_priority),
    case_defendant_names = resolve_field(defendant_names, source, source_priority),
    case_plaintiff_attorneys = resolve_field(plaintiff_attorneys, source, source_priority),
    # case_defendant_attorneys handled separately below
    case_defendant_surnames = resolve_field(defendant_surnames, source, source_priority)
  ) %>%
  mutate(
    case_plaintiff_names = case_plaintiff_names[1],
    case_defendant_names = case_defendant_names[1],
    case_plaintiff_attorneys = case_plaintiff_attorneys[1],
    # case_defendant_attorneys handled separately below
    case_defendant_surnames = case_defendant_surnames[1]
  ) %>%
  select(-source_priority) %>%
  ungroup()

#clean address_city manually
llm_data_combined <- llm_data_combined %>%
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

#create case-level census_block_group variable
llm_data_combined <- llm_data_combined %>%
  group_by(case_number) %>%
  mutate(
    n_unique_cbg = n_distinct(census_block_group[!is.na(census_block_group)]),
    census_block_group = if_else(
      n_unique_cbg == 1,
      census_block_group[!is.na(census_block_group)][1],
      NA_character_
    )
  ) %>%
  select(-n_unique_cbg) %>%
  ungroup()

### create case-level list of defendant first names ###

#set up regexes
suffix_regex <- "\\b(SR|JR|II|III|IV|V)\\.?$"

business_regex <- regex(
  "\\b(
    INC|L\\.?L\\.?C\\.?|CO\\.?|CORP\\.?|PLLC|AGENCY|WIRELESS|ESCROW|
    ESTATE|TRUST|INTERIORS|
    DOE|OCCUPANT|OCCUPANTS|PERSONS|POSSESSION|PREMISES|PROPERTY
  )\\b",
  ignore_case = TRUE
)

#create variable
llm_data_combined <- llm_data_combined %>%
  group_by(case_number) %>%
  mutate(
    case_defendant_first_names = list(
      if (!is.null(case_defendant_names[[1]]) &&
          length(case_defendant_names[[1]]) > 0) {
        
        names_list <- case_defendant_names[[1]]
        
        first_names <- sapply(names_list, function(name) {
          if (is.na(name) || name == "") return(NA_character_)
          
          name <- str_trim(str_remove(name, ",$"))
          
          # drop obvious business / placeholder defendants
          if (str_detect(name, business_regex)) return(NA_character_)
          
          # lastname, firstname (possibly with suffix)
          if (str_detect(name, ",")) {
            parts <- str_split(name, ",")[[1]]
            parts <- str_trim(parts)
            
            if (length(parts) >= 2) {
              candidate <- parts[2]
              
              # remove suffixes like ", Sr."
              candidate <- str_remove(candidate, suffix_regex)
              candidate <- str_trim(candidate)
              
              first <- str_split(candidate, "\\s+")[[1]][1]
              
              if (!str_detect(first, business_regex)) {
                return(str_to_title(first))
              }
            }
            return(NA_character_)
          }
          
          # first middle last (possibly with suffix)
          name_parts <- str_split(name, "\\s+")[[1]]
          
          # remove trailing suffix token
          if (length(name_parts) > 1 &&
              str_detect(name_parts[length(name_parts)], suffix_regex)) {
            name_parts <- name_parts[-length(name_parts)]
          }
          
          first_part <- name_parts[1]
          
          # handle single-letter initials
          if (nchar(first_part) == 1 && length(name_parts) > 1) {
            first_part <- name_parts[2]
          }
          
          if (!str_detect(first_part, business_regex)) {
            return(str_to_title(first_part))
          }
          
          NA_character_
        }, USE.NAMES = FALSE)
        
        first_names[!is.na(first_names)]
        
      } else {
        character(0)
      }
    )
  ) %>%
  ungroup()

### create case-level list of defendant attorneys ###

# consolidate attorney name variations
consolidate_cleaned_attorneys <- function(attorney_names, threshold = 0.85) {
  if (length(attorney_names) <= 1) return(attorney_names)
  
  attorney_names <- unique(attorney_names)
  n <- length(attorney_names)
  used <- rep(FALSE, n)
  consolidated <- character()
  
  for (i in seq_along(attorney_names)) {
    if (used[i]) next
    current_name <- attorney_names[i]
    used[i] <- TRUE
    
    for (j in (i+1):n) {
      if (j > n || used[j]) next
      
      # match if first+last names match OR high similarity
      parts_i <- str_split(attorney_names[i], "\\s+")[[1]]
      parts_j <- str_split(attorney_names[j], "\\s+")[[1]]
      
      first_last_match <- length(parts_i) >= 2 && length(parts_j) >= 2 &&
        parts_i[1] == parts_j[1] && 
        parts_i[length(parts_i)] == parts_j[length(parts_j)]
      
      if (first_last_match || stringsim(attorney_names[i], attorney_names[j], method = "jw") >= threshold) {
        used[j] <- TRUE
        if (nchar(attorney_names[j]) > nchar(current_name)) current_name <- attorney_names[j]
      }
    }
    consolidated <- c(consolidated, current_name)
  }
  consolidated
}

# apply to merged dataset
llm_data_combined <- llm_data_combined %>%
  group_by(case_number) %>%
  mutate(
    case_defendant_attorneys = list({
      all_attys <- c(unlist(defendant_attorneys_clean))
      if (!is.null(defendant_attorneys_at_hearing_clean)) {
        all_attys <- c(all_attys, unlist(defendant_attorneys_at_hearing_clean))
      }
      all_attys <- all_attys[!is.na(all_attys) & all_attys != ""]
      if (length(all_attys) == 0) character(0) else consolidate_cleaned_attorneys(all_attys)
    }),
    case_defendant_attorneys = case_defendant_attorneys[1]
  ) %>%
  ungroup()

#create case-level plaintiff_representation variable
llm_data_combined <- llm_data_combined %>%
  group_by(case_number) %>%
  mutate(
    plaintiff_rep = as.integer(
      any((lengths(plaintiff_attorneys) > 0 & nzchar(sapply(plaintiff_attorneys, paste, collapse=""))) |
            (lengths(plaintiff_attorneys_at_hearing) > 0 & nzchar(sapply(plaintiff_attorneys_at_hearing, paste, collapse=""))))
    )
  ) %>%
  ungroup()

### date variables ###
# convert character date columns to dates type
llm_data_combined <- llm_data_combined %>%
  mutate(
    dismissal_file_date = as.Date(dismissal_file_date),
    stay_vacate_file_date = as.Date(stay_vacate_file_date),
    summons_file_date = as.Date(summons_file_date),
    response_deadline = as.Date(response_deadline),
    writ_file_date = as.Date(writ_file_date)
  )

#create case-level filed_after_deadline variable
llm_data_combined <- llm_data_combined %>%
  group_by(case_number) %>%
  mutate(
    filed_after_deadline = if_else(
      any(!is.na(response_deadline) & !is.na(summons_file_date)),
      as.integer(
        as.Date(summons_file_date[!is.na(response_deadline) & !is.na(summons_file_date)][1]) >
          as.Date(response_deadline[!is.na(response_deadline) & !is.na(summons_file_date)][1])
      ),
      NA_integer_
    )
  ) %>%
  ungroup()

#create case-level appearance variable
llm_data_combined <- llm_data_combined %>%
  group_by(case_number) %>%
  mutate(
    min_hearing = min(hearing_date, na.rm = TRUE),
    min_hearing = if_else(is.infinite(min_hearing), NA_Date_, min_hearing),
    defendant_appearance = appearance_for == "defendant" & 
      !is.na(document_file_date) &
      (is.na(min_hearing) | document_file_date < min_hearing)  # Changed this line
  ) %>%
  mutate(
    defendant_appearance = any(defendant_appearance, na.rm = TRUE)
  ) %>%
  select(-min_hearing) %>%
  ungroup()

#create case-level appearance_pro_se variable
llm_data_combined <- llm_data_combined %>%
  group_by(case_number) %>%
  mutate(
    min_hearing = min(hearing_date, na.rm = TRUE),
    min_hearing = if_else(is.infinite(min_hearing), NA_Date_, min_hearing),
    appearance_pro_se = appearance_for == "defendant" & 
      appearance_pro_se == "Yes" &
      !is.na(document_file_date) &
      (is.na(min_hearing) | document_file_date < min_hearing)
  ) %>%
  mutate(
    appearance_pro_se = any(appearance_pro_se, na.rm = TRUE)
  ) %>%
  select(-min_hearing) %>%
  ungroup()

#create case-level hearing_held variable
llm_data_combined <- llm_data_combined %>%
  group_by(case_number) %>%
  mutate(hearing_held = as.integer(any(!is.na(hearing_date)))) %>%
  ungroup()

#create defendant_hearing_attendance
llm_data_combined <- llm_data_combined %>%
  group_by(case_number) %>%
  mutate(
    defendant_hearing_attendance = as.integer(
      any(lengths(defendants_at_hearing) > 0 | lengths(defendant_attorneys_at_hearing) > 0)
    )
  ) %>%
  ungroup()

#create case-level defendant_rep variable
llm_data_combined <- llm_data_combined %>%
  group_by(case_number) %>%
  mutate(
    defendant_rep = as.integer(
      any((lengths(defendant_attorneys) > 0 & nzchar(sapply(defendant_attorneys, paste, collapse=""))) |
            (lengths(defendant_attorneys_at_hearing) > 0 & nzchar(sapply(defendant_attorneys_at_hearing, paste, collapse=""))))
    )
  ) %>%
  ungroup()

# create case-level court_displacement variable
llm_data_combined <- llm_data_combined %>%
  group_by(case_number) %>%
  mutate(
    court_displacement = {
      # gather relevant documents with file dates
      docs <- tibble(
        source = source,
        file_date = case_when(
          source == "writ" ~ writ_file_date,
          source == "stay_vacate" ~ stay_vacate_file_date,
          source == "dismissal" ~ dismissal_file_date,
          source == "agreement" ~ document_file_date,
          TRUE ~ as.Date(NA)
        ),
        tenant_move = tenant_move
      ) %>%
        filter(source %in% c("writ", "stay_vacate", "dismissal", "agreement")) %>%
        filter(!is.na(file_date)) %>%
        arrange(file_date)
      
      if (nrow(docs) == 0) {
        # no relevant documents
        NA
      } else {
        # find last date with meaningful tenant_move info (non-empty, non-NA)
        # writs always count as meaningful even if tenant_move is NA
        docs_with_info <- docs %>%
          mutate(has_info = source == "writ" | (!is.na(tenant_move) & tenant_move != ""))
        
        last_meaningful_date <- docs_with_info %>%
          filter(has_info) %>%
          pull(file_date) %>%
          max(na.rm = TRUE)
        
        if (is.infinite(last_meaningful_date) || is.na(last_meaningful_date)) {
          NA
        } else {
          # get all docs on that date
          final_docs <- docs %>% filter(file_date == last_meaningful_date)
          
          # source priority: agreement > stay_vacate > dismissal > writ
          final_docs <- final_docs %>%
            mutate(
              priority = case_when(
                source == "agreement" ~ 1,
                source == "stay_vacate" ~ 2,
                source == "dismissal" ~ 3,
                source == "writ" ~ 4,
                TRUE ~ 999
              ),
              tenant_move_clean = if_else(
                tenant_move == "" | is.na(tenant_move),
                NA_character_,
                tenant_move
              )
            )
          
          # for same source, get most common tenant_move value
          final_docs <- final_docs %>%
            group_by(source) %>%
            mutate(
              most_common = {
                moves <- tenant_move_clean[!is.na(tenant_move_clean)]
                if (length(moves) == 0) NA_character_ else names(sort(table(moves), decreasing = TRUE))[1]
              }
            ) %>%
            ungroup()
          
          # pick highest priority source
          final_doc <- final_docs %>%
            arrange(priority, desc(!is.na(most_common)), desc(row_number())) %>%
            slice(1)
          
          source_final <- final_doc$source[1]
          move_final <- final_doc$most_common[1]
          
          # apply logic
          case_when(
            source_final == "writ" ~ TRUE,
            source_final == "stay_vacate" & move_final == "No" ~ FALSE,
            source_final == "stay_vacate" & move_final == "Yes" ~ TRUE,
            source_final == "dismissal" & move_final == "Yes" ~ TRUE,
            source_final == "dismissal" & move_final == "No" ~ FALSE,
            source_final == "agreement" & move_final == "Yes" ~ TRUE,
            source_final == "agreement" & move_final == "No" ~ FALSE,
            TRUE ~ NA
          )
        }
      }
    }
  ) %>%
  ungroup()