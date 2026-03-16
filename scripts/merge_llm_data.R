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

# alternative approach: dismissal with uncertain or no move = not displaced
llm_data_combined <- llm_data_combined %>%
  group_by(case_number) %>%
  mutate(
    court_displacement_alt = case_when(
      # if court_displacement is already non-NA, use it unless it's a dismissal with NA move
      !is.na(court_displacement) ~ court_displacement,
      # dismissals with no/missing tenant_move = not displaced
      any(source == "dismissal") ~ FALSE,
      TRUE ~ NA
    )
  ) %>%
  ungroup()

# compare two court_displacement measures
llm_data_combined %>%
  distinct(case_number, court_displacement, court_displacement_alt) %>%
  count(court_displacement, court_displacement_alt)