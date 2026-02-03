###this script
###working directory should be the main folder of the eviction-data repo

#load required packages
library(tidyverse)
library(readxl)

#make sure algorithmic_data is in environment

#check whether llm_data_combined has all of the case numbers from algorithmic_data, and review missing cases
missing_case_numbers_llm <- setdiff(algorithmic_data$case_number, llm_data_combined$case_number)
missing_cases_llm <- algorithmic_data %>%
  filter(case_number %in% missing_case_numbers_llm)

#check whether algorithmic_data has all of the case_numbers from llm_data_combined, and review missing cases
#none appear to be missing
missing_case_numbers_algo <- setdiff(llm_data_combined$case_number, algorithmic_data$case_number)
missing_cases_algo <- llm_data_combined %>%
  filter(case_number %in% missing_case_numbers_algo)

#assign variables to save
keep_llm <- c(
  "case_type",
  "filed_after_deadline",
  "amount_owed",
  "address_city",
  "address_state",
  "address_zip",
  "census_block_group",
  "plaintiff_rep",
  "defendant_appearance",
  "appearance_pro_se",
  "hearing_held",
  "defendant_hearing_attendance",
  "defendant_rep",
  "case_defendant_attorneys",
  "court_displacement",
  "case_defendant_first_names",
  "case_defendant_surnames"
)

keep_algo <- c("commercial", "year",
               "tenant_rep","writ", "dismissal", "old",
               "writ_final", "dismissal_final", "old_final")

#merge data focused on hearing and case outcomes
merged <- llm_data_combined %>%
  select(case_number, all_of(keep_llm)) %>%
  full_join(
    algorithmic_data %>% select(case_number, all_of(keep_algo)),
    by = "case_number"
  )

#create combined defendant representation variable
merged <- merged %>%
  group_by(case_number) %>%
  mutate(
    defendant_rep_merged = if_else(
      any(tenant_rep == 1 | defendant_rep == 1, na.rm = TRUE),
      1L, 0L
    )
  ) %>%
  ungroup()

#check for conflicting case types, should be blank
conflict_cases_any <- merged %>%
  group_by(case_number) %>%
  summarize(
    any_commercial1 = any(commercial == 1, na.rm = TRUE),
    any_residential = any(case_type == "residential", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(any_commercial1 & any_residential)

#create combined commercial flag
merged <- merged %>%
  group_by(case_number) %>%
  mutate(
    commercial_flag = if_else(
      any(case_type %in% "commercial" | commercial == 1, na.rm = TRUE),
      1L, 0L
    )
  ) %>%
  ungroup()

#check for other conflicting information, should return a blank tibble
vars <- setdiff(names(merged), "case_number")

within_case_conflicts <- merged %>%
  group_by(case_number) %>%
  summarise(
    across(all_of(vars), ~ n_distinct(.[!is.na(.)]), .names = "nuniq_{col}")
  ) %>%
  mutate(any_conflict = if_any(starts_with("nuniq_"), ~ .x > 1)) %>%
  filter(any_conflict)

#collapse variables and transform logical variables to binary, drop unneeded
final_merged_data <- merged %>%
  group_by(case_number) %>%
  summarise(
    across(
      all_of(vars),
      ~ .x[match(TRUE, !is.na(.x), nomatch = NA_integer_)]
    ),
    .groups = "drop"
  ) %>%
  select(-commercial, -case_type, -tenant_rep, -defendant_rep)

#read in file dates from OCLA
file_dates <- read_excel("data/case_numbers_filing_dates.xlsx")

#check for conflicting dates within case numbers, should return empty tibble
conflict_file_dates <- file_dates %>%
  group_by(case_number) %>%
  summarise(n_dates = n_distinct(file_date[!is.na(file_date)]), .groups = "drop") %>%
  filter(n_dates > 1)

conflict_file_dates

#collapse and print n of case_numbers
file_dates_collapsed <- file_dates %>%
  distinct(case_number, .keep_all = TRUE)

n_distinct(file_dates$case_number)

#look for missing case numbers in data, need to collect data on these cases somehow
setdiff(file_dates$case_number, llm_data_combined$case_number)
setdiff(file_dates$case_number, algorithmic_data$case_number)

#join dataset with file dates by case number
final_merged_data <- final_merged_data %>%
  left_join(
    file_dates %>% distinct(case_number, .keep_all = TRUE) %>% select(case_number, file_date),
    by = "case_number"
  ) %>%
  mutate(
    plaintiff_rep = as.logical(plaintiff_rep),
    hearing_held = as.logical(hearing_held),
    defendant_hearing_attendance = as.logical(defendant_hearing_attendance),
    defendant_rep_merged = as.logical(defendant_rep_merged),
    commercial_flag = as.logical(commercial_flag),
    filed_after_deadline = as.logical(filed_after_deadline),
    appearance_pro_se = as.logical(appearance_pro_se),
  )

#fill in missing file_dates manually
final_merged_data <- final_merged_data %>%
  mutate(file_date = case_when(
    case_number == "22-2-04909-5" ~ as.POSIXct("2022-02-24"),
    case_number == "22-2-04913-3" ~ as.POSIXct("2022-02-24"),
    case_number == "22-2-04935-4" ~ as.POSIXct("2022-02-25"),
    case_number == "22-2-04937-1" ~ as.POSIXct("2022-02-25"),
    case_number == "22-2-04938-9" ~ as.POSIXct("2022-02-25"),
    case_number == "22-2-04950-8" ~ as.POSIXct("2022-02-28"),
    case_number == "22-2-06054-4" ~ as.POSIXct("2022-04-27"),
    case_number == "22-2-06059-5" ~ as.POSIXct("2022-04-27"),
    case_number == "22-2-06061-7" ~ as.POSIXct("2022-04-27"),
    case_number == "22-2-06062-5" ~ as.POSIXct("2022-04-27"),
    case_number == "22-2-06065-0" ~ as.POSIXct("2022-04-27"),
    case_number == "22-2-06074-9" ~ as.POSIXct("2022-04-28"),
    case_number == "22-2-06083-8" ~ as.POSIXct("2022-04-28"),
    case_number == "22-2-06103-6" ~ as.POSIXct("2022-04-29"),
    case_number == "22-2-06106-1" ~ as.POSIXct("2022-04-29"),
    case_number == "22-2-06110-9" ~ as.POSIXct("2022-04-29"),
    case_number == "22-2-09960-2" ~ as.POSIXct("2022-11-03"),
    TRUE ~ file_date
  ))

#drop one case that was added with no relevant docs filed
final_merged_data <- final_merged_data %>%
  filter(case_number != "24-2-09919-6")

#drop case with no summons or complaint filed
final_merged_data <- final_merged_data %>%
  filter(case_number != "22-2-08148-7")

#save rds
saveRDS(final_merged_data, "data/final_merged_data.rds")
