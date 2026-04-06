### document-level validation
# libraries
library(tidyverse)
library(writexl)
library(readxl)

# load all document-level data
llm_data_summons <- readRDS("data/llm_data_summons_clean.rds")
llm_data_complaint <- readRDS("data/llm_data_complaint_clean.rds")
llm_data_minute_entry <- readRDS("data/llm_data_minute_entry_clean.rds")
llm_data_agreement <- readRDS("data/llm_data_agreement_clean.rds")
llm_data_dismissal <- readRDS("data/llm_data_dismissal_clean.rds")
llm_data_stay_vacate <- readRDS("data/llm_data_stay_vacate_clean.rds")
llm_data_appearance <- readRDS("data/llm_data_appearance_clean.rds")

# calculate validation allocation: 2% of total documents, minimum 25 per type
doc_counts <- tibble(
  doc_type = c("summons", "complaint", "minute_entry", "agreement", "dismissal", "stay_vacate", "appearance"),
  n = c(
    nrow(llm_data_summons),
    nrow(llm_data_complaint),
    nrow(llm_data_minute_entry),
    nrow(llm_data_agreement),
    nrow(llm_data_dismissal),
    nrow(llm_data_stay_vacate),
    nrow(llm_data_appearance)
  )
) %>%
  mutate(
    prop = n / sum(n),
    validation_n_prop = round(prop * (sum(n) * 0.02)),
    validation_n = pmax(validation_n_prop, 25)
  ) %>%
  select(doc_type, n, prop, validation_n)

print(doc_counts)

# extract validation_n for each doc type
validation_n_summons <- doc_counts %>% filter(doc_type == "summons") %>% pull(validation_n)
validation_n_complaint <- doc_counts %>% filter(doc_type == "complaint") %>% pull(validation_n)
validation_n_minute_entry <- doc_counts %>% filter(doc_type == "minute_entry") %>% pull(validation_n)
validation_n_agreement <- doc_counts %>% filter(doc_type == "agreement") %>% pull(validation_n)
validation_n_dismissal <- doc_counts %>% filter(doc_type == "dismissal") %>% pull(validation_n)
validation_n_stay_vacate <- doc_counts %>% filter(doc_type == "stay_vacate") %>% pull(validation_n)
validation_n_appearance <- doc_counts %>% filter(doc_type == "appearance") %>% pull(validation_n)

### summons validation ###
set.seed(100)
validation_sample_summons <- llm_data_summons %>%
  mutate(
    summons_file_date = as.Date(summons_file_date),
    year = year(summons_file_date)
  ) %>%
  group_by(year) %>%
  slice_sample(n = ceiling(validation_n_summons / n_distinct(llm_data_summons$year))) %>%
  ungroup() %>%
  slice_head(n = validation_n_summons)

validation_template_summons <- validation_sample_summons %>%
  select(row_id, document, case_number, summons_file_date, case_type) %>%
  mutate(
    case_type_manual = NA_character_,
    notes = NA_character_
  ) %>%
  rename(case_type_extracted = case_type)

write_xlsx(validation_template_summons, "data/validation_summons_template.xlsx")

### complaint validation ###
set.seed(101)
validation_sample_complaint <- llm_data_complaint %>%
  mutate(year = str_extract(direc, "\\d{4}")) %>%
  group_by(year) %>%
  slice_sample(n = ceiling(validation_n_complaint / n_distinct(llm_data_complaint$year))) %>%
  ungroup() %>%
  slice_head(n = validation_n_complaint)

validation_template_complaint <- validation_sample_complaint %>%
  select(row_id, document, case_number, case_type, amount_owed, monthly_rent) %>%
  mutate(
    case_type_manual = NA_character_,
    amount_owed_manual = NA_real_,
    monthly_rent_manual = NA_real_,
    notes = NA_character_
  ) %>%
  rename(
    case_type_extracted = case_type,
    amount_owed_extracted = amount_owed,
    monthly_rent_extracted = monthly_rent
  )

write_xlsx(validation_template_complaint, "data/validation_complaint_template.xlsx")

### minute_entry validation ###
set.seed(102)
validation_sample_minute_entry <- llm_data_minute_entry %>%
  mutate(year = year(hearing_date)) %>%
  group_by(year) %>%
  slice_sample(n = ceiling(validation_n_minute_entry / n_distinct(llm_data_minute_entry$year))) %>%
  ungroup() %>%
  slice_head(n = validation_n_minute_entry)

validation_template_minute_entry <- validation_sample_minute_entry %>%
  select(row_id, document, case_number, hearing_date, defendants_at_hearing) %>%
  mutate(
    defendants_at_hearing_manual = NA_character_,
    notes = NA_character_
  ) %>%
  rename(
    defendants_at_hearing_extracted = defendants_at_hearing
  )

write_xlsx(validation_template_minute_entry, "data/validation_minute_entry_template.xlsx")

### agreement validation ###
set.seed(103)
validation_sample_agreement <- llm_data_agreement %>%
  mutate(year = year(document_file_date)) %>%
  group_by(year) %>%
  slice_sample(n = ceiling(validation_n_agreement / n_distinct(llm_data_agreement$year))) %>%
  ungroup() %>%
  slice_head(n = validation_n_agreement)

validation_template_agreement <- validation_sample_agreement %>%
  select(row_id, document, case_number, document_file_date, agreement, tenant_move) %>%
  mutate(
    agreement_manual = NA_character_,
    tenant_move_manual = NA_character_,
    notes = NA_character_
  ) %>%
  rename(
    agreement_extracted = agreement,
    tenant_move_extracted = tenant_move
  )

write_xlsx(validation_template_agreement, "data/validation_agreement_template.xlsx")

### dismissal validation ###
set.seed(104)
validation_sample_dismissal <- llm_data_dismissal %>%
  mutate(
    dismissal_file_date = as.Date(dismissal_file_date),
    year = year(dismissal_file_date)
  ) %>%
  group_by(year) %>%
  slice_sample(n = ceiling(validation_n_dismissal / n_distinct(llm_data_dismissal$year))) %>%
  ungroup() %>%
  slice_head(n = validation_n_dismissal)

validation_template_dismissal <- validation_sample_dismissal %>%
  select(row_id, document, case_number, dismissal_file_date, tenant_move) %>%
  mutate(
    tenant_move_manual = NA_character_,
    notes = NA_character_
  ) %>%
  rename(
    tenant_move_extracted = tenant_move
  )

write_xlsx(validation_template_dismissal, "data/validation_dismissal_template.xlsx")

### stay_vacate validation ###
set.seed(105)
validation_sample_stay_vacate <- llm_data_stay_vacate %>%
  mutate(
    stay_vacate_file_date = as.Date(stay_vacate_file_date),
    year = year(stay_vacate_file_date)
  ) %>%
  group_by(year) %>%
  slice_sample(n = ceiling(validation_n_stay_vacate / n_distinct(llm_data_stay_vacate$year))) %>%
  ungroup() %>%
  slice_head(n = validation_n_stay_vacate)

validation_template_stay_vacate <- validation_sample_stay_vacate %>%
  select(row_id, document, case_number, stay_vacate_file_date, tenant_move) %>%
  mutate(
    tenant_move_manual = NA_character_,
    notes = NA_character_
  ) %>%
  rename(
    tenant_move_extracted = tenant_move
  )

write_xlsx(validation_template_stay_vacate, "data/validation_stay_vacate_template.xlsx")

### appearance validation ###
set.seed(106)
validation_sample_appearance <- llm_data_appearance %>%
  mutate(year = year(document_file_date)) %>%
  group_by(year) %>%
  slice_sample(n = ceiling(validation_n_appearance / n_distinct(llm_data_appearance$year))) %>%
  ungroup() %>%
  slice_head(n = validation_n_appearance)

validation_template_appearance <- validation_sample_appearance %>%
  select(row_id, document, case_number, document_file_date, appearance_for) %>%
  mutate(
    appearance_for_manual = NA_character_,
    notes = NA_character_
  ) %>%
  rename(
    appearance_for_extracted = appearance_for
  )

write_xlsx(validation_template_appearance, "data/validation_appearance_template.xlsx")
