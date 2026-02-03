#load packages
library(tidyverse)
library(lubridate)

# load data
llm_data_stay_vacate <- readRDS("data/llm_data_stay_vacate.rds")

# look for possible OCR errors in case number, fix if paths and case numbers are misaligned
stay_vacate_mismatch <- llm_data_stay_vacate %>%
  mutate(
    direc_case = basename(direc),
    match_direc_case = direc_case == case_number
  ) %>%
  filter(is.na(match_direc_case) | !match_direc_case) %>%
  select(row_id, case_number, direc, direc_case)

source("scripts/fix_by_path.R")
llm_data_stay_vacate_clean <- fix_by_path(llm_data_stay_vacate)

# update document_file_date variable name for merging
llm_data_stay_vacate_clean <- llm_data_stay_vacate_clean %>%
  rename(stay_vacate_file_date = document_file_date)

#save data
saveRDS(llm_data_stay_vacate_clean, "data/llm_data_stay_vacate_clean.rds")
