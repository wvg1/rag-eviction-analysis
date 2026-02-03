#load packages
library(tidyverse)
library(lubridate)

# load data
llm_data_writ <- readRDS("data/llm_data_writ.rds")

# look for possible OCR errors in case number, fix if paths and case numbers are misaligned
writ_mismatch <- llm_data_writ %>%
  mutate(
    direc_case = basename(direc),
    match_direc_case = direc_case == case_number
  ) %>%
  filter(is.na(match_direc_case) | !match_direc_case) %>%
  select(row_id, case_number, direc, direc_case)

source("scripts/fix_by_path.R")
llm_data_writ_clean <- fix_by_path(llm_data_writ)

# update document_file_date variable name for merging
llm_data_writ_clean <- llm_data_writ_clean %>%
  rename(writ_file_date = document_file_date)

#save data
saveRDS(llm_data_writ_clean, "data/llm_data_writ_clean.rds")
