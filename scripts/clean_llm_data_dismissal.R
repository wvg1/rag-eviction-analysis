#load packages
library(tidyverse)
library(lubridate)

# load data
llm_data_dismissal <- readRDS("data/llm_data_dismissal.rds")

# look for possible OCR errors in case number, fix if paths and case numbers are misaligned
dismissal_mismatch <- llm_data_dismissal %>%
  mutate(
    direc_case = basename(direc),
    match_direc_case = direc_case == case_number
  ) %>%
  filter(is.na(match_direc_case) | !match_direc_case) %>%
  select(row_id, case_number, direc, direc_case)

source("scripts/fix_by_path.R")
llm_data_dismissal_clean <- fix_by_path(llm_data_dismissal)

# update document_file_date variable name for merging
llm_data_dismissal_clean <- llm_data_dismissal_clean %>%
  rename(dismissal_file_date = document_file_date)

#save data
saveRDS(llm_data_dismissal_clean, "data/llm_data_dismissal_clean.rds")
