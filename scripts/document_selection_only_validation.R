### document selection baseline validation ###
# remember to set working directory #
library(readxl)
library(writexl)
library(tidyverse)

# load algorithmic document selection baseline data
algorithmic_data <- readRDS("data/algorithmic_data.rds")
### run algorithmic_data_wrangling

# load verified data
verified_data <- read_xlsx("data/verified_data.xlsx")

# select relevant variables from algorithmic data
algorithmic_subset <- algorithmic_data %>%
  select(case_number, appearance, hearing_held, dismissal_final, 
         old_final, writ_final, tenant_rep_final)

# select relevant variables from verified data
verified_subset <- verified_data %>%
  select(case_number, defendant_appearance_verified, hearing_held_verified,
         defendant_hearing_attendance_verified, writ_final_verified,
         dismissal_final_verified, old_final_verified, 
         defendant_rep_merged_verified)

# join algorithmic and verified data
verified_data_document_selection_only <- verified_subset %>%
  left_join(algorithmic_subset, by = "case_number")

# save xlsx
write_xlsx(verified_data_document_selection_only, "data/verified_data_document_selection_only.xlsx")
