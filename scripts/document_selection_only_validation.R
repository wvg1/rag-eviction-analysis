### document selection baseline validation ###
# remember to set working directory #
library(readxl)
library(tidyverse)

# load algorithmic document selection baseline data
algorithmic_data <- readRDS("data/algorithmic_data.rds")

# load verified data
verified_data <- read_xlsx("data/verified_data.xlsx")

# select relevant variables from algorithmic data and join to verified data
algorithmic_subset <- algorithmic_data %>%
  select(case_number, appearance, hearing_held, dismissal_final, 
         old_final, writ_final, tenant_rep_final)

verified_data_document_selection_only <- verified_data %>%
  left_join(algorithmic_subset, by = "case_number")
