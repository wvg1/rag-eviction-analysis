###this script transforms variables in the algorithmic_data df and presents several simple analyses of the transformed dataset 
###requires the algorithmic_data df created by doctype_search.R and algorithmic_doc_search.R 
###working directory should be the main folder of the eviction-data repo

#load packages
library(tidyverse)

#read in data
algorithmic_data <- readRDS("data/algorithmic_data.rds")

#rename variables
algorithmic_data <- algorithmic_data %>%
  rename(
    year = years,
    case_number = case_nos
  )

#create binary logical indicators for most variables
algorithmic_data <- algorithmic_data %>%
  mutate(across(
    c(commercial, ejectment,appearance, show_cause, hearing_held, tenant_rep, tenant_rep_denied, dismissal, dismissal_vacated, 
      writ, writ_stayed, writ_vacated, old, old_vacated),
    ~ . != 0
  ))

#check for case number duplicates, should return empty dataframe
algorithmic_data %>%
  count(case_number) %>%
  filter(n > 1)

#create final outcome variables
algorithmic_data <- algorithmic_data %>%
  mutate(
    writ_final       = writ == TRUE & writ_vacated == FALSE,
    dismissal_final  = dismissal == TRUE & dismissal_vacated == FALSE,
    old_final        = old == TRUE & old_vacated == FALSE,
    tenant_rep_final = tenant_rep == TRUE & tenant_rep_denied == FALSE
  )

