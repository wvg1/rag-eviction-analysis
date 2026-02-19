library(tidyverse)

# mcnemar
verified_data |>
  summarise(across(ends_with("_verified"), 
                   ~paste(sort(unique(as.character(.))), collapse = ", ")))

# And check the LLM prediction columns in final_merged_data
final_merged_data |>
  summarise(across(c(defendant_rep_merged, defendant_appearance, 
                     hearing_held, writ_final, dismissal_final, old_final),
                   ~paste(sort(unique(as.character(.))), collapse = ", ")))

# And confirm how many rows are in verified_data
nrow(verified_data)

# Prep algorithmic baseline (already cleaned)
algo_baseline <- algorithmic_data |>
  select(case_number, 
         algo_tenant_rep  = tenant_rep_final,
         algo_appearance  = appearance,
         algo_hearing     = hearing_held,
         algo_writ        = writ_final,
         algo_dismissal   = dismissal_final,
         algo_old         = old_final)

# Prep LLM predictions from final_merged_data
llm_preds <- final_merged_data |>
  select(case_number,
         llm_tenant_rep  = defendant_rep_merged,
         llm_appearance  = defendant_appearance,
         llm_hearing     = hearing_held,
         llm_writ        = writ_final,
         llm_dismissal   = dismissal_final,
         llm_old         = old_final)

# Build validation set: join all three, coerce verified cols to logical
val <- verified_data |>
  mutate(across(ends_with("_verified"), as.logical)) |>
  left_join(algo_baseline, by = "case_number") |>
  left_join(llm_preds,    by = "case_number")

# Check joins worked
cat("Rows after join:", nrow(val), "\n")
cat("Missing algo matches:", sum(is.na(val$algo_writ)), "\n")
cat("Missing LLM matches:", sum(is.na(val$llm_writ)), "\n")
