### run after merge_travel_time.R
### produces all results in paper order: accuracy, descriptives, models

#load packages
library(readxl)
library(writexl)
library(tidyverse)
library(scales)
library(lubridate)
library(grid)
library(patchwork)
library(broom)
library(lmtest)
library(modelsummary)
library(tidycensus)

#load environment variables
readRenviron(".env")
census_api_key(Sys.getenv("CENSUS_API_KEY"))

#load data
final_merged_data <- readRDS("data/final_merged_data.rds")

#apply analytic sample exclusions
out_of_county_cases <- c(
  "22-2-07206-2",  # Auburn
  "22-2-09620-4",  # Seattle
  "23-2-05570-1",  # Yelm
  "23-2-05739-8",  # Clinton
  "23-2-07405-5",  # Tumwater
  "24-2-05674-8",  # Auburn
  "24-2-07646-3",  # Kent
  "24-2-09155-1"   # Arlington
)

final_merged_data <- final_merged_data %>%
  filter(commercial_flag == FALSE,
         !is.na(census_block_group),
         !case_number %in% out_of_county_cases)

#confirm final n
nrow(final_merged_data)

### ACS merge ###

#pull block group median household income from ACS 5-year estimates
#using 2020 ACS to match 2020 CBG boundaries used in geocoding
#B19013_001 = median household income in the past 12 months
acs_data <- get_acs(
  geography = "block group",
  variables = "B19013_001",
  state     = "WA",
  county    = "Pierce",
  year      = 2020,
  survey    = "acs5"
)

#clean ACS data
acs_clean <- acs_data %>%
  select(
    census_block_group = GEOID,
    median_income      = estimate,
    median_income_moe  = moe
  )

#check coverage
cat("Block groups in ACS data:", nrow(acs_clean), "\n")
cat("Block groups in analytic sample:", n_distinct(final_merged_data$census_block_group), "\n")

#check for CBGs in analytic sample not in ACS data
missing_cbg <- final_merged_data %>%
  filter(!census_block_group %in% acs_clean$census_block_group) %>%
  distinct(census_block_group)

cat("CBGs in sample missing from ACS:", nrow(missing_cbg), "\n")
if (nrow(missing_cbg) > 0) print(missing_cbg)

#merge ACS data onto final_merged_data
final_merged_data <- final_merged_data %>%
  left_join(acs_clean, by = "census_block_group")

#coverage check
cat("Cases with median income:", sum(!is.na(final_merged_data$median_income)), "\n")
cat("Cases missing median income:", sum(is.na(final_merged_data$median_income)), "\n")

#summary statistics on median income
cat("\nMedian income summary:\n")
print(summary(final_merged_data$median_income))

#define low-income indicator
#threshold: below 80% of Pierce County area median income (AMI)
#2020 Pierce County AMI = $90,100 (HUD); 80% AMI = $72,080
ami_threshold <- 45050

final_merged_data <- final_merged_data %>%
  mutate(low_income = median_income < ami_threshold)

#check distribution
final_merged_data %>%
  count(low_income) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

### accuracy ###

#load verified data
verified_data <- read_xlsx("data/verified_data.xlsx") %>%
  rename_all(tolower)

#binary variables to validate
binary_vars <- c(
  "plaintiff_rep",
  "defendant_appearance",
  "hearing_held",
  "defendant_hearing_attendance",
  "writ_final",
  "dismissal_final",
  "old_final",
  "defendant_rep_merged",
  "court_displacement"
)

#merge verified data with analytic sample
validation_data <- final_merged_data %>%
  select(case_number, all_of(binary_vars)) %>%
  inner_join(
    verified_data %>% select(case_number, ends_with("_verified")),
    by = "case_number"
  )

nrow(validation_data)  # confirm n = 300

#calculate accuracy metrics for each binary variable
validation_results <- map_df(binary_vars, function(var) {
  verified_col <- paste0(var, "_verified")
  
  if (!verified_col %in% names(validation_data)) {
    return(tibble(variable = var, n_verified = 0, notes = "no verified column"))
  }
  
  pred   <- as.logical(validation_data[[var]])
  actual <- as.logical(validation_data[[verified_col]])
  
  keep   <- !is.na(actual)
  pred   <- pred[keep]
  actual <- actual[keep]
  n      <- sum(keep)
  
  if (n == 0) return(tibble(variable = var, n_verified = 0, notes = "no verified cases"))
  
  tp <- sum(pred == TRUE  & actual == TRUE,  na.rm = TRUE)
  tn <- sum(pred == FALSE & actual == FALSE, na.rm = TRUE)
  fp <- sum(pred == TRUE  & actual == FALSE, na.rm = TRUE)
  fn <- sum(pred == FALSE & actual == TRUE,  na.rm = TRUE)
  
  accuracy  <- (tp + tn) / (tp + tn + fp + fn)
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA
  recall    <- if ((tp + fn) > 0) tp / (tp + fn) else NA
  f1        <- if (!is.na(precision) & !is.na(recall) & (precision + recall) > 0)
    2 * precision * recall / (precision + recall) else NA
  
  tibble(
    variable   = var,
    n_verified = n,
    accuracy   = round(accuracy  * 100, 1),
    precision  = round(precision * 100, 1),
    recall     = round(recall    * 100, 1),
    f1         = round(f1, 3)
  )
})

print(validation_results, n = Inf)

#overall weighted accuracy metrics (for paper text)
validation_results %>%
  summarise(
    total_n   = sum(n_verified),
    accuracy  = round(weighted.mean(accuracy,  n_verified), 1),
    precision = round(weighted.mean(precision, n_verified, na.rm = TRUE), 1),
    recall    = round(weighted.mean(recall,    n_verified, na.rm = TRUE), 1),
    f1        = round(weighted.mean(f1,        n_verified, na.rm = TRUE), 3)
  )

#amount_owed accuracy (exclude zero verified amounts to avoid infinite % error)
amount_validation <- verified_data %>%
  inner_join(
    final_merged_data %>% select(case_number, amount_owed),
    by = "case_number"
  ) %>%
  filter(!is.na(amount_owed_verified), !is.na(amount_owed),
         amount_owed_verified > 0) %>%
  mutate(
    abs_error  = abs(amount_owed - amount_owed_verified),
    pct_error  = abs_error / amount_owed_verified * 100,
    within_100 = abs_error <= 100
  ) %>%
  summarise(
    n              = n(),
    pct_within_100 = round(mean(within_100) * 100, 1),
    mae            = round(mean(abs_error), 2),
    mape           = round(mean(pct_error), 2)
  )

print(amount_validation)

### descriptive statistics ###

#table x: case characteristics
final_merged_data %>%
  summarise(
    n_cases           = n(),
    pct_2022          = mean(year == "2022") * 100,
    pct_2023          = mean(year == "2023") * 100,
    pct_2024          = mean(year == "2024") * 100,
    pct_nonmonetary   = mean(is.na(amount_owed) | amount_owed == 0) * 100,
    median_amount     = median(amount_owed[amount_owed > 0], na.rm = TRUE),
    mean_amount       = mean(amount_owed[amount_owed > 0], na.rm = TRUE),
    sd_amount         = sd(amount_owed[amount_owed > 0], na.rm = TRUE),
    pct_plaintiff_rep = mean(plaintiff_rep, na.rm = TRUE) * 100,
    pct_low_income    = mean(low_income, na.rm = TRUE) * 100,
    median_drive      = median(drive_time_mins, na.rm = TRUE),
    mean_drive        = mean(drive_time_mins, na.rm = TRUE),
    sd_drive          = sd(drive_time_mins, na.rm = TRUE),
    median_transit    = median(transit_time_mins, na.rm = TRUE),
    mean_transit      = mean(transit_time_mins, na.rm = TRUE),
    sd_transit        = sd(transit_time_mins, na.rm = TRUE)
  ) %>%
  print(width = Inf)

#table x: case outcomes by tacoma vs non-tacoma
outcome_vars <- c(
  "defendant_appearance",
  "hearing_held",
  "defendant_hearing_attendance",
  "defendant_rep_merged",
  "writ_final",
  "dismissal_final",
  "old_final",
  "court_displacement"
)

final_merged_data %>%
  mutate(is_tacoma = address_city == "Tacoma") %>%
  group_by(is_tacoma) %>%
  summarise(
    n = n(),
    across(all_of(outcome_vars), ~mean(., na.rm = TRUE) * 100)
  ) %>%
  bind_rows(
    final_merged_data %>%
      summarise(
        is_tacoma = NA,
        n = n(),
        across(all_of(outcome_vars), ~mean(., na.rm = TRUE) * 100)
      )
  ) %>%
  print(width = Inf)

#conditional hearing attendance (tenant attended | hearing held)
final_merged_data %>%
  mutate(is_tacoma = address_city == "Tacoma") %>%
  filter(hearing_held == TRUE) %>%
  group_by(is_tacoma) %>%
  summarise(
    n = n(),
    conditional_attendance = mean(defendant_hearing_attendance, na.rm = TRUE) * 100
  ) %>%
  bind_rows(
    final_merged_data %>%
      filter(hearing_held == TRUE) %>%
      summarise(
        is_tacoma = NA,
        n = n(),
        conditional_attendance = mean(defendant_hearing_attendance, na.rm = TRUE) * 100
      )
  )

#representation rates by appearance and hearing attendance (for paper text)
final_merged_data %>%
  summarise(
    rep_with_appearance    = mean(defendant_rep_merged[defendant_appearance == TRUE],  na.rm = TRUE) * 100,
    rep_without_appearance = mean(defendant_rep_merged[defendant_appearance == FALSE], na.rm = TRUE) * 100,
    rep_with_hearing       = mean(defendant_rep_merged[defendant_hearing_attendance == TRUE],  na.rm = TRUE) * 100,
    rep_without_hearing    = mean(defendant_rep_merged[defendant_hearing_attendance == FALSE], na.rm = TRUE) * 100
  )

#representation without appearance by year (for paper text)
final_merged_data %>%
  filter(defendant_appearance == FALSE) %>%
  group_by(year) %>%
  summarise(pct_rep = mean(defendant_rep_merged, na.rm = TRUE) * 100)

#appearance, hearing, and attendance rates by year (for paper text)
final_merged_data %>%
  group_by(year) %>%
  summarise(
    n               = n(),
    pct_appearance  = mean(defendant_appearance,         na.rm = TRUE) * 100,
    pct_hearing     = mean(hearing_held,                 na.rm = TRUE) * 100,
    pct_attendance  = mean(defendant_hearing_attendance, na.rm = TRUE) * 100
  )

#default judgments without hearing by tacoma and year (for paper text)
final_merged_data %>%
  mutate(
    is_tacoma       = ifelse(address_city == "Tacoma", "Tacoma", "Other cities"),
    writ_no_hearing = writ_final == TRUE & hearing_held == FALSE
  ) %>%
  group_by(year, is_tacoma) %>%
  summarise(
    n                   = n(),
    pct_writ_no_hearing = mean(writ_no_hearing, na.rm = TRUE) * 100,
    .groups             = "drop"
  ) %>%
  arrange(year, is_tacoma)

### logistic regression models ###

#helper function: fit year-stratified logistic regression
fit_year_models <- function(data, outcome, predictors) {
  data %>%
    group_by(year) %>%
    group_map(~{
      formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
      glm(formula, data = .x, family = binomial)
    }, .keep = TRUE) %>%
    set_names(sort(unique(data$year)))
}

#shared base predictors
base_predictors <- c(
  "I(address_city == 'Tacoma')",
  "I(is.na(amount_owed) | amount_owed == 0)",
  "plaintiff_rep",
  "low_income"
)

#table x: predictors of tenant appearance
appearance_models <- fit_year_models(
  final_merged_data,
  outcome    = "defendant_appearance",
  predictors = c(base_predictors, "drive_time_mins", "transit_time_mins")
)

modelsummary(
  appearance_models,
  stars     = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  statistic = "std.error",
  fmt       = 3
)

#table x: predictors of hearing held
hearing_held_models <- fit_year_models(
  final_merged_data,
  outcome    = "hearing_held",
  predictors = c(base_predictors, "defendant_appearance")
)

modelsummary(
  hearing_held_models,
  stars     = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  statistic = "std.error",
  fmt       = 3
)

#table x: predictors of hearing attendance (conditional on hearing held)
attendance_models <- fit_year_models(
  final_merged_data %>% filter(hearing_held == TRUE),
  outcome    = "defendant_hearing_attendance",
  predictors = c(base_predictors, "drive_time_mins", "transit_time_mins", "defendant_appearance")
)

modelsummary(
  attendance_models,
  stars     = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  statistic = "std.error",
  fmt       = 3
)

#table x: predictors of tenant representation
rep_models <- fit_year_models(
  final_merged_data,
  outcome    = "defendant_rep_merged",
  predictors = c(base_predictors, "defendant_appearance", "defendant_hearing_attendance")
)

modelsummary(
  rep_models,
  stars     = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  statistic = "std.error",
  fmt       = 3
)

#outcome model predictors
outcome_predictors <- c(
  base_predictors,
  "defendant_appearance",
  "defendant_hearing_attendance",
  "defendant_rep_merged"
)

#table x: predictors of eviction judgment
writ_models <- fit_year_models(
  final_merged_data,
  outcome    = "writ_final",
  predictors = outcome_predictors
)

modelsummary(
  writ_models,
  stars     = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  statistic = "std.error",
  fmt       = 3
)

#table x: predictors of dismissal
dismissal_models <- fit_year_models(
  final_merged_data,
  outcome    = "dismissal_final",
  predictors = outcome_predictors
)

modelsummary(
  dismissal_models,
  stars     = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  statistic = "std.error",
  fmt       = 3
)

#table x: predictors of record protection
old_models <- fit_year_models(
  final_merged_data,
  outcome    = "old_final",
  predictors = outcome_predictors
)

modelsummary(
  old_models,
  stars     = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  statistic = "std.error",
  fmt       = 3
)

#table x: predictors of court displacement
displacement_models <- fit_year_models(
  final_merged_data,
  outcome    = "court_displacement",
  predictors = outcome_predictors
)

modelsummary(
  displacement_models,
  stars     = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  statistic = "std.error",
  fmt       = 3
)

### figures ###

#figure x: case volume and representation by month
final_merged_data %>%
  mutate(month = floor_date(file_date, "month")) %>%
  group_by(month) %>%
  summarise(
    n_cases             = n_distinct(case_number),
    defendant_rep_cases = sum(defendant_rep_merged == 1, na.rm = TRUE),
    .groups             = "drop"
  ) %>%
  ggplot(aes(x = month)) +
  geom_col(aes(y = n_cases), fill = "lightgray", alpha = 0.7) +
  geom_line(aes(y = defendant_rep_cases), color = "darkred", linewidth = 1) +
  geom_point(aes(y = defendant_rep_cases), color = "darkred", size = 2) +
  scale_y_continuous(name = "Eviction filings", labels = scales::comma) +
  labs(title = "Eviction Filings and Tenant Legal Representation",
       x = "Case filed date", color = NULL) +
  theme_minimal() +
  theme(axis.title.y = element_text(color = "black"),
        plot.title   = element_text(face = "bold"))

#figure x: administrative checkpoints by month
final_merged_data %>%
  mutate(month = floor_date(file_date, "month")) %>%
  group_by(month) %>%
  summarise(
    `Appearance`                   = mean(defendant_appearance,         na.rm = TRUE),
    `Hearings held`                = mean(hearing_held,                 na.rm = TRUE),
    `Defendant hearing attendance` = mean(defendant_hearing_attendance, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -month, names_to = "outcome", values_to = "proportion") %>%
  ggplot(aes(x = month, y = proportion, color = outcome)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("Hearings held"                = "navy",
                                "Defendant hearing attendance" = "skyblue",
                                "Appearance"                   = "darkred")) +
  labs(title = "Administrative Checkpoints by Month",
       x = "Month (case filed date)", y = "Proportion of Cases", color = NULL) +
  theme_minimal()

#figure x: appearance-based default judgments by tacoma and year
final_merged_data %>%
  mutate(
    is_tacoma       = ifelse(address_city == "Tacoma", "Tacoma", "Other cities"),
    writ_no_hearing = writ_final == TRUE & hearing_held == FALSE
  ) %>%
  group_by(year, is_tacoma) %>%
  summarise(
    pct_writ_no_hearing = mean(writ_no_hearing, na.rm = TRUE),
    .groups             = "drop"
  ) %>%
  ggplot(aes(x = year, y = pct_writ_no_hearing, color = is_tacoma, group = is_tacoma)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("Tacoma" = "darkred", "Other cities" = "navy")) +
  labs(title = "Appearance-Based Default Judgments by Location",
       x = "Year", y = "Proportion of Cases", color = NULL) +
  theme_minimal()

#figure x: outcomes by rep status over time
make_outcome_plot <- function(data, outcome_var, title) {
  data %>%
    mutate(month = floor_date(file_date, "month")) %>%
    group_by(month, defendant_rep_merged) %>%
    summarise(proportion = mean({{outcome_var}}, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(defendant_rep_merged)) %>%
    ggplot(aes(x = month, y = proportion, color = defendant_rep_merged)) +
    geom_line(linewidth = 1) +
    scale_y_continuous(
      labels = function(x) paste0(x * 100),
      breaks = c(0.25, 0.5, 0.75),
      limits = c(0, 1)
    ) +
    scale_color_manual(
      values = c("FALSE" = "navy", "TRUE" = "skyblue"),
      labels = c("FALSE" = "Unrepresented", "TRUE" = "Represented")
    ) +
    labs(title = title, x = NULL, y = "% of cases", color = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.margin     = margin(t = 5, r = 5, b = 0, l = 5, unit = "pt"),
          plot.title      = element_text(hjust = 0.5))
}

p1 <- make_outcome_plot(final_merged_data, writ_final,         "Eviction judgments")
p2 <- make_outcome_plot(final_merged_data, dismissal_final,    "Dismissals")
p3 <- make_outcome_plot(final_merged_data, old_final,          "Records protected")
p4 <- make_outcome_plot(final_merged_data, court_displacement, "Court displacement")

(p1 | p2) / (p3 | p4) +
  plot_layout(heights = c(1, 0.9), axis_titles = "collect") &
  theme(axis.title.y = element_text(margin = margin(r = 15)))
