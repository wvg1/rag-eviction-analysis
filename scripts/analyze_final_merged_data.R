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

#drop commercial cases
final_merged_data <- final_merged_data %>%
  filter(commercial_flag == 0)

#drop cases without census block group (can't do BISG)
final_merged_data <- final_merged_data %>%
  filter(!is.na(census_block_group))

#descriptive outcome stats by year
final_merged_data %>%
  group_by(year) %>%
  summarise(
    n_cases = n_distinct(case_number),
    defendant_rep = mean(defendant_rep_merged, na.rm = TRUE),
    dismissal = mean(dismissal_final, na.rm = TRUE),
    writ = mean(writ_final, na.rm = TRUE),
    old = mean(old_final, na.rm = TRUE),
    court_displacement = mean(court_displacement, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  bind_rows(
    final_merged_data %>%
      summarise(
        years = "Total",
        n_cases = n_distinct(case_number),
        defendant_rep = mean(defendant_rep_merged, na.rm = TRUE),
        dismissal = mean(dismissal_final, na.rm = TRUE),
        writ = mean(writ_final, na.rm = TRUE),
        old = mean(old_final, na.rm = TRUE),
        court_displacement = mean(court_displacement, na.rm = TRUE)
      )
  ) %>%
  print(n = Inf)

#plot case volume and representation by month
final_merged_data %>%
  mutate(month = floor_date(file_date, "month")) %>%
  group_by(month) %>%
  summarise(
    n_cases = n_distinct(case_number),
    defendant_rep_cases = sum(defendant_rep_merged == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = month)) +
  geom_col(aes(y = n_cases), fill = "lightgray", alpha = 0.7) +
  geom_line(aes(y = defendant_rep_cases), 
            color = "darkred", linewidth = 1) +
  geom_point(aes(y = defendant_rep_cases),
             color = "darkred", size = 2) +
  scale_y_continuous(
    name = "Eviction filings",
    labels = scales::comma
  ) +
  labs(
    title = "Eviction Filings and Tenant Legal Representation",
    x = "Case filed date",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "black"),
    plot.title = element_text(face = "bold")
  )

#plot appearance and hearing attendance by month
final_merged_data %>%
  mutate(month = floor_date(file_date, "month")) %>%
  group_by(month) %>%
  summarise(
    `Appearance` = mean(defendant_appearance, na.rm = TRUE),
    `Hearings held` = mean(hearing_held, na.rm = TRUE),
    `Defendant hearing attendance` = mean(defendant_hearing_attendance, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -month, names_to = "outcome", values_to = "proportion") %>%
  ggplot(aes(x = month, y = proportion, color = outcome)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("Hearings held" = "navy", 
                                "Defendant hearing attendance" = "skyblue",
                                "Appearance" = "darkred")) +
  labs(
    title = "Procedural outcomes",
    x = "Month (case filed date)",
    y = "Proportion of Cases",
    color = NULL
  ) +
  theme_minimal()

#defendant rep by appearance and hearing attendance
final_merged_data %>%
  pivot_longer(cols = c(defendant_appearance, defendant_hearing_attendance), 
               names_to = "variable", 
               values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(variable, value) %>%
  summarise(prop_defendant_rep = mean(defendant_rep_merged, na.rm = TRUE), .groups = "drop") %>%
  mutate(variable = recode(variable,
                           "defendant_appearance" = "Appearance",
                           "defendant_hearing_attendance" = "Hearing attendance"),
         variable = factor(variable, levels = c("Appearance", "Hearing attendance")),
         value = ifelse(value, "Yes", "No")) %>%
  ggplot(aes(x = variable, y = prop_defendant_rep, fill = value)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = c("No" = "navy", "Yes" = "skyblue")) +
  labs(
    title = "defendant Behaviors and Representation",
    x = NULL,
    y = "defendant Representation",
    fill = NULL
  ) +
  theme_minimal()

#behaviors and representation by location
final_merged_data %>%
  filter(!is.na(address_city)) %>%
  mutate(is_tacoma = ifelse(address_city == "Tacoma", "Tacoma", "Other cities")) %>%
  pivot_longer(cols = c(defendant_appearance, defendant_hearing_attendance, defendant_rep_merged), 
               names_to = "variable", 
               values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(is_tacoma, variable, value) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(is_tacoma, variable) %>%
  mutate(rate = count / sum(count)) %>%
  filter(value == TRUE) %>%  # Show only "Yes" rates
  mutate(variable = recode(variable,
                           "defendant_appearance" = "Appearance",
                           "defendant_hearing_attendance" = "Hearing Held",
                           "defendant_rep_merged" = "Representation"),
         variable = factor(variable, levels = c("Appearance", "Hearing Held", "Representation"))) %>%
  ggplot(aes(x = is_tacoma, y = rate, fill = variable)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = c("Appearance" = "navy", 
                               "Hearing Held" = "skyblue", 
                               "Representation" = "salmon")) +
  labs(
    title = "defendant Behaviors and Representation by Location",
    x = NULL,
    y = "Rate",
    fill = NULL
  ) +
  theme_minimal()

#outcomes by month
final_merged_data %>%
  mutate(month = floor_date(file_date, "month")) %>%
  group_by(month) %>%
  summarise(
    Dismissal = mean(dismissal_final, na.rm = TRUE),
    `Eviction judgment` = mean(writ_final, na.rm = TRUE),
    `Court displacement` = mean(court_displacement, na.rm = TRUE),
    `Record protected` = mean(old_final, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -month, names_to = "outcome", values_to = "proportion") %>%
  ggplot(aes(x = month, y = proportion, color = outcome)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("Dismissal" = "navy", 
                                "Eviction judgment" = "skyblue", 
                                "Court displacement" = "darkred",
                                "Record protected" = "gray50")) +
  labs(
    title = "Case Outcomes by Filing Month",
    x = "Month",
    y = "Proportion of Cases Filed by Month",
    color = "Case outcome"
  ) +
  theme_minimal()

#representation by month
final_merged_data %>%
  mutate(month = floor_date(file_date, "month")) %>%
  group_by(month) %>%
  summarise(prop_defendant_rep = mean(defendant_rep_merged, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = prop_defendant_rep)) +
  geom_line(color = "navy", linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "defendant Representation by Month",
    x = "Month",
    y = "Proportion of Cases"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#panel of graphs showing outcomes by rep status over time
make_outcome_plot <- function(data, outcome_var, title) {
  data %>%
    mutate(month = floor_date(file_date, "month")) %>%
    group_by(month, defendant_rep_merged) %>%
    summarise(proportion = mean({{outcome_var}}, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(defendant_rep_merged)) %>%
    ggplot(aes(x = month, y = proportion, color = defendant_rep_merged)) +
    geom_line(linewidth = 1) +
    scale_y_continuous(labels = function(x) paste0(x * 100), breaks = c(0.25, 0.5, 0.75), limits = c(0, 1)) +
    scale_color_manual(values = c("FALSE" = "navy", "TRUE" = "skyblue"),
                       labels = c("FALSE" = "Unrepresented", "TRUE" = "Represented")) +
    labs(title = title, x = NULL, y = "% of cases", color = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom", 
          plot.margin = margin(t = 5, r = 5, b = 0, l = 5, unit = "pt"),
          plot.title = element_text(hjust = 0.5))
}

p1 <- make_outcome_plot(final_merged_data, writ_final, "Eviction judgments")
p2 <- make_outcome_plot(final_merged_data, dismissal_final, "Dismissals")
p3 <- make_outcome_plot(final_merged_data, old_final, "Records protected")
p4 <- make_outcome_plot(final_merged_data, court_displacement, "Court displacement")

(p1 | p2) / (p3 | p4) + 
  plot_layout(heights = c(1, 0.9), axis_titles = "collect") &
  theme(axis.title.y = element_text(margin = margin(r = 15)))

#random sample of cases for hand coding, third iteration
set.seed(215)
final_merged_data %>%
  group_by(year) %>%
  slice_sample(n = 100) %>%
  ungroup() %>%
  arrange(year, case_number) %>%
  select(case_number, file_date, amount_owed, census_block_group, plaintiff_rep, 
         defendant_appearance, hearing_held, defendant_hearing_attendance, 
         case_defendant_surnames, writ_final, dismissal_final, old_final,
         defendant_rep_merged, commercial_flag, court_displacement) %>%
  mutate(case_defendant_surnames = map_chr(case_defendant_surnames, 
                                           ~paste(.x, collapse = ", "))) %>%
  write_xlsx("data/final_merged_data_sample_v3.xlsx")

#random sample of cases for hand coding
set.seed(9)
final_merged_data %>%
  group_by(year) %>%
  slice_sample(n = 100) %>%
  ungroup() %>%
  arrange(year, case_number) %>%
  select(case_number, file_date, filed_after_deadline, amount_owed, plaintiff_rep, 
         defendant_appearance, appearance_pro_se, hearing_held, defendant_hearing_attendance,
         writ_final, dismissal_final, old_final,defendant_rep_merged, court_displacement) %>%
  write_xlsx("data/final_merged_data_random_sample.xlsx")

### analyze accuracy of RAG pipeline based on random sample of 300 cases ###
# load verified observations
verified_data <- read_xlsx("data/verified_data.xlsx")



