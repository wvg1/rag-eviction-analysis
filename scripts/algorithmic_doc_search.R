###this script creates paired lists of included and excluded document names to search for specific named documents while excluding others
###it relies on the doctype_search function and assumes that document names in the paired lists are in case-level folders nested in year folders 
###working directory should be the main folder of the eviction-data repo, ensure folders and docs are in /data before running

#define data location
folder_location <- "data/2022-24 data"

#define doc name vectors
doc_names_commercial <- c("commercial")

doc_names_ejectment <- c("ejectment")

doc_names_appearance <- c("appearance_pro_se",
                          "response_from_defendant",
                          "response",
                          "answer",
                          "answer1",
                          "defendant_s_answer",
                          "defendants_answer_to_complaint",
                          "defendant_s_answer_to_complaint",
                          "defendant_s_response",
                          "defendants_response",
                          "answer_defendants_answer_to_complaint",
                          "response_noa")

doc_names_excluded_appearance <- c("answer_order_fee_unlawful_detainer_received",
                         "answer_to_writ_of_garnishment",
                         "answer_to_writ_of_garnishment_2nd1",
                         "answer_to_writ_of_garnishment_2nd",
                         "judgment_on_answer_of_garnishee_defendant_128_48",
                         "judgment_on_answer_of_garnishee_defendant_1638_70")

doc_names_show_cause <- c("order_to_show_cause",
                          "order_staying_writ_and_order_show_cause",
                          "order_to_show_cause_and_staying_writ",
                          "order_to_show_cause_stay_writ_and_judgment")

doc_names_hearing_minutes <- c("clerk_s_minute_entry")

doc_names_tenant_rep <- c("order_of_continuance_and_appointing_counsel",
                          "order_of_continuance_and_appointing_counsel_staying_writ",
                          "order_appointing_counsel",
                          "order_re_appointment_of_counsel_and_continuance",
                          "order_re_appointment_of_counsel_and_order_to_meet_and_confer_and_continuingwith_the_drc",
                          "order_re_appointment_of_counsel_and_order_to_meet_and_confer_with_the_drc_continuing",
                          "order_re_appointment_of_counsel_drc_meet_and_confer")

doc_names_tenant_rep_denied <- c("order_denying_appointment_of_counsel",
                                 "order_denying_motion_to_appoint_counsel",
                                 "order_denying_appointment",
                                 "order_denying_appt_of_counsel",
                                 "order_denying_appointment_of_counsel_continuing_hearing",
                                 "order_denying_appointment_and_continuance",
                                 "order_denying_motion_for_appointment_of_counsel")

doc_names_dismissal <- c("agreed_order_of_dismissal_w_out_prejudice",
                         "order_of_dismissal_partial",
                         "order_of_dismissal_w_out_prejudice",
                         "order_of_dismissal_without_prejudice",
                         "order_of_dismissal_with_prejudice",
                         "order_of_dismissal",
                         "order_of_voluntary_dismissal_with_prejudice",
                         "order_of_dismissal_with_prejudice_for_limited_dissemination",
                         "order_of_dismissal_w_out_prejudice_limited_dissemination",
                         "order_of_dismissal_order_granting_limited_dissemination",
                         "order_of_dismissal_with_prejudice_for_limited_dissemination",
                         "order_of_dismissal_w_out_prejudice_order_of_limited_dissemination",
                         "order_vacating_writ_of_restitution_default_and_order_of_dismissal_with_prejudice")

doc_names_dismissal_vacated <- c("order_of_dismissal_vacated",
                        "order_of_dismissal_w_out_prejudice_vacated",
                        "order_vacating_dismissal")

doc_names_writ <- c("default_judgment_and_order_for_writ_of_restitution",
                    "default_order_for_writ_of_restitution",
                    "judgment_and_order_for_writ_of_restitution",
                    "judgment_and_order_for_writ_of_restitution_and_tenancy_preservation_program",
                    "judgment_and_order_for_writ_of_restitution_disbursement",
                    "judgment_and_order_for_writ_of_restitution_stipulated",
                    "judgment_and_order_for_writ_of_restitution",
                    "order_for_default_and_writ_of_restitution",
                    "order_for_writ_of_restitution_and_cr2a",
                    "order_for_writ_of_restitution_and_default",
                    "order_for_writ_of_restitution_and_show_cause",
                    "order_for_writ_of_restitution_findings_of_fact",
                    "order_for_writ_of_restitution_vacating_old",
                    "order_for_writ_of_restitution",
                    "order_reissuing_writ",
                    "order_staying_writ_denying_motion_to_vacate",
                    "writ_of_restitution_issued")

doc_names_excluded_writ <- c("answer_to_writ_of_garnishment",
                   "writ_of_garnishment_with_fee",
                   "writ_of_garnishment")

doc_names_writ_stayed <- c("order_staying_writ_and_judgment",
                  "order_staying_writ_and_order_show_cause",
                  "order_staying_writ",
                  "order_to_show_cause_and_staying_writ",
                  "order_to_show_cause_stay_writ_and_judgment",
                 "order_of_continuance_and_appointing_counsel_staying_writ",
                 "order_to_show_cause_stay_writ_of_restitution")

doc_names_writ_vacated <- c("order_vacating_writ_of_restitution_default_and_order_of_dismissal_with_prejudice",
                  "default_order_for_writ_of_restitution_vacated",
                  "judgment_and_order_for_writ_of_restitution_vacated_",
                  "judgment_order_for_writ_of_restitution_vacated_04_11_24",
                  "order_for_writ_of_restitution_quashed",
                  "order_for_writ_of_restitution_vacated",
                  "order_quashing_writ",
                  "order_vacating_default_and_quashing_writ",
                  "order_vacating_default_judgment",
                  "order_vacating_default_order_and_writ_and_order_of_continuance",
                  "order_vacating_judgment_order_for_writ",
                  "order_vacating_judgment_quashing_writ",
                  "order_vacating_judgment_quash_writ",
                  "order_vacating_writ")
  
doc_names_agreement <- c("cr2a",
                    "order_on_cr2a",
                    "cr2a_agreement",
                    "order_for_writ_of_restitution_and_cr2a",
                    "agreement")

doc_names_old <- c("order_granting_limited_dissemination",
                   "order_granting_limited_dissemination_garrison",
                   "order_granting_limited_dissemination1",
                   "order_granting_limited_dissemination12",
                   "agreed_order_granting_limited_dissemination",
                   "order_of_dismissal_w_out_prejudice_limited_dissemination",
                   "order_of_dismissal_order_granting_limited_dissemination",
                   "order_of_dismissal_with_prejudice_for_limited_dissemination",
                   "order_of_dismissal_w_out_prejudice_order_of_limited_dissemination")

doc_names_old_vacated <- c("order_granting_limited_dissemination_vacated",
                  "order_for_writ_of_restitution_vacating_old")

#create paired lists for included and excluded
doc_names_list <- list(doc_names_commercial, doc_names_ejectment, list(doc_names_appearance, doc_names_excluded_appearance), doc_names_show_cause,
                       doc_names_hearing_minutes, doc_names_tenant_rep, doc_names_tenant_rep_denied, doc_names_dismissal, doc_names_dismissal_vacated, list(doc_names_writ, doc_names_excluded_writ),
                       doc_names_writ_stayed, doc_names_writ_vacated, doc_names_agreement, doc_names_old, doc_names_old_vacated)

#create vectors for data frame
years <- vector(mode = "character")
case_nos <- vector(mode = "character")
locs <- vector(mode = "character")

#define year folders 
year_folders <- list.dirs(folder_location, recursive = FALSE)

for (year_path in year_folders) {
  year <- basename(year_path)
  
  #define case folders
  case_folders <- list.dirs(year_path, recursive = FALSE)
  
  #create observations for each case 
  for (case_path in case_folders) {
    case_no <- basename(case_path)
    case_nos <- c(case_nos, case_no)
    years <- c(years, year)
    locs <- c(locs, case_path)
  }
}

#source doctype_search function from repo
source("scripts/doctype_search.R")

#create variables using doctype_search
variable_search <- lapply(doc_names_list, function(d) {
  if(is.list(d)){
    sapply(locs, function(z) doctype_search(z,d[[1]],d[[2]]))
  } else {
    sapply(locs, function(z) doctype_search(z,d))
  }
})

# create dataframe
algorithmic_data <- as.data.frame(cbind(case_nos, locs, years, as.data.frame(variable_search)))
rownames(algorithmic_data) <- NULL

#name columns in alignment with doc name vectors
colnames(algorithmic_data)[-(1:3)] <- c("commercial", "ejectment", "appearance", "show_cause", "hearing_held",
"tenant_rep", "tenant_rep_denied", "dismissal", "dismissal_vacated", "writ", "writ_stayed", "writ_vacated", "cr2a","old", "old_vacated")

#save data
saveRDS(algorithmic_data, "data/algorithmic_data.rds")
