###this script uses data_from_llm.py to document case characteristics from stay and vacate of restitution documents in eviction cases
###data_from_llm.py uses chatgpt (with the prompt below) to answer questions about cases using relevant documents
###working directory should be the main folder of the eviction-data repo

#load required packages
library(tidyverse)
library(reticulate)
library(jsonlite)

#load virtual environment and python
use_virtualenv("C:/dev/eviction-data/.venv", required = TRUE)
py_config()
py <- import_builtins()
pickle <- import("pickle")

#define python file
source_python("scripts/data_from_llm.py")

#load ocr data
ocr_final <- read_rds("data/ocr_final.rds")

#define prompt
prompt_stay_vacate <- '
You are an assistant that reads dimissal documents filed in Washington State unlawful detainer (eviction) cases and extracts fields.

Return ONLY valid raw JSON and nothing else. No prose, no code fences.

Schema:
{
  "document_file_date": "string",
  "tenant_move": "string",
  "case_number": "string"
}

STRICT RULES
- If a STRING field is unknown, set it to "" (empty string).
- Do NOT guess. Prefer "" to an invented value.
- Use only ASCII. Trim leading/trailing spaces; collapse internal whitespace to single spaces.
- No newlines in any field. No trailing commas in JSON.

DOCUMENT FILE DATE
- Set "document_file_date" based on when the document was filed in the court, rather than when it was signed.
- Search priority (stop at the first match that fits):
1) A clerk/e-filing stamp or header with words like "FILED", "E-FILED", "ACCEPTED", "ENTERED", "RECEIVED", "SUBMITTED", near a date/time (often on page 1, top-right).
2) A docket/header watermark showing a file/entry date.
- Ignore signature dates, notary acknowledgments, certificate of service/mailing dates, “DATED this …” lines, and any dates inside the narrative body.
- If only a 2-digit year is present, assume 2000–2099 (e.g., 9/5/24 → 2024-09-05).
- Normalize "document_file_date" to ISO "YYYY-MM-DD".
- Accept common variants (e.g., "9/5/24", "09/05/2024", "September 5, 2024") and normalize to "YYYY-MM-DD".

TENANT MOVE
- Set to "Yes" if the document indicates that the defendant (tenant) will leave, vacate, move out of the property, if the document indicates that the tenant will remain in the same home or preserve the tenancy, then "No".
- For example, an agreement to end the tenancy would be "Yes", while an agreement to continue the tenancy using rental assistance funds would be "No".

CASE NUMBER
- Extract the WA Superior Court case number matching regex: ^\\d{2}-\\d-\\d{5}-\\d$.
- If the digits appear with other separators (e.g., "23 2 04870 4"), normalize to "23-2-04870-4".
- If you cannot form exactly 9 digits in that pattern with high confidence, set "case_number": "".

Return only the JSON object described above.
'

#function to clean leading and trailing fences from JSON strings
clean_fences <- function(s) gsub("^```[a-zA-Z]*\\s*|\\s*```$", "", s)

#function to merge duplicate observations of the same variable: if a variable appears multiple times, combine values
merge_dupe_vars <- function(x) {
  if (is.null(names(x))) return(x)
  nm <- names(x)
  if (!any(duplicated(nm))) return(x)
  
  out <- list()
  for (var in unique(nm)) {
    vals <- x[nm == var]
    #combine scalars & vectors into a single vector, dropping NULLs
    combined <- as.character(unlist(vals, use.names = FALSE))
    #if nothing is there, keep NULL; otherwise keep unique values
    out[[var]] <- if (length(combined)) unique(combined) else NULL
  }
  out
}

#safely pull a field by name with default
pull_field <- function(x, var, default = NULL) {
  if (!is.null(x[[var]])) x[[var]] else default
}

###procedure to extract data from stay and vacate docs ###
#define document parameters
doc_names <- coalesce(ocr_final$document, "")

doc_names_stay_vacate <- c("staying_writ",
                    "issued_stayed",
                    "stay_writ",
                    "stay_of_writ",
                    "_staying",
                    "staying_enforcement",
                    "order_to_stay",
                    "judgment_and_order_for_writ_of_restitution_and_stay",
                    "order_vacating_writ_of_restitution_default_and_order_of_dismissal_with_prejudice",
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

#exclude document names containing these strings (case-insensitive)
doc_names_excluded_stay_vacate <- c("answer",
                             "motion")

#inclusion mask (case-insensitive)
inc_mask <- reduce(
  doc_names_stay_vacate,
  .init = rep(FALSE, nrow(ocr_final)),
  .f = ~ .x | str_detect(doc_names, fixed(.y, ignore_case = TRUE))
)

#exclusion mask (case-insensitive)
exc_mask <- reduce(
  doc_names_excluded_stay_vacate,
  .init = rep(FALSE, nrow(ocr_final)),
  .f = ~ .x | str_detect(doc_names, fixed(.y, ignore_case = TRUE))
)

docs_to_run_stay_vacate <- ocr_final[inc_mask & !exc_mask, ]

#create a single tibble row for data to be pulled from stay and vacate docs
to_row_stay_vacate <- function(x) {
  document_file_date <- as.character(pull_field(x, "document_file_date", NA_character_))
  tenant_move <- as.character(pull_field(x, "tenant_move", NA_character_))
  case_number <- as.character(pull_field(x, "case_number", NA_character_))
  
  tibble(
    document_file_date = as.character(pull_field(x, "document_file_date", NA_character_)),
    tenant_move = as.character(pull_field(x, "tenant_move", NA_character_)),
    case_number = as.character(pull_field(x, "case_number", NA_character_))
  )
}

#function to extract data from stay and vacate docs using prompt_stay_vacate
parse_one <- function(txt) {
  outer <- data_from_llm(prompt_stay_vacate, txt)
  obj   <- tryCatch(fromJSON(outer, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(obj) || is.null(obj$choices) || length(obj$choices) < 1) return(NULL)
  
  raw <- obj$choices[[1]]$message$content
  raw <- trimws(clean_fences(raw))
  if (!validate(raw)) return(NULL)
  
  inner <- tryCatch(fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(inner)) return(NULL)
  
  inner <- merge_dupe_vars(inner)
  to_row_stay_vacate(inner)
}

#run a test on a small subset of OCR data
n_test <- 10
set.seed(264)
docs_test_stay_vacate <- slice_sample(docs_to_run_stay_vacate, n = min(n_test, nrow(docs_to_run_stay_vacate)))

llm_test_stay_vacate <- map2_dfr(
  docs_test_stay_vacate$text,
  seq_len(nrow(docs_test_stay_vacate)),
  ~{
    res <- parse_one(.x)
    if (is.null(res)) tibble()
    else bind_cols(
      tibble(row_id = .y,
             direc    = docs_test_stay_vacate$direc[.y],
             document = docs_test_stay_vacate$document[.y]),
      res
    )
  }
)

#create dataframe containing all data from stay and vacate documents, add source identifiers
llm_data_stay_vacate <- map2_dfr(
  docs_to_run_stay_vacate$text,
  seq_len(nrow(docs_to_run_stay_vacate)),
  ~{
    res <- tryCatch(
      parse_one(.x),
      error = function(e) {
        warning(paste("Error on row", .y, ":", e$message))
        NULL
      }
    )
    if (is.null(res)) tibble()
    else bind_cols(
      tibble(row_id = .y,
             direc    = docs_to_run_stay_vacate$direc[.y],
             document = docs_to_run_stay_vacate$document[.y]),
      res
    )
  }
)

#save data
saveRDS(llm_data_stay_vacate, "data/llm_data_stay_vacate.rds")