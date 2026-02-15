###this script uses data_from_llm.py to document case characteristics from dismissal documents in eviction cases
###data_from_llm.py uses chatgpt (with the prompt below) to answer questions about cases using relevant documents
###working directory should be the main folder of the rag-eviction-analysis repo

#load required packages
library(tidyverse)
library(reticulate)
library(jsonlite)

#load virtual environment and python
use_virtualenv("C:/dev/rag-eviction-analysis/.venv", required = TRUE)
py_config()
py <- import_builtins()
pickle <- import("pickle")

#define python file
source_python("scripts/data_from_llm.py")

#load ocr data
ocr_final <- read_rds("data/ocr_final.rds")

#define prompt
prompt_dismissal <- '
You are an assistant that reads dimissal documents filed in Washington State unlawful detainer (eviction) cases and extracts fields.

Return ONLY valid raw JSON and nothing else. No prose, no code fences.

Schema:
{
  "document_file_date": "string",
  "case_number": "string",
  "tenant_move": "Yes" | "No",
  "plaintiff_names": ["string"],
  "plaintiff_attorneys": ["string"],
  "defendant_names": ["string"],
  "defendant_attorneys": ["string"]
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

CASE NUMBER
- Extract the WA Superior Court case number matching regex: ^\\d{2}-\\d-\\d{5}-\\d$.
- If the digits appear with other separators (e.g., "23 2 04870 4"), normalize to "23-2-04870-4".
- If you cannot form exactly 9 digits in that pattern with high confidence, set "case_number": "".

TENANT MOVE
- Set to "Yes" if the document indicates that the defendant (tenant) will leave, vacate, move out of the property, if the document indicates that the tenant will stay, then "No".
- For example, an agreement to end the tenancy would be "Yes", while an agreement to continue the tenancy using rental assistance funds would be "No".
- DO NOT ASSUME. For example, if a document says that the plaintiff (landlord) will not drop a complaint against a tenant, do not assume that the tenant is required to move.

NAMES
- Provide full names only (no labels like "Plaintiff:"/"Defendant:"). Split multiple persons into separate array elements.
- Remove tokens like "aka", "dba" and keep the primary legal name.

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

###procedure to extract data from dismissal docs ###
#define document parameters
doc_names <- coalesce(ocr_final$document, "")

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

#exclude document names containing these strings (case-insensitive)
doc_names_dismissal_excluded <- c(
  "declaration"
)

#inclusion mask (case-insensitive)
inc_mask <- reduce(
  doc_names_dismissal,
  .init = rep(FALSE, nrow(ocr_final)),
  .f = ~ .x | str_detect(doc_names, fixed(.y, ignore_case = TRUE))
)

#exclusion mask (case-insensitive)
exc_mask <- reduce(
  doc_names_dismissal_excluded,
  .init = rep(FALSE, nrow(ocr_final)),
  .f = ~ .x | str_detect(doc_names, fixed(.y, ignore_case = TRUE))
)

docs_to_run_dismissal <- ocr_final[inc_mask & !exc_mask, ]

#create a single tibble row for data to be pulled from dismissal docs
to_row_dismissal <- function(x) {
  document_file_date <- as.character(pull_field(x, "document_file_date", NA_character_))
  case_number <- as.character(pull_field(x, "case_number", NA_character_))
  tenant_move <- as.character(pull_field(x, "tenant_move", NA_character_))
  
  #arrays (ensure character vector, even if scalar was returned)
  pn <- as.character(unlist(pull_field(x, "plaintiff_names", character()), use.names = FALSE))
  pa <- as.character(unlist(pull_field(x, "plaintiff_attorneys", character()), use.names = FALSE))
  dn <- as.character(unlist(pull_field(x, "defendant_names", character()), use.names = FALSE))
  da <- as.character(unlist(pull_field(x, "defendant_attorneys", character()), use.names = FALSE))
  
  tibble(
    document_file_date = as.character(pull_field(x, "document_file_date", NA_character_)),
    case_number = as.character(pull_field(x, "case_number", NA_character_)),
    tenant_move = as.character(pull_field(x, "tenant_move", NA_character_)),
    plaintiff_names     = list(pn),
    plaintiff_attorneys = list(pa),
    defendant_names     = list(dn),
    defendant_attorneys     = list(da)
  )
}

#function to extract data from dismissal docs using prompt_dismissal
parse_one <- function(txt) {
  outer <- data_from_llm(prompt_dismissal, txt)
  obj   <- tryCatch(fromJSON(outer, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(obj) || is.null(obj$choices) || length(obj$choices) < 1) return(NULL)
  
  raw <- obj$choices[[1]]$message$content
  raw <- trimws(clean_fences(raw))
  if (!validate(raw)) return(NULL)
  
  inner <- tryCatch(fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(inner)) return(NULL)
  
  inner <- merge_dupe_vars(inner)
  to_row_dismissal(inner)
}

#run a test on a small subset of OCR data
n_test <- 150
set.seed(264)
docs_test_dismissal <- slice_sample(docs_to_run_dismissal, n = min(n_test, nrow(docs_to_run_dismissal)))

llm_test_dismissal <- map2_dfr(
  docs_test_dismissal$text,
  seq_len(nrow(docs_test_dismissal)),
  ~{
    res <- parse_one(.x)
    if (is.null(res)) tibble()
    else bind_cols(
      tibble(row_id = .y,
             direc    = docs_test_dismissal$direc[.y],
             document = docs_test_dismissal$document[.y]),
      res
    )
  }
)

#create dataframe containing all data from dismissal documents, add source identifiers
llm_data_dismissal <- map2_dfr(
  docs_to_run_dismissal$text,
  seq_len(nrow(docs_to_run_dismissal)),
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
             direc    = docs_to_run_dismissal$direc[.y],
             document = docs_to_run_dismissal$document[.y]),
      res
    )
  }
)

#save data
saveRDS(llm_data_dismissal, "data/llm_data_dismissal.rds")