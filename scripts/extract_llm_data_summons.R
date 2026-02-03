###this script uses data_from_llm.py to document case characteristics from a variety of summons documents in eviction cases
###data_from_llm.py uses chatgpt (with the prompt below) to answer questions about cases using summons documents
###working directory should be the main folder of the eviction-data repo

#load required packages
library(tidyverse)
library(reticulate)
library(jsonlite)

#load virtual environment and python
use_virtualenv("/Users/wvg1/Documents/eviction-data/.venv", required = TRUE)
py_config()
py <- import_builtins()
pickle <- import("pickle")

#define python file
source_python("scripts/data_from_llm.py")

#load ocr data
ocr_final <- read_rds("data/ocr_final.rds")

#define prompt
prompt_summons <- '
You are an assistant that reads Washington State unlawful-detainer (eviction) summons documents and extracts fields.

Return ONLY valid raw JSON and nothing else. No prose, no code fences.

Schema:
{
  "case_number": "string",
  "summons_file_date": "string",
  "response_deadline": "string",
  "address": "string",
  "address_street": "string",
  "address_unit": "string",
  "address_city": "string",
  "address_state": "string",
  "address_zip": "string",
  "case_type": "residential" | "commercial",
  "plaintiff_names": ["string"],
  "plaintiff_attorneys": ["string"],
  "defendant_names": ["string"],
  "confidence": {
    "case_number": 0.0,
    "summons_file_date": 0.0,
    "response_deadline": 0.0,
    "address": 0.0,
    "address_street": 0.0,
    "address_unit": 0.0,
    "address_city": 0.0,
    "address_state": 0.0,
    "address_zip": 0.0,
    "case_type": 0.0,
    "plaintiff_names": 0.0,
    "plaintiff_attorneys": 0.0,
    "defendant_names": 0.0
  }
}

CONFIDENCE SCORES
- For each field, provide a confidence score from 0.0 to 1.0.
- 1.0 = highly confident (explicit, clear evidence in document)
- 0.7-0.9 = moderately confident (clear but with minor ambiguity)
- 0.4-0.6 = low-moderate confidence (some evidence but uncertain interpretation)
- 0.0-0.3 = low confidence (little evidence, mostly guessing, or field is empty)
- Empty fields should generally have confidence 0.0 unless there is explicit evidence of absence.

STRICT RULES
- If a STRING field is unknown, set it to "" (empty string). If an ARRAY field is unknown, set it to [].
- Do NOT guess. Prefer "" to an invented value.
- Use only ASCII. Trim leading/trailing space. Collapse internal whitespace to single spaces.
- No newlines in any field. No trailing commas in JSON.

CASE NUMBER
- Extract the WA Superior Court case number matching regex: ^\\d{2}-\\d-\\d{5}-\\d$.
- If the digits appear with other separators (e.g., "23 2 04870 4"), normalize to "23-2-04870-4".
- If you cannot form exactly 9 digits in that pattern with high confidence, set "case_number": "".

SUMMONS FILE DATE
- Set "summons_file_date" based on when the document was filed in the court, rather than when it was signed.
- Search priority (stop at the first match that fits):
1) A clerk/e-filing stamp or header with words like "FILED", "E-FILED", "ACCEPTED", "ENTERED", "RECEIVED", "SUBMITTED", near a date/time (often on page 1, top-right).
2) A docket/header watermark showing a file/entry date.

- If only a 2-digit year is present, assume 2000–2099 (e.g., 9/5/24 → 2024-09-05).
- Normalize "summons_file_date" to ISO "YYYY-MM-DD".
- Accept common variants (e.g., "9/5/24", "09/05/2024", "September 5, 2024") and normalize to "YYYY-MM-DD".

RESPONSE DEADLINE
- Set "response_deadline" as the date by which a response is due. Look for phrases like "Your written response must be received by:..." and "The deadline for your written response is..."
- If only a 2-digit year is present, assume 2000–2099 (e.g., 9/5/24 → 2024-09-05).
- Normalize "summons_file_date" to ISO "YYYY-MM-DD".
- Accept common variants (e.g., "9/5/24", "09/05/2024", "September 5, 2024") and normalize to "YYYY-MM-DD".
- Do NOT guess. Prefer "" to an invented value.

ADDRESS (premises for the tenancy/defendant)
- Choose the property/premises address for the tenancy/defendant (NOT the court, NOT the plaintiff firm).
- Prefer text labeled "Premises", "Property Address", "Address for service", or near "Defendant".
- Fill components:
  - "address_street": house number + street name + suffix (e.g., "12517 47th Ave SW"). Do NOT include unit here.
  - "address_unit": unit/suite/apt/space/lot (e.g., "Apt 3", "Unit 5", "Space #21", "Lot 12"). If none, "".
  - "address_city": city name (e.g., "Lakewood"). Use title case.
  - "address_state": always "WA" for Washington. If state is not WA, leave ALL address fields as "".
  - "address_zip": 5-digit ZIP only (e.g., "98499"). If missing, leave "".
- Compose "address" exactly as:
  - If address_unit != "": "address_street address_unit, address_city, WA address_zip"
  - Else: "address_street, address_city, WA address_zip"
- If only partial address is available (e.g., missing ZIP), populate known fields instead of returning all "".
- Do NOT return PO Boxes unless they are the ONLY address and explicitly the premises; otherwise prefer premises or return "".

NAMES
- Provide full names only (no labels like "Plaintiff:"/"Defendant:"). Split multiple persons into separate array elements.
- Remove tokens like "aka", "dba" and keep the primary legal name.

CASE TYPE
- "commercial" only if clearly indicated as commercial/non-residential; otherwise "residential".

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

###procedure to extract data from summonses ###
#define document parameters
doc_names <- coalesce(ocr_final$document, "")

doc_names_summons <- c(
  "summons"
)

#exclude document names containing these strings (case-insensitive)
doc_names_summons_excluded <- c(
  "declaration",
  "mailing",
  "service",
  "stricken",
  "posting",
  "motion"

)

#inclusion mask (case-insensitive)
inc_mask <- reduce(
  doc_names_summons,
  .init = rep(FALSE, nrow(ocr_final)),
  .f = ~ .x | str_detect(doc_names, fixed(.y, ignore_case = TRUE))
)

#exclusion mask (case-insensitive)
exc_mask <- reduce(
  doc_names_summons_excluded,
  .init = rep(FALSE, nrow(ocr_final)),
  .f = ~ .x | str_detect(doc_names, fixed(.y, ignore_case = TRUE))
)

docs_to_run_summons <- ocr_final[inc_mask & !exc_mask, ]

#create a single tibble row for data to be pulled from summonses
to_row_summons <- function(x) {
  #scalars
  case_number <- as.character(pull_field(x, "case_number", NA_character_))
  summons_file_date <- as.character(pull_field(x, "summons_file_date", NA_character_))
  response_deadline <- as.character(pull_field(x, "response_deadline", NA_character_))
  address     <- as.character(pull_field(x, "address", NA_character_))
  address_street <- as.character(pull_field(x, "address_street", NA_character_))
  address_unit   <- as.character(pull_field(x, "address_unit", NA_character_))
  address_city   <- as.character(pull_field(x, "address_city", NA_character_))
  address_state  <- as.character(pull_field(x, "address_state", NA_character_))
  address_zip   <- as.character(pull_field(x, "address_zip", NA_character_))
  case_type   <- as.character(pull_field(x, "case_type", NA_character_))
  
  #arrays (ensure character vector, even if scalar was returned)
  pn <- as.character(unlist(pull_field(x, "plaintiff_names", character()), use.names = FALSE))
  pa <- as.character(unlist(pull_field(x, "plaintiff_attorneys", character()), use.names = FALSE))
  dn <- as.character(unlist(pull_field(x, "defendant_names", character()), use.names = FALSE))
  
  #confidence scores
  conf <- pull_field(x, "confidence", list())
  conf_case_num <- as.numeric(pull_field(conf, "case_number", 0.0))
  conf_summons_file_date <- as.numeric(pull_field(conf, "summons_file_date", 0.0))
  conf_response_deadline <- as.numeric(pull_field(conf, "response_deadline", 0.0))
  conf_address <- as.numeric(pull_field(conf, "address", 0.0))
  conf_address_street <- as.numeric(pull_field(conf, "address_street", 0.0))
  conf_address_unit <- as.numeric(pull_field(conf, "address_unit", 0.0))
  conf_address_city <- as.numeric(pull_field(conf, "address_city", 0.0))
  conf_address_state <- as.numeric(pull_field(conf, "address_state", 0.0))
  conf_address_zip <- as.numeric(pull_field(conf, "address_zip", 0.0))
  conf_case_type <- as.numeric(pull_field(conf, "case_type", 0.0))
  conf_pn <- as.numeric(pull_field(conf, "plaintiff_names", 0.0))
  conf_pa <- as.numeric(pull_field(conf, "plaintiff_attorneys", 0.0))
  conf_dn <- as.numeric(pull_field(conf, "defendant_names", 0.0))
  
  tibble(
    case_number = case_number,
    summons_file_date = summons_file_date,
    response_deadline = response_deadline,
    address = address,
    address_street = address_street,
    address_unit = address_unit,
    address_city = address_city,
    address_state = address_state,
    address_zip = address_zip,
    case_type = case_type,
    plaintiff_names = list(pn),
    plaintiff_attorneys = list(pa),
    defendant_names = list(dn),
    conf_case_number = conf_case_num,
    conf_summons_file_date = conf_summons_file_date,
    conf_response_deadline = conf_response_deadline,
    conf_address = conf_address,
    conf_address_street = conf_address_street,
    conf_address_unit = conf_address_unit,
    conf_address_city = conf_address_city,
    conf_address_state = conf_address_state,
    conf_address_zip = conf_address_zip,
    conf_case_type = conf_case_type,
    conf_plaintiff_names = conf_pn,
    conf_plaintiff_attorneys = conf_pa,
    conf_defendant_names = conf_dn
  )
}

#function to extract data from summons using prompt_summons
parse_one <- function(txt) {
  outer <- data_from_llm(prompt_summons, txt)
  obj   <- tryCatch(fromJSON(outer, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(obj) || is.null(obj$choices) || length(obj$choices) < 1) return(NULL)
  
  raw <- obj$choices[[1]]$message$content
  raw <- trimws(clean_fences(raw))
  if (!validate(raw)) return(NULL)
  
  # parse inner JSON to a *list* (not auto-simplified), then merge dupes
  inner <- tryCatch(fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(inner)) return(NULL)
  
  inner <- merge_dupe_vars(inner)
  to_row_summons(inner)
}

### run a test on a small subset of OCR data ###
n_test <- 800
set.seed(7555)
docs_test_summons <- slice_sample(docs_to_run_summons, n = min(n_test, nrow(docs_to_run_summons)))

llm_test_summons <- map2_dfr(
  docs_test_summons$text,
  seq_len(nrow(docs_test_summons)),
  ~{
    res <- parse_one(.x)
    if (is.null(res)) tibble()
    else bind_cols(
      tibble(row_id = .y,
             direc    = docs_test_summons$direc[.y],
             document = docs_test_summons$document[.y]),
      res
    )
  }
)

#create dataframe containing all data from summons documents, add source identifiers
llm_data_summons <- map2_dfr(
  docs_to_run_summons$text,
  seq_len(nrow(docs_to_run_summons)),
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
             direc    = docs_to_run_summons$direc[.y],
             document = docs_to_run_summons$document[.y]),
      res
    )
  }
)

#save data
saveRDS(llm_data_summons, "data/llm_data_summons.rds")