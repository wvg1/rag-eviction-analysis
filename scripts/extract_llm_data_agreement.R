### this script uses data_from_llm.py to document case characteristics from documents relating to agreements in eviction cases
### data_from_llm.py uses chatgpt (with the prompt below) to answer questions about cases using relevant documents
### working directory should be the main folder of the rag-eviction-analysis repo
### unlike the other extraction scripts in this repo, this script also extracts the reasoning for each response if desired for future use

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

#define prompt with confidence scores and reasoning
prompt_agreement <- '
You are an assistant that reads documents filed in Washington State unlawful detainer (eviction) cases and extracts fields.

Return ONLY valid raw JSON and nothing else. No prose, no code fences.

Schema:
{
  "document_file_date": "string",
  "case_number": "string",
  "agreement": "Yes" | "No",
  "tenant_move": "Yes" | "No",
  "plaintiff_names": ["string"],
  "plaintiff_attorneys": ["string"],
  "defendant_names": ["string"],
  "defendant_attorneys": ["string"],
  "reasoning": {
    "document_file_date": "string",
    "case_number": "string",
    "agreement": "string",
    "tenant_move": "string",
    "plaintiff_names": ["string"],
    "plaintiff_attorneys": ["string"],
    "defendant_names": ["string"],
    "defendant_attorneys": ["string"]
  },
  "confidence": {
    "document_file_date": 0.0,
    "case_number": 0.0,
    "agreement": 0.0,
    "tenant_move": 0.0,
    "plaintiff_names": 0.0,
    "plaintiff_attorneys": 0.0,
    "defendant_names": 0.0,
    "defendant_attorneys": 0.0
  }
}

REASONING
- For scalar fields (document_file_date, case_number, agreement, tenant_move), provide a brief explanation (1-2 sentences) of what evidence led to your conclusion.
- For array fields (plaintiff_names, plaintiff_attorneys, defendant_names, defendant_attorneys), provide one reasoning entry per extracted item, in the same order as the array.
- If a field is empty, set reasoning to empty string "".
- Quote relevant document language when helpful (under 30 words).

CONFIDENCE SCORES
- For each field, provide a confidence score from 0.0 to 1.0.
- 1.0 = highly confident (explicit, clear evidence in document)
- 0.7-0.9 = moderately confident (clear but with minor ambiguity)
- 0.4-0.6 = low-moderate confidence (some evidence but uncertain interpretation)
- 0.0-0.3 = low confidence (little evidence, mostly guessing, or field is empty)
- Empty fields should generally have confidence 0.0 unless there is explicit evidence of absence.

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
- Ignore signature dates, notary acknowledgments, certificate of service/mailing dates, "DATED this …" lines, and any dates inside the narrative body.
- If only a 2-digit year is present, assume 2000–2099 (e.g., 9/5/24 → 2024-09-05).
- Normalize "document_file_date" to ISO "YYYY-MM-DD".
- Accept common variants (e.g., "9/5/24", "09/05/2024", "September 5, 2024") and normalize to "YYYY-MM-DD".

CASE NUMBER
- Extract the WA Superior Court case number matching regex: ^\\d{2}-\\d-\\d{5}-\\d$.
- If the digits appear with other separators (e.g., "23 2 04870 4"), normalize to "23-2-04870-4".
- If you cannot form exactly 9 digits in that pattern with high confidence, set "case_number": "".

AGREEMENT
- "Yes" if the document describes an agreement between the plaintiff and defendant sides in the case, otherwise "No".
- For the purposes of this field, leases and rental agreements made prior to eviction cases should result in "No".

TENANT MOVE
- "Yes" if the agreement in the document requires the defendant (tenant) to leave, vacate, move out of the property, otherwise "No".
- For example, an agreement to end the tenancy would be "Yes" while an agreement to continue the tenancy would be "No".

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

###procedure to extract data from agreement docs ###
#define document parameters
doc_names <- coalesce(ocr_final$document, "")

doc_names_agreement <- c(
  "cr2a",
  "agreement"
)

#exclude document names containing these strings (case-insensitive)
doc_names_agreement_excluded <- c(
  "motion",
  "lease",
  "rental",
  "declaration"
)

#inclusion mask (case-insensitive)
inc_mask <- reduce(
  doc_names_agreement,
  .init = rep(FALSE, nrow(ocr_final)),
  .f = ~ .x | str_detect(doc_names, fixed(.y, ignore_case = TRUE))
)

#exclusion mask (case-insensitive)
exc_mask <- reduce(
  doc_names_agreement_excluded,
  .init = rep(FALSE, nrow(ocr_final)),
  .f = ~ .x | str_detect(doc_names, fixed(.y, ignore_case = TRUE))
)

docs_to_run_agreement <- ocr_final[inc_mask & !exc_mask, ]

#create a single tibble row for data to be pulled from agreement docs
to_row_agreement <- function(x) {
  #scalars
  document_file_date <- as.character(pull_field(x, "document_file_date", NA_character_))
  case_number <- as.character(pull_field(x, "case_number", NA_character_))
  agreement <- as.character(pull_field(x, "agreement", NA_character_))
  tenant_move <- as.character(pull_field(x, "tenant_move", NA_character_))
  
  #arrays (ensure character vector, even if scalar was returned)
  pn <- as.character(unlist(pull_field(x, "plaintiff_names", character()), use.names = FALSE))
  pa <- as.character(unlist(pull_field(x, "plaintiff_attorneys", character()), use.names = FALSE))
  dn <- as.character(unlist(pull_field(x, "defendant_names", character()), use.names = FALSE))
  da <- as.character(unlist(pull_field(x, "defendant_attorneys", character()), use.names = FALSE))
  
  #reasoning - scalars
  reasoning <- pull_field(x, "reasoning", list())
  reason_doc_date <- as.character(pull_field(reasoning, "document_file_date", ""))
  reason_case_num <- as.character(pull_field(reasoning, "case_number", ""))
  reason_agreement <- as.character(pull_field(reasoning, "agreement", ""))
  reason_tenant_move <- as.character(pull_field(reasoning, "tenant_move", ""))
  
  #reasoning - arrays (convert to character vectors, matching the extracted values)
  reason_pn <- as.character(unlist(pull_field(reasoning, "plaintiff_names", character()), use.names = FALSE))
  reason_pa <- as.character(unlist(pull_field(reasoning, "plaintiff_attorneys", character()), use.names = FALSE))
  reason_dn <- as.character(unlist(pull_field(reasoning, "defendant_names", character()), use.names = FALSE))
  reason_da <- as.character(unlist(pull_field(reasoning, "defendant_attorneys", character()), use.names = FALSE))
  
  #confidence scores
  conf <- pull_field(x, "confidence", list())
  conf_doc_date <- as.numeric(pull_field(conf, "document_file_date", 0.0))
  conf_case_num <- as.numeric(pull_field(conf, "case_number", 0.0))
  conf_agreement <- as.numeric(pull_field(conf, "agreement", 0.0))
  conf_tenant_move <- as.numeric(pull_field(conf, "tenant_move", 0.0))
  conf_pn <- as.numeric(pull_field(conf, "plaintiff_names", 0.0))
  conf_pa <- as.numeric(pull_field(conf, "plaintiff_attorneys", 0.0))
  conf_dn <- as.numeric(pull_field(conf, "defendant_names", 0.0))
  conf_da <- as.numeric(pull_field(conf, "defendant_attorneys", 0.0))
  
  tibble(
    document_file_date = document_file_date,
    case_number = case_number,
    agreement = agreement,
    tenant_move = tenant_move,
    plaintiff_names = list(pn),
    plaintiff_attorneys = list(pa),
    defendant_names = list(dn),
    defendant_attorneys = list(da),
    reason_document_file_date = reason_doc_date,
    reason_case_number = reason_case_num,
    reason_agreement = reason_agreement,
    reason_tenant_move = reason_tenant_move,
    reason_plaintiff_names = list(reason_pn),
    reason_plaintiff_attorneys = list(reason_pa),
    reason_defendant_names = list(reason_dn),
    reason_defendant_attorneys = list(reason_da),
    conf_document_file_date = conf_doc_date,
    conf_case_number = conf_case_num,
    conf_agreement = conf_agreement,
    conf_tenant_move = conf_tenant_move,
    conf_plaintiff_names = conf_pn,
    conf_plaintiff_attorneys = conf_pa,
    conf_defendant_names = conf_dn,
    conf_defendant_attorneys = conf_da
  )
}

#function to extract data from agreement docs using prompt_agreement
parse_one <- function(txt) {
  outer <- data_from_llm(prompt_agreement, txt)
  obj   <- tryCatch(fromJSON(outer, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(obj) || is.null(obj$choices) || length(obj$choices) < 1) return(NULL)
  
  raw <- obj$choices[[1]]$message$content
  raw <- trimws(clean_fences(raw))
  if (!validate(raw)) return(NULL)
  
  inner <- tryCatch(fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(inner)) return(NULL)
  
  inner <- merge_dupe_vars(inner)
  to_row_agreement(inner)
}

#run a test on a small subset of OCR data
n_test <- 15
set.seed(2666)
docs_test_agreement <- slice_sample(docs_to_run_agreement, n = min(n_test, nrow(docs_to_run_agreement)))

llm_test_agreement <- map2_dfr(
  docs_test_agreement$text,
  seq_len(nrow(docs_test_agreement)),
  ~{
    res <- parse_one(.x)
    if (is.null(res)) tibble()
    else bind_cols(
      tibble(row_id = .y,
             direc    = docs_test_agreement$direc[.y],
             document = docs_test_agreement$document[.y]),
      res
    )
  }
)

#create dataframe containing all data from agreement docs, add source identifiers
llm_data_agreement <- map2_dfr(
  docs_to_run_agreement$text,
  seq_len(nrow(docs_to_run_agreement)),
  ~{
    res <- parse_one(.x)
    if (is.null(res)) tibble()
    else bind_cols(
      tibble(row_id = .y,
             direc    = docs_to_run_agreement$direc[.y],
             document = docs_to_run_agreement$document[.y]),
      res
    )
  }
)

saveRDS(llm_data_agreement, "data/llm_data_agreement.rds")