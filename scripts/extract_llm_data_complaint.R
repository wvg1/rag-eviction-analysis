###this script uses data_from_llm.py to document case characteristics from a variety of complaint documents in eviction cases
###data_from_llm.py uses chatgpt (with the prompt below) to answer questions about cases using complaint documents
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

#read in OCR data
ocr_final <- read_rds("data/ocr_final.rds")

#define prompt
prompt_complaint <- 'You are an assistant that reads Washington State unlawful detainer (eviction) complaint documents and extracts fields.

Return ONLY valid raw JSON and nothing else. No prose, no code fences.

Schema:
  {
    "case_number": "string",
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
    "amount_owed": "string",
    "monthly_rent": "string",
    "confidence": {
      "case_number": 0.0,
      "address": 0.0,
      "address_street": 0.0,
      "address_unit": 0.0,
      "address_city": 0.0,
      "address_state": 0.0,
      "address_zip": 0.0,
      "case_type": 0.0,
      "plaintiff_names": 0.0,
      "plaintiff_attorneys": 0.0,
      "defendant_names": 0.0,
      "amount_owed": 0.0,
      "monthly_rent": 0.0
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
- Use only ASCII. Trim leading/trailing spaces. Collapse internal whitespace to single spaces.
- No newlines in any field. No trailing commas in JSON.

CASE NUMBER
- Extract the WA Superior Court case number matching regex: ^\\d{2}-\\d-\\d{5}-\\d$.
- If the digits appear with other separators (e.g., "23 2 04870 4"), normalize to "23-2-04870-4".
- If you cannot form exactly 9 digits in that pattern with high confidence, set "case_number": "".

ADDRESS (premises for the tenancy/defendant)
- Choose the property/premises address for the tenancy/defendant (NOT the court, NOT the plaintiff firm).
- Prefer text labeled "Premises", "Property Address", "Address for service", or near "Defendant".
- Fill components:
- "address_street": house number + street name + suffix (e.g., "12517 47th Ave SW"). Do NOT include unit here.
- "address_unit": unit/suite/apt/space/lot (e.g., "Apt 3", "Unit 5", "Space #21", "Lot 12"). If none, "".
- "address_city": city name (e.g., "Lakewood"). Use title case.
- "address_state": always "WA" for Washington.
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

AMOUNT OWED
- Set this as the amount owed to the plaintiff by the defendant (past due), as alleged in the complaint. Do not include attorney fees.
- Include utility fees, pet rent, and other past due fees other than attorneys fees.
- Do not assume that there is a monetary complaint. If no monetary complaint is alleged, return 0.
- If the document includes a notice to pay and vacate AND a monetary complaint, set this as the amount alleged on the later date.
- If more than one relevant amount is included, return the sum of relevant amounts. ONLY INCLUDE ONE DOLLAR AMOUNT PER DOCUMENT.

MONTHLY RENT
- Set this as the monthly rent according to the lease or tenancy agreement referenced in the complaint.
- For subsidized units or units not subject to a traditional lease, set this as the amount that the tenant is responsible for paying.
- Do NOT guess or infer. Only extract if explicitly stated in the document.
- Prefer "" (empty string) to an inaccurate answer.
- Return only a single dollar amount per document.

Return only JSON objects following this schema.
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

###procedure to extract data from complaints ###
#define document parameters (complaint)
docs_to_run_complaint <- ocr_final[grepl("complaint", ocr_final$document, ignore.case = TRUE),]

#create a single tibble row for data to be pulled from complaints
to_row_complaint <- function(x) {
  #scalars
  case_number <- as.character(pull_field(x, "case_number", NA_character_))
  address     <- as.character(pull_field(x, "address", NA_character_))
  address_street <- as.character(pull_field(x, "address_street", NA_character_))
  address_unit   <- as.character(pull_field(x, "address_unit", NA_character_))
  address_city   <- as.character(pull_field(x, "address_city", NA_character_))
  address_state  <- as.character(pull_field(x, "address_state", NA_character_))
  address_zip   <- as.character(pull_field(x, "address_zip", NA_character_))
  case_type   <- as.character(pull_field(x, "case_type", NA_character_))
  amount_owed   <- as.character(pull_field(x, "amount_owed", NA_character_))
  monthly_rent <- as.character(pull_field(x, "monthly_rent", NA_character_))
  
  #arrays (ensure character vector, even if scalar was returned)
  pn <- as.character(unlist(pull_field(x, "plaintiff_names", character()), use.names = FALSE))
  pa <- as.character(unlist(pull_field(x, "plaintiff_attorneys", character()), use.names = FALSE))
  dn <- as.character(unlist(pull_field(x, "defendant_names", character()), use.names = FALSE))
  
  #confidence scores
  conf <- pull_field(x, "confidence", list())
  conf_case_num <- as.numeric(pull_field(conf, "case_number", 0.0))
  conf_address <- as.numeric(pull_field(conf, "address", 0.0))
  conf_address_street <- as.numeric(pull_field(conf, "address_street", 0.0))
  conf_address_unit <- as.numeric(pull_field(conf, "address_unit", 0.0))
  conf_address_city <- as.numeric(pull_field(conf, "address_city", 0.0))
  conf_address_state <- as.numeric(pull_field(conf, "address_state", 0.0))
  conf_address_zip <- as.numeric(pull_field(conf, "address_zip", 0.0))
  conf_case_type <- as.numeric(pull_field(conf, "case_type", 0.0))
  conf_amount_owed <- as.numeric(pull_field(conf, "amount_owed", 0.0))
  conf_monthly_rent <- as.numeric(pull_field(conf, "monthly_rent", 0.0))
  conf_pn <- as.numeric(pull_field(conf, "plaintiff_names", 0.0))
  conf_pa <- as.numeric(pull_field(conf, "plaintiff_attorneys", 0.0))
  conf_dn <- as.numeric(pull_field(conf, "defendant_names", 0.0))
  
  tibble(
    case_number = case_number,
    address = address,
    address_street = address_street,
    address_unit = address_unit,
    address_city = address_city,
    address_state = address_state,
    address_zip = address_zip,
    case_type = case_type,
    amount_owed = amount_owed,
    monthly_rent = monthly_rent,
    plaintiff_names = list(pn),
    plaintiff_attorneys = list(pa),
    defendant_names = list(dn),
    conf_case_number = conf_case_num,
    conf_address = conf_address,
    conf_address_street = conf_address_street,
    conf_address_unit = conf_address_unit,
    conf_address_city = conf_address_city,
    conf_address_state = conf_address_state,
    conf_address_zip = conf_address_zip,
    conf_case_type = conf_case_type,
    conf_amount_owed = conf_amount_owed,
    conf_monthly_rent = conf_monthly_rent,
    conf_plaintiff_names = conf_pn,
    conf_plaintiff_attorneys = conf_pa,
    conf_defendant_names = conf_dn,
  )
}

#function to extract data from complaints using prompt_complaint
parse_one <- function(txt, doc_id = NA) {
  #add error handling wrapper
  result <- tryCatch({
    outer <- data_from_llm(prompt_complaint, txt)
    obj   <- tryCatch(fromJSON(outer, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(obj) || is.null(obj$choices) || length(obj$choices) < 1) return(NULL)
    
    raw <- obj$choices[[1]]$message$content
    raw <- trimws(clean_fences(raw))
    if (!validate(raw)) return(NULL)
    
    #parse inner JSON to a *list* (not auto-simplified), then merge dupes
    inner <- tryCatch(fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(inner)) return(NULL)
    
    inner <- merge_dupe_vars(inner)
    to_row_complaint(inner)
  }, error = function(e) {
    #handle any errors from the API or parsing
    message(sprintf("Error processing document %s (chars: %d): %s", 
                    doc_id, nchar(txt), conditionMessage(e)))
    return(NULL)
  })
  
  return(result)
}

### run a test on a small subset of OCR data ###
n_test <- 10
set.seed(79)
docs_test_complaint <- slice_sample(docs_to_run_complaint, n = min(n_test, nrow(docs_to_run_complaint)))

llm_test_complaint <- map2_dfr(
  docs_test_complaint$text,
  seq_len(nrow(docs_test_complaint)),
  ~{
    res <- parse_one(.x, doc_id = .y)  #pass row ID
    if (is.null(res)) tibble()
    else bind_cols(
      tibble(row_id = .y,
             direc    = docs_test_complaint$direc[.y],
             document = docs_test_complaint$document[.y]),
      res
    )
  }
)

#identify failed documents in test
failed_test_docs <- docs_test_complaint %>%
  mutate(row_id = row_number()) %>%
  filter(!row_id %in% llm_test_complaint$row_id) %>%
  select(row_id, direc, document, text_length = text) %>%
  mutate(text_length = nchar(text_length))

#test summary
message(sprintf("Successfully processed: %d/%d documents", nrow(llm_test_complaint), n_test))
message(sprintf("Failed: %d/%d documents", nrow(failed_test_docs), n_test))

if (nrow(failed_test_docs) > 0) {
  message("\nFailed documents:")
  print(failed_test_docs %>% select(row_id, document, text_length))
}

#create dataframe containing all data from complaint documents, add source identifiers
llm_data_complaint <- map2_dfr(
  docs_to_run_complaint$text,
  seq_len(nrow(docs_to_run_complaint)),
  ~{
    res <- parse_one(.x, doc_id = .y)  #pass row ID
    if (is.null(res)) tibble()
    else bind_cols(
      tibble(row_id = .y,
             direc    = docs_to_run_complaint$direc[.y],
             document = docs_to_run_complaint$document[.y]),
      res
    )
  }
)

#identify failed documents
failed_docs <- docs_to_run_complaint %>%
  mutate(row_id = row_number()) %>%
  filter(!row_id %in% llm_data_complaint$row_id) %>%
  select(row_id, direc, document, text_length = text) %>%
  mutate(text_length = nchar(text_length))

#save both successful and failed
saveRDS(llm_data_complaint, "data/llm_data_complaint.rds")
saveRDS(failed_docs, "data/llm_data_complaint_failed.rds")

#summarize
message(sprintf("Successfully processed: %d documents", nrow(llm_data_complaint)))
message(sprintf("Failed: %d documents", nrow(failed_docs)))