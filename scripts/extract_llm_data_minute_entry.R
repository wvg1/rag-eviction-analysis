###this script uses data_from_llm.py to document case outcomes from a variety of hearing minute documents in eviction cases
###data_from_llm.py uses chatgpt (with the prompt below) to answer questions about hearings using case documents
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
prompt_minute_entry <- 'You are an assistant that reads Washington State unlawful detainer (eviction) hearing minutes and extracts fields.

Return ONLY valid raw JSON and nothing else. No prose, no code fences.

Schema:
{
  "hearing_date": "string",
  "case_number": "string",
  "plaintiff_names": ["string"],
  "plaintiff_attorneys": ["string"],
  "plaintiffs_at_hearing": ["string"],
  "plaintiff_attorneys_at_hearing": ["string"],
  "defendant_names": ["string"],
  "defendant_attorneys": ["string"],
  "defendants_at_hearing": ["string"],
  "defendant_attorneys_at_hearing": ["string"],
  "confidence": {
    "hearing_date": 0.0,
    "case_number": 0.0,
    "plaintiff_names": 0.0,
    "plaintiff_attorneys": 0.0,
    "plaintiffs_at_hearing": 0.0,
    "plaintiff_attorneys_at_hearing": 0.0,
    "defendant_names": 0.0,
    "defendant_attorneys": 0.0,
    "defendants_at_hearing": 0.0,
    "defendant_attorneys_at_hearing": 0.0
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
- If a STRING field is unknown, set it to "" (empty string).
- Do NOT guess. Prefer "" to an invented value.
- Use only ASCII. Trim leading/trailing spaces; collapse internal whitespace to single spaces.
- No newlines in any field. No trailing commas in JSON.

HEARING DATE
- Normalize "hearing_date" to ISO format "YYYY-MM-DD".
- Accept common variants in source text (e.g., "9/5/24", "09/05/2024", "September 5, 2024") and normalize to "YYYY-MM-DD".
- If both a hearing date and a clerk "Entered"/"Filed" date appear, set "hearing_date" = the date the hearing actually occurred or was scheduled for.
- When multiple dates appear, choose in this priority:
  1) Date explicitly labeled "Hearing Date", "Date of Hearing", "On [date], the Court...", "In open court on [date]".
  2) Date near hearing header lines (e.g., document title block, caption block) clearly tied to the case hearing.
3) Calendar references like "[9:00 AM] UD Calendar on [date]".
- Ignore file system timestamps, footer print dates, or e-filing metadata unless they are explicitly the hearing date.
- If the year is missing but month/day are present and a nearby 4-digit year exists in the document header/caption, use that year. Otherwise return "".

CASE NUMBER
- Extract the WA Superior Court case number matching regex: ^\\d{2}-\\d-\\d{5}-\\d$.
- If the digits appear with other separators (e.g., "23 2 04870 4"), normalize to "23-2-04870-4".
- If you cannot form exactly 9 digits in that pattern with high confidence, set "case_number": "".

NAMES
- Normalize all personal names from LAST, FIRST MI → First MI Last. Collapse internal whitespace to single spaces.
- Preserve hyphenated and multi-word last names (e.g., "De La Cruz", "Smith-Johnson").
- Provide full names only (no labels like "Plaintiff:"/"Defendant:"). Split multiple persons into separate array elements.
- Remove tokens like "aka", "dba" and keep the primary legal name.
- If a placeholder like "ALL OCCUPANTS" appears, include it as its own array element exactly as text (ASCII, uppercase).
- If a name is truly unreadable or missing, return "" and do not invent names.

ATTENDANCE / APPEARANCES
- Arrays ending with "_at_hearing" must reflect who was present at THIS hearing per the minutes (e.g., "present", "appeared", "by phone", "by Zoom").
- If attendance is ambiguous, prefer "" or [].
- Attendance arrays (defendants_at_hearing, plaintiffs_at_hearing) must be subsets of the corresponding *_names arrays, limited to those explicitly marked as present/appearing (e.g., "present", "appeared", "by phone/Zoom"). Do not omit absent parties from *_names.

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

###procedure to extract data from minute entry docs ###
#define document parameters (minute entry)
docs_to_run_minute_entry <- ocr_final[grepl("minute", ocr_final$document, ignore.case = TRUE),]

#create a single tibble row for data to be pulled from minute entry docs
to_row_minute_entry <- function(x) {
  #scalars
  hearing_date <- as.character(pull_field(x, "hearing_date", NA_character_))
  case_number <- as.character(pull_field(x, "case_number", NA_character_))
  
  #arrays (ensure character vector, even if scalar was returned)
  pn <- as.character(unlist(pull_field(x, "plaintiff_names", character()), use.names = FALSE))
  pa <- as.character(unlist(pull_field(x, "plaintiff_attorneys", character()), use.names = FALSE))
  pnh <- as.character(unlist(pull_field(x, "plaintiffs_at_hearing", character()), use.names = FALSE))
  pah <- as.character(unlist(pull_field(x, "plaintiff_attorneys_at_hearing", character()), use.names = FALSE))
  dn <- as.character(unlist(pull_field(x, "defendant_names", character()), use.names = FALSE))
  da <- as.character(unlist(pull_field(x, "defendant_attorneys", character()), use.names = FALSE))
  dnh <- as.character(unlist(pull_field(x, "defendants_at_hearing", character()), use.names = FALSE))
  dah <- as.character(unlist(pull_field(x, "defendant_attorneys_at_hearing", character()), use.names = FALSE))
  
  #confidence scores
  conf <- pull_field(x, "confidence", list())
  conf_hearing_date <- as.numeric(pull_field(conf, "hearing_date", 0.0))
  conf_case_num <- as.numeric(pull_field(conf, "case_number", 0.0))
  conf_pn <- as.numeric(pull_field(conf, "plaintiff_names", 0.0))
  conf_pa <- as.numeric(pull_field(conf, "plaintiff_attorneys", 0.0))
  conf_pnh <- as.numeric(pull_field(conf, "plaintiffs_at_hearing", 0.0))
  conf_pah <- as.numeric(pull_field(conf, "plaintiff_attorneys_at_hearing", 0.0))
  conf_dn <- as.numeric(pull_field(conf, "defendant_names", 0.0))
  conf_da <- as.numeric(pull_field(conf, "defendant_attorneys", 0.0))
  conf_dnh <- as.numeric(pull_field(conf, "defendants_at_hearing", 0.0))
  conf_dah <- as.numeric(pull_field(conf, "defendant_attorneys_at_hearing", 0.0))
  
  tibble(
    hearing_date = hearing_date,
    case_number = case_number,
    plaintiff_names = list(pn),
    plaintiff_attorneys = list(pa),
    plaintiffs_at_hearing = list(pnh),
    plaintiff_attorneys_at_hearing = list(pah),
    defendant_names = list(dn),
    defendant_attorneys = list(da),
    defendants_at_hearing = list(dnh),
    defendant_attorneys_at_hearing = list(dah),
    conf_hearing_date = conf_hearing_date,
    conf_case_number = conf_case_num,
    conf_plaintiff_names = conf_pn,
    conf_plaintiff_attorneys = conf_pa,
    conf_plaintiffs_at_hearing = conf_pnh,
    conf_plaintiff_attorneys_at_hearing = conf_pah,
    conf_defendant_names = conf_dn,
    conf_defendant_attorneys = conf_da,
    conf_defendants_at_hearing = conf_dnh,
    conf_defendant_attorneys_at_hearing = conf_dah
  )
}
#function to extract data from minute entry docs using prompt_minute_entry
parse_one <- function(txt) {
  outer <- data_from_llm(prompt_minute_entry, txt)
  obj   <- tryCatch(fromJSON(outer, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(obj) || is.null(obj$choices) || length(obj$choices) < 1) return(NULL)
  
  raw <- obj$choices[[1]]$message$content
  raw <- trimws(clean_fences(raw))
  if (!validate(raw)) return(NULL)
  
  # parse inner JSON to a *list* (not auto-simplified), then merge dupes
  inner <- tryCatch(fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(inner)) return(NULL)
  
  inner <- merge_dupe_vars(inner)
  to_row_minute_entry(inner)
}

### run a test on a small subset of OCR data ###
n_test <- 15
set.seed(198)
docs_test_minute_entry <- slice_sample(docs_to_run_minute_entry, n = min(n_test, nrow(docs_to_run_minute_entry)))

llm_test_minute_entry <- map2_dfr(
  docs_test_minute_entry$text,
  seq_len(nrow(docs_test_minute_entry)),
  ~{
    res <- parse_one(.x)
    if (is.null(res)) tibble()
    else bind_cols(
      tibble(row_id = .y,
             direc    = docs_test_minute_entry$direc[.y],
             document = docs_test_minute_entry$document[.y]),
      res
    )
  }
)

#create dataframe containing all data from minute entry documents, add source identifiers
llm_data_minute_entry <- map2_dfr(
  docs_to_run_minute_entry$text,
  seq_len(nrow(docs_to_run_minute_entry)),
  ~{
    res <- parse_one(.x)
    if (is.null(res)) tibble()
    else bind_cols(
      tibble(row_id = .y,
             direc    = docs_to_run_minute_entry$direc[.y],
             document = docs_to_run_minute_entry$document[.y]),
      res
    )
  }
)

#save data
saveRDS(llm_data_minute_entry, "data/llm_data_minute_entry.rds")