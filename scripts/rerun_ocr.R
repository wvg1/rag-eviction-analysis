#install packages
library(tidyverse)
library(reticulate)

#set up python
use_virtualenv("./.venv", required = TRUE)
py_config()
source_python("scripts/test_ocr.py")
py <- import_builtins()
pickle <- import("pickle")

# <- set this to your top-level folder that replaces "data"
top_level <- "data/2022-24 data"

# 1) function to normalize a directory string but KEEP leading "data/" if present
clean_dir <- function(dir) {
  if (is.na(dir)) return(NA_character_)
  dir <- str_trim(dir)
  if (dir == "" || dir == "direc") return(NA_character_)
  # remove any leading "./"
  d <- str_replace(dir, "^\\./", "")
  # split into parts and drop empty entries
  parts <- unlist(str_split(d, "/"))
  parts <- parts[parts != ""]
  if (length(parts) == 0) return(NA_character_)
  # collapse consecutive duplicate parts (e.g., "2022/2022/..." -> "2022/...")
  parts_clean <- parts[c(TRUE, parts[-1] != parts[-length(parts)])]
  # If the first element is "data", keep it; otherwise keep whatever root exists
  nd <- paste(parts_clean, collapse = "/")
  # collapse repeated year at the start while preserving possible leading "data/"
  # handle "data/2022/2022/..." -> "data/2022/..." and "2022/2022/..." -> "2022/..."
  nd <- sub("^data/(\\d{4})/\\1/", "data/\\1/", nd)
  nd <- sub("^(\\d{4})/\\1/", "\\1/", nd)
  nd
}

# Apply cleaning and show a preview (first 20 unique mappings)
to_fix <- to_fix %>%
  mutate(direc_fixed = map_chr(direc, clean_dir))

cat("Preview of original vs cleaned directories:\n")
print(to_fix %>% distinct(direc, direc_fixed) %>% head(20))

# 2) helper to find the actual file path using fallbacks
find_file <- function(top_level, dir_fixed, orig_dir, doc) {
  # If dir_fixed starts with "data/", strip that part when joining with top_level
  dir_for_top <- ifelse(startsWith(dir_fixed, "data/"),
                        sub("^data/", "", dir_fixed),
                        dir_fixed)
  
  cand1 <- file.path(top_level, dir_for_top, doc)
  # alternate with explicit collapse of repeated-year at the start (still keep data/ if present)
  cand2_dir <- sub("^(data/)?(\\d{4})/\\2/", "\\1\\2/", dir_fixed)
  cand2_dir_for_top <- ifelse(startsWith(cand2_dir, "data/"),
                              sub("^data/", "", cand2_dir),
                              cand2_dir)
  cand2 <- file.path(top_level, cand2_dir_for_top, doc)
  
  cand3 <- file.path(orig_dir, doc)                       # original dir + doc
  cand4 <- file.path(getwd(), orig_dir, doc)              # absolute-ish attempt
  cand5 <- file.path(getwd(), top_level, dir_for_top, doc)  # full from getwd
  candidates <- unique(c(cand1, cand2, cand3, cand4, cand5))
  for (p in candidates) {
    if (!is.na(p) && file.exists(p)) return(normalizePath(p))
  }
  # fallback: search for the filename under top_level (recursive)
  matches <- list.files(path = top_level, pattern = paste0("^", basename(doc), "$"),
                        full.names = TRUE, recursive = TRUE)
  if (length(matches) > 0) return(normalizePath(matches[1]))
  # last-ditch: search entire working directory
  matches2 <- list.files(path = ".", pattern = paste0("^", basename(doc), "$"),
                         full.names = TRUE, recursive = TRUE)
  if (length(matches2) > 0) return(normalizePath(matches2[1]))
  NA_character_
}

# 3) Loop and run OCR only when the file is resolved
ocr_results <- list()
for (i in seq_len(nrow(to_fix))) {
  orig_dir <- to_fix$direc[i]
  dir_fixed <- to_fix$direc_fixed[i]
  doc <- to_fix$document[i]
  
  if (is.na(dir_fixed)) {
    warning("Skipping (bad/placeholder dir): ", orig_dir)
    next
  }
  
  resolved_path <- find_file(top_level, dir_fixed, orig_dir, doc)
  if (is.na(resolved_path)) {
    warning("Couldn't find file for: ", file.path(orig_dir, doc))
    next
  }
  
  cat("Processing:", resolved_path, "\n")
  # call your python function (wrapped so one failure doesn't stop the loop)
  res <- tryCatch({
    result <- analyze_layout_custom_model(resolved_path)
    # Collect page text (adjust extraction if you want more details)
    page_texts <- lapply(result$pages, function(page) {
      if (length(page$lines) > 0) {
        paste(sapply(page$lines, function(l) l$content), collapse = " ")
      } else ""
    })
    full_text <- paste(unlist(page_texts), collapse = "\n")
    tibble(
      direc = orig_dir,
      document = doc,
      resolved_path = resolved_path,
      text = full_text
    )
  }, error = function(e) {
    warning("Error on ", resolved_path, " : ", conditionMessage(e))
    NULL
  })
  
  if (!is.null(res)) ocr_results[[length(ocr_results) + 1]] <- res
}

ocr_results_df <- bind_rows(ocr_results)
cat("OCR done for", nrow(ocr_results_df), "files (results in ocr_results_df).\n")

# Optional: merge back into your ocr_combined (only if keys align)
# ocr_combined <- ocr_combined %>%
#   left_join(ocr_results_df %>% select(direc, document, text), by = c("direc", "document"))

saveRDS(ocr_results_df, file = "data/ocr_rerun.rds")
