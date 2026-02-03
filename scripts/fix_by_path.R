### this function loops though a dataframe and changes all observations of case_number to match the directory label
library(tidyverse)

fix_by_path <- function(df, orig_df = NULL, row_id_col = "row_id") {
  # df: tibble/data.frame with at least direc and case_number
  # orig_df: optional original snapshot (must contain row_id_col and case_number)
  # row_id_col: name of id column; if missing we create a temporary one and remove it at the end
  
  df <- as_tibble(df)
  
  # ensure a stable row id exists
  created_tmp_id <- FALSE
  if (!row_id_col %in% names(df)) {
    df <- df %>% mutate(.rowid_tmp = row_number())
    id_col <- ".rowid_tmp"
    created_tmp_id <- TRUE
  } else {
    id_col <- row_id_col
  }
  
  # compute normalized basename
  df <- df %>%
    mutate(
      path_case = basename(direc),
      path_case = if_else(is.na(path_case) | path_case == "", NA_character_, path_case),
      .orig_case = case_number
    )
  
  # replace case_number where path_case exists and differs
  df <- df %>%
    mutate(
      case_number = if_else(!is.na(path_case) & path_case != .orig_case, path_case, .orig_case)
    )
  
  # compute .corrected_from_path
  if (!is.null(orig_df)) {
    orig_tbl <- as_tibble(orig_df)
    if (!id_col %in% names(orig_tbl)) stop("orig_df must contain the same row id column")
    df <- df %>%
      left_join(orig_tbl %>% select(!!sym(id_col), case_number_orig = case_number),
                by = id_col) %>%
      mutate(
        case_number_orig = coalesce(case_number_orig, .orig_case),
        .corrected_from_path = case_number != case_number_orig
      ) %>%
      select(-case_number_orig)
  } else {
    df <- df %>%
      mutate(.corrected_from_path = (.orig_case != case_number))
  }
  
  # cleanup helper columns
  df <- df %>% select(-path_case, - .orig_case)
  
  if (created_tmp_id) df <- df %>% select(-.rowid_tmp)
  
  df
}
