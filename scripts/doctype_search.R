#####
# this script searches through organized folders of eviction case files to identify whether certain documents are present
# take folder_location and doc_names and identify whether each sub-folder (leaf directory) has at least one match
# folder location is the directory that must contain folders for each year, and within that, folders for each case number
# doc_names is a list of strings that match desired document types to search for in leaf directories
#######

doctype_search <- function(folder_location, doc_names, excluded_docs = NULL) {
  # processing file and doc names
  doc_names <- trimws(tolower(doc_names))
  files <- list.files(folder_location, recursive = TRUE)
  files <- trimws(tolower(files))
  if(!is.null(excluded_docs)) for(excl in excluded_docs) files <- files[!grepl(excl, files)]
  
  # loop through doc_names and return number of matches in files
  sum(sapply(doc_names, function(doc) sum(grepl(doc, files))))
}
