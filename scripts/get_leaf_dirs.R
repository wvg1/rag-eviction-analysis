#get leaf directories (cases) from root directories (years)
get_leaf_dirs <- function(root_dir) {
  # Get all directories under root_dir
  all_dirs <- list.dirs(root_dir, recursive = TRUE, full.names = TRUE)
  
  # Filter to only those directories that do NOT contain any other directories
  leaf_dirs <- all_dirs[!sapply(all_dirs, function(d) {
    any(file.info(list.files(d, full.names = TRUE))$isdir)
  })]
  
  return(leaf_dirs)
}