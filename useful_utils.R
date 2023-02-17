library(data.table)

get_checkouts <- function(years, directory="./data/checkouts/"){
  #' Returns a single dataframe of the Seattle checkouts data for the given years
  #' Requires the data.table package, which is much faster than R's built-ins.
  #' 
  #' @years - a vector of integer years
  #' @direrctory - the path to the directory of data files.
  #'               By default assumed to be ./data/checkouts/
  #' 
  #' Example: checkouts_2005_2006 <- get_checkouts(2005:2006)
  selected_files <- file.path(directory, paste0("checkouts_", years, ".csv"))
  individual_dataframes <- lapply(selected_files, fread)
  rbindlist(individual_dataframes)
}
