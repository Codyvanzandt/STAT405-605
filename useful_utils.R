library(data.table)
library(RSQLite)

get_checkouts <- function(years, directory="./data/checkouts"){
  #' Returns a single dataframe of the Seattle checkouts data for the given years
  #' Requires the data.table package, which is much faster than R's built-ins.
  #' 
  #' @years - a vector of integer years
  #' @direrctory - the path to the directory of data files.
  #'               By default assumed to be './data/checkouts/'
  #'               Files are assumed to be named 'checkouts_{year}.csv'
  #' 
  #' Example: checkouts_2005_2006 <- get_checkouts(2005:2006)
  selected_files <- file.path(directory, paste0("checkouts_", years, ".csv"))
  individual_dataframes <- lapply(selected_files, fread)
  as.data.frame(rbindlist(individual_dataframes))
}

wrap.it <- function(x, len)
{ 
  sapply(x, function(y) paste(strwrap(y, len), 
                              collapse = "\n"), 
         USE.NAMES = FALSE)
}

wrap.labels <- function(x, len)
{
  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}

get.connection <- function(directory = "./data") {
  dbConnect(SQLite(), dbname = paste0(directory, "/SQLData.db"))
}
