source("useful_utils.R")
library(tidyverse)
library(dtwclust)
library(factoextra)
library(parallel)

workers <- makeCluster(detectCores())
# load dtwclust in each one, and make them use 1 thread per worker
invisible(clusterEvalQ(workers, {
  library(dtwclust)
  RcppParallel::setThreadOptions(1L)
}))
# register your workers, e.g. with doParallel
require(doParallel)
registerDoParallel(workers)

make_book_checkout_timeseries <- function(checkouts, n=1000){
  top_titles <- checkouts %>%
    filter(MaterialType %in% c("EBOOK", "AUDIOBOOK", "BOOK")) %>%
    group_by(Title) %>%
    summarize(checkouts = sum(Checkouts)) %>%
    top_n(n, wt=checkouts) %>%
    arrange(desc(checkouts)) %>%
    pull(Title)
  
  popularity_counts <- checkouts %>%
    filter(Title %in% top_titles) %>%
    mutate(Date = as.Date(paste0(CheckoutYear,"-",CheckoutMonth,"-01"), "%Y-%m-%d")) %>%
    group_by(Date, Title) %>%
    summarize(checkouts = sum(Checkouts)) %>%
    filter(!is.na(checkouts)) %>%
    arrange(desc(Title), desc(Date))
  
  book_list <- split(popularity_counts, popularity_counts$Title)
  
  book_ts_list <- sapply(book_list, function(x) {
    setNames(x$checkouts, x$Date)
  })
  
  book_ts_list
  
}

preprocess_book_checkout_timeseries <- function(checkout_ts){
  sapply(checkout_ts, function(x){
    values <- as.vector(smooth(scale(unname(x))))
    dates <- as.Date(names(x))
    setNames(values, dates)
  })
}

checkouts <- get_checkouts(2018:2022) 
book_ts <- make_book_checkout_timeseries(checkouts, 3000) 
processed_book_ts <- preprocess_book_checkout_timeseries(book_ts)

pc_k <- tsclust(processed_book_ts, type="partitional", k = 3:30,
                distance = "sbd", centroid = "pam")

cluster_eval <- function(x){
  cvi(x, type="Sil")
}

sil_scores <- sapply(pc_k, cluster_eval)

optimal_k <- which.max(sil_scores) + 2

clus <- tsclust(processed_book_ts, type="partitional", k = optimal_k,
              distance = "sbd", centroid = "pam")

plot(clus, type="c")
