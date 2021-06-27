#'
#' Script name: generate_pubmed_batches.R
#'
#' Purpose of script: Generate a list of eUtils batch requests using a list
#' of PubMed Ids.
#' 
#' Inputs: (1) a csv file containing a column labeled pubmed_id
#'         (2) batch size
#' Output: a file containing a list of batched PubMed Ids in a 
#'         format suitable for an eUtils batch request
#'
#' Author:Fred Criscuolo
#'
#' Date Created: 2021-06-26
#'
#' Copyright (c) Fred Criscuolo, 2021
#' Email: genomicdatasci@gmail.com
#' github: http://www.github.com/fcriscuo
#' URL: https://fcriscuo.github.io
#' ---------------------------
#'
#' Notes:
#'   
#'
#' ---------------------------
#' Uncomment next two (2) lines to use BioConductor packages
#'if (!requireNamespace("BiocManager", quietly = TRUE))
#'    install.packages("BiocManager")
#'' Pacman package
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load("fs")

batch_file_path <- here::here("./data/batch_pubmed_queries.txt")

generate_pubmed_batches_from_file <- function(csv_file_path, batch_size = 20) {
  if(file.exists(batch_file_path)) {
    file.remove(batch_file_path)
  }
  query_list <- format_pubmed_queries(csv_file_path, batch_size)
  for (i in 1:nrow(query_list)){
    text <- paste(query_list$pm_id_query[i],"\n",sep="")
    readr::write_file(text, batch_file_path, append = TRUE)
  }
}

formate_pubmed_queries <- function(csv_file_path, batch_size){
  max_offset <- batch_size -1
  query_list <- tibble(pm_id_query = character())
  pubmed_id_list <-  extract_cited_by_ids(csv_file_path) 
  seq_start <- 1
  extra_query <- if (nrow(pubmed_id_list) %% batch_size == 0) 0 else 1
  num_queries <- (nrow(pubmed_id_list) / batch_size) + extra_query
  for( i in 1:num_queries) {
    seq_end <- seq_start + min(max_offset, (nrow(pubmed_id_list)-seq_start))
    query <- "'"
    for (j in seq_start:seq_end) {
      query <- paste(query,pubmed_id_list[j,], "[PMID]",sep = "")
      if(j == seq_end) {
        query <- paste(query,"'", sep="")
      } else {
        query <- paste(query, " OR ", sep ="")
      }
    }
    # print(paste("start: ", seq_start, " end: " , seq_end,
    #             "  ",query))
    query_list[nrow(query_list)+1,] <- query
    seq_start <- seq_end + 1
  }
  return (query_list)
}
