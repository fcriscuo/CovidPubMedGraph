#'
#' Script name: select_top_cited_articles.R
#'
#' Purpose of script: Select the "n" most cited articles from a supplied list od PubMedIds
#'                    Inputs are a file path to a csv file containing the PubMed Ids and the number of
#'                    top-cited articles to select.
#'                    The input file must have column names and a column labeled pubmed_id
#'                    The output is a tipple conating the n top cited articles and the count of 
#'                    the articles citing each PubMed Id
#'
#' Author:Fred Criscuolo
#'
#' Date Created: 2021-06-01
#'
#' Copyright (c) Fred Criscuolo, 2021
#' Email: genomicdatasci@gmail.com
#'
#' ---------------------------
#'
#' Notes:
#'   
#'
#' ---------------------------
#' Uncomment next two (2) lines to use BioConductor packages
#'if (!requireNamespace("BiocManager", quietly = TRUE))
#'    install.packages("BiocManager")
#' Pacman package
if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load("tidyverse", "data.table",  "httr","XML","curl",
               "digest","properties","magrittr", "readr",
               "logger", "rentrez")

source(here::here("R/fetch_pubmed_entries.R"))

default_csv_file_path <- file.path(here::here("protected_data/metadata_sample.csv"))
default_top_pubmed_count <- 200
httr::set_config(httr::config(http_version = 0))

#' set up logging
log_appender(appender_file(here::here("./logs/top_cited_pubmed.log")))

fetch_cited_by <- function(pubmed_id) {
  base_url <-
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin&id=PUBMED_ID&&tool=my_tool&email=my_email@example.com"
  df <- tibble(cite_id = character())
  url <-
    stringr::str_replace(base_url, 'PUBMED_ID', as.character(pubmed_id))
  UA <-
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2227.0 Safari/537.36"
 
  data <- tryCatch(
    {
      doc <- GET(url, user_agent(UA))
      XML::xmlParse(content(doc, "text"))
      },
    error = function(cond) {
      message(paste("ERROR parsing XML for PubMed id: ", pubmed_id, sep =""))
      message(cond)
      return (NA)
    },
    warning = function(cond){
      message(paste("Warning parsing XML for PubMed id: ", pubmed_id, sep =""))
      message(cond)
      return (NA)
    },
    finally = {}
  )
  nodes <- getNodeSet(data, '//LinkSetDb')
  if (length(nodes) > 0) {
    links <- xmlChildren(nodes[[1]])
    for (i in 1:length(links)) {
      link <- xmlChildren(links[[i]])$Id
      novel_id <-   xmlValue(link)
      if (!is.na(novel_id)) {
        df[nrow(df) + 1, ] <- novel_id
      }
    }
  }
  return (df)
}


top_cited_articles <- function(csv_file = default_csv_file_path, count = default_top_pubmed_count){
  log_info(paste("PubMed csv file: ", csv_file,"  Number of top citations: ",count, sep = ""))
  df <- tibble(pubmed_id = character(),
               cited_by_count = integer())
  
  pubmed_ids <- extract_pubmed_ids_from_csv(csv_file, Inf)
  
  for(i in 1: nrow(pubmed_ids)) {
    pubmed_id <- pubmed_ids$pubmed_id[i]
    cited_by_counts <- nrow(fetch_cited_by(pubmed_id))
    df[nrow(df)+1,] <- list(as.character(pubmed_id), as.integer(cited_by_counts))
    Sys.sleep(0.4)  # limit rate of requests sent to NCBI
  }
  log_info(paste(nrow(df), " PubMed articles have been scanned for being cited"))
  cited_by <- df %>% 
    slice_max(cited_by_count, n = count )
  print(cited_by)
  write.csv(cited_by, here::here("data/tp_cited_articles.csv"))
}



#' Function to maintain a tibble of the top n cited articles
update_top_articles <- function(top_articles, pm_id, cited_by_count) {
  # identify the current article with the lowest cited by count
  x <- tib[which.min(top_articles$count),]
  print(paste("id: ", x$id[1], " x$count: ", x$count[1], "   cited_by count:", cited_by_count, sep =""))
  if (x$count[1] < cited_by_count) {
    index <- x$id[1]
    updated_top_articles <- rows_update(top_articles, tibble(id=index,pubmed_id=pm_id,
                                                     count= cited_by_count),
                                        by = "id")
    return (updated_top_articles)
    # print(paste("Top articles id: ", x$id[1], " pubmed id: ",
    #             pm_id, " count: ", cited_by_count))
  }
 
  return (top_articles)
}


# Function to maintain a list of the top n cited PubMed entries
# input is the number of entries to maintain
select_top_n_cited_articles <-
  function(csv_file_name = "protected_data/metadata_sample.csv"
           , n ) {
    csv_file <- here::here(csv_file_name)
    snapshot_file <-  here::here("tmp/top_cited_by_snapshot.csv")
    out_file_path <-
      here::here(paste("data/top_", n, "_articles.csv", sep = ""))
    id <- seq(1:n)
    pubmed_id <- rep("XXXX", times = n)
    count <-  rep(0, times = n)
    top_articles <-  tibble(id, pubmed_id, count)
   
    # accept default count (i.e. Inf)
    pubmed_ids <- extract_pubmed_ids_from_csv(csv_file, Inf)
    for (i in 1:nrow(pubmed_ids)) {
      # determine the cited by count for this paper
      pubmed_id <-  as.character(pubmed_ids$pubmed_id[i])
      df <-  fetch_cited_by((pubmed_id))
      #print(paste ("Pubmed id: ", pubmed_id, " Count: ", nrow(df)))
      if (!is.null(nrow(df)) & nrow(df) > 0) {
        top_articles <-
          update_top_articles(top_articles, pubmed_id, nrow(df))
      }
      # save interim results
      if (i %% 1000 == 0) {
        write_csv(top_articles, snapshot_file)
        print(paste("Snapshot file wirtten at index: ", i, sep = ""))
      }
      Sys.sleep(0.4)  # limit rate of requests sent to NCBI
    }
   
    cited_by <- top_articles %>%
      arrange(desc(count))
    write_csv(cited_by, out_file_path)
  }


test_top_cited_articles <- function() {
  top_cited_articles()
}

