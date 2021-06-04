#'
#' Script name: 
#'
#' Purpose of script: top_n_articles.R
#'
#' Author:Fred Criscuolo
#'
#' Date Created: 2021-06-03
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

#' Pacman package
if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load("tidyverse", "data.table",  "httr","XML","curl",
               "digest","properties","magrittr", "readr",
               "logger", "rentrez")


extract_pubmed_ids_from_csv <- function(csv_file_path, row_count = Inf) {
  # Accept function defaults
  # Include col_names = TRUE (default) to document header requirement
  pubmed_id_list <- read_csv(csv_file_path,col_names = TRUE, guess_max = 4,
                             n_max = row_count) %>% 
    select(pubmed_id)
  log_info(paste("Read ", nrow(pubmed_id_list), " from csv file: ", csv_file_path, sep =""))
  return (pubmed_id_list)
}

fetch_cited_by_pubmed_ids <- function(pubmed_id) {
  base_url <-
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin&id=PUBMED_ID&tool=my_tool&email=my_email@example.com"
  df <- tibble(cite_id = character())
  url <-
    stringr::str_replace(base_url, 'PUBMED_ID', as.character(pubmed_id))
  
  UA <-
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2227.0 Safari/537.36"
  doc <- tryCatch({
    GET(url, user_agent(UA))
  } ,
  error = function(cond) {
    message(paste("ERROR getting URL ", url, " PubMed id: ", pubmed_id, sep =
                    ""))
    message(cond)
    return (df)
  },
  warning = function(cond) {
    message(paste("Warning getting URL ", url, "PubMed id: ", pubmed_id, sep =
                    ""))
    message(cond)
    return (df)
  },
  finally = {
  })
  data <- XML::xmlParse(content(doc, "text"))
  nodes <- getNodeSet(data, '//LinkSetDb')
  if (!is.null(nodes) && length(nodes) > 0) {
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

update_top_articles <- function(top_articles, pm_id, cited_by_count) {
  # identify the current article with the lowest cited by count
  x <- top_articles[which.min(top_articles$count),]
  if (x$count[1] < cited_by_count) {
    index <- x$id[1]
    updated_top_articles <- rows_update(top_articles, tibble(id=index,pubmed_id=pm_id,
                                                             count= cited_by_count),
                                        by = "id")
    return (updated_top_articles)
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
    pubmed_ids <- extract_pubmed_ids_from_csv(csv_file, Inf)
    for (i in 1:nrow(pubmed_ids)) {
      # determine the cited by count for this paper
      pubmed_id <-  as.character(pubmed_ids$pubmed_id[i])
      df <-  fetch_cited_by_pubmed_ids((pubmed_id))
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
