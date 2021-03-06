#'
#' Script name: covid_pubmed_graph.R
#'
#' Purpose of script: This represents the primary application script. This script will process a csv file containing a list of pubmed
#' entries that relate to COVID-19. Each of these PubMed entries will be fetched from NCBI in XML format and be
#' loaded into a local Neo4j database instance. In addition, other PubMed entries specified as refernece will
#' also be fetched and loaded. This latter process will be performed recursively until a specified
#' number of reference levels has been reached
#'
#' Author:Fred Criscuolo
#'
#' Date Created: 2021-05-20
#'
#' Copyright (c) Fred Criscuolo, 2021
#' Email: genomicdatasci@gmail.com
#'
#' ---------------------------
#'
#' Notes:
#'    TODO: refactor code to use the easyPubMed package in lieu of the rentrez package
#'
#' ---------------------------
source(here::here("R/functions/init_environment.R"))
source(here::here("R/fetch_pubmed_entries.R"))

default_csv_file_path <- here::here("protected_data/metadata_sample.csv")
default_row_count <- as.integer(props$pubmed.max.count)
max_ref_level <- props$reference.levels.default

#' Clear the existing database
clear_neo4j_database()

pubmed_id_list <- extract_pubmed_ids_from_csv(default_csv_file_path, default_row_count)


# Reference Level 1 -------------------------------------------------------

# load neo4j database with level 1 (i.e. covid) pubmed entries
level <- 1
for (i in 1:nrow(pubmed_id_list)) {
  pubmed_id <- pubmed_id_list$pubmed_id[i]
  log_info(paste("Processing PubMed ID: ", pubmed_id, " at level: ", level, sep = ""))
  load_pubmed_entry(pubmed_id, level)
  Sys.sleep(0.3)  # conform to NCBI request frequency
}


# Reference Generations 2-n -----------------------------------------------

for (i in 2:max_ref_level) {
  # get pubmed ids for previous level
  prev_level <- i - 1
  pubmed_id_list <- find_pubmed_ids_by_level(prev_level)
  print(paste("Processing level: ", i, " Found ", length(pubmed_id_list$value), " PubMed entries at level ",
    prev_level,
    sep = ""
  ))
  for (j in 1:length(pubmed_id_list$value)) {
    pubmed_id <- pubmed_id_list$value[j]
    citations <- find_citations_by_pubmed_id(pubmed_id)
    if (!is.null(citations) && nrow(citations > 0)) {
      for (k in 1:nrow(citations)) {
        ref_pubmed_id <- citations$ref_pubmed_id[k]
        cit_id <- citations$id[k]
        load_pubmed_entry(ref_pubmed_id, i)
        # create a relationship between source pubmed id and referenced pubmed id
        load_citation_pubmed_rel(cit_id, ref_pubmed_id)
        Sys.sleep(0.4)  # limit rate of requests sent to NCBI
      }
    }
  }
}


# Cited-by PubMed Nodes ---------------------------------------------------

#' For all the Pubmed nodes in the database, find the PubMed entries that cite those articles
#' If these new PubMed entries are novel, create a PubMed node for them.
#' For all cited-by entries, create a realtionship between the cited PubMed node and the cited-by node
log_info("Loading cited-by articles")
load_cited_by_articles()


log_info("******* PubMed article Neo4j loading completed")
