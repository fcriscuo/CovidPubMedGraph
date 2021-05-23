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
default_row_count <- 1000

pubmed_id_list <- extract_pubmed_ids_from_csv(default_csv_file_path, default_row_count)

# load neo4j database with level 1 (i.e. covid) pubmed entries
for (i in 1:nrow(pubmed_id_list)){
  pubmed_id <-  pubmed_id_list$pubmed_id[i]
  level <- 1
  print(paste("Processing PubMed ID: ",pubmed_id, " at level: ",level, sep=""))
  load_pubmed_entry(pubmed_id, level)
}