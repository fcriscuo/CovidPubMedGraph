#'
#' Script name: covid_pubmed_graph.R
#'
#' Purpose of script: This represents the primary application script. This script will process a csv file containing a list of pubmed
#' entries that relate to COVID-19. Each of these PubMed entries will be fetched from NCBI in XML format and be
#' loaded into a local Neo4j database instance. In addition, other PubMed entries specified in Reference elements will
#' also be fetched and loaded. This latter process will be performed recursively until a specified
#' number of reference levels has been reached. When all PubMed nodes have been loaded, the script will
#' scan the PubMed repository for articles that cite those nodes. It will the create a basic PubMed node for
#' any PubMed articles that are not already in the database. It will also create a CITED_BY relationship from
#' the cited by node to the reference article node. This allows the database to demonstrate the relative impact
#' of an article by how aoften it is cited.
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
#' 1. This script will delete all nodes and relationships in the target Neo4j database
#' 2. Prior to running this script for the first time the user should run the
#'    covid_constraint_def.R script to define constaints in the Neo4j database
#'    
#'
#' ---------------------------
source(here::here("R/functions/init_environment.R"))
source(here::here("R/fetch_pubmed_entries.R"))

csv_file_path <- here::here(props$covid.csv.file)
default_row_count <- as.integer(props$pubmed.max.count)
max_ref_level <- as.integer(props$reference.levels.default)
clear_database <- as.logical(props$load.database.mode == "CREATE")
ncbi_batch_size <- as.integer(props$batch.request.size)
primary_node_label <- props$primary.node.label

#' Clear the existing database
if(clear_database) {
  clear_neo4j_database()
}

pubmed_id_list <- extract_pubmed_ids_from_csv(csv_file_path, default_row_count)

# Reference Level 1 -------------------------------------------------------

# load neo4j database with level 1 (i.e. covid) pubmed entries
level <- 1
for (i in 1:nrow(pubmed_id_list)) {
  pubmed_id <- pubmed_id_list$pubmed_id[i]
  log_info(paste("Processing PubMed ID: ", pubmed_id, " at level: ", level, sep = ""))
  load_pubmed_entry(pubmed_id, level)
  Sys.sleep(0.3)  # conform to NCBI request frequency
}

# Reference Levels 2-n -----------------------------------------------

for (i in 2:max_ref_level) {
  # get pubmed ids for previous level
  prev_level <- i - 1
  pubmed_id_list <- find_pubmed_ids_by_level(prev_level)
  log_info(paste("Processing level: ", i, " Found ", length(pubmed_id_list$value), " PubMed entries at level ",
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
        # create a relationship between source pubmed id and a 
        # referenced pubmed id using a Citation node
        load_citation_pubmed_rel(cit_id, ref_pubmed_id)
        Sys.sleep(0.3)  # limit rate of requests sent to NCBI
      }
    }
  }
}

# Refactor Reference Nodes ------------------------------------------------
#
# Refactor the database schema to establish direct relationships between PubMed nodes
# representing original articles and PubMed nodes that represent referenced articles 
# at a different level. Remove intermediate Citation nodes where a PubMed node for
# a reference article can be created
source(here::here("R/utilities/RefactorReferenceNodes.R"))
refactor_pubmed_references()

# Cited-by PubMed Nodes ---------------------------------------------------

#' For all the Pubmed nodes in the database, find the PubMed entries that cite those articles
#' If these new PubMed entries are novel, create a PubMed node for them.
#' For all cited-by entries, create a realtionship between the cited PubMed node and the cited-by node
log_info("Loading cited-by articles")
load_cited_by_articles()

log_info("******* PubMed article Neo4j loading completed")
