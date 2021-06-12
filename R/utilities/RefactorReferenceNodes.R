#'
#' Script name: RefactorReferenceNodes.R
#'
#' Purpose of script: Refactors original Neo4j databse schema to establish
#' a direct relation between a PubMed node at level n with the PubMed nodes it
#' references directly at level n+1. Removes the Citation nodes that served as 
#' an intermediate node in the original design
#' 
#'
#' Author:Fred Criscuolo
#'
#' Date Created: 2021-06-11
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
#'' Pacman package
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load("tidyverse","data.table","logger")
source(here::here("R/functions/neo4j_functions.R"))
#' set up logging
log_appender(appender_file(here::here("./logs/refactor_references.log")))

refactor_pubmed_references <- function() {
  max_ref_level <-  resolve_max_pubmed_level()
  if (max_ref_level < 2) {
    log_info(
      paste(
        "The maximum reference level in the database is ",
        max_ref_level,
        " There are no reference nodes to refactor"
      )
    )
    return()
  }
  rel_count <- 0
  for (i in 1:(max_ref_level - 1)) {
    log_info(paste("Processing PubMed nodes at level: ", i))
    pubmed_id_list <-  find_pubmed_ids_by_level(i)$value
    for (j in 1:length(pubmed_id_list)) {
      pubmed_id <- pubmed_id_list[j]
      citation_list <- find_citations_by_pubmed_id(pubmed_id)
      if (!is.null(citation_list) && nrow(citation_list) > 0) {
        for (k in 1:nrow(citation_list)) {
          ref_pubmed_id <-  citation_list$ref_pubmed_id[k]
          if (merge_pubmed_reference_relationship (pubmed_id, ref_pubmed_id)) {
            #delete the bypassed Citation node if a relationship can be established
            delete_citationby_id(citation_list$id[k])
            
            log_info(
              paste(
                "Established reference relationship: ",
                pubmed_id,
                " -> ",
                ref_pubmed_id,
                " and deleted Citation: ",
                citation_list$citation[k]
              )
            )
            rel_count <- rel_count + 1
          }
        }
      }
    }
    log_info(paste("Created ", rel_count, " HAS_REFERENCE relationships"))
  }
  
}