#'
#' Script name: RefactorReferenceNodes.R
#'
#' Purpose of script: Refactors original Neo4j databse schema to establish
#' a direct relation between a PubMed node at level n with the PubMed nodes it
#' references directly at level n+1. Removes the Citation nodes that served as 
#' an intermediate node in the original design
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
source(here::here("R/functions/init_environment.R"))
source(here::here("R/fetch_pubmed_entries.R"))
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
  #TODO: convert for loops to tidyverse pipes
  rel_count <- 0
  for (i in 1:(max_ref_level - 1)) {
    log_info(paste("Processing PubMed nodes at level: ", i))
    pubmed_id_list <-  find_pubmed_ids_by_level(i)$value
    for (j in 1:length(pubmed_id_list)) {
      pubmed_id <- pubmed_id_list[j]
      citation_list <- find_citations_by_pubmed_id(pubmed_id)
      if (!is.null(citation_list) && nrow(citation_list) > 0) {
        for (k in 1:nrow(citation_list)) {
          resolve_referenced_article(citation_list[k])
          # ref_pubmed_id <-  citation_list$ref_pubmed_id[k]
          # if (!is_null(ref_pubmed_id) &&
          #     numbers_only(ref_pubmed_id)) {
          #   if (!pubmed_node_exists(ref_pubmed_id)) {
          #     # load any missing PubMed entries for referenced articles
          #     load_pubmed_entry(ref_pubmed_id, i)
          #   }
          #   if (merge_pubmed_reference_relationship (pubmed_id, ref_pubmed_id)) {
          #     #delete the now bypassed Citation node if a PubMed -> PubMed relationship can be established
          #     delete_citationby_id(citation_list$id[k])
          #     
          #     log_info(
          #       paste(
          #         "Established reference relationship: ",
          #         pubmed_id,
          #         " -> ",
          #         ref_pubmed_id,
          #         " and deleted Citation: ",
          #         citation_list$citation[k]
          #       )
          #     )
          #     rel_count <- rel_count + 1
          #   }
          # }  # end of valid ref_pubmed_id processing
        }  # end of citation loop
      } # end of citation processing
    }  # end of pubmed_id processing
    log_info(paste("Created ", rel_count, " HAS_REFERENCE relationships"))
  }
}

resolve_referenced_article <- function(citation){
  ref_pubmed_id <-  citation$ref_pubmed_id
  pubmed_id <- citation$pubmed_id
  if (!is.null(ref_pubmed_id) && numbers_only(ref_pubmed_id)) {
    if(!pubmed_node_exists(ref_pubmed_id)) {
      load_pubmed_entry(ref_pubmed_id)
    }
    if (merge_pubmed_reference_relationship (pubmed_id, ref_pubmed_id)) {
      delete_citationby_id(citation$id)
      log_info(paste( "Established reference relationship: ",
                      pubmed_id,
                      " -> ",
                      ref_pubmed_id,
                      " and deleted Citation: ",
                      citation_list$citation,sep=""))
    }
  }
}


