#'
#' Script name: covid_constraints_defs.R
#'
#' Purpose of script:Define constraints for the NEO4J database
#'
#' Author:Fred Criscuolo
#'
#' Date Created: 2021-05-24
#'
#' Copyright (c) Fred Criscuolo, 2021
#' Email: genomicdatasci@gmail.com
#'
#' ---------------------------
#'
#' Notes: 
#' 1. This script should be executed prior to loading the Neo4j database.
#'   
#'
#' ---------------------------
#' Function to create Neo4j database constraints

define_database_constraints <- function() {
  constraint <- 'CREATE CONSTRAINT unique_pubmed_id IF NOT EXISTS ON (n:PubMed) ASSERT n.pubmed_id IS UNIQUE'  
  execute_cypher_command(constraint)
  constraint <- 'create constraint unique_mesh_descriptor if not exists on (m:MeshHeading) assert m.descriptor_key is unique;'
  execute_cypher_command(constraint)
  constraint <- 'create constraint unique_citation_id if not exists on (c:Citation) assert c.id is unique;'
  execute_cypher_command(constraint)
  constraint <- 'create constraint unique_journal_issn if not exists on (j:Journal) assert j.journal_issn is unique'
  execute_cypher_command(constraint)
  constraint <- 'create constraint unique_issue_key if not exists on (j:JournalIssue) assert j.issue_key is unique'
  execute_cypher_command(constraint)
  constraint <- 'create constraint unique_author_id if not exists on (a:Author) assert a.id is unique'
  execute_cypher_command(constraint)
  constraint <- 'create constraint unique_keyword if not exists on (k:Keyword) assert k.keyword is unique'
  execute_cypher_command(constraint)
  execute_cypher_command('SHOW CONSTRAINTS')
}

source(here::here("R/utilities/renviron_properties.R"))
source(here::here("R/functions/neo4j_functions.R"))
define_database_constraints()