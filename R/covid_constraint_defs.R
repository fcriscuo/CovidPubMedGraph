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
  #HGNC 
  constraint <- 'create constraint unique_hgnc_id if not exists on (h:HGNC) assert h.hgnc_id is unique'
  execute_cypher_command(constraint)
  #GeneFamily
  constraint <- 'create constraint unique_gene_fmaily_id if not exists on (gf:GeneFamily) assert gf.gene_family_id is unique'
  execute_cypher_command(constraint)
  #Entrez
  constraint <- 'create constraint unique_entrez_id if not exists on (e:Entrez) assert e.entrez_id is unique'
  execute_cypher_command(constraint)
  #Ensembl
  constraint <- 'create constraint unique_ensembl_id if not exists on (e:Ensembl) assert e.ensembl_id is unique'
  execute_cypher_command(constraint)
  #RefSeq
  constraint <- 'create constraint unique_refseq_id if not exists on (r:RefSeq) assert r.refseq_accession is unique'
  execute_cypher_command(constraint)
  #Uniprot
  constraint <- 'create constraint unique_uniprot_id if not exists on (u:Uniprot) assert u.uniprot_id is unique'
  execute_cypher_command(constraint)
  #Cosmic
  constraint <- 'create constraint unique_cosmic_id if not exists on (c:Cosmic) assert c.cosmic_id is unique'
  execute_cypher_command(constraint)
  #OMIM
  constraint <- 'create constraint unique_omim_id if not exists on (o:OMIM) assert o.omim_id is unique'
  execute_cypher_command(constraint)
  
  execute_cypher_command('SHOW CONSTRAINTS')
}

source(here::here("R/utilities/renviron_properties.R"))
source(here::here("R/functions/neo4j_functions.R"))
define_database_constraints()