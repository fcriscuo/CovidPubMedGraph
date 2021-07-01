#'
#' Script name: load_hgnc_data.R
#'
#' Purpose of script: Load data from HGNC tsv file into neo4j database
#'
#' Author:Fred Criscuolo
#'
#' Date Created: 2021-06-30
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

source(here::here("R/functions/init_environment.R"))

hgnc_tsv_file_path <- here::here("protected_data/hgnc_complete_set.tsv")
hgnc <- generate_hgnc_df(hgnc_tsv_file_path)

process_hgnc_data <- function(hgnc){
  for (i in 1:nrow(hgnc)) {
    process_hgnc_gene_data(hgnc[i,])
  }
}

# UniProt Data ------------------------------------------------------------

process_uniprot_data <-  function(hgnc_row){
  if(!is.na(hgnc_row$uniprot_ids)){
    ids <- data.frame(do.call('cbind', 
                              strsplit(as.character(hgnc_row$uniprot_ids),'|'
                                       ,fixed=TRUE)))
    for( i in 1:nrow(ids)){
      id <- ids[i,]
      merge <- paste("MERGE (u:Uniprot{uniprot_id:'",
                     id, "'}) return u.uniprot_id", sep=""
                     )
      res <- execute_cypher_command(merge)
      #HGNC -> UniProt relationship
      relationship <-  paste("MATCH (h:HGNC), (u:Uniprot) WHERE h.hgnc_id = '",
                             hgnc_row$hgnc_id,"' and u.uniprot_id = '",
                             id,
                             "' MERGE (h) -[r:HAS_UNIPROT_DATA]->(u) RETURN r",
                             sep=""
                             )
      res <- execute_cypher_command(relationship)
     
    }
  }
}

# COSMIC Data -------------------------------------------------------------
process_cosmic_data <- function(hgnc_row){
  if (!is.na(hgnc_row$cosmic)) {
    merge <-  paste("MERGE (c:Cosmic {cosmic_id:'",
                    hgnc_row$cosmic,"'}) return c.cosmic_id",
                    sep=""
    )
    res <- execute_cypher_command(merge)
    # HGNC -> COSMIC relationship
    relationship <- paste("MATCH (h:HGNC), (c:Cosmic) WHERE h.hgnc_id = '",
                          hgnc_row$hgnc_id,"' and c.cosmic_id = '",
                          hgnc_row$cosmic,
                          "' MERGE (h) -[r:HAS_COSMIC_DATA]->(c) RETURN r",
                          sep=""
    )
    res <- execute_cypher_command(relationship)
    return (res)
  }
}

# OMIM Data ---------------------------------------------------------------

process_omim_data <- function(hgnc_row){
  if (!is.na(hgnc_row$omim_id)) {
    merge <-  paste("MERGE (o:OMIM {omim_id:'",
                    hgnc_row$omim,"'}) return o.omim_id",
                    sep=""
    )
    res <- execute_cypher_command(merge)
    # HGNC -> OMIM relationship
    relationship <- paste("MATCH (h:HGNC), (o:OMIM) WHERE h.hgnc_id = '",
                          hgnc_row$hgnc_id,"' and o.omim_id = '",
                          hgnc_row$omim,
                          "' MERGE (h) -[r:HAS_OMIM_DATA]->(o) RETURN r",
                          sep=""
    )
    return (res)
    res <- execute_cypher_command(relationship)
  }
}

# Ensembl Data ------------------------------------------------------------

process_ensembl_data <- function(hgnc_row){
  if (!is.na(hgnc_row$ensembl_gene_id)) {
    merge <-  paste("MERGE (e:Ensembl {ensembl_id:'",
                    hgnc_row$ensembl_gene_id,"'}) return e.ensembl_id",
                    sep=""
    )
    res <- execute_cypher_command(merge)
    # HGNC -> Ensembl relationshio
    relationship <- paste("MATCH (h:HGNC), (e:Ensembl) WHERE h.hgnc_id = '",
                          hgnc_row$hgnc_id,"' and e.ensembl_id = '",
                          hgnc_row$ensembl_gene_id,
                          "' MERGE (h) -[r:HAS_ENSEMBL_DATA]->(e) RETURN r",
                          sep=""
    )
    res <- execute_cypher_command(relationship)
    return (res)
  }
}

# RefSeq Data -------------------------------------------------------------
process_refseq_data <- function(hgnc_row){
  if (!is.na(hgnc_row$refseq_accession)) {
    merge <-  paste("MERGE (r:RefSeq {refseq_accession:'",
                    hgnc_row$refseq_accession,"'}) return r.refseq_accession",
                    sep=""
    )
    res <- execute_cypher_command(merge)
    # HGNC -> Ensembl relationship
    relationship <- paste("MATCH (h:HGNC), (rs:RefSeq) WHERE h.hgnc_id = '",
                          hgnc_row$hgnc_id,"' and rs.refseq_accession = '",
                          hgnc_row$refseq_accession,
                          "' MERGE (h) -[r:HAS_REFSEQ_DATA]->(rs) RETURN r",
                          sep=""
    )
    res <- execute_cypher_command(relationship)
    return (res)
  }
}

# Entrez Data -------------------------------------------------------------

process_entrez_data <- function(hgnc_row){
  if (!is.na(hgnc_row$entrez_id)) {
    merge <-  paste("MERGE (e:Entrez {entrez_id:'",
                    hgnc_row$entrez_id,"'}) return e.entrez_id",
                    sep=""
                    )
    res <- execute_cypher_command(merge)
    # HGNC -> Entrez relationshio
    relationship <- paste("MATCH (h:HGNC), (e:Entrez) WHERE h.hgnc_id = '",
                          hgnc_row$hgnc_id,"' and e.entrez_id = '",
                          hgnc_row$entrez_id,
                          "' MERGE (h) -[r:HAS_ENTREZ_DATA]->(e) RETURN r",
                          sep=""
    )
    res <- execute_cypher_command(relationship)
    return (res)
  }
}

# HGNC core data -------------------------------------------------------------

process_hgnc_gene_data <- function(hgnc_row){
  #print(hgnc_row)
  merge <- paste("MERGE (h:HGNC{ hgnc_id:'",
                 hgnc_row$hgnc_id, "', symbol:'",
                 hgnc_row$symbol, "', name:'",
                 hgnc_row$name,"', locus_group:'",
                 hgnc_row$locus_group, "', locus_type:'",
                 hgnc_row$locus_type, "', location:'",
                 hgnc_row$location,"', alias_symbol:'",
                 hgnc_row$alias_symbol,"', alias_name:'",
                 hgnc_row$alias_name,"'}) return h.hgnc_id",
                 sep=""
                 )
  res <- execute_cypher_command(merge)
  process_gene_family_data(hgnc_row)
  process_entrez_data(hgnc_row)
  process_ensembl_data(hgnc_row)
  process_refseq_data(hgnc_row)
  process_cosmic_data(hgnc_row)
  process_uniprot_data(hgnc_row)
  process_omim_data(hgnc_row)
}

# Gene Family Data --------------------------------------------------------
process_gene_family_data <- function(hgnc_row) {
  merge <- paste("MERGE (gf:GeneFamily { gene_family_id:'",
                 hgnc_row$gene_family_id,"', gene_family:'",
                 hgnc_row$gene_family,"'}) return gf.gene_family_id",
                 sep="")
  res <- execute_cypher_command(merge)
  #create relationship from gene to gene family
  relationship <- paste("MATCH (h:HGNC), (gf:GeneFamily) WHERE h.hgnc_id = '",
                       hgnc_row$hgnc_id,"' and gf.gene_family_id = '",
                        hgnc_row$gene_family_id,
                        "' MERGE (h) -[r:HAS_GENE_FAMILY]->(gf) RETURN r",
                        sep=""
  )
  res <- execute_cypher_command(relationship)
}

generate_hgnc_df <- function(tsv_file_path) {
  df <- read_tsv(tsv_file_path, col_names = TRUE, guess_max = 4,
                 col_types = list(
                   .default = col_character(),
                   "hgnc_id" = col_character(),
                   "symbol" = col_character(),
                   "name" = col_character(),
                   "locus_group" = col_character(),
                   "locus_type" = col_character(),
                   "location" = col_character(),
                   "alias_symbol" = col_character(),
                   "alias_name" = col_character(),
                   "gene_family" = col_character(),
                   "gene_family_id" = col_character(),
                   "entrez_id" = col_character(),
                   "ensembl_gene_id" = col_character(),
                   "refseq_accession" = col_character(),
                   "uniprot_ids" = col_character(),
                   "cosmic" = col_character(),
                   "omim_id" = col_character(),
                   "orphanet" = col_character(),
                   "pseudogene.org" = col_character(),
                   "iuphar" = col_character(),
                   "cd" = col_character(),
                   "lncrnadb" = col_character(),
                   "homeodb" = col_character(),
                   "imgt" = col_character(),
                   "intermediate_filament_db" = col_character(),
                   "bioparadigms_slc" = col_character(),
                   "mirbase" = col_character(),
                   "mamit-trnadb" = col_character(),
                   "horde_id" = col_character(),
                   "enzyme_id" = col_character()
                 )
                 ) %>% 
    filter(status == "Approved") %>% 
    select(hgnc_id, symbol, name, locus_group, locus_type, location,
           alias_symbol, alias_name, gene_family, gene_family_id,
           entrez_id, ensembl_gene_id, refseq_accession, uniprot_ids,
           cosmic, omim_id
           )
#  log_info(paste("Read ", nrow(df), " from tsv file: ", tsv_file_path, sep =""))
  return (df)
}

