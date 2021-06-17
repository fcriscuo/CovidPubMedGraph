#'
#' Script name: fetch_pubmed_enties.R
#' Purpose of script:
#'
#' Author:Fred Criscuolo
#'
#' Date Created: 2021-05-05
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



# PubMed CSV file ---------------------------------------------------------

extract_pubmed_ids_from_csv <- function(csv_file_path, row_count = Inf) {
  # Accept function defaults
  # Include col_names = TRUE (default) to document header requirement
  pubmed_id_list <- read_csv(csv_file_path,col_names = TRUE, guess_max = 4,
                             col_types = list(
                               "pubmed_id" = col_character()),
                             n_max = row_count) %>% 
    select(pubmed_id) 
  log_info(paste("Read ", nrow(pubmed_id_list), " from csv file: ", csv_file_path, sep =""))
  return (pubmed_id_list)
}


# Cited-By functions ------------------------------------------------------

#' Function to query NCBI for PubMed articles that cite the article 
#' identified by the specified PubMed id
#' Returns a tibble of the cited-by pubmed ids

fetch_cited_by_pubmed_ids <- function(pubmed_id){
  base_url <- paste(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin&id=PUBMED_ID&&tool=my_tool&email=",ncbi_email,"&api_key=",ncbi_api_key,sep = "")
  df <- tibble(cite_id = character())
  url <- stringr::str_replace(base_url, 'PUBMED_ID', as.character(pubmed_id))
  UA <- "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2227.0 Safari/537.36"
  doc <- tryCatch(
    {GET(url, user_agent(UA))} ,
    error = function(cond) {
      message(paste("ERROR getting URL ",url, " PubMed id: ", pubmed_id, sep =""))
      message(cond)
      return (df)
    },
    warning = function(cond){
      message(paste("Warning getting URL ",url, "PubMed id: ", pubmed_id, sep =""))
      message(cond)
      return (df)
    },
    finally = {}
  )
  data <- XML::xmlParse(content(doc, "text"))
  nodes <- getNodeSet(data,'//LinkSetDb')
  links <- xmlChildren(nodes[[1]])
  for (i in 1: length(links)){
    link <- xmlChildren(links[[i]])$Id
    novel_id <-   xmlValue(link)
    if (!is.na(novel_id)) {
      df[nrow(df) +1,] <- novel_id
    }
  }
  return (df)
}

#' Function to associate a PubMed article with PubMed articles that cite it
#' If nodes for citing PubMed articles do not exists in the database,
#' basic PubMed nodes without childern are created.
#' A CITED_BY relationship between the origin PubMed node and the citing 
#' PubMed node is created
#' The cited_by_count property in the origin PubMed node is incremented
load_cited_by_articles <- function(count = .Machine$integer.max) {
  all_pubmed_ids <-  find_all_pubmed_ids(count)
  log_info(paste("Found ", nrow(all_pubmed_ids), " PubMed nodes in the database"))
  for (i in 1:nrow(all_pubmed_ids)) {
    pubmed_id <- all_pubmed_ids$value[i]
    if (get_cited_by_count(pubmed_id) == 0) {
      cited_by_ids <- fetch_cited_by_pubmed_ids(all_pubmed_ids$value[i])
      set_cited_by_count(pubmed_id, nrow(cited_by_ids))
      log_info(paste(
        "CITED_BY_COUNT property for PubMed node: ",
        pubmed_id,
        " = ",
        nrow(cited_by_ids),
        sep = ""
      ))
      for (j in 1:nrow(cited_by_ids)) {
        cited_by_id <- cited_by_ids$cite_id[j]
        # is this PubMed id already in the database?
        if (!any(all_pubmed_ids$value == cited_by_id)) {
          doc <-  fetch_pubmed_xml_doc(cited_by_id, FALSE)
          load_pubmed_node(resolve_pubmed_node_properties(doc, cited_by_id, 0))
        }
        # Create a CITED_BY relationship between original PubMed node and the
        # PubMed node that cited it
        load_cited_by_pubmed_rel(pubmed_id, cited_by_id)
        
      }
    }
  }
}


# PubMed Entry ------------------------------------------------------------

#' Function to retrieve a PubMed entry identified by its pubmed_id and load its 
#' properties into the Neo4j databse
#' The level parameter represents the distance from Covid entries (level 1)
load_pubmed_entry <- function(pubmed_id, level) {
  if (!numbers_only(pubmed_id)) {
    log_info(paste("load_pubmed_entry: ",pubmed_id, " is an invalid PubMed Id"))
    return()
  }
  if (!pubmed_node_exists(pubmed_id)) {
      doc <- fetch_pubmed_xml_doc(pubmed_id, FALSE)
      if(node_count(doc, "//PMID") <1){
        log_info(paste("load_pubmed_entry: Invalid XML document for ",pubmed_id, sep=""))
        return()
      }
      #PubMed node
      log_info(paste("Creating PubMed node: ", pubmed_id, " level ", level, sep =
                       ""))
      load_pubmed_node(resolve_pubmed_node_properties(doc, pubmed_id, level))
      #Article IDs
      if (node_count(doc, "//PubmedData/ArticleIdList") > 0) {
        merge_article_ids(resolve_article_id_list(doc))
      }
      #Author node(s)
      if (node_count(doc, "//Author") > 0) {
        load_authors(resolve_pubmed_authors(doc))
      }
      #Mesh heading nodes
      if (node_count(doc, "//MeshHeadingList") > 0) {
        #print("MeshHeadings")
        load_mesh_headings(resolve_mesh_headings(doc))
      }
      #Journal
      if (node_count(doc, "//Journal") > 0) {
        load_journal_data(resolve_pubmed_article_journal(doc))
      }
      #Keyword
      if (node_count(doc, "//KeywordList") > 0) {
        load_keywords(resolve_pubmed_keywords(doc), pubmed_id)
      }
      #References
      if (node_count(doc, "//Reference") > 0) {
        load_reference_citations(resolve_pubmed_references(doc))
      }
      # confirm that new pubmed entry is in database
      log_info(paste(
        "new PubMed node: ",
        pubmed_id,
        " loaded = ",
        pubmed_node_exists(pubmed_id),
        sep = ""
      ))
  } else {
    log_info("PubMed ID: {pubmed_id} at level: {level} is already loaded")
  }
  return()
}

