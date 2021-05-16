#'
#' Script name: pubmed.functions.R
#'
#' Purpose of script: A collection of functions supporting processing
#' of PubMed XML documents retrieved from NCBI using the Entrez API
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
#' Notes: This script should only be sourced after all required packages have been loaded
#'        by the packages.R script
#'   
#'
#' ---------------------------


#' Function to retrieve a PubMed entry specified by a pubmed id from Entrez
#' and parse it into an R XML document
#' The retrieved XML document can be saved into the tmp folder by
#' setting the save_xml parameter to TRUE. 
#' Default value is specified in app_default.properties file
#' 
fetch_pubmed_xml_doc <- function(pubmed_id, save_xml = props$save.pubmed.xml.default) {
  xml_res <- entrez_fetch(db="pubmed", id=pubmed_id, rettype = "xml")
  if (save_xml) {
    write_temp_xml_file(xml_res, pubmed_id)
  }
  doc <- xmlParse(xml_res)
  return(doc)
}

#' Function that will return a tibble of specified XML nodes from a supplied XML document
resolve_xml_node_by_name <- function(doc,node_name) {
  nodes <- as_tibble(xmlToDataFrame(nodes = getNodeSet(doc,node_name ) ))
  return(nodes)
}

#' Function that will output an XML document to the project's temp folder
#' The filename is determined by the pubmed id
write_temp_xml_file <- function(pubmed_xml, pubmed_id) {
  file_name <- paste(as.character(pubmed_id), ".xml", sep = "")
  fileConn <- file(here::here("tmp",file_name))
  writeLines(pubmed_xml, fileConn)
  close(fileConn)
}

#' Function to dynamically resolve the pubmed id from the document
resolve_pubmed_id <- function(doc){
  id <- xmlValue(getNodeSet(doc,"//PMID")[[1]])
  return (id)
}

#' Function that will retrieve PubMed entries, in XML format
#' based on a supplied list of pubmed ids
#' Primary purpose is to retrieve PubMed entries specified as a reference
#' in another PubMed entry
fetch_referenced_documents <- function(references) {
  docs <- 
    references$ArticleIdList %>% 
    map(.,fetch_pubmed_xml_doc) 
}

#' Function to return a tibble of a PubMed document's references
#' Mutate the tibble to include the pubMed id of the referencing 
#' article. This facilitates creating Neo4j relationships
resolve_pubmed_references <- function(doc) {
  pubmed_id <- resolve_pubmed_id(doc)
  refs <- resolve_xml_node_by_name(doc,"//Reference") %>% 
    mutate(CitedBy = pubmed_id)
  return(refs)
}

#'Function to resolve the article's DOI 
resolve_article_doi <- function(doc){
  nodes <- getNodeSet(doc,"//ArticleId")
 for (i in 1:length(nodes)) {
  attrs <- xmlAttrs(nodes[[i]])
  if (attrs["IdType"] == "doi") {
    return (xmlValue(nodes[[i]]))
  }
 }
    return("")
}

#' Function to parse a PubMed entry in XML format to
#' resolve the Mesh heading descriptors and qualifiers
#' Mutate the data frame to include the pubmed id for the
#' current article. This facilitates creating a Neo4j relationship
#' between the mesh heading and the article
resolve_mesh_headings <- function(doc){
  pubmed_id <- resolve_pubmed_id(doc)
  df <- tibble(descriptor_key = character(),
                   descriptor_name = character(),
                   qualifier_key = character(),
                   qualifier_name = character(),
                   pubmed_id = character()
                   )
  nodes <-  getNodeSet(doc,"//MeshHeading")
  if (length(nodes) > 0 ) {
  for (i in 1: length(nodes)){
    # process the descriptor
    descriptor_node <-  xmlChildren(nodes[[i]])[[1]]
     des_name <- xmlValue(descriptor_node)
     des_ui <- xmlGetAttr(descriptor_node, "UI")
     # process the qualifier name if there is one
     qual_name <- NA
     qual_ui <- NA
     if( xmlSize(nodes[[i]]) > 1) {
       qualifier_node <- xmlChildren(nodes[[i]])[[2]]
       qual_name <- xmlValue(qualifier_node)
       qual_ui <- xmlGetAttr(qualifier_node,"UI")
     }
     df[nrow(df) + 1,] <- list(des_ui, des_name, qual_ui,
                               qual_name, pubmed_id)
  }
}
   return (df)
}

#' Function to return a tibble of a PubMed document's authors
#' Add an id column based on a checksum of the author's name components.
#' Used to facilitate identifying the same author in another 
#' paper
#' n.b. Affiliation is not included because an author may have >1 affiliations
#'      or an author may change their affiliation
resolve_pubmed_authors <- function(doc) {
  pm_id <- resolve_pubmed_id(doc)
  authors <- resolve_xml_node_by_name(doc,"//Author") %>% 
    mutate(id = digest2int(paste(LastName, ForeName, Initials,sep=""),0L)) %>% 
    mutate(pubmed_id = pm_id)
  return(authors)
}

#' Function to return a PubMed document's abstract text
#' value(s)
resolve_pubmed_abstract <- function(doc) {
  text <- ""
  abstract <- resolve_xml_node_by_name(doc, "//AbstractText") %>% 
    paste(text,.,sep=" ")
    
  return(abstract[[1]])
}

#' Function to return a tibble of a PubMed document's article title
resolve_pubmed_article_title <- function(doc) {
  title <- resolve_xml_node_by_name(doc, "//ArticleTitle")
  return(title)
}

#' Function to return attributes for the article's journal
#' 
resolve_pubmed_article_journal <- function(doc) {
  journal <- tibble(pubmed_id = character(),
               journal_issn = character(),
                   journal_title = character(),
                   journal_iso_abbrev = character())
  pm_id <- resolve_pubmed_id(doc)
  journal_node <- getNodeSet(doc, "//Journal")[[1]]
  issn <- xmlValue(xmlChildren(journal_node)$ISSN)
  title <- xmlValue(xmlChildren(journal_node)$Title)
  abbrev <- xmlValue(xmlChildren(journal_node)$ISOAbbreviation)
  journal[1,] <- list(pm_id,issn, title, abbrev)
  issue <- resolve_article_journal_issue(doc)
  df <- dplyr::bind_cols(journal,issue) %>% 
    mutate(issue_key = paste(journal_issn,issue_key, sep=":"))
  return (df)
}

resolve_article_journal_issue <- function(doc){
  df <- tibble( issue_key = character(),
                  volume = character(),
                   issue = character(),
                    year = integer(),
                    month = character(),
                   pgn = character()
                   )
  journal_issue_node <- getNodeSet(doc,"//JournalIssue")[[1]]
  
  if (!is.null(journal_issue_node) ) {
  vol <-  xmlValue(xmlChildren(journal_issue_node)$Volume)
  is <- xmlValue(xmlChildren(journal_issue_node)$Issue)
  key <- paste(vol,is, sep=":")
  pub_date_node <- xmlChildren(journal_issue_node)$PubDate
  if (!is.null(pub_date_node)){
  year <- as.integer(xmlValue(xmlChildren(pub_date_node)$Year))
  key <- paste(key,year, sep=":")
  month <- xmlValue(xmlChildren(pub_date_node)$Month)
  }
  pagination_node <- getNodeSet(doc,"//Pagination")[[1]]
  if(!is.null(pagination_node)){
  pgs <-  xmlValue(xmlChildren(pagination_node)$MedLinePgn)
  }
  df[1,] <- list(key,vol, is, year, month, pgs)
}
  return (df)
}

 
test_pubmed_functions <- function(pubmed_id) {
  doc <- fetch_pubmed_xml_doc(pubmed_id,props$save.pubmed.xml.default)
  print(paste("PubMed id:", pubmed_id, sep = " "))
  print(paste("DOI: ", resolve_article_doi(doc)))
  title <- resolve_pubmed_article_title(doc)
  print(paste("title:",title, sep = " "))
  journal <- resolve_pubmed_article_journal(doc)
  print("Journal: ")
  print(resolve_pubmed_article_journal(doc))
  print("Journal Issue:")
  print(resolve_article_journal_issue(doc))
  abstract <- resolve_pubmed_abstract(doc)
  print("Abstract(s):")
  abstract %>% 
    print(.)
  # test resolving mesh headings
  print("Mesh Headings:")
  resolve_mesh_headings(doc) %>%
    print(.)
  #Authors
  print("Authors:")
  resolve_pubmed_authors(doc) %>% 
    print(.)
  #References
  print("References:")
  refs <-  resolve_pubmed_references(doc)
  print(refs)
  # test retrieving pubmed entries for referenced articles
  # only the first four for testing
  test_refs <-  if (length(refs > 4)) refs[1:4,] else refs
  test_docs <- test_refs$ArticleIdList %>% 
    fetch_pubmed_xml_doc(.,FALSE) 
  test_docs %>% resolve_pubmed_article_title(.)
}




