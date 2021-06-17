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
#' Notes: 
#' 1. Functions are dependent upon the global environment established by
#'    the init_environment.R script  
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


# PubMed Properties -------------------------------------------------------
#' Function to extract the basic properties of a PubMed node
#' This is used to process  PubMed entries that
#' are fetched from NCBI
resolve_pubmed_node_properties <- function(doc,pubmed_id, level) {
  doi <- resolve_article_doi(doc)
  title <- resolve_pubmed_article_title(doc)
  abstract <- resolve_pubmed_abstract(doc)
  level = level
  return (tibble(pubmed_id=pubmed_id, doi = doi, title = title, abstract = abstract,
                 level = level))
}

#' Function to dynamically resolve the pubmed id from the document
resolve_pubmed_id <- function(doc){
  id <- xmlValue(getNodeSet(doc,"//PMID")[[1]])
  return (id)
}

#'Function to resolve the article's DOI 
resolve_article_doi <- function(doc) {
  nodes <- getNodeSet(doc, "//ArticleId")
  if ( length(nodes) > 0) {
    for (i in 1:length(nodes)) {
      attrs <- xmlAttrs(nodes[[i]])
      if (attrs["IdType"] == "doi") {
        return (xmlValue(nodes[[i]]))
      }
    }
  }
  return("")
}

#' Function to return a PubMed document's abstract text value(s)
#' PubMed abstract texts can contain special characters that affect
#' loading into Neo4j databases. They need to be filtered out
resolve_pubmed_abstract <- function(doc) {
  text <- ""
  str <- resolve_xml_node_by_name(doc, "//AbstractText") %>% 
    paste(text,.,sep=" ")
  # filter out special characters that break neo4j loads
  abstract <- str_replace_all(str, "[^[:alnum:]]", " ") %>% str_replace_all(.,"[ ]+", " ") 
  return(abstract)
}

#' Function to return a tibble of a PubMed document's article title
resolve_pubmed_article_title <- function(doc) {
  title <- resolve_xml_node_by_name(doc, "//ArticleTitle")
  return(title)
}

# PubMed References -------------------------------------------------------

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
#' Mutate the tibble to include the pubmed id of the referencing 
#' article. This facilitates creating Neo4j relationships
resolve_pubmed_references <- function(doc) {
  pubmed_id <- resolve_pubmed_id(doc)
  refs <- resolve_xml_node_by_name(doc,"//Reference") 
  if("ArticleIdList" %in% colnames(refs)){
    refs <- rename(refs, ref_pm_id = ArticleIdList)
  }
  if ("Citation" %in% colnames(refs)) {
    refs <- rename(refs, citation = Citation)
    refs <- refs %>% 
      filter(!is_null(pubmed_id) && numbers_only(pubmed_id)) %>% 
    mutate(cited_by_pm = pubmed_id) %>% 
    mutate(citation_id = digest2int(citation,0L))
  }
  return(refs)
}

fetch_referenced_refs <- function(refs, gen_number, results) {
  gen_number <- gen_number +1
  # for testing just select the first five references in a document
  test_refs <-  if (nrow(refs) > 5) refs[1:5,] else refs
  if (gen_number < 4  & nrow(test_refs > 0)) {
    # fetch the referenced documents using the pubmed id number
    test_docs <- 
      test_refs$ArticleIdList %>% 
      map(.,fetch_pubmed_xml_doc) 
    for (i in 1:length(test_docs)) {
      doc <-  test_docs[[i]]
      print(paste("Generation: ", gen_number, "processing referenced document ",
                  resolve_pubmed_id(doc)   ))
      refs2 <- resolve_pubmed_references(doc)
      new_results <- rbind(refs2, results)
      print(paste("Cumulative results = ", nrow(new_results), sep=""))
      fetch_referenced_refs(refs2,gen_number,new_results)
    }
  } else {
    print(paste("Current reference count = ", nrow(results),
                sep = ""))
    return(new_results)
  }
}

# PubMed Article IDs ------------------------------------------------------

#' Function to parse a PubMed entry's Artticle ID list
#' The Article Id for the PubMed Id is not included since it is already the 
#' key in the PubMed node
#' Mutate the resulting data fram to incude the parent pubmed id to faciltate
#' creating a Neo4j relationship
#' Only the first set of Article IDs is processed to avoid picking up the
#' article ids for referenced articles
resolve_article_id_list <- function(doc) {
  pubmed_id <-  resolve_pubmed_id(doc)
  df <- tibble(id = character(),
               id_type = character(),
               pubmed_id = character()
          )
  article_list_node <- getNodeSet(doc,"//PubmedData//ArticleIdList")
  nodes <- xmlChildren(article_list_node[[1]])  # Article IDs in the 1st ArticleIdList
  if (!is_null(nodes) && length(nodes) > 0){
    for (i in 1:length(nodes)) {
      id_type <- xmlGetAttr(nodes[[i]],"IdType")
        id <-  xmlValue(nodes[[i]])
        df[nrow(df) +1,] <- list(id, id_type, pubmed_id)
    }
  }
  df <- df %>% 
    filter(.$id_type != 'pubmed')
  return(df)
}

# Keywords ----------------------------------------------------------------

resolve_pubmed_keywords <- function(doc){
  pubmed_id <- resolve_pubmed_id(doc)
  df <- tibble("keyword" = character(),
                "pubmed_id" = character())
  nodes <- getNodeSet(doc,"//Keyword")
  if (!is_null(nodes) && length(nodes) > 0) {
    for ( i in 1:length(nodes)) {
      keyword_node <- xmlChildren(nodes[[i]])[[1]]
      keyword <- trimws(xmlValue(keyword_node))
      df[nrow(df) + 1,] <- list(keyword,pubmed_id)
    }
  }
  return (df)
}

# Mesh Headings -----------------------------------------------------------


#' Function to parse a PubMed entry in XML format to
#' resolve the Mesh heading descriptors and qualifiers
#' Mutate the data frame to include the pubmed id for the
#' current article. This facilitates creating a Neo4j relationship
#' between the mesh heading and the article
resolve_mesh_headings <- function(doc) {
  pubmed_id <- resolve_pubmed_id(doc)
  df <- tibble(
    descriptor_key = character(),
    descriptor_name = character(),
    qualifier_key = character(),
    qualifier_name = character(),
    pubmed_id = character()
  )
  nodes <-  getNodeSet(doc, "//MeshHeading")
  if (!is_null(nodes) && length(nodes) > 0) {
    for (i in 1:length(nodes)) {
      # process the descriptor
      descriptor_node <-  xmlChildren(nodes[[i]])[[1]]
      des_name <- xmlValue(descriptor_node)
      des_ui <- xmlGetAttr(descriptor_node, "UI")
      # process the qualifier name if there is one
      qual_name <- NA
      qual_ui <- NA
      if (xmlSize(nodes[[i]]) > 1) {
        qualifier_node <- xmlChildren(nodes[[i]])[[2]]
        qual_name <- xmlValue(qualifier_node)
        qual_ui <- xmlGetAttr(qualifier_node, "UI")
      }
      df[nrow(df) + 1, ] <- list(des_ui, des_name, qual_ui,
                                 qual_name, pubmed_id)
    }
  }
  return (df)
}

# PubMed Authors ----------------------------------------------------------

#' Function to return a tibble of a PubMed document's authors
#' Add an id column based on a checksum of the author's name components.
#' Used to facilitate identifying the same author in another 
#' paper
#' n.b. Affiliation is not included because an author may have >1 affiliations
#'      or an author may change their affiliation
resolve_pubmed_authors <- function(doc) {
  pm_id <- resolve_pubmed_id(doc)
  name_tibble <- tibble(LastName = character(),
                        ForeName = character(),
                        Initials = character())
  
  nodes <- getNodeSet(doc, "//Author")
  if (!is_null(nodes) && length(nodes) > 0) {
    for (i in 1:length(nodes)) {
      children <- xmlChildren(nodes[[i]])
      LastName  <-  xmlValue(children$LastName)
      ForeName <- xmlValue(children$ForeName)
      Initials <-  xmlValue(children$Initals)
      name_tibble[nrow(name_tibble) + 1, ] <-
        list(LastName, ForeName, Initials)
    }
  }
  authors <- name_tibble %>%
    mutate(id = digest2int(paste(LastName, ForeName, Initials, sep = ""), 0L)) %>%
    mutate(pubmed_id = pm_id)
  
  return (authors)
}

# PubMed Journal ----------------------------------------------------------

#' Function to return attributes for the article's journal
#' 
  resolve_pubmed_article_journal<- function(doc) {
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
  # Pagination data may be missing
  pgs <- ""
  pgn_nodes <- doc["//Pagination"]
  if(length(pgn_nodes)>0) {
    #pagination_node <- getNodeSet(doc,"//Pagination")[[1]]
    pgs <-  xmlValue(xmlChildren(pgn_nodes[[1]])$MedlinePgn)
  }
  df[1,] <- list(key,vol, is, year, month, pgs)
}
  return (df)
}



 



