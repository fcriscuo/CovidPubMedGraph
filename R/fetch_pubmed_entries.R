#'
#' Script name: FetchPubMedEntries.R
#'
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


extract_pubmed_ids_from_csv <- function(csv_file_path, row_count = Inf) {
  # Accept function defaults
  # Include col_names = TRUE (default) to document header requirement
  pubmed_id_list <- read_csv(csv_file_path,col_names = TRUE, n_max = row_count) %>% 
    select(pubmed_id)
  print(paste("Read ", nrow(pubmed_id_list), " from csv file: ", csv_file_path, sep =""))
  return (pubmed_id_list)
}



#' Function to retrieve a PubMed entry identified by its pubmed_id and load its 
#' properties into the Neo4j databse
#' The level parameter represents the distance from Covid entries (level 1)
load_pubmed_entry <- function(pubmed_id, level) {
  if (!pubmed_node_exists(pubmed_id)){
    doc <- fetch_pubmed_xml_doc(pubmed_id,FALSE)
    #PubMed node
    print("PubMed")
    load_pubmed_node(resolve_pubmed_node_properties(doc,level))
    #Article IDs
    if (node_count(doc,"//PubmedData/ArticleIdList") > 0) {
      print("Article Ids")
      merge_article_ids(resolve_article_id_list(doc))
    }
    #Author node(s)
  
    if (node_count(doc,"//Author") > 0) {
      print("Authors")
      load_authors(resolve_pubmed_authors(doc))
    }
    #Mesh heading nodes
    
     if (node_count(doc,"//MeshHeadingList") > 0) {
       print("MeshHeadings")
      load_mesh_headings(resolve_mesh_headings(doc))
    }
    #Journal
    if( node_count(doc,"//Journal") > 0){
      print("Journal")
      load_journal_data(resolve_pubmed_article_journal(doc))
    }
    #References
    if(node_count(doc,"//Reference") > 0){
      print("References")
        load_reference_citations(resolve_pubmed_references(doc))
    }
  }
}
