#'
#' Script name: utilities.R
#'
#' Purpose of script: General utility functions for the application
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
#'   
#'
#' ---------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)

#' Function that will process a csv file of PebMed entires and return
#' a tibble of PubMed ids
#' The csv file is required to a column namsed pubmed_id

extract_pubmed_ids_from_csv <- function(csv_file_path, row_count = Inf) {
  # Accept function defaults
  # Include col_names = TRUE (default) to document header requirement
  pubmed_id_list <- read_csv(csv_file_path,col_names = TRUE, n_max = row_count) %>% 
    select(pubmed_id)
  print(paste("Read ", nrow(pubmed_id_list), " from csv file: ", csv_file_path, sep =""))
  return (pubmed_id_list)
}

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

node_count <- function(doc, xpath) {
  return (length(unlist(xpathApply(doc, xpath, saveXML))))
}
 
#' Function that determines if a specified child node exists under a specified parent node
node_exists <- function(doc,child_node_name, reference_node_name = "//Article"){
  nodes <- as_tibble(xmlToDataFrame(nodes = getNodeSet(doc,reference_node_name ) ))
  present <- if(child_node_name %in% names(nodes)) TRUE else FALSE
  #print(paste("Node: ",node_name," present = ",present,sep=""))
  return (present)
}

#' Function that will output an XML document to the project's temp folder
#' The filename is determined by the pubmed id
write_temp_xml_file <- function(pubmed_xml, pubmed_id) {
  file_name <- paste(as.character(pubmed_id), ".xml", sep = "")
  fileConn <- file(here::here("tmp",file_name))
  writeLines(pubmed_xml, fileConn)
  close(fileConn)
}

test_extract_pubmed_ids_from_csv <- function(){
  csv_file_path <- here::here("protected_data/metadata_sample.csv")
  pubmed_id_list <- extract_pubmed_ids_from_csv(csv_file_path)
 pubmed_id_list
}