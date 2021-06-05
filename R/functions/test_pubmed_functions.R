#'
#' Script name: test_pubmed_functions.R
#'
#' Purpose of script:
#'
#' Author:Fred Criscuolo
#'
#' Date Created: 2021-05-09
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
source(here::here("R/functions/init_environment.R"))

test_all_pubmed_functions <- function(pubmed_id) {
  doc <- fetch_pubmed_xml_doc(pubmed_id,TRUE)
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
  #Referencestest
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

test_xml_nodes_present <- function(node_name){

  doc <- xmlParse(file = here::here("tmp/18725932.xml"))
  nodes <- as_tibble(xmlToDataFrame(nodes = getNodeSet(doc,"//Article" ) ))
  node_names = names(nodes)
  present <- if(node_name %in% names(nodes)) TRUE else FALSE
  print(paste("Node: ",node_name," presence = ",present,sep=""))
  return (present)
}

fetch_test_pubmed_xml <- function() {
  return (xmlParse(file = here::here("./tmp/30296429.xml")))
}


test_node_exists <- function(pubmed_id,child_node_name, reference_node_name = "//Article"){
  doc <- fetch_pubmed_xml_doc(pubmed_id,TRUE)
  #return (node_exists(doc, child_node_name, reference_node_name))
  nodes = getNodeSet(doc,reference_node_name)
  print(nodes)
  nodes <- as.tibble(xmlToDataFrame(nodes = getNodeSet(doc,reference_node_name ) ))
  print(names(nodes))
  present <- if(child_node_name %in% names(nodes)) TRUE else FALSE
  #print(paste("Node: ",node_name," present = ",present,sep=""))
  return (present)
}

test_pubmed_authors <- function(){
  #doc <- xmlParse(file = here::here("tmp/20205784.xml"))
  pm_id <- '20205784'
  name_tibble <- tibble(LastName = character(),
                        ForeName = character(),
                        Initials = character()) 
  doc <- fetch_pubmed_xml_doc(pubmed_id,TRUE)
  nodes <- getNodeSet(doc,"//Author")
  for (i in 1: length(nodes)) {
    children <- xmlChildren(nodes[[i]])
    LastName  <-  xmlValue(children$LastName)
    ForeName <- xmlValue(children$ForeName)
    Initials <-  xmlValue(children$Initals)
    name_tibble[nrow(name_tibble) + 1,] <- list(LastName, ForeName, Initials)
  } 
  authors <- name_tibble %>% 
    mutate(id = digest2int(paste(LastName, ForeName, Initials,sep=""),0L)) %>% 
    mutate(pubmed_id = pm_id)
  print(authors)
  return (authors)
}

process_author_node <- function(node, df) {
  children <- xmlChildren(node[[1]])
  lname  <-  xmlValue(children$LastName)
  fname <- xmlValue(children$ForeName)
  init <-  xmlValue(children$Initals)
  df[nrow(df)+1,] <- list(lname, fname, init)
  print(df)
  return (df)
}

test_pubmed_authors_tidy <- function(pubmed_id = "20205784"){
  authors <- tibble(LastName = character(),
                        ForeName = character(),
                        Initials = character()) 
  doc <- fetch_pubmed_xml_doc(pubmed_id,TRUE)
  nodes <- getNodeSet(doc,"//Author")
   nodes %>% 
    process_author_node(.,authors) %>% 
    #add_row(authors,.$lname, .$fname, .$init) %>% 
     mutate(id = digest2int(paste(LastName, ForeName, Initials,sep=""),0L)) %>% 
     mutate(pubmed_id = pm_id)
   print(authors)
   return (authors)
   
}

test_all_pubmed_functions("18725932")
n <- doc["//Pagination"]

mock_db_load <- function(content,pubmed_id){
  content %>% 
    map(print(paste("pubmed id: ", pubmed_id, " content ",., sep = "")))
  
}


test_resolve_pubmed_keywords <- function(doc){
  pubmed_id <- resolve_pubmed_id(doc)
  l <-  getNodeSet(doc,"//Keyword") %>%
     map(xmlValue) 
  return(unlist(l))
}


test_fetch_keywords <- function() {
  doc <- fetch_test_pubmed_xml()
  print(test_resolve_pubmed_keywords(doc))
}

resolve_id <- function(node){
  link <- xmlChildren(node[[1]]$Id)
  return (xmlValue(link))
}

# Test getting cited by data from NCBI ------------------------------------
test_fetch_cited_by <- function(pubmed_id) {
  base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin&id=PUBMED_ID&&tool=my_tool&email=my_email@example.com"
  df <- tibble(cite_id = character())
  url <- stringr::str_replace(base_url, 'PUBMED_ID', pubmed_id)
  UA <- "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2227.0 Safari/537.36"
  doc <- GET(url, user_agent(UA))
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

test_update_top_articles <- function(top_articles, pm_id, cited_by_count) {
  # identify the current article with the lowest cited by count
  x <- tib[which.min(top_articles$count),]
  print(x)
  if (x$count[1] < cited_by_count) {
    top_articles <- rows_update(top_articles, tibble(id=x$id[1],pubmed_id=pm_id,
                                                     count= cited_by_count),
                                by="id")
    # print(paste("Top articles id: ", x$id[1], " pubmed id: ",
    #             pm_id, " count: ", cited_by_count))
  }
  return (top_articles)
}


# Function to maintain a list of the top n cited PubMed entries
# input is the number of entries to maintain
test_top_pubmeds <- function(n=500) {
  source(here::here("R/utilities/select_top_cited_articles.R"))
  id <- seq(1:n)
  pubmed_id <- rep("XXXX", times = n)
  count <-  rep( 0, times = n)
  top_articles <-  tibble(id, pubmed_id, count)
  csv_file <- here::here("protected_data/metadata_sample.csv")
  # accept default count (i.e. Inf)
  pubmed_ids <- extract_pubmed_ids_from_csv(csv_file)
  for (i in 1:nrow(pubmed_ids)){
    # determine the cited by count for this paper
    pubmed_id <-  as.character(pubmed_ids$pubmed_id[i])
    print(paste("Processing pubmed_id: ", pubmed_id, sep=""))
    df <-  fetch_cited_by((pubmed_id))
    top_articles <- test_update_top_articles(top_articles, pubmed_id, nrow(df))
    Sys.sleep(0.4)  # limit rate of requests sent to NCBI
  }
  out_file_path <- here::here(paste("data/top_",n,"_articles.csv", sep=""))
  cited_by <- top_articles %>% 
    arrange(desc(count))
  write.csv(cited_by, out_file_path)
}
