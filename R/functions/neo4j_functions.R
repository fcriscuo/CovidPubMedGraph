#'
#' Script name: neo4j_functions
#'
#' Purpose of script: Collection of functions that interact with the Neo4j 
#' database.
#'
#' Author:Fred Criscuolo
#'
#' Date Created: 2021-05-12
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
#' 

# Neo4j Connection --------------------------------------------------------

neo4j_user  <-  Sys.getenv("NEO4J_USER")
neo4j_password <- Sys.getenv("NEO4J_PASSWORD")
con <- neo4j_api$new(
  url = "http://localhost:7474",
  db = "neo4j",
  user = neo4j_user, 
  password = neo4j_password
)

# Utility Functions -------------------------------------------------------

#' COVID-19 csv file processing
#' Create new PubMed/Covid nodes for PubMed entries in a csv formatted document
load_primary_pubmed_nodes_from_csv <- function(filename) {
  create <-  paste('LOAD CSV WITH HEADERS FROM "file:', filename,
                   '" AS line CREATE (:PubMed:Covid {pubmed_id: line.pubmed_id,
                   title: line.title, doi: line.doi, abstract: line.abstract});'
                   ,sep ="") 
  execute_cypher_command(create)
  # set the level of these new nodes
  set_level <- "MATCH (c:Covid) SET c.level = 1 RETURN c.pubmed_id, c.level"
  execute_cypher_command(set_level)
}

#' clear Neo4j database
clear_neo4j_database <- function() {
  delete <- "MATCH (n) DETACH DELETE (n)"
  execute_cypher_command(delete)
  log_info("Existing NEO4J database cleared")
}

#' Generic Neo4j Cypher command execution
execute_cypher_command <- function(query) {
  result <- query %>% call_neo4j(con)
  return (result)
}

#' Match all nodes with Covid label
match_covid_ids <- function() {
  match <- 'match (c:Covid) return c.pubmed_id ;'
  ids <- execute_cypher_command(match)
  return (ids)
}

#' Match a specified PubMed node
match_pubmed_by_id <- function(pubmed_id) {
  query <- paste("MATCH (pubmed:PubMed {pubmed_id: '", pubmed_id,
                 "'}) return pubmed", sep = "")
  #print(query)
  pubmed <- execute_cypher_command(query)
  return (pubmed)
}

#' Find the pubmed_id value for all PubMed nodes in the database
#' Support an optional limit for testing

find_all_pubmed_ids <- function(count = .Machine$integer.max ) {
  query <- paste("MATCH (p:PubMed) return p.pubmed_id LIMIT ",
                 as.integer(count),sep ="")
  res <- execute_cypher_command(query)
  return (res$p.pubmed_id)
}

#' Function to find all the PubMed nodes in the database at a specified level
find_pubmed_ids_by_level <- function(level){
  match <- paste("MATCH (p:PubMed {level: ",level, "} ) return p.pubmed_id")
  res <-  (execute_cypher_command(match))
  ids <- res$p.pubmed_id
 return( as.list(ids))
}

#' Function to determine if a PubMed node exists
pubmed_node_exists <- function(pubmed_id) {
  query <-  paste("OPTIONAL MATCH (p:PubMed{pubmed_id:'", pubmed_id,
                  "'}) RETURN p IS NOT NULL AS PREDICATE", sep = "")
  res <-  execute_cypher_command(query)
  exists <- res$PREDICATE[[1]]
  return (as.logical(exists))
}

# Citations ---------------------------------------------------------------

#'  Article Citations
#'  Merge the Citation node into the database
load_reference_citations <- function(refs) {
  if (nrow(refs)> 0 ){
    for (i in 1:nrow(refs)) {
      merge <- paste("MERGE (c:Citation{ id:",
                     refs$citation_id[i], ", citation:'",
                     refs$citation[i], 
                     "', ref_pubmed_id:'", refs$ref_pm_id[i],
                     "'}) return c.id",sep ="")
   
     res <- execute_cypher_command(merge)
     # create a relationship between the Citation node and the PubMed node
     relationship <- paste("MATCH (p:PubMed), (c:Citation) WHERE p.pubmed_id = '",
                           refs$cited_by_pm[i],"' and c.id = ",
                           refs$citation_id[i],
                           " MERGE (p) -[r:HAS_CITATION]->(c) RETURN r",
                           sep=""
                          )
     res <- execute_cypher_command(relationship)
    }
  }
  return ()
}

#' Function to find all the Citation nodes for a specified PubMed Id
#' Filter for Citations with valid PubMed Ids
find_citations_by_pubmed_id <- function(pubmed_id) {
  query <-"MATCH (p:PubMed{pubmed_id:'PUBMED_ID'})-[:HAS_CITATION]->(c) RETURN p,c"
  query <- str_replace(query,"PUBMED_ID",pubmed_id)
  res <- execute_cypher_command(query)
  return(res$c)
}

#' Create a Relationship between the Citation node and the referenced PubMed node
#' This establishes a connection between two (2) levels of PubMed entries
#' Check for an existing relationship
load_citation_pubmed_rel <- function(citation_id, ref_pubmed_id){
  # if (as.logical(res[[1]])) {
  #   warning(paste("HAS_PUBMED_REF relationship between Citationd Id: ", citation_id,
  #                 " and PubMed Id: ", ref_pubmed_id, " already exixts"))
  #   return ()
  # }
  query <-  paste("MATCH (c:Citation{id: CITATION}), (p:PubMed{pubmed_id:'PUBMED_ID'}) ",
                " MERGE (c) - [r:HAS_PUBMED_REF] -> (p) ",
                " ON CREATE SET r.alreadyExisted=FALSE  ",
                " ON MATCH SET r.alreadyExisted=TRUE  ",
                " RETURN r.alreadyExisted;", sep ="")
  query <- str_replace(query, "CITATION", as.character(citation_id))
  query <- str_replace(query,"PUBMED_ID", ref_pubmed_id)
   return( execute_cypher_command(query))
}

# CITED_BY Relationship ---------------------------------------------------

#' Create a relationship between a PubMed node and another PubMed node that
#' cites the first node as a  reference
#' Update the cited_by_count property in the origin node
#' 
 
load_cited_by_pubmed_rel <- function(pubmed_id, cited_by_id) {
  relationship <- paste("MATCH (p:PubMed), (c:PubMed) WHERE p.pubmed_id = 'PUBMED_ID' ",
                 " AND c.pubmed_id = 'CITED_BY_ID' ",
                 " MERGE (p) -[r:CITED_BY] -> (c) RETURN r", sep ="")
  relationship <- str_replace(relationship, "PUBMED_ID", pubmed_id)
  relationship <- str_replace(relationship, "CITED_BY_ID", cited_by_id)
  log_info(relationship)
  execute_cypher_command(relationship)
# increment cited_by_count property
update <-  paste("MATCH (p:PubMed {pubmed_id:'PUBMED_ID'}) ",
                 " SET p.cited_by_count = p.cited_by_count +1 ",
                 " RETURN p.cited_by_count", sep ="")
update <-  str_replace(update,"PUBMED_ID", pubmed_id)
return(execute_cypher_command(update))
}

#' Reset cited_by_count in specified PubMed node to 0
#' Useful for repeating cited by scans
clear_cited_by_count <- function(pubmed_id) {
  update <- paste("MATCH (p:PubMed{pubmed_id:'PUBMED_ID'}) SET p.cited_by_count = 0")
  update <- str_replace(update,"PUBMED_ID",pubmed_id)
  return (execute_cypher_command(update))
}


# Authors -----------------------------------------------------------------

#' Merge a PubMed Author into the database
merge_pubmed_author <- function(author) {
  query <- paste("MERGE (author:Author {lastname:'", author$LastName,
                 "' , firstname: '", author$ForeName,"', initials:'",
                 author$Initials, "', id:", author$id,"}) return author.id", sep="" )
  # print(query)
  res <-  execute_cypher_command(query)
  # print(res)
  return (as.integer(res$author.id))
}

#' Load all authors for this document into the database
#' Merge author nodes
#' Establish a relationship between the Author nodes and the parent PubMed node
load_authors <- function(authors){
  #result <- vector(mode = "character", length = length(authors))
 for (i in 1: length(authors)){
   pubmed_id <- authors$pubmed_id[i]
   if (!is.na(pubmed_id)) {
   author_id <- merge_pubmed_author(authors[i,])
   merge_rel <- paste("MATCH (p:PubMed), (a:Author) WHERE p.pubmed_id = '", pubmed_id,"' AND a.id = ",author_id, 
          " MERGE (p) -[r:HAS_AUTHOR] ->(a) RETURN r",sep="")
   execute_cypher_command(merge_rel)
   }
 }
    return ()
}

# Keyword nodes -----------------------------------------------------------

#' Merge novel keywords into Neo4j nodes
#' Merge a relationship between a PubMed node and the keyword node
load_keywords <- function(keywords, pubmed_id) {
  for (i in 1:nrow(keywords)){
    merge_node <- paste("MERGE (k:Keyword {keyword:'",
                            keywords$keyword[i], "'}) return k")
    execute_cypher_command(merge_node)
    #create relationship between pubmed & keyword
    merge_rel <-  paste ("MATCH (p:PubMed), (k:Keyword)  WHERE ",
                         " p.pubmed_id = '", pubmed_id,
                         "' AND k.keyword = '", keywords$keyword[i],
                         "' MERGE (p) -[r:HAS_KEYWORD]-> (k) RETURN (r)"
                         ,sep = "")
    execute_cypher_command(merge_rel)
  }
}

# Article ID node ---------------------------------------------------------

#' Function to merge non-pubmed identifiers into the database
#' Creates/merges an ArticleId node and establishes a relationship between
#' that node and the parent PubMed node
#' This relationship has a property: IdType
merge_article_ids <- function(article_ids){
  results <- vector(mode = "character" , length = nrow(article_ids))
  if (nrow(article_ids) > 0){
    for (i in 1:nrow(article_ids)){
      pubmed_id <- article_ids$pubmed_id[i]
      article_id <- article_ids$id[i]
      id_type <- article_ids$id_type[i]
      merge <- paste("MERGE (art_id:ArticleId {article_id:'", 
                     article_id,"'})"
                    ,sep="")
      execute_cypher_command(merge)
      # establish a relationship between pubmed & id
      relationship <- paste("MATCH (p:PubMed), (id:ArticleId) ",
                    "WHERE p.pubmed_id = '", pubmed_id,
                    "' AND id.article_id = '", article_id,
                    "' MERGE (p) - [r:HAS_ID{id_type:'", id_type,
                    "'}] -> (id) RETURN r" 
        ,sep="")
     # print(relationship)
      execute_cypher_command(relationship)
      results[i] <- paste("Processed article id: ", article_id,
                          " for pubmed id ", pubmed_id, sep = "")
    }
  }
  return (results)
}

# Mesh Heading node -------------------------------------------------------

#' Merge a MeshHeading node into the Neo4j database
merge_mesh_heading <- function(mesh){
  merge <- paste("MERGE (mesh:MeshHeading {descriptor_key:'",
                 mesh$descriptor_key, "', descriptor_name:'",
                 mesh$descriptor_name,"' }) return mesh.descriptor_key", 
                 sep = "")
  res <- execute_cypher_command(merge)
  return (as.character(mesh$descriptor_key))
}

#' Load all Mesh headings for a document into the database
#' Establish a relationship between the MeshHeading node and the parent PubMed
#' node
load_mesh_headings <- function(mesh_headings){
  
  # load the mesh headings for this document and establish relationship to document node
  result <-  vector(mode = "character", length = length(mesh_headings))
  for (i in 1:length(mesh_headings)){
    pubmed_id = mesh_headings$pubmed_id[i]
    mesh_key <-  merge_mesh_heading(mesh_headings[i,])
    match <- paste("MATCH (p:PubMed), (m:MeshHeading) WHERE p.pubmed_id = '",
                   pubmed_id,"' AND m.descriptor_key = '",
                   mesh_key, 
                   "' MERGE (p) - [r:HAS_MESH_HEADING] -> (m) RETURN r", sep ="")
    execute_cypher_command(match)
    result[i] <- mesh_key
    
  }
  return (result)
}

# Journal node functions --------------------------------------------------

#' Load journal information for this paper
#' Create a new JournalIssue node and a relationship to the parent PubMed node
#' Merge a new/existing Journal node and create a realtionship to the 
#' appropriate JournalIssue node 
#' PubMed --> JOURNAL_ISSUE --> JOURNAL
#' 
load_journal_data <-function(journal) {
  
  # create a new Journal Issue node
  create <- paste( "CREATE (issue:JournalIssue{ issue_key:'", journal$issue_key, 
                   "', volume:'", journal$volume, "', issue: '",journal$issue,
                   "', year:",as.integer(journal$year), ", month:'", journal$month, 
                   "', pgn:'", journal$pgn,"'})", sep="")
  #print(create)
  execute_cypher_command(create)
  # merge a new/existing Journal 
  merge <- paste("MERGE (journal:Journal{journal_issn:'", journal$journal_issn,
                 "', journal_title:'", journal$journal_title, 
                 "', journal_iso_abbrev:'",journal$journal_iso_abbrev,
                 "'}) ", sep = "" )
  #print(merge)
   execute_cypher_command(merge)
  # create a HAS_JOURNAL relationship between JournalIssue & Journal nodes
   relationship_1 <- paste("MATCH (issue:JournalIssue), 
                           (journal:Journal) WHERE issue.issue_key ='",
                         journal$issue_key, "' AND journal.journal_issn = '", 
                         journal$journal_issn,
                         "' MERGE (issue) -[r:HAS_JOURNAL] -> (journal) return r"
                         ,sep = "")
   #print(relationship_1)
   execute_cypher_command(relationship_1)
   # create a HAS_JOURNAL_ISSUE between the PubMed node and the JournalIssue node
   relationship_2 <- paste("MATCH (p:PubMed), 
                           (issue:JournalIssue) WHERE p.pubmed_id = '", 
                           journal$pubmed_id,
                          "' AND issue.issue_key = '",journal$issue_key,
                          "' MERGE (p) - [r:HAS_JOURNAL_ISSUE] -> (issue) return r"
                          ,sep = "")
   #print(match)
   execute_cypher_command(relationship_2)
}

# PubMed node -------------------------------------------------------------

#' Function to MERGE or CREATE a PubMed node
#' PubMed nodes at level 1 also have a Covid label
load_pubmed_node <- function(pubmed_properties){
  level <- pubmed_properties$level[1]
  #Easier to have two Cypher statements, than adding a second label
  # to a node using APOC
  # TODO: evaluate APOC CALL
  merge <- paste("MERGE (pm:PubMed {pubmed_id:'",
                 pubmed_properties$pubmed_id[1],
                 "',doi:'",pubmed_properties$doi[1],
                 "', title:'", pubmed_properties$title[1],
                 "', abstract:'", pubmed_properties$abstract[1],
                 "', level:", level,
                 ", cited_by_count:0",
                 "}) return pm.pubmed_id", sep=""
                 )
  create <- paste("CREATE (pm:PubMed:Covid {pubmed_id:'",
                  pubmed_properties$pubmed_id[1],
                  "',doi:'",pubmed_properties$doi[1],
                  "', title:'", pubmed_properties$title[1],
                  "', abstract:'",pubmed_properties$abstract[1],
                  "', level:", level,
                  "}) return pm.pubmed_id", sep=""
  )
  if(level == 1) {
    execute_cypher_command(create)
  } else {
  execute_cypher_command(merge)
  }
  
}



