#'
#' Script name: neo4j_functions
#'
#' Purpose of script: Set up Neo4j enviornment and establish connection to local
#' Neo4j database
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
#' Notes: Specify the 4.x branch of the neo4r github repository
#'        Neo4j database user and password specified in ~/.Renviron file
#'
#' ---------------------------
#' 
#' load the packages we need:  (uncomment as required)
#' remotes::install_github("davidlrosenblum/neo4r",ref="4.x")
require(neo4r)

neo4j_user  <-  Sys.getenv("NEO4J_USER")
neo4j_password <- Sys.getenv("NEO4J_PASSWORD")
con <- neo4j_api$new(
  url = "http://localhost:7474",
  db = "covid.db",
  user = neo4j_user, 
  password = neo4j_password
)

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
  print(query)
  pubmed <- execute_cypher_command(query)
  return (pubmed)
}

#' Author functions
#' Merge a PubMed Author into the database
merge_pubmed_author <- function(author) {
  query <- paste("MERGE (author:Author {lastname:'", author$LastName,
                 "' , firstname: '", author$ForeName,"', initials:'",
                author$Initials, "', id:", author$id,"}) return author.id", sep="" )
  #print(query)
  res <-  execute_cypher_command(query)
  # print(res)
  return (as.integer(res$author.id))
  
}
#' Load all authors for this document into the database
#' Merge author nodes
#' Establish a relationship between the Author nodes and the parent PubMed node
load_authors <- function(authors){
  result <- vector(mode = "character", length = length(authors))
 for (i in 1: length(authors)){
   
   pubmed_id <- authors$pubmed_id[i]
   if (!is.na(pubmed_id)) {
   #pubmed <-  match_pubmed_by_id(pubmed_id)
   author_id <- merge_pubmed_author(authors[i,])
   query <- paste("MATCH (p:PubMed), (a:Author) WHERE p.pubmed_id = '", pubmed_id,"' AND a.id = ",author_id, 
          " CREATE (p) -[r:HAS_AUTHOR] ->(a) RETURN r",sep="")
   #print(query)
   result[i] <- execute_cypher_command(query)
   }
   
 }
    return (result)
}

#' Function to determine if a PubMed node exists
#' Avoids unnecessary MERGE statements
pubmed_node_exists <- function(pubmed_id) {
  query <-  paste("OPTIONAL MATCH (p:PubMed{pubmed_id:'", pubmed_id,
                  "'}) RETURN p IS NOT NULL AS PREDICATE", sep = "")
  res <-  execute_cypher_command(query)
  return (as.logical(res$PREDICATE))
}


#' Mesh Heading functions
#' Merge a MeshHeading node into the Neo4j database
merge_mesh_heading <- function(mesh){
  merge <- paste("MERGE (mesh:MeshHeading {descriptor_key:'",
                 mesh$descriptor_key, "', descriptor_name:'",
                 mesh$descriptor_name,"' }) return mesh.descriptor_key", sep = "")
  res <- execute_cypher_command(merge)
  return (as.character(mesh$descriptor_key))
}

#' Load all Mesh headings for a document into the database
#' Establish a relationship between the MeshHeading node and the parent PubMed
#' node
load_mesh_headings <- function(mesh_headings){
  # create a constraint if one doesn't exist
  constraint <- 'create constraint unique_mesh_descriptor if not exists on (m:MeshHeading) assert m.descriptor_key is unique;'
  execute_cypher_command(constraint)
  # load the mesh headings for this document and establish relationship to document node
  result <-  vector(mode = "character", length = length(mesh_headings))
  for (i in 1:length(mesh_headings)){
    pubmed_id = mesh_headings$pubmed_id[i]
    mesh_key <-  merge_mesh_heading(mesh_headings[i,])
    match <- paste("MATCH (p:PubMed), (m:MeshHeading) WHERE p.pubmed_id = '", pubmed_id,"' AND m.descriptor_key = '",
                   mesh_key, "' CREATE (p) - [r:HAS_MESH_HEADING] -> (m) RETURN r", sep ="")
    execute_cypher_command(match)
    result[i] <- mesh_key
    
  }
  return (result)
}

#' Article Journal and Journal Issue nodes
#' The Journal and Issue are separated to facilitate identifying the journals
#' that contain the most papers

#' Load journal information for this paper
#' Create a new JournalIssue node and a relationship to the parent PubMed node
#' Merge a new/existing JournalIssue node and create a realtionship to the 
#' appropriate JournalIssue node 
#' PubMed --> JOURNAL_ISSUE --> JOURNAL
#' 
load_journal_data <-function(journal) {
  constraint_1 <- 'create constraint unique_journal_issn if not exists on (j:Journal) assert j.journal_issn is unique'
  execute_cypher_command(constraint_1)
  constraint_2 <- 'create constraint unique_issue_key if not exists on (j:JournalIssue) assert j.issue_key is unique'
  # create a new Journal Issue node
  create <- paste( "CREATE (issue:JournalIssue{ issue_key:'", journal$issue_key, 
                   "', volume:'", journal$volume, "', issue: '",journal$issue,
                   "', year:",journal$year, ", month:'", journal$month, 
                   "', pgn:'", journal$pgn,"'})", sep="")
  #print(create)
  execute_cypher_command(create)
  # merge a new/existing Journal issue
  merge <- paste("MERGE (journal:Journal{journal_issn:'", journal$journal_issn,
                 "', journal_title:'", journal$journal_title, 
                 "', journal_iso_abbrev:'",journal$journal_iso_abbrev,
                 "'}) ", sep = "" )
  print(merge)
   execute_cypher_command(merge)
  # create a HAS_JOURNAL relationship between JournalIssue & Journal nodes
   relationship_1 <- paste("MATCH (issue:JournalIssue), (journal:Journal) WHERE issue.issue_key ='",
                         journal$issue_key, "' AND journal.journal_issn = '", journal$journal_issn,
                         "' CREATE (issue) -[r:HAS_JOURNAL] -> (journal) return r"
                         ,sep = "")
   print(relationship_1)
   execute_cypher_command(relationship_1)
   # create a HAS_JOURNAL_ISSUE between the PubMed node and the JournalIssue node
   relationship_2 <- paste("MATCH (p:PubMed), (issue:JournalIssue) WHERE p.pubmed_id = '", journal$pubmed_id,
                          "' AND issue.issue_key = '",journal$issue_key,
                          "' CREATE (p) - [r:HAS_JOURNAL_ISSUE] -> (issue) return r"
                          ,sep = "")
   print(match)
   execute_cypher_command(relationship_2)
}

#' COVID-19 csv file processing
#' Create new PubMed/Covid nodes for PubMed entries in a csv formatted document
load_primary_pubmed_nodes_from_csv <- function(filename) {
 create <-  paste('LOAD CSV WITH HEADERS FROM "file:', filename,
            '" AS line CREATE (:PubMed:Covid {pubmed_id: line.pubmed_id, title: line.title, doi: line.doi, abstract: line.abstract});'
        ,sep ="") 
  execute_cypher_command(create)
  constraint <- 'CREATE CONSTRAINT unique_pubmed IF NOT EXISTS ON (n:PubMed) ASSERT n.pubmed_is IS UNIQUE'  
  execute_cypher_command(constraint)
}

'LOAD CSV WITH HEADERS FROM "file:///Users/fcriscuo/RDev/CovidPubMedGraph/protected_data/metadata_sample.csv" AS line CREATE (:PubMed {pubmed_id: line.pubmed_id, title: line.title, doi: line.doi});' %>% 
  call_neo4j(con)
