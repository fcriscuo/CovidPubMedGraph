## ---------------------------
##
## Script name: Neo4jConDemo.R
##
## Purpose of script: Test connecting to local neo4j database
##
## Author:Fred Criscuolo
##
## Date Created: 2021-05-03
##
## Copyright (c) Fred Criscuolo, 2021
## Email: genomicdatasci@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## load up the packages we will need:  (uncomment as required)
## need to reference a specific git branch to use with Neo4j v4
## remotes::install_github("davidlrosenblum/neo4r",ref="4.x")
require(tidyverse)
require(data.table)
require(neo4r)
require(magrittr)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

# source("functions/summarise_data.R") 

## ---------------------------
con <- neo4j_api$new(
  url = "http://localhost:7474",
  db = "covid.db",
  user = "neo4j", 
  password = "fjc92677"
)


# Note that play_movies is only available for versions >= 0.1.3 
play_movies() %>%
  call_neo4j(con)

# issue test Cypher query
'MATCH (tom {name: "Tom Hanks"}) RETURN tom;' %>%
  call_neo4j(con)

# query results in JSON
'MATCH (cloudAtlas {title: "Cloud Atlas"}) RETURN cloudAtlas;' %>%
  call_neo4j(con, output = "json")
# output as a graph
'MATCH (tom:Person {name: "Tom Hanks"})-[act:ACTED_IN]->(tomHanksMovies) RETURN act,tom,tomHanksMovies' %>%
  call_neo4j(con, type = "graph")

res <- 'MATCH (tom:Person {name:"Tom Hanks"})-[a:ACTED_IN]->(m)<-[:ACTED_IN]-(coActors) RETURN m AS acted,coActors.name' %>%
  call_neo4j(con, type = "graph")
unnest_nodes(res$nodes)

## Load data ----
## Test loding data from CSV file

load_cypher_query <- "CREATE (pm:PubMed)
  SET pm = row,
  pm.pubmed_id = row.pubmed_id,
  pm.title = row.title,
  pm.doi = row.doi,
  pm.abstract = row.abstract,
  pm.journal = row.journal;"


'LOAD CSV WITH HEADERS FROM "file:///Users/fcriscuo/RDev/CovidPubMedGraph/protected_data/metadata_sample.csv" AS line CREATE (:PubMed {pubmed_id: line.pubmed_id, title: line.title, doi: line.doi});' %>% 
  call_neo4j(con)

metadata_file_url <- "file:///Users/fcriscuo/RDev/CovidPubMedGraph/protected_data/metadata_sample.csv"

load_csv(url = metadata_file_url, con = con, 
         header = TRUE, periodic_commit = 100,
         as = "row", on_load = load_cypher_query2)
  
  

