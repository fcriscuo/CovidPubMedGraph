#'
#' Script name: init_environment.R
#'
#' Purpose of script: Establish the global environment for loading PubMed data
#'                    into the Neo4j database
#'
#' Author:Fred Criscuolo
#'
#' Date Created: 2021-06-09
#'
#' Copyright (c) Fred Criscuolo, 2021
#' Email: genomicdatasci@gmail.com
#'
#' ---------------------------
#'
#' Notes:
#' 1. Uncomment lines that load install packages from Bioconducto and github
#'    as required
#' 2. The pacman package is used to manage packages from CRAN
#' 3. For R 4.x, the neo4r package must be loaded from the 4.x branch on github
#'   
#' ---------------------------
#' Uncomment following lines to use BioConductor packages
#'if (!requireNamespace("BiocManager", quietly = TRUE))
#'    install.packages("BiocManager")
# packages <- c("rentrez")
# BiocManager::install(packages, update = TRUE)

#' Pacman package

if (!require("pacman")) install.packages("pacman"); library(pacman)
# grateful package
if (!require("grateful")) install_github("Pakillo/grateful")

pacman::p_load("tidyverse", "data.table", "rentrez","XML", 
                  "digest","properties","magrittr", "readr",
               "logger", "curl","httr","grateful")

#' Read the properties file
propfile <-  here::here("resources/app_default.properties")
props <- read.properties(propfile, fields = c("save.pubmed.xml.default",
                                              "load.database.mode",
                                              "reference.levels.default",
                                              "pubmedid.column.name",
                                              "pubmed.max.count",
                                              "top.cited.log.file",
                                              "neo4j.load.log.file",
                                              "covid.csv.file",
                                              "batch.request.size",
                                              "primary.node.label"))
#' load the 4.x branch of the neo4r package from github
 #remotes::install_github("davidlrosenblum/neo4r",ref="4.x")
require(neo4r)
# Generate markdown with citations and references
#cite_packages(out.format = "md", out.dir = here::here("markdown"))  
#' set up logging for database load
log_appender(appender_file(here::here(props$neo4j.load.lofg.file)))

#' Environment properties in .Renviron
source(here::here("R/utilities/renviron_properties.R"))
source(here::here("R/functions/xml_utilities.R"))
source(here::here("R/functions/pubmed_functions.R"))
source(here::here("R/functions/neo4j_functions.R"))
source(here::here("R/fetch_pubmed_entries.R"))

#log_info ("Application environment initialized......")

numbers_only <- function(x) !grepl("\\D", x)