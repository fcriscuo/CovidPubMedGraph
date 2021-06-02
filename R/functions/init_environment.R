# if (!requireNamespace("BiocManager", quietly = TRUE))
# install.packages("BiocManager")
# 
# packages <- c("rentrez")
# BiocManager::install(packages, update = TRUE)
if (!require("pacman")) install.packages("pacman"); library(pacman)

pacman::p_load("tidyverse", "data.table", "rentrez","XML", 
                  "digest","properties","magrittr", "readr",
               "logger")

#' Read the properties file
propfile <-  here::here("resources/app_default.properties")
props <- read.properties(propfile, fields = c("save.pubmed.xml.default",
                                              "reference.levels.default",
                                              "pubmedid.column.name",
                                              "pubmed.max.count"))
#' load the 4.x branch of the neo4r package from github
 #remotes::install_github("davidlrosenblum/neo4r",ref="4.x")
require(neo4r)

#cite_packages(out.format = "md", out.dir = here::here("markdown"))  # Generate markdown with citations and references
#' set up logging
log_appender(appender_file(here::here("./logs/covid_pubmed.log")))
log_info("Required Bioconductor and Rstats packages loaded")

source(here::here("R/functions/xml_utilities.R"))
source(here::here("R/functions/pubmed_functions.R"))
source(here::here("R/functions/neo4j_functions.R"))
source(here::here("R/fetch_pubmed_entries.R"))

log_info ("Application environment initialized......")