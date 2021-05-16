if (!requireNamespace("BiocManager", quietly = TRUE))
install.packages("BiocManager")

packages <- c("rentrez")
BiocManager::install(packages, update = TRUE)
if (!require("pacman")) install.packages("pacman"); library(pacman)

pacman::p_load("tidyverse", "data.table", "rentrez","XML", 
                 "grateful", "digest","properties","magrittr")

#' Read the properties file
propfile <-  here::here("resources/app_default.properties")
props <- read.properties(propfile, fields = c("save.pubmed.xml.default", "reference.levels.default"))

cite_packages(out.format = "md", out.dir = here::here("markdown"))  # Generate markdown with citations and references

print("Required Bioconductor and Rstats packages loaded")

