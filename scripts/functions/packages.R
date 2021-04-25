f (!requireNamespace("BiocManager", quietly = TRUE))
install.packages("BiocManager")

packages <- c("rentrez")
BiocManager::install(packages, update = TRUE)

require(tidyverse)
require(data.table)
require(rentrez)

print("Required Bioconductor and Rstats packages loaded")