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


require(tidyverse)
require(data.table)
source(here::here("scripts/functions/init_environment.R"))      # loads up all the packages we need
source(here::here("scripts/functions/pubmed.functions.R"))

sample_path <- here::here("protected_data/metadata_sample.csv")
sample <- readr::read_csv(sample_path, col_names=TRUE)
head(sample)