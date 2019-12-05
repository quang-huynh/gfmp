library(gfplot)
library(gfutilities)
library(gfdlm)
library(DLMtool)
library(MSEtool)
library(dplyr)
library(ggplot2)
filter <- dplyr::filter # Chris sometimes has problems. #So does Robyn :-D

source_rmd <- function(rmd_file){
  knitr::knit(rmd_file, output = tempfile())
}

rex_om_rmd <- list.files("report", pattern = "om-rex\\.Rmd",
  full.names = TRUE)
source_rmd(rex_om_rmd)


