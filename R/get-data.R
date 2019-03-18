#' Create the data file for the given species by running gfplot routines
#'
#' @param species_name the name of the species or species code as described in gfplot
#' @param file the full path filename including extension .rds
#'
#' @export
#'
#' @examples
#' fetch_data()
fetch_data <- function(species_name = "shortraker rockfish",
                       file = file.path(here::here("generated-data"),
                                        paste0(gsub(" ",
                                                    "-",
                                                    species_name),
                                               ".rds"))){
  d <- list()
  d$commercial_samples <- gfplot::get_commercial_samples(species_name)
  d$survey_samples <- gfplot::get_survey_samples(species_name)
  d$catch <- gfplot::get_catch(species_name)
  saveRDS(d, file)
}

#' Load the data in from the data file for the given species
#'
#' @param species_name the name of the species or species code as described in gfplot
#' @param file the full path filename including extension .rds
#'
#' @return the contents of the rds file as a list
#' @export
#'
#' @examples
#' d <- load_data()
load_data <- function(species_name = "shortraker rockfish",
                      file = file.path(here::here("generated-data"),
                                       paste0(gsub(" ",
                                                   "-",
                                                   species_name),
                                              ".rds"))){
  if(!file.exists(file)){
    stop("Error, file ", file, " does not exist. To create it, run fetch_data().\n")
  }
  readRDS(file)
}

#' Does the data file exist or not for the given species
#'
#' @param species_name the name of the species or species code as described in gfplot
#' @param file the full path filename including extension .rds
#'
#' @return the contents of the rds file as a list
#' @export
#'
#' @examples
#' data_file_exists("shortraker rockfish")
data_file_exists <- function(species_name = "shortraker rockfish",
                             file = file.path(here::here("generated-data"),
                                              paste0(gsub(" ",
                                                          "-",
                                                          species_name),
                                                     ".rds"))){
  file.exists(file)
}
