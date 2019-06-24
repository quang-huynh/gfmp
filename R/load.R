load_data_factory <- function(species_name, file){
  created_by_load_data_factory <- function(unsorted_only = TRUE,
                                           private = FALSE){
    if(!file.exists(file)){
      d <- list()
      d$commercial_samples <- gfdata::get_commercial_samples(species_name,
                                                             unsorted_only = unsorted_only)
      d$catch <- gfdata::get_catch(species_name)
      d$survey_samples <- gfdata::get_survey_samples(species_name)
      d$survey_index <- gfdata::get_survey_index(species_name)
      saveRDS(d, file = file)
    }else{
      d <- readRDS(file)
    }
    if(private){
      d$commercial_samples <- NULL
      d$catch <- NULL
    }
    d
  }
  created_by_load_data_factory
}

load_data_pcod <- load_data_factory("pacific cod",
                                    file = here::here("generated-data",
                                             "pacific-cod.rds"))

load_data_shortraker <- load_data_factory("shortraker rockfish",
                                          file = here::here("generated-data",
                                                   "shortraker-rockfish.rds"))

load_data_rex <- load_data_factory("rex sole",
                                   file = here::here("generated-data",
                                                     "rex-sole.rds"))
