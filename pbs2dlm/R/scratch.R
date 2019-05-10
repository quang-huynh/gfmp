library(tidyverse)

#' Make a defualt custom descriptions CSV file in the inst directory
#' This shouldn't be run unless you want to change the structure of the file
generate_default_custom_descriptions_file <- function(){
  ds <- as_tibble(DLMtool::StockDescription) %>% mutate(SlotType = "Stock")
  df <- as_tibble(DLMtool::FleetDescription) %>% mutate(SlotType = "Fleet")
  do <- as_tibble(DLMtool::ObsDescription) %>% mutate(SlotType = "Obs")
  di <- as_tibble(DLMtool::ImpDescription) %>% mutate(SlotType = "Imp")

  d <- bind_rows(ds, df, do, di) %>%
    transmute(slot_type = SlotType,
              slot = Slot,
              use_custom_description = FALSE,
              custom_description = Description)
  readr::write_csv(d, "inst/alt-slot-descriptions.csv")
}
