
## Pacific Cod
library(dplyr)
library(gfplot)
library(gfdata)
library(gfutilities)
library(pbs2dlm)
library(readr)

starting_year <- 1956
ending_year <- 2018
all_years <- seq(starting_year, ending_year)

#get the OM
ompc <- readRDS(here::here("generated-data", "pcod-om.rds"))

#Get the data (Areas 5ABCD only)
pcod_areas <- c("05","06","07","08") #QCS and HS for comm samples, surv samples and catch
pcod_survey_areas <- c("Hecate Strait Multispecies Assemblage Bottom Trawl", "Queen Charlotte Sound Synoptic Bottom Trawl" ,"Hecate Strait Synoptic Bottom Trawl") #QCS and HS for survey index
science_name <- "Gadus macrocephalus"
species_name <- "Pacific Cod"

species_file <- here::here(
  "generated-data",
  paste0(gsub(" ", "-", species_name), ".rds")
)

if (!file.exists(species_file)) {
  dpc <- list()
  dpc$commercial_samples <- gfdata::get_commercial_samples(species_name, unsorted_only = FALSE)
  dpc$survey_samples <- gfdata::get_survey_samples(species_name)
  dpc$catch <- gfdata::get_catch(species_name)
  dpc$survey_index <- gfdata::get_survey_index(species_name)
  saveRDS(dpc, file = species_file)
} else {
  dpc <- readRDS(species_file)
}

#Get only 5ABCD data
dpc$commercial_samples <- dpc$commercial_samples %>%
  dplyr::filter(major_stat_area_code %in% pcod_areas)

dpc$survey_samples <- dpc$survey_samples %>%
  dplyr::filter(survey_series_desc %in% pcod_survey_areas)

dpc$catch <- dpc$catch %>%
  dplyr::filter(major_stat_area_code %in% pcod_areas)

dpc$survey_index <- dpc$survey_index %>%
  dplyr::filter(survey_series_desc %in% pcod_survey_areas)

#get indices
indicespc <- dpc$survey_index

#get commercial CPUE from file (direct from assessment reference case dat file)
commcpuepc <- read_csv(here::here("sra", "Pcod_CPUE.csv"))

#Survey catch at age - combined HS and QCS synoptic surveys
caapc <- dplyr::filter(dpc$survey_samples) %>%
  pbs2dlm::tidy_caa(yrs = all_years)
caapc <- caapc[1, , ]
colnames(caapc) <- 1:ncol(caapc)
rownames(caapc) <- all_years

#########################################################
#Survey CAL by survey
#Hecate Strait Multspecies Assemblage
calpc_hsmas <- dplyr::filter(dpc$survey_samples, survey_abbrev=="HS MSA") %>%
  pbs2dlm::tidy_cal(yrs = all_years, interval = 5)
(length_bins <- get_cal_bins(calpc_hsmas, length_bin_interval = 5))
(calpc_hsmas <- calpc_hsmas[1, , ])
colnames(calpc_hsmas) <- length_bins
rownames(calpc_hsmas) <- all_years

#Hecate Strait Synoptic
calpc_hss <- dplyr::filter(dpc$survey_samples, survey_abbrev=="SYN HS") %>%
  pbs2dlm::tidy_cal(yrs = all_years, interval = 5)
(length_bins <- get_cal_bins(calpc_hss, length_bin_interval = 5))
(calpc_hss <- calpc_hss[1, , ])
colnames(calpc_hss) <- length_bins
rownames(calpc_hss) <- all_years

#Queen Charlotte Sound Synoptic
calpc_qcss <- dplyr::filter(dpc$survey_samples, survey_abbrev=="SYN QCS") %>%
  pbs2dlm::tidy_cal(yrs = all_years, interval = 5)
(length_bins <- get_cal_bins(calpc_qcss, length_bin_interval = 5))
(calpc_qcss <- calpc_qcss[1, , ])
colnames(calpc_qcss) <- length_bins
rownames(calpc_qcss) <- all_years

#########################################################
#Commercial catch at length
comcalpc <- dplyr::filter(dpc$commercial_samples) %>%
  pbs2dlm::tidy_cal(yrs = all_years, interval = 5, unsorted_only=FALSE)
(length_bins <- get_cal_bins(comcalpc, length_bin_interval = 5))
(comcalpc <- comcalpc[1, , ])
colnames(comcalpc) <- length_bins
rownames(comcalpc) <- all_years

#Mean length in commercial data
mean_lengthpc <- dplyr::filter(dpc$commercial_samples) %>%
  pbs2dlm::tidy_mean_length(unsorted_only=FALSE) %>%
  dplyr::filter(n > 10, year <= ending_year, year >= starting_year) %>%
  right_join(tibble(year = all_years), by = "year")
  as.matrix(mean_lengthpc)

#Commercial catch
if ("catch" %in% names(dpc)) {
  catchpc <- dpc$catch %>%
    filter(gear == "BOTTOM TRAWL", year %in% all_years) %>%
    gfplot::tidy_catch() %>%
    group_by(year) %>%
    summarize(value = sum(value)) %>%
    right_join(tibble(year = all_years), by = "year")
saveRDS(catchpc, file = here::here("generated-data", "pcod-catch.rds"))
} else {
  catchpc <- readRDS(here::here("generated-data", "pcod-catch.rds"))
}

  as.matrix(catchpc)
  colnames(catchpc) <- c("year", "catch_t")
  catchpc$catch_t <- catchpc$catch_t/1000

# catch per unit effort from the trawl fleet only for now:
cpuepc <- readr::read_csv(here::here("generated-data", "pcod-cpue.csv"))

#Add to list object
pcod_data_summary <- list()

pcod_data_summary$indices <- indicespc
pcod_data_summary$comcpue <- commcpuepc
pcod_data_summary$caa <- caapc
pcod_data_summary$cal_hsmas <- calpc_hsmas
pcod_data_summary$cal_hss <- calpc_hss
pcod_data_summary$cal_qcss <- calpc_qcss
pcod_data_summary$cal_com <- comcalpc
pcod_data_summary$catch <- catchpc
pcod_data_summary$mean_length <- mean_lengthpc

#Write to file
saveRDS(pcod_data_summary, file = here::here("generated-data", "pcod-data-summary.rds"))

