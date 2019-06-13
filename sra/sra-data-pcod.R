
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

#get the OM -- this file is made using om-specification-pcod.Rmd
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

######
# Start Quang's analysis here
#devtools::install_github('tcarruth/MSEtool')
library(MSEtool); library(dplyr)
pcod_data_summary <- readRDS(here::here("generated-data", "pcod-data-summary.rds"))
pcod_om <- readRDS(here::here("generated-data", "pcod-om.rds"))

# plot CAA - combines QCS and HS surveys
plot_composition(1956:2018, obs = pcod_data_summary$caa)

# plot CAL
plot_composition(1956:2018, obs = pcod_data_summary$cal_hsmas, CAL_bins = colnames(pcod_data_summary$cal_hsmas) %>% as.numeric())
plot_composition(1956:2018, obs = pcod_data_summary$cal_hss, CAL_bins = colnames(pcod_data_summary$cal_hss) %>% as.numeric())
plot_composition(1956:2018, obs = pcod_data_summary$cal_qcss, CAL_bins = colnames(pcod_data_summary$cal_qcss) %>% as.numeric())

plot_composition(1956:2018, obs = pcod_data_summary$cal_com, CAL_bins = colnames(pcod_data_summary$cal_com) %>% as.numeric())

CAL_sur <- pcod_data_summary$cal_hss
CAL_sur[, 1:18] <- CAL_sur[, 1:18] + pcod_data_summary$cal_qcss


CAL <- pcod_data_summary$cal_com
CAL[1:41, ] <- NA # Do not use commerical CAL prior to 1996 because they lack discards of smaller fish

Chist <- pcod_data_summary$catch$catch_t

indices <- reshape2::dcast(pcod_data_summary$indices, year ~ survey_abbrev, value.var = "biomass") %>%
  dplyr::right_join(data.frame(year = 1956:2017))

#RF Added commercial CPUE
#Join the commercial cpue index to the survey indices
indices  <- dplyr::full_join(indices, pcod_data_summary$comcpue)
#remove the CV column for commercial CPUE
indices <- indices[,-ncol(indices)]

Index <- indices[, -1] %>% as.matrix() %>% rbind(rep(NA, 5))

pcod_om@nsim <- 50L
pcod_om@Linfsd <- pcod_om@Msd <- pcod_om@Ksd <- c(0, 0)

##### No comps - SRA becomes a de facto delay-difference model
#pcod_no_comps_SRA <- MSEtool:::SRA_scope(pcod_om, Chist = Chist, Index = Index, I_type = rep(1, 5),cores = 10, report = TRUE)
pcod_no_comps_SRA <- scope(pcod_om, Chist = Chist, Index = Index, I_type = rep(1, 5),cores = 10, report = TRUE)
trsaveRDS(pcod_no_comps_SRA$OM, file = here::here("sra/pcod_no_comps_om.rds"))
saveRDS(pcod_no_comps_SRA$report, file = here::here("sra/pcod_no_comps_SRA_report.rds"))

pcod_no_comps_om <- readRDS("sra/pcod_no_comps_om.rds")
pcod_no_comps_SRA_report <- readRDS("sra/pcod_no_comps_SRA_report.rds")

MSEtool:::plot_SRA_scope(pcod_no_comps_om, Chist = matrix(Chist, ncol = 1), Index = Index,
                         report = pcod_no_comps_SRA_report, Year = 1956:2018)

# Use catch, index, and commercial length comps, downweighting the length comps
# pcod_cal_com_SRA <- MSEtool:::SRA_scope(pcod_om, Chist = Chist, Index = Index, I_type = rep(1, 5),
#                               CAL = CAL, length_bin = colnames(CAL) %>% as.numeric(), LWT = list(CAL = 0.05),
#                               cores = 10, report = TRUE)
# saveRDS(pcod_cal_com_SRA$OM, file = "sra/pcod_cal_com_om.rds")
# saveRDS(pcod_cal_com_SRA$report, file = "sra/pcod_cal_com_SRA_report.rds")
#
# pcod_cal_com_om <- readRDS("sra/pcod_cal_com_om.rds")
# pcod_cal_com_SRA_report <- readRDS("sra/pcod_cal_com_SRA_report.rds")
#
# pcod_cal_com_om@cpars$Find[pcod_cal_com_om@cpars$Find > 1] <- 1 # There are some very high F's, so set max. F = 1
#
# MSEtool:::plot_SRA_scope(pcod_cal_com_om, Chist = matrix(Chist, ncol = 1), Index = Index, CAL = array(CAL, c(dim(CAL), 1)),
#                          report = pcod_cal_com_SRA_report, Year = 1956:2018)
#
# #### Use catch, index, and combined age/length comps from Hecate Straight and QCS surveys
# pcod_cal_sur_SRA <- MSEtool:::SRA_scope(pcod_om, Chist = Chist, Index = Index, I_type = rep(1, 5),
#                               CAA = pcod_data_summary$caa,
#                               CAL = CAL_sur, length_bin = colnames(CAL_sur) %>% as.numeric(), LWT = list(CAA = 0.1, CAL = 0.1),
#                               cores = 10, report = TRUE)
# saveRDS(pcod_cal_sur_SRA$OM, file = "sra/pcod_cal_sur_om.rds")
# saveRDS(pcod_cal_sur_SRA$report, file = "sra/pcod_cal_sur_SRA_report.rds")
#
# pcod_cal_sur_om <- readRDS("sra/pcod_cal_sur_om.rds")
# pcod_cal_sur_SRA_report <- readRDS("sra/pcod_cal_sur_SRA_report.rds")
#
# pcod_cal_sur_om@cpars$Find[pcod_cal_sur_om@cpars$Find > 1] <- 1 # There are some very high F's, so set max. F = 1
#
# MSEtool:::plot_SRA_scope(pcod_cal_sur_om, Chist = matrix(Chist, ncol = 1), Index = Index, CAL = array(CAL_sur, c(dim(CAL_sur), 1)),
#                          CAA = array(pcod_data_summary$caa, c(dim(pcod_data_summary$caa), 1)),
#                          report = pcod_cal_sur_SRA_report, Year = 1956:2018)




