library(dplyr)
species_name <- "rex sole"
starting_year <- 1988
ending_year <- 2018
all_years <- seq(starting_year, ending_year)

science_name <- "Glyptocephalus zachirusadus"
# drex <- load_data_rex()

# rex_areas <- c("03", "04", "05", "06", "07", "08", "09")
rex_areas <- c("03", "04")
rex_surveys <- c("West Coast Vancouver Island Synoptic Bottom Trawl")
#"Queen Charlotte Sound Synoptic Bottom Trawl",
#"Hecate Strait Synoptic Bottom Trawl")

#    major_stat_area_code major_stat_area_name
#  1 00                   UNKNOWN: NO POSITION INFORMATION
#  2 01                   4B: STRAIT OF GEORGIA
#  3 02                   3B: CAPE FLATTERY (47 20' to 220 T)
#  4 03                   3C: S.W. VANCOUVER ISLAND
#  5 04                   3D: N.W. VANCOUVER ISLAND
#  6 05                   5A: SOUTHERN Q.C. SOUND
#  7 06                   5B: NORTHERN Q.C. SOUND
#  8 07                   5C: SOUTHERN HECATE STRAIT
#  9 08                   5D: NORTHERN HECATE STRAIT
# 10 09                   5E: WEST COAST Q.C. ISLANDS

science_name <- "Glyptocephalus zachirusadus"

drex <- load_data_rex()
drex$commercial_samples <- drex$commercial_samples %>%
  dplyr::filter(major_stat_area_code %in% rex_areas) %>%
  dplyr::mutate(year = lubridate::year(trip_start_date)) %>%
  dplyr::filter(year <= 2018)

drex$survey_samples <- drex$survey_samples %>%
  dplyr::filter(major_stat_area_code %in% rex_areas) %>%
  dplyr::mutate(year = lubridate::year(trip_start_date)) %>%
  dplyr::filter(year <= 2018)

drex$catch <- drex$catch %>%
  dplyr::filter(major_stat_area_code %in% rex_areas) %>%
  dplyr::mutate(year = lubridate::year(fe_start_date)) %>%
  dplyr::filter(year <= 2018)

drex$survey_index <- drex$survey_index %>%
  dplyr::filter(survey_series_desc %in% rex_surveys) %>%
  dplyr::filter(year <= 2018)

rex_om <- readRDS(here::here("generated-data", "rex-om.rds"))
rex_om@M
rex_om@Linf
rex_om@K
rex_om@t0
rex_om@a
rex_om@b
rex_om@L50
rex_om@L50_95
# Cobs could be made slightly bigger than 0; it is now zero but
# is much larger for older observations:
rex_om@Cobs
rex_om@Perr
# from WCVI:
rex_om@Iobs <- c(0.08, 0.10)

make_cal <- function(dat,
                     survey,
                     yrs,
                     length_bin = 5) {
  cal <- dat %>%
    filter(survey_abbrev == survey) %>%
    gfdlm::tidy_cal(yrs = yrs, interval = length_bin)

  length_bins <- gfdlm::get_cal_bins(cal, length_bin_interval = length_bin)
  list(cal = cal[1, , ], length_bins = length_bins)
}

# cal_wchg <- make_cal(drex$survey_samples, "SYN WCHG", yrs = all_years)
# cal_qcs <- make_cal(drex$survey_samples, "SYN QCS", yrs = all_years)
cal_wcvi <- make_cal(drex$survey_samples, "SYN WCVI", yrs = all_years)

# caa_wchg <- filter(drex$survey_samples, survey_abbrev == "SYN WCHG") %>%
#   gfdlm::tidy_caa(yrs = all_years)
# caa_wchg[1, , ]
#
# caa_qcs <- filter(drex$survey_samples, survey_abbrev == "SYN QCS") %>%
#   gfdlm::tidy_caa(yrs = all_years)
# caa_qcs[1, , ]

mean_length <- filter(drex$survey_samples, survey_abbrev == "SYN WCVI") %>%
  gfdlm::tidy_mean_length() %>%
  filter(n > 10, year <= ending_year, year >= starting_year) %>%
  right_join(tibble(year = all_years), by = "year") %>%
  pull(mean_length)

mean_length

# bottom trawl catch:
if ("catch" %in% names(drex)) {
  catch <- drex$catch %>%
    # filter(gear == "BOTTOM TRAWL") %>%
    gfplot::tidy_catch() %>%
    group_by(year) %>%
    summarize(value = sum(value)) %>%
    right_join(tibble(year = all_years), by = "year") %>%
    #mutate(value = ifelse(year >= 1990 & year <= 1995, value / 3, value)) %>%
    pull(value)
  saveRDS(catch, file = here::here("generated-data", "rex-catch.rds"))
} else {
  catch <- readRDS(here::here("generated-data", "rex-catch.rds"))
}
catch
plot(all_years, catch, type = "o")

# catch per unit effort from the trawl fleet only for now:
#cpue <- read.csv(here::here("generated-data", "shortraker-cpue.csv"))

indexes <- gfplot::tidy_survey_index(drex$survey_index) %>%
  # filter(survey_abbrev %in% c("SYN WCHG", "SYN QCS", "SYN WCVI", "IPHC FISS")) %>%
  filter(survey_abbrev %in% c("SYN WCVI")) %>%
  reshape2::dcast(year ~ survey_abbrev, value.var = "biomass") %>%
  right_join(tibble(year = all_years), by = "year") %>%
  # left_join(rename(select(cpue, year, est), trawl_cpue = est), by = "year") %>%
  select(-year) %>%
  as.matrix()

indexes

# The first four are from surveys and the last one is from commercial catch
# per unit effort from the trawl fleet:
I_type <- c("B")
#
# rex_om2 <- SRA_scope(rex_om, Chist = Catch, Index = Index, CAL = CAL, C_eq = 0.5 * Catch[1],
#   length_bin = Data@CAL_bins[1:28] + 3, I_type = "B", cores = 8)

