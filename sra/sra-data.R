library("dplyr")
species <- "shortraker rockfish"
starting_year <- 1978
ending_year <- 2018
all_years <- seq(starting_year, ending_year)

# d_commercial <- gfdata::get_commercial_samples(species, unsorted_only = TRUE)
d_survey <- gfdata::get_survey_samples(species, ssid = c(1, 3, 4, 16))

cal_commercial <- d_commercial %>%
  pbs2dlm::tidy_cal(yrs = all_years, interval = 5)
(length_bins <- get_cal_bins(cal, length_bin_interval = 5))
(cal_commercial <- cal_commercial[1, , ])

cal_wchg <- dplyr::filter(d_survey, survey_abbrev == "SYN WCHG") %>%
  pbs2dlm::tidy_cal(yrs = all_years, interval = 5)
(length_bins <- get_cal_bins(cal, length_bin_interval = 5))
(cal_wchg <- cal_wchg[1, , ])

caa <- dplyr::filter(d_survey, survey_abbrev == "SYN WCHG") %>%
  pbs2dlm::tidy_caa(yrs = all_years)
caa[1, , ]

mean_length <- dplyr::filter(d_survey, survey_abbrev == "SYN WCHG") %>%
  pbs2dlm::tidy_mean_length() %>%
  dplyr::filter(n > 10, year <= ending_year, year >= starting_year) %>%
  right_join(tibble(year = all_years), by = "year")

tail(mean_length)
