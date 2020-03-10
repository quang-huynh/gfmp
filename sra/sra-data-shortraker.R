library(dplyr)
species_name <- "shortraker rockfish"
starting_year <- 1977
ending_year <- 2018
all_years <- seq(starting_year, ending_year)

science_name <- "Sebastes borealis"

d_short <- load_data_shortraker()
d_privacy <- load_data_shortraker(private = TRUE)

short_om <- readRDS(here::here("generated-data", "shortraker-om.rds"))
short_om@M
short_om@Linf
short_om@K
short_om@t0
short_om@a
short_om@b
short_om@L50
short_om@L50_95
# Cobs could be made slightly bigger than 0; it is now zero but
# is much larger for older observations:
short_om@Cobs
short_om@Perr
# this is a rough guess as a composite across many indexes; each index has its own CV:
short_om@Iobs <- c(0.4, 0.6)

make_raker_cal <- function(dat, survey, length_bin = 5) {
  dat <- filter(dat, survey_abbrev == survey)
  cal <- gfdlm::tidy_cal(dat, yrs = all_years, interval = length_bin)
  length_bins <- gfdlm::get_cal_bins(cal, length_bin_interval = length_bin)
  list(cal = cal[1, , ], length_bins = length_bins)
}

cal_wchg <- make_raker_cal(d_short$survey_samples, "SYN WCHG")
cal_qcs <- make_raker_cal(d_short$survey_samples, "SYN QCS")
cal_wcvi <- make_raker_cal(d_short$survey_samples, "SYN WCVI")

caa_wchg <- filter(d_short$survey_samples, survey_abbrev == "SYN WCHG") %>%
  gfdlm::tidy_caa(yrs = all_years)
caa_wchg[1, , ]

caa_qcs <- filter(d_short$survey_samples, survey_abbrev == "SYN QCS") %>%
  gfdlm::tidy_caa(yrs = all_years)
caa_qcs[1, , ]

mean_length <- filter(d_short$survey_samples, survey_abbrev == "SYN WCHG") %>%
  gfdlm::tidy_mean_length() %>%
  filter(n > 10, year <= ending_year, year >= starting_year) %>%
  right_join(tibble(year = all_years), by = "year") %>%
  pull(mean_length)

mean_length

# note that we be should correcting catch between 1990 and 1995.
# For now just do that by dividing the catch in those years by 3:

# bottom trawl catch:
if ("catch" %in% names(d_short)) {
  catch <- d_short$catch %>%
    filter(gear == "BOTTOM TRAWL") %>%
    gfplot::tidy_catch() %>%
    group_by(year) %>%
    summarize(value = sum(value)) %>%
    right_join(tibble(year = all_years), by = "year") %>%
    mutate(value = ifelse(year >= 1990 & year <= 1995, value / 3, value)) %>%
    pull(value)
  saveRDS(catch, file = here::here("generated-data", "shortraker-catch.rds"))
} else {
  catch <- readRDS(here::here("generated-data", "shortraker-catch.rds"))
}

catch
plot(all_years, catch, type = "o")

# hook and line catch:
if ("catch" %in% names(d_short)) {
  catch_hl <- d_short$catch %>%
    filter(gear == "HOOK AND LINE") %>%
    gfplot::tidy_catch() %>%
    group_by(year) %>%
    summarize(value = sum(value)) %>%
    right_join(tibble(year = all_years), by = "year") %>%
    mutate(value = ifelse(year >= 1990 & year <= 1995, value / 3, value)) %>%
    pull(value)
  saveRDS(catch_hl, file = here::here("generated-data", "shortraker-catch-hl.rds"))
} else {
  catch_hl <- readRDS(here::here("generated-data", "shortraker-catch-hl.rds"))
}

catch_hl
plot(all_years, catch_hl, type = "o")

# catch per unit effort from the trawl fleet only for now:
cpue <- read.csv(here::here("generated-data", "shortraker-cpue.csv"))

# we are working on something similar for the hook and line fleet

indexes <- gfplot::tidy_survey_index(d_short$survey_index) %>%
  filter(survey_abbrev %in% c("SYN WCHG", "SYN QCS", "SYN WCVI", "IPHC FISS")) %>%
  reshape2::dcast(year ~ survey_abbrev, value.var = "biomass") %>%
  right_join(tibble(year = all_years), by = "year") %>%
  left_join(rename(select(cpue, year, est), trawl_cpue = est), by = "year") %>%
  select(-year) %>%
  as.matrix()

indexes

# The first four are from surveys and the last one is from commercial catch
# per unit effort from the trawl fleet:
I_type <- c("B", "B", "B", "B", "VB")
