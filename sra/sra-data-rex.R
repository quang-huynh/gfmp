library(dplyr)
species_name <- "rex sole"
starting_year <- 1977
ending_year <- 2018
all_years <- seq(starting_year, ending_year)

science_name <- "Glyptocephalus zachirusadus"
drex <- load_data_rex()

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
# this is a rough guess as a composite across many indexes; each index has its own CV:
rex_om@Iobs <- c(0.4, 0.6)

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

cal_wchg <- make_cal(drex$survey_samples, "SYN WCHG", yrs = all_years)
cal_qcs <- make_cal(drex$survey_samples, "SYN QCS", yrs = all_years)
cal_wcvi <- make_cal(drex$survey_samples, "SYN WCVI", yrs = all_years)

# caa_wchg <- filter(drex$survey_samples, survey_abbrev == "SYN WCHG") %>%
#   gfdlm::tidy_caa(yrs = all_years)
# caa_wchg[1, , ]
#
# caa_qcs <- filter(drex$survey_samples, survey_abbrev == "SYN QCS") %>%
#   gfdlm::tidy_caa(yrs = all_years)
# caa_qcs[1, , ]

mean_length <- filter(drex$survey_samples, survey_abbrev == "SYN WCHG") %>%
  gfdlm::tidy_mean_length() %>%
  filter(n > 10, year <= ending_year, year >= starting_year) %>%
  right_join(tibble(year = all_years), by = "year") %>%
  pull(mean_length)

mean_length

# bottom trawl catch:
if ("catch" %in% names(drex)) {
  catch <- drex$catch %>%
    filter(gear == "BOTTOM TRAWL") %>%
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

# hook and line catch for Rex is negligible:
if ("catch" %in% names(drex)) {
  catch_hl <- drex$catch %>%
    filter(gear == "HOOK AND LINE") %>%
    gfplot::tidy_catch() %>%
    group_by(year) %>%
    summarize(value = sum(value)) %>%
    right_join(tibble(year = all_years), by = "year") %>%
    #mutate(value = ifelse(year >= 1990 & year <= 1995, value / 3, value)) %>%
    pull(value)
  saveRDS(catch_hl, file = here::here("generated-data", "rex-catch-hl.rds"))
} else {
  catch_hl <- readRDS(here::here("generated-data", "rex-catch-hl.rds"))
}
catch_hl
plot(all_years, catch_hl, type = "o")

# catch per unit effort from the trawl fleet only for now:
#cpue <- read.csv(here::here("generated-data", "shortraker-cpue.csv"))

indexes <- gfplot::tidy_survey_index(drex$survey_index) %>%
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
