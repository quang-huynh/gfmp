library(dplyr)
library(DLMtool)
library(MSEtool)

species_name <- "Rex Sole"
starting_year <- 1996
ending_year <- 2018
all_years <- seq(starting_year, ending_year)

drex <- readRDS(here::here("generated-data", "rex-filter-data.rds"))

rex_om <- readRDS(here::here("generated-data", "rex-om.rds"))
rex_om@M
rex_om@Linf
rex_om@K
rex_om@t0
rex_om@a
rex_om@b
rex_om@L50
rex_om@L50_95
rex_om@LFS
rex_om@L5
rex_om@Cobs
rex_om@Perr
rex_om@Msd <- c(0, 0)
rex_om@Linfsd <- c(0, 0)
rex_om@Ksd <- c(0, 0)
rex_om@nyears
rex_om@maxage
assertthat::assert_that(identical(rex_om@nyears, length(all_years)))

make_cal <- function(dat, survey, yrs, length_bin = 5) {
  cal <- dat %>%
    dplyr::filter(survey_abbrev == survey) %>%
    gfdlm::tidy_cal(yrs = yrs, interval = length_bin)

  length_bins <- gfdlm:::get_cal_bins(cal, length_bin_interval = length_bin)
  list(cal = cal[1, , ], length_bins = length_bins)
}

cal_wcvi <- make_cal(drex$survey_samples, "SYN WCVI", yrs = all_years)

mean_length <- dplyr::filter(drex$survey_samples, survey_abbrev == "SYN WCVI") %>%
  gfdlm::tidy_mean_length() %>%
  dplyr::filter(n > 10, year <= ending_year, year >= starting_year) %>%
  right_join(tibble(year = all_years), by = "year") %>%
  pull(mean_length)

mean_length

if ("catch" %in% names(drex)) {
  catch <- drex$catch %>%
    gfplot::tidy_catch() %>%
    group_by(year) %>%
    summarize(value = sum(value)) %>%
    right_join(tibble(year = all_years), by = "year") %>%
    pull(value)
  saveRDS(catch, file = here::here("generated-data", "rex-catch.rds"))
} else {
  catch <- readRDS(here::here("generated-data", "rex-catch.rds"))
}
catch
plot(all_years, catch, type = "o")

# catch per unit effort from the trawl fleet only:
# cpue <- read.csv(here::here("generated-data", "rex-cpue.csv"))

indexes <- drex$survey_index %>%
  dplyr::filter(survey_abbrev %in% c("SYN WCVI")) %>%
  select(year, biomass, re) %>%
  right_join(tibble(year = all_years),by  = "year") %>%
  # left_join(rename(select(cpue, year, est), trawl_cpue = est), by = "year") %>%
  select(-year) %>%
  as.matrix()

indexes
plot(all_years, indexes[, 1L])
plot(all_years, indexes[, 2L], type = "o")

MSEtool::plot_composition(all_years,
  obs = cal_wcvi$cal,
  CAL_bins = cal_wcvi$length_bins
)

I_sd <- indexes[, 2L]
I_sd

rex_om@M
rex_om@maxage
rex_om@L5
rex_om@LFS

rex_om@nsim <- 48
cores <- floor(parallel::detectCores() / 2)
cores <- 4

rex_om@Cobs <- c(0, 0)
rex_om@Cbiascv <- c(0, 0)
library(MSEtool)
rex_sra3 <- MSEtool::SRA_scope(rex_om,
  # CAL = cal_wcvi$cal, length_bin = cal_wcvi$length_bins,
  Chist = catch, Index = indexes[, 1], integrate = FALSE,
  # C_eq = catch[1],
  # FIXME: consider setting this to assumed equilibrium catch to estimate initial depletion:
  C_eq = 0,
  I_sd = I_sd, I_type = "B", cores = cores,
  drop_nonconv = TRUE
)

plot(rex_sra3)
hist(rex_sra2@OM@cpars$D)
matplot(t(rex_sra2@OM@cpars$Perr_y), type = "l", lty = 1, col = "#00000040")
hist(rex_sra2@OM@cpars$AC)
matplot(t(rex_sra2@SSB), type = "l", lty = 1, col = "#00000040")
matplot(t(rex_sra2@OM@cpars$Find), type = "l", lty = 1, col = "#00000040", ylim = c(0, 2))
matplot(t(Data@Cat)[, 1], type = "l")

saveRDS(rex_sra3, file = here::here("generated-data", "rex-sra.rds"))
