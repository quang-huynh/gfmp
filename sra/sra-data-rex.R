library(dplyr)
species_name <- "rex sole"
starting_year <- 1996
ending_year <- 2018
all_years <- seq(starting_year, ending_year)
drex <- load_data_rex() %>% filter_data_rex()

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
rex_om@Cobs
rex_om@Perr
rex_om@nsim <- 4
rex_om@Msd <- c(0, 0)
rex_om@Linfsd <- c(0, 0)
rex_om@Ksd <- c(0, 0)
rex_om@nyears
stopifnot(identical(rex_om@nyears, length(all_years)))

# from WCVI:
# rex_om@Iobs <- c(0.09, 0.09) # specified in `MSEtool::SRA_scope(I_sd = ...)`

make_cal <- function(dat, survey, yrs, length_bin = 5) {
  cal <- dat %>%
    filter(survey_abbrev == survey) %>%
    gfdlm::tidy_cal(yrs = yrs, interval = length_bin)

  length_bins <- gfdlm::get_cal_bins(cal, length_bin_interval = length_bin)
  list(cal = cal[1, , ], length_bins = length_bins)
}

cal_wcvi <- make_cal(drex$survey_samples, "SYN WCVI", yrs = all_years)

mean_length <- filter(drex$survey_samples, survey_abbrev == "SYN WCVI") %>%
  gfdlm::tidy_mean_length() %>%
  filter(n > 10, year <= ending_year, year >= starting_year) %>%
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
plot(all_years, catch)

# catch per unit effort from the trawl fleet only:
# cpue <- read.csv(here::here("generated-data", rex-cpue.csv"))

indexes <- drex$survey_index %>%
  filter(survey_abbrev %in% c("SYN WCVI")) %>%
  select(year, biomass, re) %>%
  right_join(tibble(year = all_years), by = "year") %>%
  # left_join(rename(select(cpue, year, est), trawl_cpue = est), by = "year") %>%
  select(-year) %>%
  as.matrix()

indexes
plot(all_years, indexes[, 1L])
plot(all_years, indexes[, 2L])

MSEtool::plot_composition(all_years,
  obs = cal_wcvi$cal,
  CAL_bins = cal_wcvi$length_bins
)

rex_om@nsim <- 8L
rex_sra1 <- MSEtool:::SRA_scope(rex_om,
  Chist = catch, Index = indexes[, 1L],
  CAL = cal_wcvi$cal, length_bin = cal_wcvi$length_bins, I_sd = indexes[, 2L],
  I_type = 1L, cores = 1L, report = TRUE
)

MSEtool::plot_SRA_scope(rex_sra1$OM,
  Chist = catch,
  CAL = cal_wcvi$cal,
  Index = indexes[, 1L], report_list = rex_sra1$report, Year = all_years
)

rex_om@nsim <- 48L
cores <- floor(parallel::detectCores() / 2)
rex_sra2 <- MSEtool:::SRA_scope(rex_om,
  Chist = catch, Index = indexes[, 1L], integrate = FALSE,
  I_sd = indexes[, 2L], I_type = 1L, cores = cores, report = TRUE
)
hist(rex_sra2$OM@cpars$D)
matplot(t(rex_sra2$OM@cpars$Perr_y), type = "l", lty = 1, col = "#00000040")
hist(rex_sra2$OM@cpars$AC)
matplot(t(rex_sra2$output$SSB), type = "l", lty = 1, col = "#00000040")

plot(all_years, exp(rex_sra2$report[[1]]$log_rec_dev))

# FIXME: apical F needs to be bounded to something reasonable; look into what's causing this here (low survey index and high catches in 2005)
matplot(t(rex_sra2$OM@cpars$Find),
  type = "l", lty = 1, col = "#00000040",
  ylim = c(0, 3)
)
plot(rex_sra2$OM@EffYears, rex_sra2$OM@EffLower, type = "o", ylim = c(0, 3))
lines(rex_sra2$OM@EffYears, rex_sra2$OM@EffUpper, type = "o")

MSEtool::plot_SRA_scope(rex_sra2$OM,
  Chist = catch,
  Index = indexes[, 1L], report_list = rex_sra2$report, Year = all_years
)
