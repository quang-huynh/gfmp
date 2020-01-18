library(dplyr)
library(DLMtool)
library(MSEtool)
library(here)
library(cowplot)
library(ggplot2)

species_name <- "Rex Sole"
starting_year <- 1996
ending_year <- 2018
all_years <- seq(starting_year, ending_year)
nyear <- length(all_years)

fig_dir <- here("report", "figure")
if (!dir.exists(fig_dir)) dir.create(fig_dir)

drex <- readRDS(here("generated-data", "rex-filter-data.rds"))

rex_om <- readRDS(here("generated-data", "rex-om.rds"))
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
rex_om@M
rex_om@maxage
rex_om@h
rex_om@L5
rex_om@LFS
assertthat::assert_that(identical(rex_om@nyears, length(all_years)))

make_cal <- function(dat, survey, yrs, length_bin = 5) {
  cal <- dat %>%
    dplyr::filter(survey_abbrev == survey) %>%
    gfdlm::tidy_cal(yrs = yrs, interval = length_bin)

  length_bins <- gfdlm:::get_cal_bins(cal, length_bin_interval = length_bin)
  list(cal = cal[1, , ], length_bins = length_bins)
}

cal_wcvi <- make_cal(drex$survey_samples, "SYN WCVI", yrs = all_years)

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
# cpue <- readRDS("../../gfs/report/cpue-cache/rex-sole.rds")
# readr::write_csv(cpue, path = here("generated-data", "rex-cpue.csv"))
cpue <- read.csv(here("generated-data", "rex-cpue.csv")) %>%
  filter(area == "3CD")

indexes <- drex$survey_index %>%
  dplyr::filter(survey_abbrev %in% c("SYN WCVI")) %>%
  select(year, biomass, re) %>%
  right_join(tibble(year = all_years),by  = "year") %>%
  left_join(rename(select(cpue, year, est, se_link), trawl_cpue = est, trawl_sd = se_link), by = "year") %>%
  select(-year) %>%
  as.matrix()

indexes
plot(all_years, indexes[, 1L], type = "o")
# plot(all_years, indexes[, 2L], type = "o")

plot(all_years, indexes[, 3L], type = "o")

MSEtool::plot_composition(all_years,
  obs = cal_wcvi$cal,
  CAL_bins = cal_wcvi$length_bins
)

I_sd <- indexes[, 2L]
I_sd

rex_om@nsim <- 200
cores <- floor(parallel::detectCores() / 1)

rex_om@Cobs <- c(0, 0)
rex_om@Cbiascv <- c(0, 0)

# Set up alternative OMs for reference set and robustness set below -----------

# Alternative Reference Set OMs: Ceq, the fraction of 1996 catch in 1995
# Impacts depletion and stock size

#ceq0 is the "unfished in 1995" scenario

fit_sra_rex <- function(om, c_eq = 0.5, ...) {
  MSEtool::SRA_scope(rex_om,
    Chist = catch, Index = indexes[, 1], integrate = FALSE,
    C_eq = c_eq * catch[1],
    I_sd = I_sd, I_type = "B", cores = cores,
    drop_nonconv = TRUE, mean_fit = TRUE, ...
  )
}
rex_sra_ceq0 <- fit_sra_rex(rex_om, c_eq = 0)
rex_sra_ceq50 <- fit_sra_rex(rex_om, c_eq = 0.5)
rex_sra_ceq100 <- fit_sra_rex(rex_om, c_eq = 1)
rex_sra_ceq200 <- fit_sra_rex(rex_om, c_eq = 2)

saveRDS(rex_sra_ceq0, file = here("generated-data", "rex-sra-ceq0.rds"))
saveRDS(rex_sra_ceq50, file = here("generated-data", "rex-sra-ceq50.rds"))
saveRDS(rex_sra_ceq100, file = here("generated-data", "rex-sra-ceq100.rds"))
saveRDS(rex_sra_ceq200, file = here("generated-data", "rex-sra-ceq200.rds"))

quantile(rex_sra_ceq0@OM@cpars$D)
quantile(rex_sra_ceq10@OM@cpars$D)
quantile(rex_sra_ceq50@OM@cpars$D)
quantile(rex_sra_ceq100@OM@cpars$D)
quantile(rex_sra_ceq200@OM@cpars$D)

# Alternative Reference Set OMs: M --------------------------------------------
# Only look at higher M and time-varying M. M in BC unlikely to be lower than GOA.

rex_om@M
rex_om_high_m <- rex_om
rex_om_high_m@M <- c(0.25, 0.25)
rex_sra_high_m <- fit_sra_rex(rex_om_high_m)

saveRDS(rex_sra_high_m, file = here("generated-data", "rex-sra-high-m.rds"))

quantile(rex_sra_ceq50@OM@cpars$D)
quantile(rex_sra_high_m@OM@cpars$D)

# Alternative Reference Set OMs: Steepness (h) --------------------------------

rex_om_low_h <- rex_om
rex_om@h
rex_om_low_h@h <- c(0.5, 0.7)
rex_sra_low_h <- fit_sra_rex(rex_om_low_h)
saveRDS(rex_sra_low_h, file = here("generated-data", "rex-sra-low-h.rds"))

quantile(rex_sra_ceq50@OM@cpars$D)
quantile(rex_sra_low_h@OM@cpars$D)
median(rex_sra_ceq50@OM@cpars$D)
median(rex_sra_low_h@OM@cpars$D)

rex_om_high_h <- rex_om
rex_om@h
rex_om_high_h@h <- c(0.95, 0.95)
rex_sra_high_h <- fit_sra_rex(rex_om_high_h)
saveRDS(rex_sra_high_h, file = here("generated-data", "rex-sra-high-h.rds"))

quantile(rex_sra_ceq50@OM@cpars$D)
quantile(rex_sra_high_h@OM@cpars$D)

# Robustness Set OMs: M increasing/decreasing over time -----------------------

rex_om_inc_m <- rex_om
rex_om_inc_m@nyears
rex_om_inc_m@proyears
M_age <- array(0.2,
  c(rex_om_inc_m@nsim, rex_om_inc_m@maxage, rex_om_inc_m@nyears + rex_om_inc_m@proyears))
m <- seq(0.2, 0.4, length.out = rex_om_inc_m@proyears)
for (i in seq_along(m)) M_age[, , rex_om_inc_m@nyears + i] <- m[i]
plot(M_age[1,1,]) # one sim; one age
rex_om_inc_m@cpars$M_ageArray <- M_age
rex_sra_inc_m <- fit_sra_rex(rex_om_inc_m)
saveRDS(rex_sra_inc_m, file = here("generated-data", "rex-sra-inc-m.rds"))

rex_om_dec_m <- rex_om
M_age <- array(0.20,
  c(rex_om_dec_m@nsim, rex_om_dec_m@maxage, rex_om_dec_m@nyears + rex_om_dec_m@proyears))
m <- seq(0.20, 0.10, length.out = rex_om_dec_m@proyears)
for (i in seq_along(m)) M_age[, , rex_om_dec_m@nyears + i] <- m[i]
plot(M_age[1,1,]) # one sim; one age
rex_om_dec_m@cpars$M_ageArray <- M_age
rex_sra_dec_m <- fit_sra_rex(rex_om_dec_m)
saveRDS(rex_sra_dec_m, file = here("generated-data", "rex-sra-dec-m.rds"))

# Some plots ------------------------------------------------------------------

scenarionames <- c(
  "ceq0", "ceq50",
  "ceq100", "ceq200", "high-m",
  "low-h", "high-h", "inc-m", "dec-m")
scenarios_human <- c(
  "Catch eq. 0%", "Catch eq. 50%",
  "Catch eq. 100%", "Catch eq. 200%", "M = 0.25",
  "h = 0.5-0.7", "h = 0.95", "M increasing", "M decreasing")
tibble(scenarionames, scenarios_human) # look good?

sra_rex <- map(scenarionames, ~ {
  readRDS(here("generated-data", paste0("rex-sra-", .x, ".rds")))
})
names(sra_rex) <- scenarionames

get_depletion <- function(x, scenario) {
  depletion <- x@SSB / sapply(x@Misc, getElement, "E0_SR")
  d1 <- t(apply(depletion[, -nyear], 2,
    FUN = quantile,
    probs = c(0.025, 0.5, 0.975)
  )) %>%
    as.data.frame() %>%
    cbind(all_years) %>%
    mutate(scenario = scenario) %>%
    rename(lwr = 1, med = 2, upr = 3, year = all_years)
  d2 <- t(apply(depletion[, -nyear], 2, FUN = quantile, probs = c(0.25, 0.75))) %>%
    as.data.frame() %>%
    cbind(all_years) %>%
    rename(lwr50 = 1, upr50 = 2, year = all_years)

  left_join(d1, d2, by = "year")
}

g <- purrr::map2_df(sra_rex, scenarios_human, get_depletion) %>%
  ggplot(aes(year, med, ymin = lwr, ymax = upr)) +
  geom_ribbon(fill = "grey90") +
  geom_ribbon(fill = "grey70", mapping = aes(ymin = lwr50, ymax = upr50)) +
  geom_line() +
  facet_wrap(vars(scenario)) +
  gfplot::theme_pbs() +
  labs(x = "Year", y = "Depletion")
ggsave(file.path(fig_dir, paste0("rex-compare-SRA-depletion-panel.png")),
  width = 8, height = 6
)

# # Make multipanel plots using purrr and cowplot
# initDepletionPlots <- purrr::map2(scenarios, scenarios_human, make_initD)
# g <- cowplot::plot_grid(plotlist = initDepletionPlots, align = "hv", nrow = 4, ncol = 2)
# ggsave(file.path(fig_dir, paste0("rex-compare-SRA-init-depletion-panel.png")),
#   width = 11, height = 12
# )
#
# DepletionPlots <- purrr::map2(scenarios, scenarios_human, make_Depletion)
# g <- cowplot::plot_grid(plotlist = DepletionPlots, align = "hv", nrow = 4, ncol = 2)
# ggsave(file.path(fig_dir, paste0("rex-compare-SRA-depletion-panel.png")),
#   width = 11, height = 12
# )
#
# SSBPlots <- purrr::map2(scenarios, scenarios_human, make_Biomass)
# g <- cowplot::plot_grid(plotlist = SSBPlots, align = "hv", nrow = 4, ncol = 2)
# ggsave(file.path(fig_dir, paste0("rex-compare-SRA-SSB-panel.png")),
#   width = 11, height = 12
# )
#
