library("dplyr")
library("DLMtool")
library("MSEtool")
library("here")
library("purrr")
library("ggplot2")

library("future")
cores <- floor(future::availableCores()/2)
plan(multisession, workers = cores)

species_name <- "Rex Sole"
starting_year <- 1996
ending_year <- 2019
all_years <- seq(starting_year, ending_year)
nyear <- length(all_years)

fig_dir <- here("report", "figure")
if (!dir.exists(fig_dir)) dir.create(fig_dir)

drex <- readRDS(here("generated-data", "rex-filter-data.rds"))
d_catch <- readRDS(here::here("generated-data", "rex-catch2.rds"))
drex$catch <- dplyr::filter(d_catch, year >= 1996, year <= 2019)

# msa <- gfdata::get_survey_samples("rex sole", ssid = 2)
# saveRDS(msa, file = here("generated-data", "msa-ages-rex.rds"))
# msa <- readRDS(here("generated-data", "msa-ages-rex.rds"))
#
# msa <- dplyr::filter(msa, !is.na(age))
# if (!file.exists(here("generated-data", "rex-vb.rds"))) {
#   set.seed(1)
#   vb_model <- gfplot::fit_vb(msa, sex = "all", method = "mcmc", iter = 8000)
#   saveRDS(vb_model, file = here("generated-data", "rex-vb.rds"))
# } else {
#   vb_model <- readRDS(here("generated-data", "rex-vb.rds"))
# }
# vb_model$model
# vb_post <- rstan::extract(vb_model$model)
#
# plot(vb_model$predictions)

rex_om <- readRDS(here("generated-data", "rex-om.rds"))
nsim <- rex_om@nsim

rex_om@M
rex_om@Cobs
rex_om@Msd <- c(0, 0)
rex_om@Linfsd <- c(0, 0)
rex_om@Ksd <- c(0, 0)
rex_om@nyears
assertthat::assert_that(identical(rex_om@nyears, length(all_years)))

make_cal <- function(dat, survey, yrs, length_bin = 2) {
  cal <- dat %>%
    dplyr::filter(survey_abbrev == survey, sex == 2) %>%
    gfdlm::tidy_cal(yrs = yrs, interval = length_bin)

  length_bins <- gfdlm:::get_cal_bins(cal, length_bin_interval = length_bin)
  list(cal = cal[1, , ], length_bins = length_bins)
}

cal_wcvi <- make_cal(drex$survey_samples, "SYN WCVI", yrs = all_years)

catch <- drex$catch %>%
  gfplot::tidy_catch() %>%
  group_by(year) %>%
  summarize(value = sum(value)) %>%
  right_join(tibble(year = all_years), by = "year") %>%
  pull(value)
plot(all_years, catch, type = "o")

# d_cpue <- readRDS("/Volumes/Extreme-SSD/gfs/report/data-cache/cpue-index-dat.rds")
# d_cpue <- gfdata::get_cpue_index()
# saveRDS(d_cpue, file = here::here("generated-data", "trawl-cpue-data.rds"))
# ind <- gfsynopsis::fit_cpue_indices(dat = d_cpue,
#   species = "rex sole", areas = "3[CD]",
#   cache = here::here("generated-data"),
#   year_range = c(1996, 2019),
#   save_model = TRUE)
# saveRDS(ind, file =  here::here("generated-data", "rex-cpue-index.rds"))
cpue <- readRDS(here::here("generated-data", "rex-cpue-index.rds"))
ggplot(cpue, aes(year, est)) + geom_line() + facet_wrap(~model) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5)

indexes <- drex$survey_index %>%
  dplyr::filter(survey_abbrev %in% c("SYN WCVI")) %>%
  select(year, biomass, re) %>%
  right_join(tibble(year = all_years),by  = "year") %>%
  left_join(rename(select(cpue, year, est, se_link), trawl_cpue = est, trawl_sd = se_link), by = "year") %>%
  rename(syn_wcvi = biomass, syn_wcvi_sd = re)

# MSEtool::plot_composition(all_years,
#   obs = cal_wcvi$cal,
#   CAL_bins = cal_wcvi$length_bins
# )

rex_om@nsim <- nsim # 10 extras in case some don't converge
cores <- floor(parallel::detectCores() / 2)

rex_om@Cobs <- c(0, 0)
rex_om@Cbiascv <- c(0, 0)
rex_om@D <- c(0.3, 0.8) # gets replaced

# Set up alternative OMs for reference set and robustness set -----------------

saveRDS(indexes, file = here("generated-data/rex-indexes.rds"))

# .commercial_vul <- c(30, 18, 1)
# .surv_vul <- c(28, 12, 1)

# matching maturity, approximately:
.commercial_vul <- c(32, 20, 1)
.surv_vul <- c(32, 20, 1)

# # maturity ogive:
# mean(vb_post$linf) * (1-exp(-mean(vb_post$k) * (2 - mean(vb_post$t0))))
# mean(vb_post$linf) * (1-exp(-mean(vb_post$k) * (4.5 - mean(vb_post$t0))))
# mean(vb_post$linf) * (1-exp(-mean(vb_post$k) * (8 - mean(vb_post$t0))))

fit_sra_rex_cpue <- function(om,
  c_eq = 2,
  commercial_vul = .commercial_vul,
  surv_vul = .surv_vul,
  ...) {
  MSEtool::SRA_scope(om,
    Chist = catch,
    C_eq = c_eq * catch[1],
    Index = cbind(indexes$syn_wcvi, indexes$trawl_cpue),
    I_sd = cbind(indexes$syn_wcvi_sd, indexes$trawl_sd * 1.5),
    I_type = c("est", 1),
    s_selectivity = c("logistic", "logistic"),
    cores = 1,
    drop_nonconv = TRUE,
    vul_par = matrix(commercial_vul, 3, 1),
    s_vul_par = matrix(c(surv_vul, commercial_vul), 3, 2),
    map_vul_par = matrix(NA, 3, 1),
    map_s_vul_par = matrix(NA, 3, 2),
    f_name = "Commercial trawl",
    drop_highF = TRUE,
    s_name = c("SYN WCVI", "Commercial CPUE"),
    mean_fit = TRUE,
    ...
  )
}

rex_sra_ceq150 %<-% fit_sra_rex_cpue(rex_om, c_eq = 1.5)
rex_sra_ceq200 %<-% fit_sra_rex_cpue(rex_om, c_eq = 2)

# rex_sra_ceq125 %<-% fit_sra_rex_cpue(rex_om, c_eq = 1.25)
# saveRDS(rex_sra_ceq125, file = here("generated-data", "rex-sra-ceq125.rds"))
#
# rex_sra_ceq250 %<-% fit_sra_rex_cpue(rex_om, c_eq = 2.5)
# saveRDS(rex_sra_ceq250, file = here("generated-data", "rex-sra-ceq250.rds"))
#
# rex_sra_ceq300 %<-% fit_sra_rex_cpue(rex_om, c_eq = 3)
# saveRDS(rex_sra_ceq300, file = here("generated-data", "rex-sra-ceq300.rds"))

# plot(rex_sra_ceq200)
# plot(rex_sra_ceq150)

# age at fifty percent maturity:
# om@Linf[1] * (1-exp(-om@K[1] * (3 - om@t0[1])))
# 37.2 * (1-exp(-0.17 * (7 - -0.57)))

# 32.67298 * (1-exp(-0.4258144 * (4 - -0.5972734)))

# Alternative Reference Set OMs: M --------------------------------------------
# Only look at higher M and time-varying M. M in BC unlikely to be lower than GOA.

rex_om@M
rex_om_high_m <- rex_om
rex_om_high_m@M <- c(0.30, 0.30)
rex_sra_high_m %<-% fit_sra_rex_cpue(rex_om_high_m)
# plot(rex_sra_high_m)

# Alternative Reference Set OMs: Steepness (h) --------------------------------

rex_om_high_h <- rex_om
quantile(rex_om@cpars$h)
rex_om_high_h@cpars$h <- rep(0.95, length(rex_om@cpars$h))
rex_sra_high_h %<-% fit_sra_rex_cpue(rex_om_high_h)

# No CPUE ---------------------------------------------------------------------

fit_sra_rex_no_cpue <- function(om,
  c_eq = 2,
  commercial_vul = .commercial_vul,
  surv_vul = .surv_vul,
  ...) {
  MSEtool::SRA_scope(om,
    Chist = catch,
    C_eq = c_eq * catch[1],
    Index = cbind(indexes$syn_wcvi),
    I_sd = cbind(indexes$syn_wcvi_sd),
    I_type = c("est"),
    s_selectivity = c("logistic"),
    cores = 1,
    drop_nonconv = TRUE,
    vul_par = matrix(commercial_vul, 3, 1),
    s_vul_par = matrix(c(surv_vul), 3, 1),
    map_vul_par = matrix(NA, 3, 1),
    map_s_vul_par = matrix(NA, 3, 1),
    f_name = "Commercial trawl",
    drop_highF = TRUE,
    s_name = c("SYN WCVI"),
    mean_fit = TRUE,
    ...
  )
}

rex_sra_no_cpue %<-% fit_sra_rex_no_cpue(rex_om, c_eq = 3)

# plot(rex_sra_no_cpue)

# No CPUE and lightly fished before 1996 --------------------------------------

rex_sra_light %<-% fit_sra_rex_no_cpue(rex_om, c_eq = .5)

# plot(rex_sra_light)

# Growth parameters from Oregon @hosie1976 ------------------------------------

# om <- rex_om
# om@K <- c(0.17, 0.17)
# om@Linf <- c(37.2, 37.2)
# om@t0 <- c(-0.57, -0.57)
# rex_sra_oregon <- fit_sra_rex_cpue(
#   om,
#   c_eq = 2,
#   commercial_vul = c(25, 15, 1),
#   surv_vul = c(24, 12, 1)
# )
# # plot(rex_sra_oregon)
# saveRDS(rex_sra_oregon, file = here("generated-data", "rex-sra-oregon.rds"))

# Shift commercial selectivity curve left -------------------------------------

rex_sra_sel1 %<-% fit_sra_rex_cpue(rex_om, commercial_vul = c(28, 12, 1), surv_vul = c(28, 12, 1))
# plot(rex_sra_sel1)

# Robustness Set OMs: M increasing over time ----------------------------------

rex_om_inc_m <- rex_om
rex_om_inc_m@nyears
rex_om_inc_m@proyears
M_age <- array(0.2,
  c(rex_om_inc_m@nsim, rex_om_inc_m@maxage, rex_om_inc_m@nyears + rex_om_inc_m@proyears))
m <- seq(0.2, 0.4, length.out = rex_om_inc_m@proyears)
for (i in seq_along(m)) M_age[, , rex_om_inc_m@nyears + i] <- m[i]
plot(M_age[1,1,]) # one sim; one age
rex_om_inc_m@cpars$M_ageArray <- M_age
rex_sra_inc_m %<-% fit_sra_rex_cpue(rex_om_inc_m)

# Save SRAs -------------------------------------------------------------------

saveRDS(rex_sra_inc_m, file = here("generated-data", "rex-sra-inc-m.rds"))
saveRDS(rex_sra_sel1, file = here("generated-data", "rex-sra-sel1.rds"))
saveRDS(rex_sra_ceq150, file = here("generated-data", "rex-sra-ceq150.rds"))
saveRDS(rex_sra_ceq200, file = here("generated-data", "rex-sra-ceq200.rds"))
saveRDS(rex_sra_high_m, file = here("generated-data", "rex-sra-high-m.rds"))
saveRDS(rex_sra_high_h, file = here("generated-data", "rex-sra-high-h.rds"))
saveRDS(rex_sra_no_cpue, file = here("generated-data", "rex-sra-no-cpue.rds"))
saveRDS(rex_sra_light, file = here("generated-data", "rex-sra-no-cpue-light.rds"))

# Set up the scenario names ---------------------------------------------------

sc <- tibble::tribble(
  ~scenario, ~scenario_human, ~scenario_type,
  "ceq150", "Ceq. 150%", "Reference",
  "ceq200", "Ceq. 200%", "Reference",
  "high-m", "Higher M", "Reference",
  "high-h", "Higher steepness", "Reference",
  "sel1", "Lower selectivity", "Reference",
  # "oregon", "Oregon growth", "Reference",
  "no-cpue", "No CPUE Ceq. 250%", "Reference",
  "no-cpue-light", "No CPUE Ceq. 50%", "Reference",
  "inc-m", "M inc.", "Robustness"
)
sc <- mutate(sc, order = seq_len(n()))
saveRDS(sc, file = here("generated-data/rex-scenarios.rds"))

sra_rex <- purrr::map(sc$scenario, ~ {
  readRDS(here("generated-data", paste0("rex-sra-", .x, ".rds")))
})
names(sra_rex) <- sc$scenario

# Some plots ------------------------------------------------------------------

# FIXME: get this into gfdlm:
get_depletion <- function(x, scenario) {
  depletion <- x@SSB / sapply(x@Misc, getElement, "E0_SR")
  last_year <- dim(depletion)[2]
  all_years <- seq(x@OM@CurrentYr - x@OM@nyears + 1, x@OM@CurrentYr)

  d1 <- t(apply(depletion[, -last_year], 2,
    FUN = quantile,
    probs = c(0.025, 0.5, 0.975)
  )) %>%
    as.data.frame() %>%
    cbind(all_years) %>%
    mutate(scenario = scenario) %>%
    rename(lwr = 1, med = 2, upr = 3, year = all_years)
  d2 <- t(apply(depletion[, -last_year], 2,
    FUN = quantile,
    probs = c(0.25, 0.75))) %>%
    as.data.frame() %>%
    cbind(all_years) %>%
    rename(lwr50 = 1, upr50 = 2, year = all_years)

  left_join(d1, d2, by = "year")
}

g <- purrr::map2_df(sra_rex, sc$scenario_human, get_depletion) %>%
  mutate(scenario = factor(scenario, levels = sc$scenario_human)) %>%
  ggplot(aes(year, med, ymin = lwr, ymax = upr)) +
  geom_ribbon(fill = "grey90") +
  geom_ribbon(fill = "grey70", mapping = aes(ymin = lwr50, ymax = upr50)) +
  geom_line() +
  facet_wrap(vars(scenario)) +
  gfplot::theme_pbs() +
  labs(x = "Year", y = "Depletion")
ggsave(here::here("report/figure/rex-compare-SRA-depletion-panel.png"),
  width = 8, height = 6
)

sra_rex %>% set_names(sc$scenario_human) %>%
  gfdlm::plot_index_fits(survey_names = c("SYN WCVI", "Commercial CPUE")) +
  ylim(0, 2.5)
ggsave(here::here("report/figure/rex-index-fits.png"), width = 5.5, height = 9.5)

# FIXME: get this into gfdlm:
get_sra_selectivity <- function(sc_name) {
  sra <- sra_rex[[sc_name]]
  x <- do.call(cbind, purrr::map(sra@Misc, ~ .$vul_len))
  out <- reshape2::melt(x) %>%
    rename(Length = Var1, iter = Var2)
  out$scenario <- sc_name
  out
}
sel <- map_dfr(sc$scenario[1], get_sra_selectivity) # pick one
sel %>%
  ggplot(aes(Length, value, group = paste(iter))) +
  geom_line(alpha = 0.15) +
  gfplot::theme_pbs() +
  ylab("Selectivity") + xlab("Length") +
  coord_cartesian(expand = FALSE, ylim = c(-0.01, 1.01))
ggsave(here::here("report/figure/rex-selectivity.png"), width = 5, height = 3)

# sra_rex[[1]]@Misc[[1]]$s_vul[1,,1]
# sra_rex[[1]]@Misc[[1]]$s_vul[1,,2]
