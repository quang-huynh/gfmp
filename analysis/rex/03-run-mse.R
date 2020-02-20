# 2020-02-19
# DO NOTE USE ME!
# See 05-plot-all.R for now.
library("DLMtool")
library("MSEtool")
library("dplyr")
library("purrr")
library("ggplot2")
library("gfdlm")
library("here")
library("assertthat")

# Settings --------------------------------------------------------------------

sp <- "rex" # Species: used in filenames
cores <- floor(parallel::detectCores() / 2L)
sc <- readRDS(here("generated-data/rex-scenarios.rds"))
sc # look good?
nsim <- 50
interval <- 2L
base_om <- "ceq100" # affects "failed" MP example trajectory plot
mp <- suppressMessages(readr::read_csv(here("analysis", "rex", "mp.txt"), comment = "#"))
as.data.frame(mp) # look good?
reference_mp <- c("FMSYref75", "NFref", "FMSYref")
ref_mp_cols <- c("grey60", "grey20", "grey85") %>% set_names(reference_mp)

catch_breaks <- c(0, 100000, 200000, 300000)
catch_labels <- c("0", "100", "200", "300")

# Satisficing rules:
LT_LRP_thresh <- 0.8
STC_thresh <- 0.7
this_year <- 2018

# Set up PMs ------------------------------------------------------------------

catch <- readRDS(here("generated-data", "rex-catch.rds"))
yrs <- length(catch)
ref_catch <- mean(catch[(yrs-5+1):yrs])
ref_aadc <- gfdlm:::get_aadc(catch[(yrs-10+1):yrs])

`LT LRP` <- gfdlm::pm_factory("SBMSY", 0.4, c(36, 50))
`LT USR` <- gfdlm::pm_factory("SBMSY", 0.8, c(36, 50))
FMSY <- DLMtool::PNOF
STC <- gfdlm::pm_factory("LTY", 1, c(1, 10))
LTC <- gfdlm::pm_factory("LTY", 1, c(36, 50))
AADC <- gfdlm::pm_factory("AADC", ref_aadc, c(1, 50))
PM <- c("LT LRP", "LT USR", "FMSY", "STC", "LTC", "AADC")

# Set up and checks -----------------------------------------------------------

stopifnot(all(reference_mp %in% mp$mp))
fig_dir <- here("report", "figure")
if (!dir.exists(fig_dir)) dir.create(fig_dir)
.ggsave <- function(filename, width, height) {
  ggsave(file.path(fig_dir, paste0(sp, "-", filename, ".png")),
    width = width, height = height
  )
}

get_filtered_scenario <- function(type, column) {
  dplyr::filter(sc, scenario_type == type) %>%
    pull(!!column) %>%
    set_names()
}
scenarios <- sc$scenario %>% set_names()
scenarios_human <- sc$scenario_human %>% set_names()
scenarios_ref <- get_filtered_scenario("Reference", "scenario")
scenarios_ref_human <- get_filtered_scenario("Reference", "scenario_human")
scenarios_rob <- get_filtered_scenario("Robustness", "scenario")
scenarios_rob_human <- get_filtered_scenario("Robustness", "scenario_human")

# Read OMs --------------------------------------------------------------------

om <- map(scenarios, ~ {
  om <- readRDS(here("generated-data", paste0(sp, "-sra-", .x, ".rds")))@OM
  if (om@nsim < nsim) {
    stop("nsim set larger than in conditioned OM.", call. = FALSE)
  }
  om@nsim <- nsim
  om@interval <- interval
  om
})

# Fit MPs to all OMs ----------------------------------------------------------

.SP6040_prior <- add_SP_prior(.SP6040_gf, r_prior = c(0.3, 0.1))
.SP8040_prior <- add_SP_prior(.SP8040_gf, r_prior = c(0.3, 0.1))
.SP4010_prior <- add_SP_prior(.SP4010_gf, r_prior = c(0.3, 0.1))

missing <- mp$mp[!map_lgl(mp$mp, exists)]
assert_that(length(missing) == 0,
  msg = glue::glue(
    "{paste(missing, collapse = ', ')} do not exist in the current environment."
  ))

# testing:
# .mp <- c(
#   "CC_hist20", "CC1.2", "CC1.1", "CC1.0", "CC0.9", "CC0.8", "CC0.7", "CC0.6",
#   ".Iratio2",
#   ".GB_slope6_0.66", ".GB_slope6_1", ".GB_slope8_0.66", ".GB_slope8_1",
#   ".Islope0.2_80", ".Islope0.2_100", ".Islope0.4_80", ".Islope0.4_100",
#   ".IDX", ".IDX_smooth",
#   ".IT10_hist", ".IT5_hist",
#   ".Itarget_base", ".Itarget_w0.8", ".Itarget_x0.2", ".Itarget_x0.8", ".Itarget_d1.2", ".Itarget_d0.8",
#   ".ITM_hist",
#   ".SP4010_prior", ".SP6040_prior", ".SP8040_prior",
#   "NFref", "FMSYref", "FMSYref75"
# )

fit_scenario <- function(scenario) {
  file_name <- here("generated-data", paste0(sp, "-mse-", scenario, ".rds"))
  if (!file.exists(file_name)) {
    message("Running closed-loop-simulation for ", scenario, " OM")
    mse <- runMSE(OM = om[[scenario]], MPs = mp$mp, parallel = TRUE)
    saveRDS(mse, file = file_name)
  } else {
    message("Loading closed-loop-simulation for ", scenario, " OM")
    mse <- readRDS(file_name)
  }
  mse
}
DLMtool::setup(cpus = cores)
mse <- map(scenarios, fit_scenario)
snowfall::sfStop()
for (i in seq_along(mse)) mse[[i]]@OM$RefY <- ref_catch * 1

# Satisficing -----------------------------------------------------------------

pm_df_list <- map(mse[scenarios_ref], ~ gfdlm::get_probs(.x, PM))
pm_df_list_rob <- map(mse[scenarios_rob], ~ gfdlm::get_probs(.x, PM))
pm_df <- bind_rows(pm_df_list, .id = "scenario")
pm_avg <- group_by(pm_df, MP) %>% summarise_if(is.numeric, mean)
pm_min <- group_by(pm_df, MP) %>% summarise_if(is.numeric, min)
saveRDS(pm_df_list, file = here("generated-data", "rex-pm-all.rds"))

# plot_tigure_facet(pm_df_list)
plot_tigure(pm_avg, satisficed = c("LT LRP" = 0.8, "STC" = 0.5))
plot_tigure(pm_min)

mp_sat <- dplyr::filter(pm_avg, `LT LRP` > 0.8, `STC` > 0.6) %>%
  # arrange(-`LT LRP`, -`LT LRP`, -`LT USR`, -`STC`, -`LTC`) %>%
  pull(MP)
mp_sat <- mp_sat[!mp_sat %in% reference_mp]
mp_sat
stopifnot(length(mp_sat) >= 1)
stopifnot(length(mp_sat) <= 8) # for RColorBrewer::brewer.pal()
mp_sat_with_ref <- union(mp_sat, reference_mp)
mp_not_sat <- mp$mp[!mp$mp %in% mp_sat_with_ref]
stopifnot(length(mp_not_sat) > 1)

mse_sat <- map(scenarios, ~ DLMtool::Sub(mse[[.x]], MPs = mp_sat))
mse_sat_with_ref <- map(scenarios_ref, ~ DLMtool::Sub(mse[[.x]], MPs = mp_sat_with_ref))

pm_df_list_sat <- map(pm_df_list, dplyr::filter, MP %in% mp_sat)
pm_df_list_sat_with_ref <- map(pm_df_list, dplyr::filter, MP %in% mp_sat_with_ref)

custom_pal <- c(RColorBrewer::brewer.pal(8, "Set2")[seq_along(mp_sat)], ref_mp_cols) %>%
  set_names(mp_sat_with_ref)
