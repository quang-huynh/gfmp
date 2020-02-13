library("DLMtool")
library("MSEtool")
library("dplyr")
library("purrr")
library("ggplot2")
library("gfdlm")
library("here")
# load_all("../gfdlm")

# Settings --------------------------------------------------------------------

sp <- "rex" # Species: used in filenames
cores <- floor(parallel::detectCores() / 2L)
sc <- readRDS("generated-data/rex-scenarios.rds")
sc # look good?
nsim <- 200L
interval <- 2L
base_om <- "ceq100" # affects some default plots
mp <- suppressMessages(readr::read_csv(here("analysis", "rex", "mp.txt"), comment = "#"))
as.data.frame(mp) # look good?
reference_mp <- c("FMSYref75", "NFref", "FMSYref")
ref_mp_cols <- c("grey60", "grey20", "grey85") %>% set_names(reference_mp)
#
catch_breaks <- c(0, 100000, 200000, 300000)
catch_labels <- c("0", "100", "200", "300")

# Satisficing rules:
LT_LRP_thresh <- 0.8
STC_thresh <- 0.7

# Set up PMs ------------------------------------------------------------------

`LT LRP` <- gfdlm::pm_factory("SBMSY", 0.4, c(36, 50))
`LT USR` <- gfdlm::pm_factory("SBMSY", 0.8, c(36, 50))
`FMSY` <- DLMtool::PNOF
`AAVC` <- DLMtool::AAVY
`STC` <- gfdlm::pm_factory("LTY", 0.5, c(1, 10))
`LTC` <- gfdlm::pm_factory("LTY", 0.5, c(36, 50))
PM <- c("LT LRP", "LT USR", "FMSY", "STC", "LTC", "AAVC")

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
scenarios_ref <- get_filtered_scenario("Reference", "scenario")
scenarios_rob <- get_filtered_scenario("Robustness", "scenario")

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
names(om) <- scenarios

# Fit MPs to all OMs ----------------------------------------------------------

fit_scenario <- function(scenario) {
  file_name <- here("generated-data", paste0(sp, "-mse-", scenario, ".rds"))
  if (!file.exists(file_name)) {
    message("Running closed-loop-simulation for ", scenario, " OM")
    DLMtool::setup(cpus = cores)
    mse <- runMSE(OM = om[[scenario]], MPs = mp$mp, parallel = TRUE)
    snowfall::sfStop()
    saveRDS(mse, file = file_name)
  } else {
    message("Loading closed-loop-simulation for ", scenario, " OM")
    mse <- readRDS(file_name)
  }
  mse
}
mse <- map(scenarios, fit_scenario)
names(mse) <- scenarios

# Satisficing -----------------------------------------------------------------

pm_df_list <- map(mse[scenarios_ref], ~ gfdlm::get_probs(.x, PM))
pm_df_list_rob <- map(mse[scenarios_rob], ~ gfdlm::get_probs(.x, PM))
pm_df <- bind_rows(pm_df_list, .id = "scenario")
saveRDS(pm_df, file = here::here("generated-data/rex-pm-all.rds"))
pm_avg <- group_by(pm_df, MP) %>% summarise_if(is.numeric, mean)
pm_min <- group_by(pm_df, MP) %>% summarise_if(is.numeric, min)
#
mp_sat <- dplyr::filter(pm_min, `LT LRP` > LT_LRP_thresh, `STC` > STC_thresh) %>%
  arrange(-`LT LRP`, -`LT LRP`, -`LT USR`, -`STC`, -`LTC`, -AAVC) %>%
  pull(MP)
mp_sat <- mp_sat[!mp_sat %in% reference_mp]
mp_sat
stopifnot(length(mp_sat) > 1)
stopifnot(length(mp_sat) <= 8) # for RColorBrewer::brewer.pal() "Set2"
mp_sat_with_ref <- union(mp_sat, reference_mp)
mp_not_sat <- mp$mp[!mp$mp %in% mp_sat_with_ref]
stopifnot(length(mp_not_sat) > 1)

custom_pal <- c(RColorBrewer::brewer.pal(length(mp_sat), "Set2"), ref_mp_cols) %>%
  set_names(mp_sat_with_ref)

satisficed_criteria <- c("LT LRP" = LT_LRP_thresh, "STC" = STC_thresh)

mp_eg_not_sat <- c(
  "CC_hist",
  "CC90",
  ".GB_slope8_0.66",
  ".Islope0.2_80",
  ".ICI2",
  ".IDX_smooth",
  ".IT5_hist",
  ".ITM_hist",
  ".SP6040_prior"
)

plots <- gfdlm::plot_factory(
  mse_list = mse,
  pm = PM,
  scenario_df = sc,
  mp_sat = mp_sat,
  mp_not_sat = mp_not_sat,
  mp_not_sat2 = mp_eg_not_sat,
  mp_ref = reference_mp,
  custom_pal = custom_pal,
  eg_scenario = "ceq100",
  tradeoff = c("LT LRP", "STC"),
  catch_breaks = catch_breaks,
  catch_labels = catch_labels,
  satisficed_criteria = satisficed_criteria
)
