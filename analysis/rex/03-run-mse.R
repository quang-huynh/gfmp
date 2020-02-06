library("DLMtool")
library("MSEtool")
library("dplyr")
library("purrr")
library("ggplot2")
library("gfdlm")
library("here")

# Settings --------------------------------------------------------------------

sp <- "rex" # Species: used in filenames
cores <- floor(parallel::detectCores() / 2L)
sc <- readRDS("generated-data/rex-scenarios.rds")
sc # look good?
nsim <- 200L
interval <- 2L
base_om <- "ceq100" # affects some default plots
mp <- readr::read_csv(here::here("data", "mp.txt"),
  comment = "#",
  col_types = readr::cols(
    mp = readr::col_character(),
    type = readr::col_character()
  )
)
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
scenarios_human <- sc$scenarios_human %>% set_names()
scenarios_ref <- get_filtered_scenario("Reference", "scenario")
scenarios_ref_human <- get_filtered_scenario("Reference", "scenarios_human")
scenarios_rob <- get_filtered_scenario("Robustness", "scenario")
scenarios_rob_human <- get_filtered_scenario("Robustness", "scenarios_human")

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

mse_sat <- purrr::map(scenarios, ~ DLMtool::Sub(mse[[.x]], MPs = mp_sat))
mse_sat_with_ref <- purrr::map(scenarios_ref, ~ DLMtool::Sub(mse[[.x]], MPs = mp_sat_with_ref))

pm_df_list_sat <- map(pm_df_list, dplyr::filter, MP %in% mp_sat)
pm_df_list_sat_with_ref <- map(pm_df_list, dplyr::filter, MP %in% mp_sat_with_ref)

custom_pal <- c(RColorBrewer::brewer.pal(length(mp_sat), "Set2"), ref_mp_cols) %>%
  set_names(mp_sat_with_ref)
