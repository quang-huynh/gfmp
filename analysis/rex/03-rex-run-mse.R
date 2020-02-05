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

catch_breaks <- c(0, 100000, 200000)
catch_labels <- c("0", "100", "200")

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
  arrange(-`LT LRP`) %>%
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

# Tigure plots ----------------------------------------------------------------

g <- gfdlm::plot_tigure(pm_avg)
.ggsave("pm-table-avg", 4.25, 6.25)
g <- gfdlm::plot_tigure(pm_min,
  satisficed = c("LT LRP" = LT_LRP_thresh, "STC" = STC_thresh)
)
.ggsave("pm-table-min", 4.25, 6.25)

# mp_order <- arrange(pm_avg, `LT LRP`, `LT USR`, `STC`, `LTC`, AAVC) %>%
#   dplyr::filter(MP %in% mp_sat_with_ref) %>% pull(MP)

g <- map(pm_df_list, dplyr::filter, MP %in% mp_sat_with_ref) %>%
  set_names(scenarios_ref_human) %>%
  plot_tigure_facet()
.ggsave("pm-tigures-ref-set", 12, 7.5)

g <- map(pm_df_list_rob, dplyr::filter, MP %in% mp_sat_with_ref) %>%
  set_names(scenarios_rob_human) %>%
  plot_tigure_facet()
.ggsave("pm-tigures-rob-set", 8.5, 3.3)

# Convergence -----------------------------------------------------------------

walk(names(mse_sat_with_ref), ~ {
  g <- gfdlm::plot_convergence(mse_sat_with_ref[[.x]], PM) +
    scale_color_brewer(palette = "Set2")
  .ggsave(paste0("converge-", .x), 6.5, 6.5)
})

# Projections -----------------------------------------------------------------

walk(names(mse_sat_with_ref), ~ {
  g <- plot_main_projections(mse_sat_with_ref[[.x]],
    catch_breaks = catch_breaks,
    catch_labels = catch_labels)
  .ggsave(paste0("projections-satisficed-", .x), 6.5, 6.5)
})

# All not satisficed ones for "base":
DLMtool::Sub(mse[[base_om]], MPs = mp_not_sat) %>%
  plot_main_projections(catch_breaks = catch_breaks,
    catch_labels = catch_labels)
.ggsave(paste0("projections-all-not-satisficed"), 6.5, 27)

# Example not satisficed ones for "base":
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
mp_eg_not_sat <- mp_eg_not_sat[mp_eg_not_sat %in% mp_not_sat]
g <- DLMtool::Sub(mse[[base_om]], MPs = mp_eg_not_sat) %>%
  plot_main_projections(catch_breaks = catch_breaks,
    catch_labels = catch_labels)
.ggsave(paste0("projections-eg-not-satisficed"), 6.5, 6.5)

# Kobe ------------------------------------------------------------------------

walk(names(mse_sat_with_ref), ~ {
  g <- gfdlm::plot_kobe(mse_sat_with_ref[[.x]])
  .ggsave(paste0("kobe-", .x), 8, 7.5)
})

# Radar plots -----------------------------------------------------------------

custom_pal <- c(RColorBrewer::brewer.pal(length(mp_sat), "Set2"), ref_mp_cols) %>%
  set_names(mp_sat_with_ref)

g <- pm_df_list %>% map(dplyr::filter, MP %in% mp_sat_with_ref) %>%
  set_names(scenarios_ref_human) %>%
  plot_radar_facet(custom_pal = custom_pal)
.ggsave("spider-satisficed-panel-reference", 12, 11)

g <- pm_df_list_rob %>% map(dplyr::filter, MP %in% mp_sat_with_ref) %>%
  set_names(scenarios_rob_human) %>%
  plot_radar_facet(custom_pal = custom_pal)
.ggsave("spider-satisficed-panel-robustness", 10, 5)

g <- pm_avg %>% dplyr::filter(MP %in% mp_sat_with_ref) %>%
  plot_radar(custom_pal = custom_pal)
.ggsave("spider-satisficed-avg-reference", 6, 6)

g <- pm_min %>% dplyr::filter(MP %in% mp_sat_with_ref) %>%
  plot_radar(custom_pal = custom_pal)
.ggsave("spider-satisficed-min-reference", 6, 6)

g <- pm_min %>% dplyr::filter(MP %in% mp_sat_with_ref) %>%
  plot_radar(custom_pal = custom_pal)
.ggsave("spider-satisficed-min-reference", 6, 6)

# Parallel coordinate plots ---------------------------------------------------

g <- pm_df_list %>% map(dplyr::filter, MP %in% mp_sat_with_ref) %>%
  set_names(scenarios_ref_human) %>%
  gfdlm::plot_parallel_coords(type = "facet", custom_pal = custom_pal)
.ggsave("parallel-coordinates", 8, 6.6)

g <- pm_df_list %>% map(dplyr::filter, MP %in% mp_sat_with_ref) %>%
  gfdlm::plot_parallel_coords(type = "single", custom_pal = custom_pal)
.ggsave("parallel-coordinates-avg", 5, 3)

# Bivariate trade-off plots ---------------------------------------------------

g <- pm_df_list %>%
  map(dplyr::filter, MP %in% union(mp_sat, reference_mp[reference_mp != "NFref"])) %>%
  gfdlm::plot_tradeoff("LT LRP", "STC", custom_pal = custom_pal)
.ggsave("bivariate-trade-off-reference", 8, 6)

g <- pm_df_list_rob %>%
  map(dplyr::filter, MP %in% union(mp_sat, reference_mp[reference_mp != "NFref"])) %>%
  gfdlm::plot_tradeoff("LT LRP", "STC", custom_pal = custom_pal) +
  facet_wrap(~scenario, ncol = 2)
.ggsave("bivariate-trade-off-robustness", 6, 3)

# Psychedelic pyramid worms ---------------------------------------------------

walk(names(mse_sat_with_ref), ~{
  g <- plot_worms(mse_sat_with_ref[[.x]], this_year = this_year)
  .ggsave(paste0("neon-worms-", .x), 8, 6.6)
})

# Sensitivity plots -----------------------------------------------------------

slots <- c("D", "hs", "M", "ageM", "L50", "Linf", "K", "Isd")

g <- DLMtool::Sub(mse[[base_om]], MPs = mp_sat) %>%
  gfdlm::plot_sensitivity(`LT LRP`, slots = slots,
    ylab = expression(Mean~SSB/SSB[MSY]~"in"~years~36-50))
.ggsave("sensitivity-bbmsy-base", 12.5, 8)

g <- DLMtool::Sub(mse[[base_om]], MPs = mp_sat) %>%
  gfdlm::plot_sensitivity(`STY`, slots = slots,
    ylab = "Mean catch/reference catch in years 6-20")
.ggsave("sensitivity-yield-base", 12.5, 8)

g <- DLMtool::Sub(mse[[base_om]], MPs = mp_sat) %>%
  gfdlm::plot_sensitivity_trajectory("B_BMSY", slots = slots) +
  coord_cartesian(ylim = c(0, 4))
.ggsave("sensitivity-traj-bbmsy-base", 12.5, 7)

g <- DLMtool::Sub(mse[[base_om]], MPs = mp_sat) %>%
  gfdlm::plot_sensitivity_trajectory("F_FMSY", slots = slots) +
  coord_cartesian(ylim = c(0, 4))
.ggsave("sensitivity-traj-ffmsy-base", 12.5, 7)

# Optimize PNG files on Unix --------------------------------------------------

# cores <- round(parallel::detectCores() / 2L)
# files_per_core <- 5L
# setwd(fig_dir)
# if (!gfplot:::is_windows()) {
#   system(paste0(
#     "find -X . -name '*.png' -print0 | xargs -0 -n ",
#     files_per_core, " -P ", cores, " optipng -strip all"
#   ))
# }
# setwd(here::here())
