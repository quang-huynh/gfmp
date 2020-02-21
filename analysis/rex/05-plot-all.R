library("DLMtool")
library("MSEtool")
library("dplyr")
library("purrr")
library("ggplot2")
library("gfdlm")
library("here")
library("assertthat")
# load_all("../gfdlm")

# Settings --------------------------------------------------------------------

sp <- "rex" # Species: used in filenames
cores <- floor(parallel::detectCores() / 2L)
sc <- readRDS("generated-data/rex-scenarios.rds")
sc # look good?
nsim <- 50L
interval <- 2L
base_om <- "ceq150" # affects some default plots
mp <- suppressMessages(readr::read_csv(here("analysis", "rex", "mp.txt"), comment = "#"))
as.data.frame(mp) # look good?
reference_mp <- c("FMSYref75", "NFref", "FMSYref")
ref_mp_cols <- c("grey45", "grey10", "grey75") %>% set_names(reference_mp)
#
catch_breaks <- seq(0, 500000, 100000)
catch_labels <- catch_breaks/1000

# Satisficing rules:
# LT_LRP_thresh <- 0.8
# STC_thresh <- 0.7

# Set up PMs ------------------------------------------------------------------

d_catch <- readRDS(here::here("generated-data", "rex-catch2.rds"))
d_catch <- dplyr::filter(d_catch, year >= 1996, year <= 2019)
catch <- d_catch %>%
  gfplot::tidy_catch() %>%
  group_by(year) %>%
  summarize(value = sum(value)) %>%
  pull(value)

yrs <- length(catch)
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
.ggsave <- function(filename, width, height, ...) {
  ggsave(file.path(fig_dir, paste0(sp, "-", filename, ".png")),
    width = width, height = height, ...
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
  om@TACSD <- c(0, 0)
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

# # testing:
# .mp <- c(
#   # "CC_hist20", "CC1.2", "CC1.1", "CC1.0", "CC0.9", "CC0.8", "CC0.7", "CC0.6",
#   # ".Iratio2"
#   # ".GB_slope6_0.66", ".GB_slope6_1", ".GB_slope8_0.66", ".GB_slope8_1",
#   # ".Islope0.2_80", ".Islope0.2_100", ".Islope0.4_80", ".Islope0.4_100",
#   # ".IDX", ".IDX_smooth",
#   # ".IT10_hist", ".IT5_hist",
#   ".Itarget_base", ".Itarget_w0.8", ".Itarget_x0.2", ".Itarget_x0.8", ".Itarget_d1.2", ".Itarget_d0.8"
#   # ".ITM_hist",
#   # ".SP4010_prior", ".SP6040_prior", ".SP8040_prior",
#   # "NFref", "FMSYref", "FMSYref75"
# )

# .mp <- "CC1.0"
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
  mse@OM$CurrentYr <- om[[1]]@CurrentYr # fixed in latest DLMtool
  mse
}
DLMtool::setup(cpus = cores)
mse <- map(scenarios, fit_scenario)
snowfall::sfStop()

ref_catch <- mean(catch[(yrs-5+1):yrs])
for (i in seq_along(mse)) mse[[i]]@OM$RefY <- ref_catch

# plot_main_projections(mse[[2]])

# Satisficing -----------------------------------------------------------------

pm_df_list <- map(mse[scenarios_ref], ~ gfdlm::get_probs(.x, PM))
pm_df_list_rob <- map(mse[scenarios_rob], ~ gfdlm::get_probs(.x, PM))
pm_df <- bind_rows(pm_df_list, .id = "scenario")
pm_avg <- group_by(pm_df, MP) %>% summarise_if(is.numeric, mean)
pm_min <- group_by(pm_df, MP) %>% summarise_if(is.numeric, min)
saveRDS(pm_df_list, file = here("generated-data", "rex-pm-all.rds"))

plot_tigure(pm_avg, satisficed = c("LT LRP" = 0.7, "STC" = 0.5, "LTC" = 0.2))
plot_tigure(pm_min, satisficed = c("LT LRP" = 0.7, "STC" = 0.5, "LTC" = 0.2))

mp_sat <- dplyr::filter(pm_min, `LT LRP` > 0.7, `STC` > 0.5, `LTC` > 0.2) %>%
  # arrange(-`LT LRP`, -`LT LRP`, -`LT USR`, -`STC`, -`LTC`, -AAVC) %>%
  pull(MP)
mp_sat <- mp_sat[!mp_sat %in% reference_mp]
mp_sat
stopifnot(length(mp_sat) >= 1)
stopifnot(length(mp_sat) <= 8) # for RColorBrewer::brewer.pal()
mp_sat_with_ref <- union(mp_sat, reference_mp)
mp_not_sat <- mp$mp[!mp$mp %in% mp_sat_with_ref]
stopifnot(length(mp_not_sat) > 1)

custom_pal <- c(RColorBrewer::brewer.pal(8, "Set2")[seq_along(mp_sat)], ref_mp_cols) %>%
  set_names(mp_sat_with_ref)

satisficed_criteria <- c("LT LRP" = 0.7, "STC" = 0.5, "LTC" = 0.2)

mp_eg_not_sat <- c(
  "CC_hist20",
  "CC1.0",
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
  eg_scenario = "ceq200",
  tradeoff = c("LT LRP", "STC"),
  catch_breaks = catch_breaks,
  catch_labels = catch_labels,
  satisficed_criteria = satisficed_criteria,
  skip_projections = FALSE,
  dodge = 0.8
)

pm_angle <- theme(
  axis.text.x.bottom = element_text(angle = 90, hjust = 1),
  panel.grid.major.y = element_line(colour = "grey85"),
  panel.grid.minor.y = element_line(colour = "grey96")
)
# plots$dot_refset + pm_angle

.ggsave("dot-refset-avg", width = 6, height = 5, plot = plots$dot_refset_avg)

# plots$radar_refset_avg
# plots$radar_refset_min

.ggsave("tradeoff-refset", width = 7, height = 4, plot = plots$tradeoff_refset)

# plots$tradeoff_robset

.ggsave("tigure-refset", width = 6.5, height = 8, plot = plots$tigure_refset)
.ggsave("tigure-refset-min", width = 4, height = 7, plot = plots$tigure_refset_min)
.ggsave("tigure-refset-avg", width = 4, height = 7, plot = plots$tigure_refset_avg)

# plots$radar_refset
# plots$lollipops_refset

g <- plots$projections_index +
  scale_x_continuous(breaks = seq(1975, 2090, 25)) +
  coord_cartesian(ylim = c(0, 9))
.ggsave("projections-index", width = 12, height = 8, plot = g)

# plots$projections$`no-cpue-light`
plots$projections$`inc-m`
# plots$projections$ceq200
# plots$projections$`high-m`

walk(names(plots$projections), ~{
  .ggsave(paste0("projections-", .x), width = 8, height = 8,
    plot = plots$projections[[.x]])
})
.ggsave("projections-not-sat2", width = 6.5, height = 9, plot = plots$projections_not_sat2)
.ggsave("projections-not-sat", width = 6.5, height = 20, plot = plots$projections_not_sat)
.ggsave("projections-scenarios-ref", width = 8, height = 8, plot = plots$projections_scenarios)

x <- purrr::map(scenarios_ref,
  ~ DLMtool::Sub(mse[[.x]], MPs = mp_sat_with_ref)) %>%
  set_names(scenarios_ref_human)

plot_index(x, type = "AddInd")

g <- c("Ceq. 150%", "Ceq. 200%", "Lightly fished", "No CPUE") %>%
  set_names() %>%
  map(~x[[.]]) %>%
  plot_scenario_projections()
.ggsave("projections-scenarios-ref1", width = 8, height = 8)

g <- c("Ceq. 150%", "M = 0.35", "h = 0.5-0.7" , "h = 0.95") %>%
  set_names() %>%
  map(~x[[.]]) %>%
  plot_scenario_projections()
.ggsave("projections-scenarios-ref2", width = 8, height = 8)

g <- c("Ceq. 150%", "Shifted sel.", "Oregon growth" , "Ceq. 200%") %>%
  set_names() %>%
  map(~x[[.]]) %>%
  plot_scenario_projections()
.ggsave("projections-scenarios-ref2", width = 8, height = 8)

x <- purrr::map("inc-m",
  ~ DLMtool::Sub(mse[[.x]], MPs = mp_sat_with_ref)) %>%
  set_names("inc-m")

plot_projection_ts(x[[length(x)]])
# plot_projection_ts(x[[1]])
plot_main_projections(x[[length(x)]])
