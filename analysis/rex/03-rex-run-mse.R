library("DLMtool")
library("MSEtool")
library("dplyr")
library("purrr")
library("ggplot2")
library("gfdlm")
library("here")
source(here("analysis/rex/plot-functions.R"))

# Settings --------------------------------------------------------------------

cores <- floor(parallel::detectCores() / 2L)
sc <- readRDS("generated-data/rex-scenarios.rds")
sc # look good?
nsim <- 200L
base_om <- "ceq100" # affects some default plots
mp <- readr::read_csv(here::here("data", "mp.txt"), comment = "#",
  col_types = readr::cols(
    mp = readr::col_character(),
    type = readr::col_character())
)
as.data.frame(mp) # look good?
reference_mp <- c("FMSYref75", "NFref", "FMSYref")
sp <- "rex"

# Satisficing rules:
LT_LRP_thresh <- 0.8
ST_Catch_thresh <- 0.7

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

scenarios <- as.character(sc$scenario)
scenarios_human <- as.character(sc$scenarios_human)
base_i <- which(base_om == scenarios)
scenarios_ref <- filter(sc, scenario_type == "Reference") %>%
  pull(scenario) %>% as.character()
scenarios_ref_human <- filter(sc, scenario_type == "Reference") %>%
  pull(scenarios_human) %>% as.character()
scenarios_rob <- filter(sc, scenario_type == "Robustness") %>%
  pull(scenario) %>% as.character()
scenarios_rob_human <- filter(sc, scenario_type == "Robustness") %>%
  pull(scenarios_human) %>% as.character()
scenarios_ref_i <- which(scenarios %in% scenarios_ref)
scenarios_rob_i <- which(scenarios %in% scenarios_rob)

.ggsave <- function(filename, width, height) {
  ggsave(file.path(fig_dir, paste0(sp, "-", filename, ".png")),
    width = width, height = height)
}

# Read OMs --------------------------------------------------------------------

om <- map(scenarios, ~ {
  om <- readRDS(here(
    "generated-data",
    paste0(sp, "-sra-", .x, ".rds")
  ))@OM
  if (om@nsim < nsim)
    stop("nsim set larger than in conditioned OM.", call. = FALSE)
  om@nsim <- nsim
  om@interval <- 2
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

pm_list <- map(mse[scenarios_ref_i], ~ gfdlm::get_probs(.x, PM))

pm_all <- purrr::map2_dfr(mse[scenarios_ref_i], scenarios_ref,
  ~data.frame(gfdlm::get_probs(.x, PM), scenario = .y, stringsAsFactors = FALSE)) %>%
  as_tibble()
names(pm_all) <- gsub("\\.", " ", names(pm_all))
pm_avg <- group_by(pm_all, MP) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
pm_min <- group_by(pm_all, MP) %>%
  summarise_if(is.numeric, min, na.rm = TRUE)

g <- gfdlm::plot_prob_tigure(pm_avg)
.ggsave("pm-table-avg", 4.25, 6.25)

g <- gfdlm::plot_prob_tigure(pm_min,
  satisficed = c("LT LRP" = LT_LRP_thresh, "ST Catch" = ST_Catch_thresh))
.ggsave("pm-table-min", 4.25, 6.25)

mp_sat <- dplyr::filter(pm_min, `LT LRP` > LT_LRP_thresh, `ST Catch` > ST_Catch_thresh) %>%
  arrange(-`LT LRP`) %>% pull(MP)
mp_sat
mp_sat <- mp_sat[!mp_sat %in% reference_mp]
mp_sat
stopifnot(length(mp_sat) > 1)
mp_sat_ref <- union(mp_sat, reference_mp)
mp_not_sat <- mp$mp[!mp$mp %in% mp_sat_ref]
stopifnot(length(mp_not_sat) > 1)

mse_sat_ref <- purrr::map(union(scenarios, scenarios_ref),
  ~DLMtool::Sub(mse[[.x]], MPs = mp_sat_ref)) %>% setNames(scenarios)
mse_sat <- purrr::map(scenarios,
  ~DLMtool::Sub(mse[[.x]], MPs = mp_sat)) %>% setNames(scenarios)
mse_not_sat <- purrr::map(scenarios,
  ~DLMtool::Sub(mse[[.x]], MPs = mp_not_sat)) %>% setNames(scenarios)

saveRDS(pm_all, file = here::here("generated-data/rex-pm-all.rds"))

# Convergence -----------------------------------------------------------------

walk(mp_sat_ref, ~{
  g <- gfdlm::plot_convergence(mse_sat[[.x]], PM, ylim = c(0.5, 1)) +
    scale_color_brewer(palette = "Set2") +
    facet_wrap(vars(pm_name), ncol = 2)
  .ggsave(paste0("converge-", .x), 6.5, 6.5)
})

# Projections -----------------------------------------------------------------

walk(mp_sat_ref,
  ~make_projection_plot(mse_sat_ref[[.x]],
    mptype = "satisficed",
    scenario = .x,
    catch_breaks = c(0, 100000, 200000),
    catch_labels = c("0", "100", "200"))
)

mp_order <- arrange(pm_avg, `LT LRP`, `LT P80`, `STY`, `LTY`, AAVY) %>%
  filter(MP %in% mp_sat_ref) %>%
  pull(MP)
tigures <- map(mse_sat_ref, make_table_plot, mp_order = mp_order)
g <- plot_grid_pbs(tigures, labels = scenarios_ref_human)
.ggsave("pm-tigures-ref-set", 12, 7.5)
tigures <- map(scenarios_rob, make_table_plot, mp_order = mp_order)
g <- plot_grid_pbs(tigures, labels = scenarios_rob_human)
.ggsave("pm-tigures-rob-set", 8.5, 3.3)

walk(mp_sat_ref, ~{
  g <- gfdlm::plot_kobe(
    mse_sat_ref[[.x]],
    xlim = c(0, 3.5),
    ylim = c(0, 3.5), alpha = c(0.1, 0.25, 0.5, 0.75))
  .ggsave("kobe-", .x, 8, 7.5)
})

# Make multipanel plot of satisficed spider plots for all scenarios
custom_pal <- c(RColorBrewer::brewer.pal(length(mp_sat), "Set2"),
  "grey60", "grey20", "grey85")
names(custom_pal) <- mp_sat_ref
custom_pal
spider_plots <- walk(scenarios_ref, make_spider,
  MPs = mp_sat_ref,
  mptype = "satisficed", custom_pal = custom_pal
)
spider_plots <- map(scenarios_ref, make_spider,
  MPs = mp_sat,
  save_plot = FALSE, custom_pal = custom_pal, legend = FALSE
)
g <- plot_grid_pbs(
  plotlist = spider_plots, labels = scenarios_ref_human, spider_margins = TRUE
)
.ggsave("spider-satisficed-panel", 11, 10)
spider_plots_rob <- map(scenarios_rob, make_spider,
  MPs = mp_sat,
  save_plot = FALSE, custom_pal = custom_pal, legend = FALSE
)
g <- plot_grid_pbs(
  plotlist = spider_plots_rob, labels = scenarios_rob_human, spider_margins = TRUE
)
.ggsave("spider-satisficed-panel-robust", 8, 4)

# Make multipanel plot of spider plots for satisficed MPs; averaged only
type_order <- forcats::fct_relevel(mp$type, "Reference", after = 0L)
spider_plots <- split(mp, type_order) %>%
  map(~ spider_base(filter(pm_avg, MP %in% .x$mp)))
g <- plot_grid_pbs(plotlist = spider_plots, labels = names(spider_plots),
  spider_margins = TRUE, ncol = 2) +
  theme(plot.margin = unit(c(0.2, 0.2, -0.5, 1.0), "lines"))
.ggsave("all-mptypes-avg-panel", 9.5, 10)
# just average:
g <- spider_base(filter(pm_avg, MP %in% mp_sat_ref)) +
  scale_colour_manual(values = custom_pal)
.ggsave("satisficed-avg", width = 6, height = 6)

# Make not satisficed plot for base (these MPs not tested in other scenarios)
DLMtool::Sub(mse[[base_om]], MPs = mp_not_sat) %>%
  make_projection_plot(scenario = base_om, mptype = "not-satisficed", height = 27)

make_kobe_plot(base_om, MPs = mp_not_sat, mptype = "not-satisficed",
  show_contours = FALSE)
# Example not satisficed ones:
toplot <- c(
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

DLMtool::Sub(mse[[base_om]], MPs = mp_not_sat) %>%
  make_projection_plot(MPs = toplot[toplot %in% mp_not_sat],
  mptype = "eg-not-satisficed", scenario = base_om, height = 9,
  catch_breaks = c(0, 100000, 200000),
  catch_labels = c("0", "100", "200"))

# Psychedelic pyramid worms ---------------------------------------------------

walk(names(mse_sat_ref), ~{
  plot_worm(mse_sat_ref[[.x]], this_year = this_year, prob = prob)
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

cores <- round(parallel::detectCores() / 2L)
files_per_core <- 5L
setwd(fig_dir)
if (!gfplot:::is_windows()) {
  system(paste0(
    "find -X . -name '*.png' -print0 | xargs -0 -n ",
    files_per_core, " -P ", cores, " optipng -strip all"
  ))
}
setwd(here::here())
