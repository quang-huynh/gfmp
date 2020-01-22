library("DLMtool")
library("MSEtool")
library("dplyr")
library("purrr")
library("ggplot2")
library("gfdlm")
library("here")
source(here("analysis/rex/plot-functions.R"))

# Settings --------------------------------------------------------------------

cores <- floor(parallel::detectCores() / 1L)
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

# Set up PMs ------------------------------------------------------------------

`LT P40` <- gfdlm::pm_factory("SBMSY", 0.4, c(36, 50))
`LT P80` <- gfdlm::pm_factory("SBMSY", 0.8, c(36, 50))
STY <- gfdlm::pm_factory("LTY", 0.5, c(6, 20))
LTY <- gfdlm::pm_factory("LTY", 0.5, c(36, 50))
PM <- c("LT P40", "LT P80", "STY", "LTY", "AAVY", "PNOF")

# Satisficing rules:
LT_P40_thresh <- 0.8
STY_thresh <- 0.7

# Read OMs --------------------------------------------------------------------

omrex <- map(scenarios, ~ {
  om <- readRDS(here(
    "generated-data",
    paste0("rex-sra-", .x, ".rds")
  ))@OM
  if (om@nsim < nsim)
    stop("nsim set larger than in conditioned OM.", call. = FALSE)
  om@nsim <- nsim
  om@interval <- 2
  om
})
names(omrex) <- scenarios

# Fit MPs to all OMs ----------------------------------------------------------

fit_scenario <- function(scenario) {
  file_name <- here("generated-data", paste0("rex-mse-", scenario, ".rds"))
  if (!file.exists(file_name)) {
    message("Running closed-loop-simulation for ", scenario, " OM")
    DLMtool::setup(cpus = cores)
    mse <- runMSE(OM = omrex[[scenario]], MPs = mp$mp, parallel = TRUE)
    snowfall::sfStop()
    saveRDS(mse, file = file_name)
  } else {
    message("Loading closed-loop-simulation for ", scenario, " OM")
    mse <- readRDS(file_name)
  }
  mse
}
rex_mse <- map(scenarios, fit_scenario)
names(rex_mse) <- scenarios

# Satisficing -----------------------------------------------------------------

pm_all <- purrr::map2_dfr(rex_mse[scenarios_ref_i], scenarios_ref,
  ~data.frame(gfdlm::get_probs(.x, PM), scenario = .y, stringsAsFactors = FALSE)) %>%
  as_tibble()
names(pm_all) <- gsub("\\.", " ", names(pm_all))
pm_avg <- group_by(pm_all, MP) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
pm_min <- group_by(pm_all, MP) %>%
  summarise_if(is.numeric, min, na.rm = TRUE)

g <- gfdlm::plot_probs(pm_avg)
ggsave(file.path(fig_dir, "rex-pm-table-avg.png"), width = 4.25, height = 6.25)

g <- gfdlm::plot_probs(pm_min)
ggsave(file.path(fig_dir, "rex-pm-table-min.png"), width = 4.25, height = 6.25)

rex_satisficed <- dplyr::filter(pm_min, `LT P40` > LT_P40_thresh, STY > STY_thresh) %>%
  arrange(-`LT P40`) %>% pull(MP)
rex_satisficed
rex_satisficed <- rex_satisficed[!rex_satisficed %in% reference_mp]
rex_satisficed
stopifnot(length(rex_satisficed) > 1)
rex_satisficed_ref <- union(rex_satisficed, reference_mp)
rex_not_satisficed <- mp$mp[!mp$mp %in% rex_satisficed_ref]
stopifnot(length(rex_not_satisficed) > 1)

saveRDS(pm_all, file = here::here("generated-data/rex-pm-all.rds"))
# Convergence -----------------------------------------------------------------

g <- DLMtool::Sub(rex_mse[[base_i]], MPs = rex_satisficed) %>%
  gfdlm::plot_convergence(PM, ylim = c(0.68, 1)) +
  scale_color_brewer(palette = "Set2") +
  facet_wrap(vars(pm_name), ncol = 2)
ggsave(file.path(fig_dir, "rex-converge.png"), width = 6.5, height = 6.5)

# Plots -----------------------------------------------------------------------

walk(scenarios, make_projection_plot,
  MPs = rex_satisficed_ref,
  mptype = "satisficed",
  catch_breaks = c(0, 100000, 200000),
  catch_labels = c("0", "100", "200")
)

# 1 is always base:
# p <- gfdlm::get_probs(rex_mse[[1L]], PM)
# g <- gfdlm::plot_probs(p)
# ggsave(file.path(fig_dir, paste0("rex-pm-table-", "base", ".png")),
#   width = 4.25, height = 6.5
# )

mp_order <- arrange(pm_avg, `LT P40`, `LT P80`, `STY`, `LTY`, AAVY) %>%
  filter(MP %in% rex_satisficed_ref) %>%
  # left_join(rename(mp, MP = mp)) %>%
  # arrange()
  pull(MP)
tigures <- map(scenarios_ref, make_table_plot, mp_order = mp_order)
g <- plot_grid_pbs(tigures, labels = scenarios_ref_human)
ggsave(file.path(fig_dir, paste0("rex-pm-tigures-ref-set", ".png")),
  width = 12, height = 7.5
)
tigures <- map(scenarios_rob, make_table_plot, mp_order = mp_order)
g <- plot_grid_pbs(tigures, labels = scenarios_rob_human)
ggsave(file.path(fig_dir, paste0("rex-pm-tigures-rob-set", ".png")),
  width = 8.5, height = 3.3
)

walk(scenarios, make_kobe_plot,
  MPs = rex_satisficed_ref,
  mptype = "satisficed"
)

# Make multipanel plot of satisficed spider plots for all scenarios
custom_pal <- c(RColorBrewer::brewer.pal(length(rex_satisficed), "Set2"),
  "grey60", "grey20", "grey85")
names(custom_pal) <- rex_satisficed_ref
custom_pal
spider_plots <- walk(scenarios_ref, make_spider,
  MPs = rex_satisficed_ref,
  mptype = "satisficed", custom_pal = custom_pal
)
spider_plots <- map(scenarios_ref, make_spider,
  MPs = rex_satisficed,
  save_plot = FALSE, custom_pal = custom_pal, legend = FALSE
)
g <- plot_grid_pbs(
  plotlist = spider_plots, labels = scenarios_ref_human, spider_margins = TRUE
)
ggsave(file.path(fig_dir, "rex-spider-satisficed-panel.png"), width = 11, height = 10)
spider_plots_rob <- map(scenarios_rob, make_spider,
  MPs = rex_satisficed,
  save_plot = FALSE, custom_pal = custom_pal, legend = FALSE
)
g <- plot_grid_pbs(
  plotlist = spider_plots_rob, labels = scenarios_rob_human, spider_margins = TRUE
)
ggsave(file.path(fig_dir, "rex-spider-satisficed-panel-robust.png"), width = 8, height = 4)

# Make multipanel plot of spider plots for satisficed MPs; averaged only
type_order <- forcats::fct_relevel(mp$type, "Reference", after = 0L)
spider_plots <- split(mp, type_order) %>%
  map(~ spider_base(filter(pm_avg, MP %in% .x$mp)))
g <- plot_grid_pbs(plotlist = spider_plots, labels = names(spider_plots),
  spider_margins = TRUE, ncol = 2) +
  theme(plot.margin = unit(c(0.2, 0.2, -0.5, 1.0), "lines"))
ggsave(file.path(fig_dir, "rex-spider-all-mptypes-avg-panel.png"),
  width = 9.5, height = 10
)
# just average:
g <- spider_base(filter(pm_avg, MP %in% rex_satisficed_ref)) +
  scale_colour_manual(values = custom_pal)
ggsave(file.path(fig_dir, "rex-spider-satisficed-avg.png"), width = 6, height = 6)

# Make not satisficed plot for base (these MPs not tested in other scenarios)
make_projection_plot(base_om, MPs = rex_not_satisficed,
  mptype = "NOT-satisficed", height = 27)
make_kobe_plot(base_om, MPs = rex_not_satisficed, mptype = "NOT-satisficed",
  show_contours = FALSE)
# Example not satisficed ones:
toplot <- c(
  "CC_hist",
  # "CC_hist20",
  # "CC100",
  "CC90",
  # "CC80",
  # "CC70",
  # "CC60",
  # ".GB_slope6_0.66",
  ".GB_slope8_0.66",
  # ".GB_slope8_1",
  ".Islope0.2_80",
  # ".Islope0.2_100",
  # ".Islope0.4_80",
  # ".Islope0.4_100",
  ".ICI2",
  # ".IDX",
  ".IDX_smooth",
  # ".IT10_hist",
  ".IT5_hist",
  # ".Itarget5",
  ".ITM_hist",
  # ".SP4010_prior",
  ".SP6040_prior"
  # ".SP8040_prior"
)
make_projection_plot(base_om, MPs = toplot[toplot %in% rex_not_satisficed],
  mptype = "eg-NOT-satisficed", height = 9,
  catch_breaks = c(0, 100000, 200000),
  catch_labels = c("0", "100", "200"))

# Psychedelic pyramid worms ---------------------------------------------------

walk(scenarios, make_neon_worm_plot,
  MPs = rex_satisficed_ref,
  mptype = "satisficed"
)
# .d3 <- gfdlm:::get_ts(DLMtool::Sub(rex_mse[[base_i]], MPs = rex_satisficed))
# .d2 <- gfdlm:::get_ts_quantiles(.d3, probs = c(0.2, 0.2))
# .d <- filter(.d2)
#
# now <- filter(.d2, real_year == 2018)
#
# m <- reshape2::dcast(.d, mp_name + real_year ~ Type, value.var = "m") %>%
#   rename(b_m = B_BMSY, f_m = F_FMSY)
# l <- reshape2::dcast(.d, mp_name + real_year ~ Type, value.var = "l") %>%
#   rename(b_l = B_BMSY, f_l = F_FMSY)
# u <- reshape2::dcast(.d, mp_name + real_year ~ Type, value.var = "u") %>%
#   rename(b_u = B_BMSY, f_u = F_FMSY)
# dd <- left_join(m, l, by = c("mp_name", "real_year")) %>%
#   left_join(u, by = c("mp_name", "real_year"))
#
# poly_df <- split(dd, paste(dd$mp_name, dd$real_year)) %>%
#   map_df(~ data.frame(
#     x = c(.$b_m, .$b_l, .$b_m, .$b_u, .$b_m),
#     y = c(.$f_l, .$f_m, .$f_u, .$f_m, .$f_l),
#     real_year = unique(.$real_year),
#     mp_name = unique(.$mp_name), stringsAsFactors = FALSE)
#   )
#
# now <- filter(.d2, real_year == 2018)
# now_m <- reshape2::dcast(now, mp_name + real_year ~ Type, value.var = "m") %>%
#   rename(b_m = B_BMSY, f_m = F_FMSY)
# end <- filter(.d2, real_year == max(.d2$real_year))
# end_m <- reshape2::dcast(end, mp_name + real_year ~ Type, value.var = "m") %>%
#   rename(b_m = B_BMSY, f_m = F_FMSY)
# start <- filter(.d2, real_year == min(.d2$real_year))
# start_m <- reshape2::dcast(start, mp_name + real_year ~ Type, value.var = "m") %>%
#   rename(b_m = B_BMSY, f_m = F_FMSY)
# other <- bind_rows(now_m, end_m) %>%
#   bind_rows(start_m)
#
# g <- dd %>%
#   mutate(b_rad = abs(b_u - b_l)/2) %>%
#   mutate(f_rad = abs(f_u - f_l)/2) %>%
#   ggplot(aes(b_m, f_m, colour = real_year)) +
#   geom_polygon(aes(x = x, y = y, fill = real_year, group = real_year),
#     data = poly_df, alpha = 0.2, inherit.aes = FALSE, colour = NA) +
#   geom_path(lwd = 1.6, lineend = "round", linejoin = "bevel", colour = "white") +
#   geom_path(lwd = 1.0, lineend = "round", linejoin = "bevel") +
#   scale_color_viridis_c(option = "C", direction = -1) +
#   scale_fill_viridis_c(option = "C", direction = -1) +
#   gfplot::theme_pbs() + facet_wrap(~mp_name) +
#   coord_fixed(xlim = c(0, 3), ylim = c(0, 3)) +
#   geom_vline(xintercept = c(0.4, 0.8), lty = 2, alpha = 0.2, lwd = 0.5) +
#   geom_hline(yintercept = 1, lty = 2, alpha = 0.2, lwd = 0.5) +
#   labs(fill = "Year", colour = "Year", x = expression(SSB/SSB[MSY]), y = expression(F/F[MSY]), pch = "Year") +
#   geom_point(data = other, mapping = aes(x = b_m, y = f_m, pch = as.factor(real_year)), inherit.aes = FALSE, col = "white", stroke = 1.6) +
#   geom_point(data = other, mapping = aes(x = b_m, y = f_m, pch = as.factor(real_year)), inherit.aes = FALSE, col = "black", stroke = 1) +
#   scale_shape_manual(values = c(2, 4, 21))
#
# ggsave(file.path(fig_dir, "rex-neon-worms-base.png"), width = 7, height = 6.6)

# Sensitivity plots -----------------------------------------------------------

slots <- c("D", "hs", "M", "ageM", "L50", "Linf", "K", "Isd")

g <- DLMtool::Sub(rex_mse[[base_i]], MPs = rex_satisficed) %>%
  gfdlm::plot_sensitivity(`LT P40`, slots = slots,
  ylab = expression(Mean~SSB/SSB[MSY]~"in"~years~36-50))
ggsave(file.path(fig_dir, "rex-sensitivity-bbmsy-base.png"), width = 12.5, height = 8)

g <- DLMtool::Sub(rex_mse[[base_i]], MPs = rex_satisficed) %>%
  gfdlm::plot_sensitivity(`STY`, slots = slots,
    ylab = "Mean catch/reference catch in years 6-20")
ggsave(file.path(fig_dir, "rex-sensitivity-yield-base.png"), width = 12.5, height = 8)

g <- DLMtool::Sub(rex_mse[[base_i]], MPs = rex_satisficed) %>%
  gfdlm::plot_sensitivity_trajectory("B_BMSY", slots = slots) +
  coord_cartesian(ylim = c(0, 4))
ggsave(file.path(fig_dir, "rex-sensitivity-traj-bbmsy-base.png"), width = 12.5, height = 7)

g <- DLMtool::Sub(rex_mse[[base_i]], MPs = rex_satisficed) %>%
  gfdlm::plot_sensitivity_trajectory("F_FMSY", slots = slots) +
  coord_cartesian(ylim = c(0, 4))
ggsave(file.path(fig_dir, "rex-sensitivity-traj-ffmsy-base.png"), width = 12.5, height = 7)

# Optimize PNG files on Unix --------------------------------------------------

cores <- round(parallel::detectCores() / 2L)
files_per_core <- 5
setwd(fig_dir)
if (!gfplot:::is_windows()) {
  system(paste0(
    "find -X . -name '*.png' -print0 | xargs -0 -n ",
    files_per_core, " -P ", cores, " optipng -strip all"
  ))
}
setwd(here::here())
