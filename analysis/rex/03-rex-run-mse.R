library(DLMtool)
library(MSEtool)
library(dplyr)
library(purrr)
library(ggplot2)
library(gfdlm)
library(here)

# Settings: -------------------------------------------------------------------

cores <- floor(parallel::detectCores() / 1)
scenarios <- c("ceq0", "ceq10", "ceq50",
  "ceq100")
scenarios_human <- c("Catch eq. 0%", "Catch eq. 10%", "Catch eq. 50%",
  "Catch eq. 100%")
.nsim <- 100
base_om <- "ceq50"
mp <- readr::read_csv(here::here("data", "mp.txt"), comment = "#")

# Set up and checks: ----------------------------------------------------------
fig_dir <- here("report", "figure")
if (!dir.exists(fig_dir)) dir.create(fig_dir)
base_i <- which(base_om == scenarios)
stopifnot(identical(length(scenarios_human), length(scenarios)))

# Set up PMs ------------------------------------------------------------------

`LT P40` <- gfdlm::pm_factory("SBMSY", 0.4, c(36, 50))
`LT P80` <- gfdlm::pm_factory("SBMSY", 0.8, c(36, 50))
STY <- gfdlm::pm_factory("LTY", 0.5, c(6, 20))
LTY <- gfdlm::pm_factory("LTY", 0.5, c(36, 50))
PM <- c("LT P40", "LT P80", "STY", "LTY", "AAVY", "PNOF")

# Read OMs --------------------------------------------------------------------

omrex <- map(scenarios, ~ {
  om <- readRDS(here(
    "generated-data",
    paste0("rex-sra-", .x, ".rds")
  ))@OM
  om@nsim <- .nsim
  om@interval <- 2
  om
})
names(omrex) <- scenarios

# Fit base MSE ----------------------------------------------------------------

file_name <- here("generated-data", paste0("rex-mse-", base_om, ".rds"))
if (!file.exists(file_name)) {
  message("Running closed-loop-simulation for base OM")
  DLMtool::setup(cpus = cores)
  rex_mse_base <- runMSE(OM = omrex[[base_i]], MPs = mp$mp, parallel = TRUE)
  snowfall::sfStop()
  saveRDS(rex_mse_base, file = file_name)
} else {
  message("Loading closed-loop-simulation for base OM")
  rex_mse_base <- readRDS(file_name)
}
# DLMtool::Converge(rex_mse_base)

rex_probs <- gfdlm::get_probs(rex_mse_base, PM)
reference_mp <- c("FMSYref75", "NFref", "FMSYref")
rex_satisficed <- dplyr::filter(rex_probs, `LT P40` > 0.9, STY > 0.75) %>%
  arrange(-`LT P40`) %>%
  pull(MP)
rex_satisficed <- rex_satisficed[!rex_satisficed %in% reference_mp]
stopifnot(length(rex_satisficed) > 1)
rex_satisficed_ref <- union(rex_satisficed, reference_mp)
rex_not_satisficed <- mp$mp[!mp$mp %in% rex_satisficed_ref]
stopifnot(length(rex_not_satisficed) > 1)

# Fit satisficed MPs to other OMs----------------------------------------------

fit_scenario <- function(scenario) {
  file_name <- here("generated-data", paste0("rex-mse-", scenario, ".rds"))
  if (!file.exists(file_name)) {
    message("Running closed-loop-simulation for ", scenario, " OM")
    DLMtool::setup(cpus = cores)
    mse <- runMSE(
      OM = omrex[[scenario]], MPs = rex_satisficed_ref,
      parallel = TRUE
    )
    snowfall::sfStop()
    saveRDS(mse, file = file_name)
  } else {
    message("Loading closed-loop-simulation for ", scenario, " OM")
    mse <- readRDS(file_name)
  }
  mse
}
rex_mse_scenarios <- map(scenarios[-base_i], fit_scenario)
rex_mse <- c(rex_mse_base, rex_mse_scenarios)
names(rex_mse) <- c(scenarios[base_i], scenarios[-base_i])

# Plots ---------------------------------------------------------

make_table_plot <- function(scenario) {
  gfdlm::get_probs(rex_mse[[scenario]], PM) %>%
    gfdlm::plot_probs()
  ggsave(file.path(fig_dir, paste0("rex-pm-table-", scenario, ".png")),
    width = 4.25, height = 7
  )
}

make_projection_plot <- function(scenario, MPs, mptype, height = 12) {
  x <- DLMtool::Sub(rex_mse[[scenario]], MPs = MPs)
  g1 <- gfdlm::plot_projection_ts(x, type = c("SSB", "FM")) +
    coord_cartesian(expand = FALSE, ylim = c(0, 4.5)) +
    scale_y_continuous(breaks = c(1, 2, 3, 4)) +
    theme(strip.text.y = element_blank())

  g2 <- gfdlm::plot_projection_ts(x,
    type = "C", clip_ylim = 1.3,
    catch_reference = 1
  ) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank()
    )

  g <- cowplot::plot_grid(g1, g2, rel_widths = c(2, 1), align = "h")
  ggsave(file.path(
    fig_dir,
    paste0("rex-projections-", mptype, "-", scenario, ".png")
  ),
  width = 11, height = height
  )
}

make_kobe_plot <- function(scenario, MPs, mptype, ...) {
  x <- DLMtool::Sub(rex_mse[[scenario]], MPs = MPs)
  g <- gfdlm::plot_contours(x,
    xlim = c(0, 3.5),
    ylim = c(0, 3.5), alpha = c(0.1, 0.25, 0.50), ...
  )
  ggsave(file.path(
    fig_dir,
    paste0("rex-kobe-", mptype, "-", scenario, ".png")
  ),
  width = 8, height = 7.5
  )
}

make_spider <- function(scenario, MPs, mptype, save_plot = TRUE) {
  g <- DLMtool::Sub(rex_mse[[scenario]], MPs = MPs) %>%
    gfdlm::spider(pm_list = PM, palette = "Set2")# +
    # scale_color_viridis_d()
  if (save_plot) {
    ggsave(file.path(
      fig_dir,
      paste0("rex-spider-", mptype, "-", scenario, ".png")
    ),
    width = 6, height = 6
    )
  }
  g
}

walk(scenarios, make_table_plot)
walk(scenarios, make_projection_plot,
  MPs = rex_satisficed_ref,
  mptype = "satisficed"
)
walk(scenarios, make_kobe_plot,
  MPs = rex_satisficed_ref,
  mptype = "satisficed"
)
spider_plots <- purrr::map(scenarios, make_spider,
  MPs = rex_satisficed_ref,
  mptype = "satisficed"
)

plot_grid_pbs <- function(plotlist, align = "hv",
                          label_fontface = "bold", label_size = 12,
                          hjust = 0, spider_margins = TRUE, ...) {
  out <- cowplot::plot_grid(
    plotlist = plotlist, align = align,
    label_fontface = label_fontface, hjust = hjust, label_size = label_size, ...
  )
  if (spider_margins)
    out <- out + theme(plot.margin = unit(c(0.2, 0.2, -0.7, 1.0), "lines"))
  out
}

# Make multipanel plot of satisficed spider plots for all scenarios
spider_plots <- map(scenarios, make_spider,
  MPs = rex_satisficed,
  save_plot = FALSE
)
g <- plot_grid_pbs(
  plotlist = spider_plots,
  labels = scenarios_human
)
ggsave(file.path(fig_dir, paste0("rex-spider-satisficed-panel.png")),
  width = 9, height = 8
)

# Make multipanel plot of spider plots for all MPtypes - Base scenario only
type_order <- forcats::fct_relevel(mp$type, "Reference", after = 0L)
spider_plots <- split(mp, type_order) %>%
  map(~ {
    make_spider(scenario = scenarios[base_i], MPs = .x$mp, save_plot = FALSE) +
      scale_color_brewer(palette = "Set2")
  })
g <- plot_grid_pbs(plotlist = spider_plots, labels = names(spider_plots),
  ncol = 2) + theme(plot.margin = unit(c(0.2, 0.2, -0.5, 1.0), "lines"))
ggsave(file.path(fig_dir, "rex-spider-all-mptypes-base-panel.png"),
  width = 9.5, height = 10
)

# Make not satisficed plot for base (these MPs not tested in other scenarios)
make_projection_plot("base", MPs = rex_not_satisficed,
  mptype = "NOT-satisficed", height = 27)
make_kobe_plot("base", MPs = rex_not_satisficed, mptype = "NOT-satisficed",
  show_contours = FALSE)
