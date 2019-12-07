library(DLMtool)
library(MSEtool)
library(dplyr)
library(purrr)
library(ggplot2)
library(gfdlm)
library(here)

cores <- floor(parallel::detectCores() / 2)

fig_dir <- here("report", "figure")
if (!dir.exists(fig_dir)) dir.create(fig_dir)

scenarios <-c("base","ceq10","ceq50","ceq100")
assertthat::assert_that(scenarios[[1]] == "base")

mp <- readr::read_csv(here::here("data", "mp.txt"), comment = "#")
mp_list <- split(mp, mp$type)
mp_types <- unique(mp$type)

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
    paste0("rex-sra-", .x, ".rds")))@OM
  om@nsim <- 48
  om@interval <- 2
  om
})
names(omrex) <- scenarios

#slotNames(omrex$base)
omrex$base@Dobs

# Fit base MSE ----------------------------------------------------------------

file_name <- here("generated-data", "rex-mse-base.rds")
if (!file.exists(file_name)) {
  DLMtool::setup(cpus = cores)
  rex_mse_base <- runMSE(OM = omrex[["base"]], MPs = mp$mp, parallel = TRUE)
  snowfall::sfStop()
  saveRDS(rex_mse_base, file = file_name)
} else {
  rex_mse_base <- readRDS(file_name)
}
# DLMtool::Converge(rex_mse_base)

rex_probs <- gfdlm::get_probs(rex_mse_base, PM)
reference_mp <- c("FMSYref75", "NFref", "FMSYref")
rex_satisficed <- dplyr::filter(rex_probs, `LT P40` > 0.9, STY > 0.75) %>%
  arrange(-`LT P40`) %>%
  pull(MP)
# remove the satisficed ref MPs because putting them all back in below:
rex_satisficed <- rex_satisficed[!rex_satisficed %in% reference_mp]
rex_satisficed_ref <- union(rex_satisficed, reference_mp)

# For illustration get MPs that are NOT satisficed:
rex_not_satisficed <- mp$mp[!mp$mp %in% rex_satisficed_ref]

# May not need:
# rex_mse_sub <- DLMtool::Sub(rex_mse, MPs = rex_satisficed)
# rex_mse_sub_ref <- DLMtool::Sub(rex_mse, MPs = rex_satisficed_ref)
# rex_mse_sub_not_satisficed <- DLMtool::Sub(rex_mse, MPs = rex_not_satisficed)

# Fit satisficed MPs to other OMs----------------------------------------------

fit_scenario <- function(scenario) {
  file_name <- here("generated-data", paste0("rex-mse-", scenario, ".rds"))
  if (!file.exists(file_name)) {
    DLMtool::setup(cpus = cores)
    mse <- runMSE(OM = omrex[[scenario]], MPs = rex_satisficed_ref, parallel = TRUE)
    snowfall::sfStop()
    saveRDS(mse, file = file_name)
  } else {
    mse <- readRDS(file_name)
  }
  mse
}

# First is always "base", fit the rest:
rex_mse_scenarios <- map(scenarios[-1], fit_scenario)
rex_mse <- c(rex_mse_base, rex_mse_scenarios)
names(rex_mse) <- scenarios

# Plots ---------------------------------------------------------

make_table_plot <- function(scenario) {
  gfdlm::get_probs(rex_mse[[scenario]], PM) %>%
    gfdlm::plot_probs()
  ggsave(file.path(fig_dir, paste0("rex-pm-table-", scenario, ".png")),
    width = 4.25, height = 7)
}

make_projection_plot <- function(scenario, MPs, mptype) {
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
  ggsave(file.path(fig_dir, paste0("rex-projections-",mptype,"-", scenario, ".png")),
    width = 11, height = 12)
}

make_kobe_plot <- function(scenario, MPs, mptype) {
  x <- DLMtool::Sub(rex_mse[[scenario]], MPs = MPs)
  g <- gfdlm::plot_contours(x,
    xlim = c(0, 3.5),
    ylim = c(0, 3.5), alpha = c(0.1, 0.25, 0.50)
  )
  ggsave(file.path(fig_dir, paste0("rex-kobe-", mptype,"-", scenario, ".png")),
    width = 8, height = 7.5)
}

make_spider <- function(scenario, MPs, mptype, save_plot = TRUE) {
  g <- DLMtool::Sub(rex_mse[[scenario]], MPs = MPs) %>%
    gfdlm::spider(pm_list = PM, palette = "Set2")
  if (save_plot) {
    ggsave(file.path(fig_dir,
      paste0("rex-spider-", mptype,"-", scenario, ".png")),
      width = 6, height = 6)
  }
  g
}

purrr::walk(scenarios, make_table_plot)
purrr::walk(scenarios, make_projection_plot, MPs = rex_satisficed_ref, mptype = "satisficed")
purrr::walk(scenarios, make_kobe_plot, MPs = rex_satisficed_ref, mptype = "satisficed")
spider_plots <- purrr::map(scenarios, make_spider, MPs = rex_satisficed_ref, mptype = "satisficed")

#Make multipanel plot of satisficed spider plots for all scenarios
nscenarios <- length(scenarios)
nrowpanel <- round(nscenarios/2,0)
spider_plots2  <- purrr::map(scenarios, make_spider, MPs = rex_satisficed_ref, save_plot = FALSE)
g <- cowplot::plot_grid(plotlist = spider_plots2, align = "hv",nrow = nrowpanel, ncol = 2)
ggsave(file.path(fig_dir, paste0("rex-spider-satisficed-panel.png")),
       width = 11, height = 12)

#Make multipanel plot of spider plots for all MPtypes - Base scenario only
spider_plots3 <- split(mp, mp$type) %>%
  purrr::map(~ make_spider(scenario = "base", MPs = .x$mp, save_plot = FALSE))
g <- cowplot::plot_grid(plotlist = spider_plots3,align = "hv",nrow = 3, ncol = 2)
ggsave(file.path(fig_dir, paste0("rex-spider-all-mptypes-base-panel.png")),
       width = 11, height = 12)

#make not satisficed plot for base (these MPs not tested in other scenarios)
make_projection_plot("base",MPs = rex_not_satisficed, mptype = "NOT-satisficed")
make_kobe_plot("base",MPs = rex_not_satisficed, mptype = "NOT-satisficed")
make_spider("base",MPs = rex_not_satisficed, mptype = "NOT-satisficed")

