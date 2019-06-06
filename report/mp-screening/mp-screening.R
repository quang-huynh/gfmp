library(dplyr)
library(ggplot2)
library(DLMtool)
library(here)

dir.create(here("report/mp-screening/om"), showWarnings = FALSE)
dir.create(here("report/mp-screening/mse-generated"), showWarnings = FALSE)

base_url <- "http://www.datalimitedtoolkit.org/Case_Studies_Table/"
download.file(paste0(base_url, "Pacific_Ocean_Perch_QC_BC_DFO/OM.rdata"),
  here("report/mp-screening/om/pop.rds"))
download.file(paste0(base_url, "Redbanded_Rockfish_BC_DFO/OM.rdata"),
  here("report/mp-screening/om/redbanded.rds"))
download.file(paste0(base_url, "Rougheye_Rockfish_BC_DFO/OM.rdata"),
  here("report/mp-screening/om/rougheye.rds"))
download.file(paste0(base_url, "Shortspine_Thornyhead_BC_DFO/OM.rdata"),
  here("report/mp-screening/om/shortspine.rds"))
download.file(paste0(base_url, "Yelloweye_Rockfish_BC_DFO/OM.rdata"),
  here("report/mp-screening/om/yelloweye.rds"))
download.file(paste0(base_url, "Arrowtooth_Flounder_BC_DFO/OM.rdata"),
  here("report/mp-screening/om/arrowtooth.rds"))

pop_om <- readRDS(here("report/mp-screening/om/pop.rds"))
rdb_om <- readRDS(here("report/mp-screening/om/redbanded.rds"))
rgh_om <- readRDS(here("report/mp-screening/om/rougheye.rds"))
srt_om <- readRDS(here("report/mp-screening/om/shortspine.rds"))
yel_om <- readRDS(here("report/mp-screening/om/yelloweye.rds"))
arr_om <- readRDS(here("report/mp-screening/om/arrowtooth.rds"))

folder <- here("report/mp-screening/mse-generated")
oms <- list(pop = pop_om, rdb = rdb_om, rgh = rgh_om,
  srt = srt_om, yel = yel_om, arr = arr_om)
mse <- list()

lapply(oms, function(x) x@beta) # hyper stability/hyper depletion
lapply(oms, function(x) x@Ibiascv)
lapply(oms, function(x) x@Cobs)
lapply(oms, function(x) x@Cbiascv)
lapply(oms, function(x) x@interval)
# What should the assessment interval be?
# Every 5 years?

lapply(oms, function(x) x@proyears)

# Which OMs make use of cpars and for which slots?
lapply(oms, function(x) names(x@cpars))

# Let's look at the CV on the survey index
# since this will be important to many of the MPs:
lapply(oms, function(x) if ("Iobs" %in% names(x@cpars))
  range(x@cpars$Iobs) else x@Iobs)

# survey CV should be closer to 0.2-0.35 for POP (synopsis report mean synoptic CVs)
oms$pop@Iobs <- c(0.20, 0.35)
# survey CV for shortspine looks reasonable
# survey CV for yelloweye should be around 0.1-0.2 (looks good)
# arrowtooth should be around 0.15-0.25 (synopsis report)
oms$arr@Iobs <- c(0.15, 0.25)

candidate_mps <- readr::read_csv(here("report/data/dlmtool-mps.csv")) %>%
  filter(Candidate == "Y") %>%
  rename(mp = `Management Procedure`)
mps_keep <- gsub(" ", "", unlist(strsplit(candidate_mps$mp, "\\(\\)")))
mps_keep <- sort(union(mps_keep, DLMtool::avail("Reference")))

# FIXME: Error: Islope3 is not a valid MP!?
mps_keep <- mps_keep[!mps_keep %in% c("Islope3")]

DLMtool::setup(cpus = parallel::detectCores())
for (i in seq_along(oms)) {
  message("Running ", names(oms)[i], "...")
  oms[[i]]@seed <- 42
  oms[[i]]@nsim <- 100
  oms[[i]]@Cobs <- c(0.05, 0.10)
  oms[[i]]@Cbiascv <- c(0.05, 0.05)
  oms[[i]]@interval <- 5 # otherwise a mix of 3 and 4

  fi <- paste0(file.path(folder, names(oms)[i]), ".rds")
  if (!file.exists(fi)) {
    mse[[i]] <- runMSE(OM = oms[[i]], MPs = mps_keep, parallel = TRUE, ntrials = 1000)
    saveRDS(mse[[i]], file = fi)
  } else {
    mse[[i]] <- readRDS(fi)
  }
}
snowfall::sfStop()

source("R/plot-probability-table.R")
source("R/plots.R")
source("R/trade-plots.R")
source("R/pm-functions.R")
library(gfutilities)

pm <- lapply(mse, function(x) {
  pm_pass(x,
    pm_list = list("PNOF", "P100", "P10", "P40", "LTY", "AAVY"),
    lims = c(0, 0, 0, 0, 0, 0))
})
for (i in seq_along(oms)) pm[[i]]$species <- names(oms)[i]

# pm    probcap
# <fct> <fct>
#   1 PNOF  Prob. F < FMSY (Years 1 - 50)
# 2 P100  Prob. SB > SBMSY (Years 1 - 50)
# 4 P10   Prob. SB > 0.1 SBMSY (Years 1 - 50)
# 5 P40   Prob. SB > 0.4 SBMSY (Years 1 - 50)
# 6 LTY   Prob. Yield > 0.5 Ref. Yield (Years 41-50)

wide_pm <- bind_rows(pm) %>%
  as.data.frame() %>%
  filter(!species %in% c("rdb"), mp != "YPR") %>%
  filter(pm %in% c("PNOF", "P100", "P40", "LTY", "AAVY")) %>%
  reshape2::dcast(class + species + mp ~ pm, value.var = "prob")

# relative to peak MP:
top_pm <- wide_pm %>%
  group_by(species) %>%
  mutate(P100 = P100 / max(P100), PNOF = PNOF / max(PNOF),
    P40 = P40 / max(P40), LTY = LTY / max(LTY), AAVY = AAVY / max(AAVY)) %>%
  filter(P40 > 0.8) %>%
  filter(P100 > 0.5) %>%
  filter(LTY > 0.5) %>%
  filter(class != "Reference") %>%
  group_by(species) %>%
  top_n(n = 8L, wt = LTY) %>%
  as.data.frame()
#
# # absolute performance:
# top_pm <- wide_pm %>%
#   group_by(species) %>%
#   filter(P40 > 0.90) %>%
#   filter(P100 > 0.50) %>%
#   filter(class != "Reference") %>%
#   group_by(species) %>%
#   top_n(n = 10L, wt = LTY) %>%
#   as.data.frame()

top_pm
sort(table(top_pm$mp))

top_pm_names <- unique(top_pm$mp)
length(top_pm_names)

top_top_pm_names <- names(table(top_pm$mp))[table(top_pm$mp) > 1L]
top_top_pm_names

species_names <- tibble(species = c("pop", "rgh", "srt", "yel", "arr"),
  species_full = c("pacific ocean perch", "rougheye rockfish", "shortspine thornyhead",
    "yelloweye rockfish", "arrowtooth flounder"))

# FIXME: Should the constant catch MPs be removed? They don't have any feedback built in.
plot_pm <- function(x, y, colour) {
  wide_pm %>%
    left_join(species_names, by = "species") %>%
    filter(mp %in% top_top_pm_names | class == "Reference" | mp %in% c("DD", "AvC")) %>%
    ggplot(aes_string(x = x, y = y)) +
    geom_point(aes_string(colour = colour, shape = "class")) +
    xlim(0, 1) + ylim(0, 1) +
    facet_wrap(~species_full) +
    ggrepel::geom_text_repel(aes_string(label = "mp", colour = colour)) +
    scale_color_viridis_c(direction = -1) +
    scale_shape_manual(values = c("Reference" = 4, "Output" = 21)) +
    gfplot::theme_pbs()
}
plot_pm("P100", "LTY", "AAVY")
plot_pm("P100", "LTY", "PNOF")
plot_pm("P40", "LTY", "AAVY")
plot_pm("P40", "PNOF", "LTY")
plot_pm("P100", "PNOF", "LTY")

# https://stackoverflow.com/a/46999174
calculate_radar <- function(mydf) {
  df <- cbind(mydf[, -1], mydf[,2])
  myvec <- c(t(df))
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / (ncol(df) - 1))
  xx <- myvec * sin(rep(c(angles[-ncol(df)], angles[1]), nrow(df)))
  yy <- myvec * cos(rep(c(angles[-ncol(df)], angles[1]), nrow(df)))
  graphData <- data.frame(group = rep(mydf[, 1], each = ncol(mydf)),
    x = xx, y = yy)
  graphData
}

calculate_spokes <- function(mydf) {
  .n <- ncol(mydf) - 1
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / (.n))
  data.frame(x = 0, y = 0, xend = sin(angles[-1]), yend = cos(angles[-1]))
}

make_radar <- function(.species) {
  dat <- wide_pm %>%
    filter(mp %in% top_top_pm_names, species == .species) %>%
    select(-1, -2)

  spokes_data <- calculate_spokes(dat)
  spokes_data$pm <- names(dat[, -1])
  radar_data <- calculate_radar(dat)
  label_data <- data.frame(x = 0, y = c(0.5, 0.75))

  ## for ggrepel labels:
  # radar_data$pm <- rep(c(names(dat[,-1]), ""), length(unique(dat[,1])))
  # labs <- filter(radar_data, pm %in% c("AAVY", "PNOF")) %>%
  #   group_by(group) %>%
  #   summarize(xx = approx(x = x, y = y, n = 7)$x[6],
  #     yy = approx(x = x, y = y, n = 7)$y[6])

  radar_data %>%
    ggplot(aes(x = x, y = y)) +
    geom_segment(
      data = spokes_data,
      aes(x = x, y = y, xend = xend, yend = yend), colour = "grey75", lty = 1
    ) +
    # FIXME: do this more elegantly in the data:
    geom_path(
      data = rbind(spokes_data, spokes_data[1, ]),
      aes(x = xend * 0.5, y = yend * 0.5), colour = "grey75", lty = 2
    ) +
    geom_path(
      data = rbind(spokes_data, spokes_data[1, ]),
      aes(x = xend * 0.75, y = yend * 0.75), colour = "grey75", lty = 2
    ) +
    geom_path(
      data = rbind(spokes_data, spokes_data[1, ]),
      aes(x = xend * 1, y = yend * 1), colour = "grey75", lty = 2
    ) +
    geom_path(aes(colour = as.factor(group)), lwd = 0.8) +
    coord_equal() +
    geom_text(data = spokes_data, aes(
      x = xend * 1.1, y = yend * 1.1,
      label = pm
    ), colour = "grey30") +
    geom_text(
      data = label_data,
      aes(x = x, y = y, label = y), colour = "grey50",
      nudge_y = 0.04, hjust = 0, nudge_x = 0.01
    ) +
    gfplot::theme_pbs() +
    labs(colour = "MP") +
    # scale_color_viridis_d() +
    scale_color_brewer(palette = "Set2") +
    # guides(colour = FALSE) +
    # ggrepel::geom_text_repel(data = labs,
    #   aes(x = xx, y = yy, label = group, colour = group),
    #   nudge_y = 0.1, nudge_x = -0.1) +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank()
    ) +
    theme(legend.position="right")

}

out <- lapply(species_names$species, make_radar)
pdf("report/figure/screening-radar.pdf", width = 17, height = 9)
cowplot::plot_grid(plotlist = out, labels = species_names$species_full, label_fontface = "plain", hjust = 0, label_x = 0.05)
dev.off()
