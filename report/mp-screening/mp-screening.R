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
  oms[[i]]@seed <- 42L
  oms[[i]]@nsim <- 100L
  fi <- paste0(file.path(folder, names(oms)[i]), ".rds")
  if (!file.exists(fi)) {
    mse[[i]] <- runMSE(OM = oms[[i]], MPs = mps_keep, parallel = TRUE)
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

# r_plot(pm[[3]])
# r_plot(pm[[4]])
# r_plot(pm[[5]])
# plot(mse[[1]])

wide_pm <- bind_rows(pm) %>%
  as.data.frame() %>%
  filter(!species %in% c("rdb"), mp != "YPR") %>%
  filter(pm %in% c("PNOF", "P100", "P40", "LTY", "AAVY")) %>%
  reshape2::dcast(class + species + mp ~ pm, value.var = "prob")

top_pm <- wide_pm %>%
  group_by(species) %>%
  mutate(P100 = P100 / max(P100), PNOF = PNOF / max(PNOF),
    P40 = P40 / max(P40), LTY = LTY / max(LTY), AAVY = AAVY / max(AAVY)) %>%
  # filter(AAVY > 0.50) %>%
  filter(P40 > 0.90) %>%
  filter(P100 > 0.50) %>%
  # filter(LTY > 0.50) %>%
  as.data.frame() %>%
  filter(class != "Reference") %>%
  arrange(species, -LTY, -P40, -AAVY, -PNOF) %>%
  group_by(species) %>%
  top_n(n = 5L, wt = P40) %>%
  # arrange(species, -LTY) %>%
  # group_by(species) %>%
  # top_n(n = 10L) %>%
  as.data.frame()

top_pm
sort(table(top_pm$mp))

top_pm_names <- unique(top_pm$mp)
length(top_pm_names)

top_top_pm_names <- names(table(top_pm$mp))[table(top_pm$mp) > 1L]
top_top_pm_names

species_names <- tibble(species = c("pop", "rgh", "srt", "yel", "arr"),
  species_full = c("pacific ocean perch", "rougheye rockfish", "shortspine thornyhead",
    "yelloweye rockfish", "arrowtooth flounder"))

plot_pm <- function(x, y, colour) {
  wide_pm %>%
    left_join(species_names, by = "species") %>%
    # filter(mp %in% top_pm_names | class == "Reference") %>%
    filter(mp %in% top_top_pm_names | class == "Reference" | mp %in% c("DD", "AvC")) %>%
    ggplot(aes_string(x = x, y = y)) +
    geom_point(aes_string(colour = colour, shape = "class")) +
    xlim(0, 1) + ylim(0, 1) +
    facet_wrap(~species_full) +
    ggrepel::geom_text_repel(aes(label = mp), colour = "grey50") +
    scale_color_viridis_c(direction = -1) +
    scale_shape_manual(values = c("Reference" = 4, "Output" = 21)) +
    gfplot::theme_pbs()
}
plot_pm("P100", "LTY", "AAVY")
plot_pm("P40", "LTY", "AAVY")
plot_pm("P40", "PNOF", "LTY")
plot_pm("P100", "PNOF", "LTY")
