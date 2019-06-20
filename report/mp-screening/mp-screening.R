library(dplyr)
library(ggplot2)
library(DLMtool)
library(here)

dir.create(here("report/mp-screening/om"), showWarnings = FALSE)
dir.create(here("report/mp-screening/mse-generated"), showWarnings = FALSE)

download_om <- function(species,
                        web_file,
                        base_url = "http://www.datalimitedtoolkit.org/Case_Studies_Table/",
                        method = "wget",
                        overwrite = FALSE){
  fn <- paste0(species, ".rds")
  if(overwrite | !file.exists(here("report/mp-screening/om", fn))){
    download.file(paste0(base_url, web_file),
                  here("report/mp-screening/om", fn),
                  method = method)
  }
}

overwrite <- FALSE
download_om("pop", "Pacific_Ocean_Perch_QC_BC_DFO/OM.rdata", overwrite = overwrite)
download_om("redbanded", "Redbanded_Rockfish_BC_DFO/OM.rdata", overwrite = overwrite)
download_om("rougheye", "Rougheye_Rockfish_BC_DFO/OM.rdata", overwrite = overwrite)
download_om("shortspine", "Shortspine_Thornyhead_BC_DFO/OM.rdata", overwrite = overwrite)
download_om("yelloweye", "Yelloweye_Rockfish_BC_DFO/OM.rdata", overwrite = overwrite)
download_om("arrowtooth", "Arrowtooth_Flounder_BC_DFO/OM.rdata", overwrite = overwrite)

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
mps_keep <- gsub(" ", "", unlist(strsplit(candidate_mps$mp, " ")))
mps_keep <- sort(union(mps_keep, DLMtool::avail("Reference")))

library(MSEtool)
mps_keep <- union(mps_keep, c("DDSS_MSY", "DDSS_4010", "SP_MSY", "SP_4010"))

# FIXME: Error: Islope3 is not a valid MP!?
mps_keep <- mps_keep[!mps_keep %in% c("Islope3")]
mps_keep <- sort(mps_keep)

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
source("R/plot-probability-table.R")
source("R/plot-probability-table.R")
source("R/plot-probability-table.R")
library(gfutilities)

probs <- lapply(mse, function(x) {
  get_probs(x, "P40", "P100", "PNOF", "LTY", "AAVY")
})

lapply(seq_along(probs), function(i) {
  pdf(paste0("report/figure/screening-probs-", names(oms)[i], ".pdf"), width = 5, height = 10)
  print(plot_probs(probs[[i]]))
  dev.off()
})

pm <- lapply(mse, function(x) {
  eval_pm(x,
    pm_list = list("PNOF", "P100", "P10", "P40", "LTY", "AAVY"))
})
for (i in seq_along(oms)) pm[[i]]$species <- names(oms)[i]


wide_pm <- bind_rows(pm) %>%
  as.data.frame() %>%
  # filter(!species %in% c("rdb"), mp != "YPR") %>%
  filter(mp != "YPR") %>%
  filter(pm %in% c("PNOF", "P100", "P40", "LTY", "AAVY")) %>%
  reshape2::dcast(class + species + mp ~ pm, value.var = "prob")

# relative performance:
# top_pm <- wide_pm %>%
#   group_by(species) %>%
#   mutate(P100 = P100 / max(P100), PNOF = PNOF / max(PNOF),
#     P40 = P40 / max(P40), LTY = LTY / max(LTY), AAVY = AAVY / max(AAVY)) %>%
#   filter(PNOF > 0.5) %>%
#   filter(P40 > 0.8) %>%
#   filter(P100 > 0.5) %>%
#   filter(LTY > 0.5) %>%
#   filter(class != "Reference") %>%
#   group_by(species) %>%
#   top_n(n = 10L, wt = LTY) %>%
#   as.data.frame()
#
# # absolute performance:
top_pm <- wide_pm %>%
  group_by(species) %>%
  mutate(LTY_FMSYref = LTY[mp == "FMSYref"]) %>%
  # filter(LTY  > 0.50 * LTY_FMSYref) %>%
  filter(LTY  > 0.60) %>%
  filter(PNOF > 0.60) %>%
  filter(P100 > 0.60) %>%
  filter(P40  > 0.90) %>%
  filter(class != "Reference") %>%
  group_by(species) %>%
  # top_n(n = 20L, wt = LTY) %>%
  as.data.frame()

omitted <- filter(wide_pm, !mp %in% as.character(unique(top_pm$mp)), class != "Reference") %>%
  pull(mp) %>% as.character() %>% unique() %>% sort()
omitted

top_pm
sort(table(top_pm$mp))

top_pm_names <- unique(top_pm$mp)
length(top_pm_names)
saveRDS(top_pm, file = here("generated-data/top-mp-screening.rds"))

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
saveRDS(wide_pm, file = here("generated-data/wide-pm-screening.rds"))
plot_pm("P100", "LTY", "AAVY")
plot_pm("P100", "LTY", "PNOF")
plot_pm("P40", "LTY", "AAVY")
plot_pm("P40", "PNOF", "LTY")
plot_pm("P100", "PNOF", "LTY")


# -------------------------------------------------------------------------------------------------
# Plot grid of spiders by species
make_radar_plot <- function(mps, file_name = NULL, ...){
  out <- lapply(seq_along(species_names$species), function(x){
    tmp <- as.data.frame(pm[[x]]) %>%
      filter(mp %in% mps) %>%
      filter(pm %in% c("PNOF", "P100", "P40", "LTY", "AAVY"))
    ggspider::spider_web(tmp, "mp", "pm", "prob")
  })
  if (!is.null(file_name)) {
    pdf(file_name, width = 11, height = 12)
    on.exit(dev.off())
  }
  cowplot::plot_grid(plotlist = out,
                     labels = gfsynopsis:::first_cap(species_names$species_full),
                     label_fontface = "plain", hjust = 0, label_x = 0.05, nrow = 3)
}

average_catch_mp <- c("AvC", "CC1", "CC2", "CC3", "CC4", "CC5")
data_moderate_mp <- c("DD", "DD4010", "SP_4010", "SP_MSY")
length_mp <- c("L95target", "Lratio_BHI", "Lratio_BHI2", "Lratio_BHI3",
  "LstepCC1", "LstepCC2", "Ltarget1", "Ltarget2", "Fdem_ML", "YPR_ML")
reference_mp <- c("NFref", "FMSYref", "FMSYref75")
index_mp <- c("Iratio", "Islope1", "Islope2", "Islope4", "Itarget1",
  "Itarget2", "Itarget3", "Itarget4", "ITM ICI", "ICI2", "SBT1", "SBT2")

make_radar_plot(average_catch_mp,
  file_name = here::here("report/figure/screening-radar-average-catch.pdf"))

make_radar_plot(data_moderate_mp,
file_name = here::here("report/figure/screening-radar-data-moderate.pdf"))

make_radar_plot(length_mp,
file_name = here::here("report/figure/screening-radar-length.pdf"))

make_radar_plot(reference_mp,
  file_name = here::here("report/figure/screening-radar-reference.pdf"))

make_radar_plot(index_mp,
  file_name = here::here("report/figure/screening-radar-index.pdf"))
