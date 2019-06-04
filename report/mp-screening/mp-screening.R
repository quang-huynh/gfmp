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

pop_om <- readRDS(here("report/mp-screening/om/pop.rds"))
rdb_om <- readRDS(here("report/mp-screening/om/redbanded.rds"))
rgh_om <- readRDS(here("report/mp-screening/om/rougheye.rds"))
srt_om <- readRDS(here("report/mp-screening/om/shortspine.rds"))
yel_om <- readRDS(here("report/mp-screening/om/yelloweye.rds"))

folder <- here("report/mp-screening/mse-generated")

oms <- list(pop = pop_om, rdb = rdb_om, rgh = rgh_om, srt = srt_om, yel = yel_om)
mse <- list()

candidate_mps <- readr::read_csv(here("report/data/dlmtool-mps.csv")) %>%
  filter(Candidate == "Y") %>%
  rename(mp = `Management Procedure`)

all_mps <- union(avail("Output"), avail("Reference"))
mps_keep <- vapply(all_mps, function(x) any(grepl(x, candidate_mps$mp)),
  FUN.VALUE = logical(1L))
mps_keep <- names(mps_keep[mps_keep])

DLMtool::setup(cpus = parallel::detectCores())
for (i in seq_along(oms)) {
  oms[[i]]@seed <- 42L
  oms[[i]]@nsim <- 48L
  fi <- paste0(file.path(folder, names(oms)[i]), ".rds")
  if (!file.exists(fi)) {
    mse[[i]] <- runMSE(OM = oms[[i]], MPs = mps_keep, parallel = TRUE)
    saveRDS(mse[[i]], file = fi)
  } else {
    mse[[i]] <- readRDS(fi)
  }
}
snowfall::sfStop()

plot(mse[[1]])
