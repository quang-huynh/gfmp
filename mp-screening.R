library(dplyr)
library(ggplot2)
library(DLMtool)

dir.create(here::here("report/mp-screening/om"), showWarnings = FALSE)
dir.create(here::here("report/mp-screening/mse-generated"), showWarnings = FALSE)

download.file("http://www.datalimitedtoolkit.org/Case_Studies_Table/Pacific_Ocean_Perch_QC_BC_DFO/OM.rdata",
  here::here("report/mp-screening/om/pop.rds"))

download.file("http://www.datalimitedtoolkit.org/Case_Studies_Table/Redbanded_Rockfish_BC_DFO/OM.rdata",
  here::here("report/mp-screening/om/redbanded.rds"))

download.file("http://www.datalimitedtoolkit.org/Case_Studies_Table/Rougheye_Rockfish_BC_DFO/OM.rdata",
  here::here("report/mp-screening/om/rougheye.rds"))

download.file("http://www.datalimitedtoolkit.org/Case_Studies_Table/Shortspine_Thornyhead_BC_DFO/OM.rdata",
  here::here("report/mp-screening/om/shortspine.rds"))

download.file("http://www.datalimitedtoolkit.org/Case_Studies_Table/Yelloweye_Rockfish_BC_DFO/OM.rdata",
  here::here("report/mp-screening/om/yelloweye.rds"))

pop_om <- readRDS(here::here("report/mp-screening/om/pop.rds"))
rdb_om <- readRDS(here::here("report/mp-screening/om/redbanded.rds"))
rgh_om <- readRDS(here::here("report/mp-screening/om/rougheye.rds"))
srt_om <- readRDS(here::here("report/mp-screening/om/shortspine.rds"))
yel_om <- readRDS(here::here("report/mp-screening/om/yelloweye.rds"))

folder <- here::here("report/mp-screening/mse-generated")

oms <- list(pop = pop_om, rdb = rdb_om, rgh = rgh_om, srt = srt_om, yel = yel_om)
mse <- list()

candidate_mps <- readr::read_csv(here::here("report/data/dlmtool-mps.csv")) %>%
  filter(Candidate == "Y") %>%
  rename(mp = `Management Procedure`)

all_mps <- union(avail("Output"), avail("Reference"))
mps_keep <- vapply(all_mps, function(x) any(grepl(x, candidate_mps$mp)),
  FUN.VALUE = logical(1L))
mps_keep <- names(mps_keep[mps_keep])

DLMtool::setup(cpus = parallel::detectCores())

for (i in seq_along(oms)) {
  oms$seed <- 56129 # not sure if doing anything with this version
  fi <- paste0(folder, names(oms)[i])
  if (!file.exists(fi)) {
    mse[[i]] <- runMSE(OM = oms[[i]], MPs = mps, parallel = TRUE)
    saveRDS(mse[[i]], file = fi)
  } else {
    mse[[i]] <- readRDS(fi)
  }
}
