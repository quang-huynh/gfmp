library(dplyr)
library(DLMtool)
library(MSEtool)
library(here)
library(cowplot)

species_name <- "Rex Sole"
starting_year <- 1996
ending_year <- 2018
all_years <- seq(starting_year, ending_year)
nyear <- length(all_years)

fig_dir <- here("report", "figure")
if (!dir.exists(fig_dir)) dir.create(fig_dir)

drex <- readRDS(here("generated-data", "rex-filter-data.rds"))

rex_om <- readRDS(here("generated-data", "rex-om.rds"))
rex_om@M
rex_om@Linf
rex_om@K
rex_om@t0
rex_om@a
rex_om@b
rex_om@L50
rex_om@L50_95
rex_om@LFS
rex_om@L5
rex_om@Cobs
rex_om@Perr
rex_om@Msd <- c(0, 0)
rex_om@Linfsd <- c(0, 0)
rex_om@Ksd <- c(0, 0)
rex_om@nyears
rex_om@maxage
rex_om@M
rex_om@maxage
rex_om@h
rex_om@L5
rex_om@LFS
assertthat::assert_that(identical(rex_om@nyears, length(all_years)))

make_cal <- function(dat, survey, yrs, length_bin = 5) {
  cal <- dat %>%
    dplyr::filter(survey_abbrev == survey) %>%
    gfdlm::tidy_cal(yrs = yrs, interval = length_bin)

  length_bins <- gfdlm:::get_cal_bins(cal, length_bin_interval = length_bin)
  list(cal = cal[1, , ], length_bins = length_bins)
}

cal_wcvi <- make_cal(drex$survey_samples, "SYN WCVI", yrs = all_years)

mean_length <- dplyr::filter(drex$survey_samples, survey_abbrev == "SYN WCVI") %>%
  gfdlm::tidy_mean_length() %>%
  dplyr::filter(n > 10, year <= ending_year, year >= starting_year) %>%
  right_join(tibble(year = all_years), by = "year") %>%
  pull(mean_length)

mean_length

if ("catch" %in% names(drex)) {
  catch <- drex$catch %>%
    gfplot::tidy_catch() %>%
    group_by(year) %>%
    summarize(value = sum(value)) %>%
    right_join(tibble(year = all_years), by = "year") %>%
    pull(value)
  saveRDS(catch, file = here::here("generated-data", "rex-catch.rds"))
} else {
  catch <- readRDS(here::here("generated-data", "rex-catch.rds"))
}
catch
plot(all_years, catch, type = "o")

# catch per unit effort from the trawl fleet only:
# cpue <- readRDS("../../gfs/report/cpue-cache/rex-sole.rds")
# readr::write_csv(cpue, path = here("generated-data", "rex-cpue.csv"))
cpue <- read.csv(here("generated-data", "rex-cpue.csv")) %>%
  filter(area == "3CD")

indexes <- drex$survey_index %>%
  dplyr::filter(survey_abbrev %in% c("SYN WCVI")) %>%
  select(year, biomass, re) %>%
  right_join(tibble(year = all_years),by  = "year") %>%
  left_join(rename(select(cpue, year, est, se_link), trawl_cpue = est, trawl_sd = se_link), by = "year") %>%
  select(-year) %>%
  as.matrix()

indexes
plot(all_years, indexes[, 1L], type = "o")
# plot(all_years, indexes[, 2L], type = "o")

plot(all_years, indexes[, 3L], type = "o")

MSEtool::plot_composition(all_years,
  obs = cal_wcvi$cal,
  CAL_bins = cal_wcvi$length_bins
)

I_sd <- indexes[, 2L]
I_sd

rex_om@nsim <- 50
cores <- floor(parallel::detectCores() / 1)

rex_om@Cobs <- c(0, 0)
rex_om@Cbiascv <- c(0, 0)
library(MSEtool)
rex_sra_ceq0 <- MSEtool::SRA_scope(rex_om,
  # CAL = cal_wcvi$cal, length_bin = cal_wcvi$length_bins,
  Chist = catch, Index = indexes[, 1], integrate = FALSE,
  C_eq = 0,
  I_sd = I_sd, I_type = "B", cores = cores,
  drop_nonconv = TRUE, mean_fit = TRUE
)
rex_sra_ceq50 <- MSEtool::SRA_scope(rex_om,
  Chist = catch, Index = indexes[, 1], integrate = FALSE,
  C_eq = 0.5*catch[1],
  I_sd = I_sd, I_type = "B", cores = cores,
  drop_nonconv = TRUE, mean_fit = TRUE
)

rex_sra_ceq10 <- MSEtool::SRA_scope(rex_om,
  Chist = catch, Index = indexes[, 1], integrate = FALSE,
  C_eq = 0.1*catch[1],
  I_sd = I_sd, I_type = "B", cores = cores,
  drop_nonconv = TRUE, mean_fit = TRUE
)

rex_sra_ceq100 <- MSEtool::SRA_scope(rex_om,
  Chist = catch, Index = indexes[, 1], integrate = FALSE,
  C_eq = catch[1],
  I_sd = I_sd, I_type = "B", cores = cores,
  drop_nonconv = TRUE, mean_fit = TRUE
)
quantile(rex_sra_ceq100@OM@cpars$D)

rex_sra_ceq200 <- MSEtool::SRA_scope(rex_om,
  Chist = catch, Index = indexes[, 1], integrate = FALSE,
  C_eq = 2*catch[1],
  I_sd = I_sd, I_type = "B", cores = cores,
  drop_nonconv = TRUE, mean_fit = TRUE
)
quantile(rex_sra_ceq200@OM@cpars$D)

rex_om@M
rex_om_high_m <- rex_om
rex_om_high_m@M <- c(0.25, 0.25)
rex_sra_high_m <- MSEtool::SRA_scope(rex_om_high_m,
  Chist = catch, Index = indexes[, 1], integrate = FALSE,
  C_eq = 0.5*catch[1],
  I_sd = I_sd, I_type = "B", cores = cores,
  drop_nonconv = TRUE, mean_fit = TRUE
)
quantile(rex_sra_ceq50@OM@cpars$D)
quantile(rex_sra_high_m@OM@cpars$D)

rex_om@M
rex_om_low_m <- rex_om
rex_om_low_m@M <- c(0.10, 0.1)
rex_sra_low_m <- MSEtool::SRA_scope(rex_om_low_m,
  Chist = catch, Index = indexes[, 1], integrate = FALSE,
  C_eq = 0.5*catch[1],
  I_sd = I_sd, I_type = "B", cores = cores,
  drop_nonconv = TRUE, mean_fit = TRUE
)
quantile(rex_sra_ceq50@OM@cpars$D)
quantile(rex_sra_low_m@OM@cpars$D)
quantile(rex_sra_high_m@OM@cpars$D)

rex_om_low_h <- rex_om
rex_om@h
rex_om_low_h@h <- c(0.4, 0.6)
rex_sra_low_h <- MSEtool::SRA_scope(rex_om_low_h,
  Chist = catch, Index = indexes[, 1], integrate = FALSE,
  C_eq = 0.5*catch[1],
  I_sd = I_sd, I_type = "B", cores = cores,
  drop_nonconv = TRUE, mean_fit = TRUE
)
quantile(rex_sra_ceq50@OM@cpars$D)
quantile(rex_sra_low_h@OM@cpars$D)
median(rex_sra_ceq50@OM@cpars$D)
median(rex_sra_low_h@OM@cpars$D)

rex_om_high_h <- rex_om
rex_om@h
rex_om_high_h@h <- c(0.9, 0.9)
rex_sra_high_h <- MSEtool::SRA_scope(rex_om_high_h,
  Chist = catch, Index = indexes[, 1], integrate = FALSE,
  C_eq = 0.5*catch[1],
  I_sd = I_sd, I_type = "B", cores = cores,
  drop_nonconv = TRUE, mean_fit = TRUE
)
quantile(rex_sra_ceq50@OM@cpars$D)
quantile(rex_sra_high_h@OM@cpars$D)

# rex_sra_cpue <- MSEtool::SRA_scope(rex_om,
#   data = list(
#     Chist = catch,
#     Index = indexes[, c("biomass", "trawl_cpue")],
#     C_eq = 0.5*catch[1],
#     I_sd = indexes[, c("re", "trawl_sd")], I_type = c("B", "B")),
#   cores = cores,
#   drop_nonconv = TRUE
# )
# quantile(rex_sra_ceq50@OM@cpars$D)
# quantile(rex_sra_cpue@OM@cpars$D)

rex_sra_ceq100 <- MSEtool::SRA_scope(rex_om,
  Chist = catch, Index = indexes[, 1], integrate = FALSE,
  C_eq = catch[1],
  I_sd = I_sd, I_type = "B", cores = cores,
  drop_nonconv = TRUE
)

scenarios <- c(rex_sra_base, rex_sra_ceq10, rex_sra_ceq50,rex_sra_ceq100)
scenarionames <- c("base","ceq10","ceq50","ceq100")
scenarios_human <- c("Base OM", "Catch eq. 10%", "Catch eq. 50%", "Catch eq. 100%")


#Compare initial depletion, depletion and biomass results:
make_initD <- function(scenario,scenario_name) {
   g <- scenario@OM@cpars$D %>% as.data.frame() %>% rename(D =1)  %>%
     ggplot(aes(D)) +
     geom_histogram(binwidth=0.05)+
     ggtitle(scenario_name) + theme(plot.title = element_text(hjust = 0.5))
   g
 }

 make_Depletion <- function(scenario,scenario_name) {
   Depletion <- scenario@SSB/sapply(scenario@Misc, getElement, 'E0_SR')
   probsDepletion <- t(apply(Depletion[,-nyear],2,FUN=quantile,probs=c(0.025,0.5,0.975))) %>%
     as.data.frame() %>%
     cbind(all_years) %>%
     rename(Lower=1, Median=2, Upper=3, Year=all_years)

   g <- ggplot(probsDepletion) +
     geom_ribbon(data=probsDepletion, aes(x=Year, ymin=Lower, ymax=Upper),
                 inherit.aes = FALSE, fill = "blue", alpha=0.2)+
     geom_line(aes(Year,Median), colour="blue", lwd=2)+
     scale_y_continuous(limits = c(0,1.2), breaks = seq(0,1.2,by = 0.2), name="Spawning depletion")+
     scale_x_continuous(limits = c(starting_year,ending_year),breaks = seq(starting_year,ending_year,by = 4)) +
     ggtitle(scenario_name) + theme(plot.title = element_text(hjust = 0.5))
   g
 }

 make_Biomass <- function(scenario,scenario_name) {
   SSB <- scenario@SSB/1000
   probsSSB <- t(apply(SSB[,-nyear],2,FUN=quantile,probs=c(0.025,0.5,0.975))) %>%
     as.data.frame() %>%
     cbind(all_years) %>%
     rename(Lower=1, Median=2, Upper=3, Year=all_years)

   g <- ggplot(probsSSB) +
     geom_ribbon(data=probsSSB, aes(x=Year, ymin=Lower, ymax=Upper),
                 inherit.aes = FALSE, fill = "purple", alpha=0.2)+
     geom_line(aes(Year,Median), colour="purple", lwd=2)+
     scale_y_continuous(limits = c(0,5000), breaks = seq(0,5000,by = 1000), name="Spawning biomass (x 1,000)")+
     scale_x_continuous(limits = c(starting_year,ending_year),breaks = seq(starting_year,ending_year,by = 4)) +
     ggtitle(scenario_name) + theme(plot.title = element_text(hjust = 0.5))
   g
 }


#Make multipanel plots using purrr and cowplot
initDepletionPlots  <- purrr::map2(scenarios, scenarios_human, make_initD)
g <- cowplot::plot_grid(plotlist = initDepletionPlots, align = "hv",nrow = 2, ncol = 2)
ggsave(file.path(fig_dir, paste0("rex-compare-SRA-init-depletion-panel.png")),
        width = 11, height = 12)

DepletionPlots  <- purrr::map2(scenarios, scenarios_human, make_Depletion)
g <- cowplot::plot_grid(plotlist = DepletionPlots, align = "hv",nrow = 2, ncol = 2)
ggsave(file.path(fig_dir, paste0("rex-compare-SRA-depletion-panel.png")),
        width = 11, height = 12)

SSBPlots  <- purrr::map2(scenarios, scenarios_human, make_Biomass)
g <- cowplot::plot_grid(plotlist = SSBPlots, align = "hv",nrow = 2, ncol = 2)
ggsave(file.path(fig_dir, paste0("rex-compare-SRA-SSB-panel.png")),
       width = 11, height = 12)

saveRDS(rex_sra_ceq0, file = here("generated-data", "rex-sra-ceq0.rds"))
saveRDS(rex_sra_ceq10, file = here("generated-data", "rex-sra-ceq10.rds"))
saveRDS(rex_sra_ceq50, file = here("generated-data", "rex-sra-ceq50.rds"))
saveRDS(rex_sra_ceq100, file = here("generated-data", "rex-sra-ceq100.rds"))

quantile(rex_sra_ceq0@OM@cpars$D)
quantile(rex_sra_ceq10@OM@cpars$D)
quantile(rex_sra_ceq50@OM@cpars$D)
quantile(rex_sra_ceq100@OM@cpars$D)

