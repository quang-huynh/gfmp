library(MSEtool)
library(ggplot2)
library(dplyr)

Data <- readRDS("rex-data-example.rds")
matplot(t(Data@Ind), type = "l")
Data@Ind <- Data@Ind[, 1:67]
Data@Cat <- Data@Cat[, 1:67]
matplot(t(Data@Ind)[, 1], type = "l")
matplot(t(Data@Cat)[, 1], type = "l")
m <- MSEtool::SP(1, Data, silent = TRUE, start = list(FMSY = plogis(-9.707256), MSY = exp(0.87)))

m <- MSEtool::SP(1, Data, silent = TRUE, start = list(r_prior = c(0.2, 0.1)), use_r_prior = TRUE)
m@SD

library(tmbstan)
mm <- tmbstan(m@obj, chains = 2, iter = 1000, cores = 2, init = "last.par.best", control = list(adapt_delta = 0.98))
mm


m@SD

# Appears converged at first glance:
m@SD$pdHess
m@SD$gradient.fixed

# But the standard errors have blown up:
m@SD

# Iterate through the MSE model fits:
Data <- readRDS("rex-data-example.rds")
out <- purrr::map_df(23:71, function(i) {
  dat <- Data
  dat@Ind <- Data@Ind[, 1:i]
  dat@Cat <- Data@Cat[, 1:i]
  m <- MSEtool::SP(1, dat)
  data.frame(year = i, FMSY = m@FMSY, F_FMSY = unname(m@F_FMSY[length(m@F_FMSY)]), r = m@TMB_report$r)
})

out %>%
  reshape2::melt(id.vars = "year") %>%
  ggplot(aes(year, value)) + geom_line() + facet_wrap(~variable, scale = "free_y")

r_prior <- MSEtool:::r_prior_fn(1, Data, r_reps = 1e3)
mean(r_prior)
sd(r_prior)

# Here, the mean of the r prior needs to be lower (~0.2 or lower with SD of ~0.1)
# for it to not have convergence problems.
# (Can be seen through trial and error.)
out <- purrr::map_df(23:71, function(i) {
  dat <- Data
  dat@Ind <- Data@Ind[, 1:i]
  dat@Cat <- Data@Cat[, 1:i]
  m <- MSEtool::SP(1, dat, use_r_prior = FALSE,
    start = list(r_prior = c(mean(r_prior), 0.1), log_FMSY = -2.707256, log_MSY = 11.87))
  data.frame(year = i, FMSY = m@FMSY, F_FMSY = unname(m@F_FMSY[length(m@F_FMSY)]), r = m@TMB_report$r)
})

out %>%
  reshape2::melt(id.vars = "year") %>%
  ggplot(aes(year, value)) + geom_line() + facet_wrap(~variable, scale = "free_y")

# Propose also checking the standard errors on the fixed effects:

sp_debug <- function (x, Data, reps = 1) {
  dependencies <- "Data@Cat, Data@Ind"
  do_Assessment <- SP(x = x, Data = Data, control = list(iter.max = 1e4, eval.max = 2e4), n_seas = 1, use_r_prior = FALSE)
  Rec <- HCR_ramp(Assessment = do_Assessment, reps = reps, LRP = 0.4, TRP = 0.6, RP_type = "SSB_SSBMSY")
  # or set it to NA or whatever else you do when it does not converge:
  if (!is.na(Rec@TAC)) {
    if (as.list(do_Assessment@SD, "Std. Error")$log_FMSY > 1) {
      warning("Std. Error too large, etc...")
      Rec@TAC <- Data@MPrec[x]
    }
  }
  Rec@Misc <- MSEtool:::Assess_diagnostic(x, Data, do_Assessment, include_assessment = TRUE)
  return(Rec)
}

