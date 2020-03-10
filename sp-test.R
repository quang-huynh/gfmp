library(MSEtool)
library(ggplot2)
library(dplyr)

Data <- readRDS("rex-data-example.rds")
matplot(t(Data@Ind), type = "l")
Data@Ind <- Data@Ind[, 1:65]
Data@Cat <- Data@Cat[, 1:65]
matplot(t(Data@Ind)[, 1], type = "l")
matplot(t(Data@Cat)[, 1], type = "l")
m <- MSEtool::SP(1, Data, silent = TRUE)

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
  m <- MSEtool::SP(1, dat, use_r_prior = TRUE,
    start = list(r_prior = c(mean(r_prior), 0.1)))
  data.frame(year = i, FMSY = m@FMSY, F_FMSY = unname(m@F_FMSY[length(m@F_FMSY)]), r = m@TMB_report$r)
})

out %>%
  reshape2::melt(id.vars = "year") %>%
  ggplot(aes(year, value)) + geom_line() + facet_wrap(~variable, scale = "free_y")

# -----------------------

library(DLMtool)
library(MSEtool)

sp_debug <- function (x, Data, reps = 1) {
  dependencies <- "Data@Cat, Data@Ind"
  do_Assessment <- SP(x = x, Data = Data, control = list(iter.max = 1e4, eval.max = 2e4), n_seas = 1, use_r_prior = FALSE)
  Rec <- HCR_ramp(Assessment = do_Assessment, reps = reps, LRP = 0.4, TRP = 0.6, RP_type = "SSB_SSBMSY")
  Cr <- length(Data@Cat[x, ])
  cct <- Data@Cat[x, Cr]

  if (!is.na(Rec@TAC)) {
    if (as.list(do_Assessment@SD, "Std. Error")$log_FMSY > 1) {
      warning("Std. Error too large.")
      Rec@TAC <- Data@MPrec[x]
    }
    # if (Rec@TAC > 1.2 * cct) {
    #   warning("TAC > 1.2 last TAC.")
    #   Rec@TAC <- 1.2 * cct
    # }
    # if (Rec@TAC < .5 * cct) {
    #   warning("TAC < 0.5 last TAC.")
    #   Rec@TAC <- .5 * cct
    # }
  }
  Rec@Misc <- MSEtool:::Assess_diagnostic(x, Data, do_Assessment, include_assessment = TRUE)
  return(Rec)
}
class(sp_debug) <- "MP"
.sp_debug <- gfdlm::reduce_survey(sp_debug)

omrex <- readRDS("generated-data/rex-sra.rds")@OM
omrex@cpars$Data <- NULL
omrex@TACSD <- c(0, 0)
omrex@interval <- 2
omrex@nsim <- 3
omrex@DR <- 0
omrex@seed <- 13
# omrex@proyears <- 12

rex_mse <- runMSE(OM = omrex, MPs = c("FMSYref75", "FMSYref", "sp_debug"), PPD = TRUE)

gfdlm::plot_projection_ts(rex_mse, type = "TAC")
gfdlm::plot_projection_ts(rex_mse, type = "B_BMSY")
gfdlm::plot_projection_ts(rex_mse, type = "F_FMSY")

# inspect then 'c' to continue to next time step.

matplot(t(rex_mse@TAC[,,]), type = "l", ylim = c(0, max(rex_mse@TAC[,,])*1.04), yaxs = "i")
matplot(t(rex_mse@TAC[,,]), type = "l", yaxs = "i")


# sp_debug <- make_MP(SP, HCR_MSY, diagnostic = 'full')
# omrex@seed <- 13
# rex_mse <- runMSE(omrex, MPs = "sp_debug", PPD = TRUE)
par(mfrow = c(2, 1))
matplot(t(rex_mse@TAC[,,]), type = "l", ylim = c(0, max(rex_mse@TAC[,,])*1.04), yaxs = "i")
# diagnostic_AM(rex_mse) # convergence diagnostics
# retrospective_AM(MSE)


Data <- rex_mse@Misc$Data[[1]]
saveRDS(Data, file = "~/Desktop/rex-data-example.rds")
matplot(t(Data@Ind), type = "l")
Data@CV_Ind

Data@LHYear
Data@Ind <- Data@Ind[,1:65]
Data@Cat <- Data@Cat[,1:65]
m <- MSEtool::SP(1, Data, silent = TRUE, n_seas = 1)

# matplot(t(Data@Cat), type = "l")
matplot(t(rex_mse@TAC[,,]), type = "l", ylim = c(0, max(rex_mse@TAC[,,])*1.04), yaxs = "i")

# Data@
for (i in 23:71) {
  Data <- rex_mse@Misc$Data[[1]]
  Data@Ind <- Data@Ind[,1:i]
  Data@Cat <- Data@Cat[,1:i]
  m <- MSEtool::SP(1, Data, silent = TRUE, n_seas = 1, start = list(r_prior = c(0.6, 0.5)), use_r_prior = TRUE)
  print(c(i, m@FMSY, unname(m@F_FMSY[length(m@F_FMSY)]), m@TMB_report$r))
}

# m <- MSEtool::SP_SS(1, Data, integrate = TRUE, fix_sigma = TRUE, silent = FALSE, start = list(sigma = 0.1), use_r_prior = TRUE)
plot(m)




rex_mse@OM

# ----------------------

r_prior_fn <- function(x = 1, Data, r_reps = 1e2, SR_type = c("BH", "Ricker"), seed = NULL) {
  SR_type <- match.arg(SR_type)

  # set.seed(x)
  M <- trlnorm(r_reps, Data$Mort[x], DataCV_Mort[x])
  steep <- sample_steepness3(r_reps, Data@steep[x], Data@CV_steep[x], SR_type)

  max_age <- Data@MaxAge
  a <- Data@wla[x]
  b <- Data@wlb[x]
  Linf <- Data@vbLinf[x]
  K <- Data@vbK[x]
  t0 <- Data@vbt0[x]
  La <- Linf * (1 - exp(-K * (c(1:max_age) - t0)))
  Wa <- a * La ^ b

  A50 <- min(0.5 * max_age, iVB(t0, K, Linf, Data@L50[x]))
  A95 <- max(A50+0.5, iVB(t0, K, Linf, Data@L95[x]))
  mat_age <- 1/(1 + exp(-log(19) * (c(1:max_age) - A50)/(A95 - A50)))
  mat_age <- mat_age/max(mat_age)

  log_r <- vapply(1:r_reps, function(y) uniroot(Euler_Lotka_fn, c(-6, 2), M = M[y], h = steep[y], weight = Wa,
    mat = mat_age, maxage = max_age, SR_type = SR_type)$root, numeric(1))
  return(exp(log_r))
}

Euler_Lotka_fn <- function(log_r, M, h, weight, mat, maxage, SR_type) {
  M <- rep(M, maxage)
  surv0 <- exp(-M)
  NPR <- c(1, cumprod(surv0[1:maxage-1]))
  NPR[maxage] <- NPR[maxage]/(1 - surv0[maxage])

  SBPR <- sum(NPR * weight * mat)
  CR <- ifelse(SR_type == "BH", 4*h/(1-h), (5*h)^1.25)
  R_per_S <- CR/SBPR

  EL <- R_per_S * sum(NPR * weight * mat * exp(-exp(log_r) * c(1:maxage)))
  return(EL - 1)
}

Euler_Lotka_fn(0.5, 0.2, h = 0.8, weight = 10, maxage = 80, SR_type = "BH", mat=0.5)
