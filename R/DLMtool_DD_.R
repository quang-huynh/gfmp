#This is DLMtool::DD_
#This is the Delay Difference MP. It optimises DD_R which is the actual delay
# difference model used in the MP

#For investigation, RF has renamed DD_ to myDLDD and renamed DD_R to myDLDDR

myDLDD <-function (x, Data, reps = 100, hcr = NULL)
{
  Winf <- Data@wla[x] * Data@vbLinf[x]^Data@wlb[x]
  age <- 1:Data@MaxAge
  la <- Data@vbLinf[x] * (1 - exp(-Data@vbK[x] * ((age - Data@vbt0[x]))))
  wa <- Data@wla[x] * la^Data@wlb[x]
  a50V <- iVB(Data@vbt0[x], Data@vbK[x], Data@vbLinf[x], Data@L50[x])
  a50V <- max(a50V, 1)
  yind <- which(!is.na(Data@Cat[x, ] + Data@Ind[x, ]))[1]
  yind <- yind:length(Data@Cat[x, ])
  Year <- Data@Year[yind]
  C_hist <- Data@Cat[x, yind]
  I_hist <- Data@Ind[x, yind]
  if (any(is.na(C_hist))) {
    C.xind <- 1:length(C_hist)
    C_hist <- approx(x = C.xind[!is.na(C_hist)], y = C_hist[!is.na(C_hist)],
                     n = length(C.xind))$y
  }
  if (any(is.na(I_hist))) {
    I.xind <- 1:length(I_hist)
    I_hist <- approx(x = I.xind[!is.na(I_hist)], y = I_hist[!is.na(I_hist)],
                     n = length(I.xind))$y
  }
  E_hist <- C_hist/I_hist
  E_hist <- E_hist/mean(E_hist)
  ny_DD <- length(C_hist)
  k_DD <- ceiling(a50V)
  k_DD[k_DD > Data@MaxAge/2] <- ceiling(Data@MaxAge/2)
  Rho_DD <- (wa[k_DD + 2] - Winf)/(wa[k_DD + 1] - Winf)
  Alpha_DD <- Winf * (1 - Rho_DD)
  So_DD <- exp(-Data@Mort[x])
  wa_DD <- wa[k_DD]
  UMSYpriorpar <- c(1 - exp(-Data@Mort[x] * 0.5), 0.3)
  UMSYprior <- c(alphaconv(UMSYpriorpar[1], prod(UMSYpriorpar)),
                 betaconv(UMSYpriorpar[1], prod(UMSYpriorpar)))
  params <- log(c(UMSYpriorpar[1]/(1 - UMSYpriorpar[1]), 3 *
                    mean(C_hist, na.rm = T), Data@Mort[x]))
  opt <- optim(params, myDLDDR, opty = 1, So_DD = So_DD, Alpha_DD = Alpha_DD,
               Rho_DD = Rho_DD, ny_DD = ny_DD, k_DD = k_DD, wa_DD = wa_DD,
               E_hist = E_hist, C_hist = C_hist, UMSYprior = UMSYprior,
               method = "BFGS", hessian = TRUE)
  if (reps > 1) {
    samps <- mvtnorm::rmvnorm(reps, opt$par, solve(opt$hessian))
  }
  else {
    samps <- matrix(c(opt$par[1], opt$par[2], opt$par[3]),
                    nrow = 1)
  }
  UMSY <- 1/(1 + exp(-opt$par[1]))
  EMSY <- -log(1 - UMSY)/exp(opt$par[3])
  eff <- EMSY/E_hist[Data@LHYear[1] - Year[1] + 1]
  eff[!is.finite(eff)] <- 0.01
  eff[eff > 1e+05] <- 0.01
  getVals <- sapply(1:reps, function(i) DD_R(params = samps[i,
                                                            ], opty = 2, So_DD = So_DD, Alpha_DD = Alpha_DD, Rho_DD = Rho_DD,
                                             ny_DD = ny_DD, k_DD = k_DD, wa_DD = wa_DD, E_hist = E_hist,
                                             C_hist = C_hist, UMSYprior = UMSYprior))
  TAC <- unlist(getVals[1, ])
  dep <- unlist(getVals[2, ])
  Cpredict <- do.call("cbind", getVals[3, ])
  B_DD <- do.call("cbind", getVals[4, ])
  if (!is.null(hcr)) {
    cond1 <- !is.na(dep) & dep < hcr[1] & dep > hcr[2]
    cond2 <- !is.na(dep) & dep < hcr[2]
    TAC[cond1] <- TAC[cond1] * (dep[cond1] - hcr[2])/(hcr[1] -
                                                        hcr[2])
    TAC[cond2] <- TAC[cond2] * tiny
  }
  return(list(TAC = TAC, C_hist = C_hist, I_hist = I_hist,
              E_hist = E_hist, dep = dep, Cpredict = Cpredict, B_DD = B_DD,
              hcr = hcr, Year = Data@Year[yind], eff = eff))
}



