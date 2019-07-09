#This is DLMtool::DD_R
#This is the internal Delay Difference model that gets optimised in the MP DD_

#For investigation, RF has renamed DD_ to myDLDD and renamed DD_R to myDLDDR

myDLDDR <- function (params, opty, So_DD, Alpha_DD, Rho_DD, ny_DD, k_DD,
          wa_DD, E_hist, C_hist, UMSYprior)
{
  UMSY_DD = 1/(1 + exp(-params[1]))
  MSY_DD = exp(params[2])
  q_DD = exp(params[3])
  SS_DD = So_DD * (1 - UMSY_DD) #Fished survival rate
  Spr_DD = (SS_DD * Alpha_DD/(1 - SS_DD) + wa_DD)/(1 - Rho_DD *SS_DD) #spawners per recruit
  DsprDu_DD = ((Alpha_DD + Spr_DD * (1 + Rho_DD - 2 * Rho_DD *
                                       SS_DD))/((1 - Rho_DD * SS_DD) * (1 - SS_DD)) + Alpha_DD *
                 SS_DD/((1 - Rho_DD * SS_DD) * (1 - SS_DD)^2) - Spr_DD/(1 -
                                                                          SS_DD)) * -So_DD
  Arec_DD = 1/(((1 - UMSY_DD)^2) * (Spr_DD + UMSY_DD * DsprDu_DD))
  Brec_DD = UMSY_DD * (Arec_DD * Spr_DD - 1/(1 - UMSY_DD))/MSY_DD
  Spr0_DD = (So_DD * Alpha_DD/(1 - So_DD) + wa_DD)/(1 - Rho_DD *
                                                      So_DD)
  Ro_DD = (Arec_DD * Spr0_DD - 1)/(Brec_DD * Spr0_DD)
  Bo_DD = Ro_DD * Spr0_DD
  No_DD = Ro_DD/(1 - So_DD)
  B_DD <- rep(NA, ny_DD + 1)
  N_DD <- rep(NA, ny_DD + 1)
  R_DD <- rep(NA, ny_DD + k_DD)
  Cpred_DD <- rep(NA, ny_DD)
  B_DD[1] = Bo_DD
  N_DD[1] = No_DD
  R_DD[1:k_DD] = Ro_DD
  for (tt in 1:ny_DD) {
    Surv_DD = So_DD * exp(-q_DD * E_hist[tt])
    Cpred_DD[tt] = B_DD[tt] * (1 - exp(-q_DD * E_hist[tt]))
    Sp_DD = B_DD[tt] - Cpred_DD[tt]
    R_DD[tt + k_DD] = Arec_DD * Sp_DD/(1 + Brec_DD * Sp_DD)
    B_DD[tt + 1] = Surv_DD * (Alpha_DD * N_DD[tt] + Rho_DD *
                                B_DD[tt]) + wa_DD * R_DD[tt + 1]
    N_DD[tt + 1] = Surv_DD * N_DD[tt] + R_DD[tt + 1]
  }
  tiny <- 1e-15
  Cpred_DD[Cpred_DD < tiny] <- tiny
  if (opty == 1) {
    umsy_penalty <- ifelse(Spr_DD + UMSY_DD * DsprDu_DD >
                             0, 0, UMSY_DD * 100)
    alpha_penalty <- ifelse(Arec_DD * Spr_DD * (1 - UMSY_DD) -
                              1 > 0, 0, UMSY_DD * 100)
    sigma <- sqrt(sum((log(C_hist) - log(Cpred_DD))^2)/ny_DD)
    test <- dnorm(log(C_hist), log(Cpred_DD), sigma, log = T)
    test2 <- dbeta(UMSY_DD, UMSYprior[1], UMSYprior[2], log = T)
    test[is.na(test)] <- -1000
    test[test == (-Inf)] <- -1000
    if (is.na(test2) | test2 == -Inf | test2 == Inf)
      test2 <- 1000
    return(-sum(test, test2) + umsy_penalty + alpha_penalty)
  }
  else {
    return(list(TAC = UMSY_DD * B_DD[ny_DD + 1], dep = B_DD[tt +1]/Bo_DD, Cpred_DD = Cpred_DD, B_DD = B_DD))
  }
}
