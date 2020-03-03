load("~/Downloads/rex.rda")
om@nsim <- 3

m <- MSEtool::SRA_scope(om, mean_fit = TRUE,
  Chist = catch,
  C_eq = 2 * catch[1],
  Index = cbind(indexes$syn_wcvi),
  I_sd = cbind(indexes$syn_wcvi_sd),
  I_type = c(1),
  s_selectivity = rep("logistic", 1),
  cores = 1,
  drop_nonconv = FALSE,
  vul_par = matrix(c(32, 20, 1), 3, 1),
  s_vul_par = matrix(c(32, 20, 1), 3, 1),
  map_vul_par = matrix(NA, 3, 1),
  map_s_vul_par = matrix(NA, 3, 1),
  f_name = "Commercial trawl",
  s_name = c("SYN WCVI")
)

library(dplyr)

# Get correlation matrix. R0 and F_eq correlation = 0.999
corr <- cov2cor(m@mean_fit$SD$cov.fixed) %>% round(3) %>%
  structure(dimnames = list(names(m@mean_fit$SD$par.fixed), names(m@mean_fit$SD$par.fixed))) %>% View()

SD_full <- TMB::sdreport(m@mean_fit$obj, m@mean_fit$opt$par, getJointPrecision = TRUE) # Get correlation of derived parameters, by default joint precision = FALSE

# Correlation between R0 and q = 0.69 (steepness is fixed so corr = 0)
corr_derived_par <- cov2cor(SD_full$cov) %>% round(3) %>%
  structure(dimnames = list(names(SD_full$value), names(SD_full$value))) %>% View()
