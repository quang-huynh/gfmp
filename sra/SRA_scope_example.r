
# Download SRA branch
devtools::install_github("tcarruth/MSEtool", ref = "SRA")

library(MSEtool)

testOM_2 <- testOM
testOM_2@Linfsd <- testOM_2@Ksd <- testOM_2@Msd <- c(0, 0)

#### Generate some data from testOM and then run them through SRA_scope
Hist <- runMSE(testOM_2, Hist = TRUE)
Data <- Hist@Data


# Scenario 1, full Catch, index, and length comps
ind <- 1
Catch <- Data@Cat[ind, ]
CAL <- Data@CAL[ind, , ]
Index <- Data@Ind[ind, ]

OM_out <- SRA_scope(testOM_2, Chist = Catch, Index = Index, CAL = CAL, length_bin = Data@CAL_bins[1:28] + 3, I_type = "B", cores = 8)

# Scenario 2, full catch, and most recent 10 years of index and length comps
ind <- 1
Catch <- Data@Cat[ind, ]
CAL <- Data@CAL[ind, , ]
CAL[1:40, ] <- NA
Index <- Data@Ind[ind, ]
Index[1:40] <- NA

OM_out <- SRA_scope(testOM_2, Chist = Catch, Index = Index, CAL = CAL, length_bin = Data@CAL_bins[1:28] + 3, I_type = "B", cores = 8)


# Scenario 2a, full catch, and most recent 10 years of index and length comps
ind <- 1
Catch <- Data@Cat[ind, ]
CAL <- Data@CAL[ind, , ]
CAL[1:40, ] <- NA
Index <- Data@Ind[ind, ]
Index[1:40] <- NA

OM_out <- SRA_scope(testOM_2, Chist = Catch, Index = Index, CAL = CAL, length_bin = Data@CAL_bins[1:28] + 3,
                    selectivity = "dome", I_type = "B", cores = 8)


# Scenario 3, no index, only recent 10 years of lengths
ind <- 1
Catch <- Data@Cat[ind, ]
CAL <- Data@CAL[ind, , ]
CAL[1:40, ] <- NA

OM_out <- SRA_scope(testOM_2, Chist = Catch, CAL = CAL, length_bin = Data@CAL_bins[1:28] + 3, I_type = "B", cores = 8)


# Scenario 4, same as 3 but missing the first 10 years of catch, try to estimate initial depletion

ind <- 1
Catch <- Data@Cat[ind, -c(1:10)]
CAL <- Data@CAL[ind, -c(1:10), ]
CAL[1:30, ] <- NA
Index <- Data@Ind[ind, -c(1:10)]
Index[1:30] <- NA

OM <- SRA_scope(testOM_2, Chist = Catch, Index = Index, CAL = CAL, C_eq = 0.5 * Catch[1],
                length_bin = Data@CAL_bins[1:28] + 3, I_type = "B", cores = 8)

