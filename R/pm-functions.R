# Performance Metrics

#' Function factory for creating DLMtool Performace Metrics (class "PM") functions
#'
#' @param pm_type The type of performance metric
#' @param ref Reference level to use in secondary part of probability
#' @param yrs A vector of years to include. If NULL, all will be used.
#'
#' @return A DLMtool PM function
#' @export
#'
#' @examples
#' P10 <- pm_factory("SBMSY", 0.1)
pm_factory <- function(pm_type,
                       ref = 0.1,
                       yrs = NULL){
  force(pm_type)
  force(ref)
  force(yrs)
  pm_obj <- new("PMobj")
  pm_obj@Ref <- ref
  if(pm_type == "SBMSY"){
    created_by_pm_factory <- function(mse_obj){
      yrs <- ChkYrs(yrs, mse_obj)
      pm_obj@Name <- "Spawning Biomass relative to SBMSY"
      pm_obj@Caption <- paste0("Prob. SB > ", ifelse(ref == 1, "", ref), " SBMSY (Years ", yrs[1], " - ", yrs[2], ")")
      pm_obj@Stat <- mse_obj@B_BMSY[ , , yrs[1]:yrs[2]]
      pm_obj@Prob <- calcProb(pm_obj@Stat > pm_obj@Ref, mse_obj)
      pm_obj@Mean <- calcMean(pm_obj@Prob)
      pm_obj@MPs <- mse_obj@MPs
      browser()
      pm_obj
    }
  }else if(pm_type == "AAVY"){
    created_by_pm_factory <- function(mse_obj){
      yrs <- ChkYrs(yrs, mse_obj)
      pm_obj@Name <-  paste0("Average Annual Variability in Yield (Years ", yrs[1], "-", yrs[2], ")")
      pm_obj@Caption <-  paste0('Prob. AAVY < ', ref * 100, "% (Years ", yrs[1], "-", yrs[2], ")")
      y1 <- yrs[1]:(yrs[2] - 1)
      y2 <-(yrs[1] + 1):yrs[2]
      if(mse_obj@nMPs > 1){
        pm_obj@Stat <-  apply(((((mse_obj@C[, , y1] - mse_obj@C[, , y2]) /
                                  mse_obj@C[, , y2]) ^ 2) ^ 0.5),
                              c(1, 2),
                              mean)
      }else{
        pm_obj@Stat <- array(apply(((((mse_obj@C[, 1, y1] - mse_obj@C[, 1, y2]) /
                                        mse_obj@C[, 1, y2]) ^ 2) ^ 0.5), c(1), mean))
      }
      pm_obj@Prob <- calcProb(pm_obj@Stat < pm_obj@Ref, mse_obj)
      pm_obj@Mean <- calcMean(pm_obj@Prob)
      pm_obj@MPs <- mse_obj@MPs
      pm_obj
    }
  }else if(pm_type == "PNOF"){
    created_by_pm_factory <- function(mse_obj){
      yrs <- ChkYrs(yrs, mse_obj)
      pm_obj@Name <- "Probability of not overfishing (F<FMSY)"
      pm_obj@Caption <- paste0("Prob. F < ", ifelse(ref == 1, "", ref), " FMSY (Years ", yrs[1], " - ", yrs[2], ")")
      pm_obj@Stat <- mse_obj@F_FMSY[ , , yrs[1]:yrs[2]]
      pm_obj@Prob <- calcProb(pm_obj@Stat < pm_obj@Ref, mse_obj)
      pm_obj@Mean <- calcMean(pm_obj@Prob)
      pm_obj@MPs <- mse_obj@MPs
      pm_obj
    }
  }else if(pm_type == "LTY"){
    created_by_pm_factory <- function(mse_obj){
      yrs <- ChkYrs(yrs, mse_obj)
      pm_obj@Name <- paste0("Average Yield relative to Reference Yield (Years ", yrs[1], "-", yrs[2], ")")
      pm_obj@Caption <- paste0("Prob. Yield > ", ifelse(ref == 1, "", ref), " Ref. Yield (Years ", yrs[1], " - ", yrs[2], ")")
      ref_yield <- array(mse_obj@OM$RefY, dim = dim(mse_obj@C[, , yrs[1]:yrs[2]]))
      pm_obj@Stat <- mse_obj@C[, , yrs[1]:yrs[2]] / ref_yield
      pm_obj@Prob <- calcProb(pm_obj@Stat > pm_obj@Ref, mse_obj)
      pm_obj@Mean <- calcMean(pm_obj@Prob)
      pm_obj@MPs <- mse_obj@MPs
      pm_obj
    }
  }else if(pm_type == "Yield"){
    created_by_pm_factory <- function(mse_obj){
      yrs <- ChkYrs(yrs, mse_obj)
      pm_obj@Name <- paste0("Average Yield relative to Reference Yield (Years ", yrs[1], "-", yrs[2], ")")
      pm_obj@Caption <- paste0("Prob. Yield > ", ifelse(ref == 1, "", ref), " Ref. Yield (Years ", yrs[1], " - ", yrs[2], ")")
      ref_yield <- array(mse_obj@OM$RefY, dim = dim(mse_obj@C[, , yrs[1]:yrs[2]]))
      pm_obj@Stat <- mse_obj@C[, , yrs[1]:yrs[2]] / ref_yield
      pm_obj@Prob <- calcProb(pm_obj@Stat > pm_obj@Ref, mse_obj)
      pm_obj@Mean <- calcMean(pm_obj@Prob)
      pm_obj@MPs <- mse_obj@MPs
      pm_obj
    }
  }
  class(created_by_pm_factory) <- "PM"
  created_by_pm_factory
}

P10 <- pm_factory("SBMSY", 0.1)
P10_yrs6_20 <- pm_factory("SBMSY", 0.1, c(6, 20))
P10_yrs21_35 <- pm_factory("SBMSY", 0.1, c(21, 35))
P10_yrs36_50 <- pm_factory("SBMSY", 0.1, c(36, 50))
P40 <- pm_factory("SBMSY", 0.4)
P40_yrs6_20 <- pm_factory("SBMSY", 0.4, c(6, 20))
P40_yrs21_35 <- pm_factory("SBMSY", 0.4, c(21, 35))
P40_yrs36_50 <- pm_factory("SBMSY", 0.4, c(36, 50))
P100 <- pm_factory("SBMSY", 1)
P100_yrs6_20 <- pm_factory("SBMSY", 1, c(6, 20))
P100_yrs21_35 <- pm_factory("SBMSY", 1, c(21, 35))
P100_yrs36_50 <- pm_factory("SBMSY", 1, c(36, 50))
PNOF <- pm_factory("PNOF", 1)
PNOF_yrs6_20 <- pm_factory("PNOF", 1, c(6, 20))
PNOF_yrs21_35 <- pm_factory("PNOF", 1, c(21, 35))
PNOF_yrs36_50 <- pm_factory("PNOF", 1, c(36, 50))
LTY <- pm_factory("LTY", 0.5)
Yield <- pm_factory("Yield", 1)
AAVY <- pm_factory("AAVY", 0.2)
