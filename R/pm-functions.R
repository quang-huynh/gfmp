# Performance Metrics

pm_factory <- function(pm_type,
                       ref = 0.1,
                       yrs = NULL){
  force(pm_type)
  force(ref)
  force(yrs)
  pm_obj <- new("PMobj")
  pm_obj@Ref <- ref
  if(pm_type == "SBMSY"){
    tmp <- function(mse_obj){
      yrs <- ChkYrs(yrs, mse_obj)
      pm_obj@Name <- "Spawning Biomass relative to SBMSY"
      pm_obj@Caption <- paste0("Prob. SB > ", ifelse(ref == 1, "", ref), " SBMSY (Years ", yrs[1], " - ", yrs[2], ")")
      pm_obj@Stat <- mse_obj@B_BMSY[ , , yrs[1]:yrs[2]]
      pm_obj@Prob <- calcProb(pm_obj@Stat > pm_obj@Ref, mse_obj)
      pm_obj@Mean <- calcMean(pm_obj@Prob)
      pm_obj@MPs <- mse_obj@MPs
      pm_obj
    }
  }else if(pm_type == "AAVY"){
    tmp <- function(mse_obj){
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
    tmp <- function(mse_obj){
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
    tmp <- function(mse_obj){
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
    tmp <- function(mse_obj){
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
  class(tmp) <- "PM"
  tmp
}

P10 <- pm_factory("SBMSY", 0.1)
P40 <- pm_factory("SBMSY", 0.4)
P100 <- pm_factory("SBMSY", 1)
PNOF <- pm_factory("PNOF", 1)
LTY <- pm_factory("LTY", 0.5)
Yield <- pm_factory("Yield", 1)
AAVY <- pm_factory("AAVY", 0.2)

