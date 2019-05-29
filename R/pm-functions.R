# PBS-specific Performance Metric functions

#' @rdname PerformanceMetric
#' @export
Punder80 <- function(MSEobj=NULL, Ref=0.8, Yrs=NULL) {
  Yrs <- ChkYrs(Yrs, MSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "Spawning Biomass relative to SBMSY"

  if (Ref !=1) {
    PMobj@Caption <- paste0('Prob. SB < ', Ref, ' SBMSY (Years ', Yrs[1], ' - ', Yrs[2], ')')
  } else {
    PMobj@Caption <- paste0('Prob. SB < SBMSY (Years ', Yrs[1], ' - ', Yrs[2], ')')
  }

  PMobj@Ref <- Ref
  PMobj@Stat <- MSEobj@B_BMSY[,,Yrs[1]:Yrs[2]] # Performance Metric statistic of interest - here SB/SBMSY
  PMobj@Prob <- calcProb(PMobj@Stat < PMobj@Ref, MSEobj) # calculate probability Stat < 0.8 nsim by nMP

  PMobj@Mean <- calcMean(PMobj@Prob) # calculate mean probability by MP
  PMobj@MPs <- MSEobj@MPs
  PMobj

}
class(Punder80) <- "PM"


#' @rdname PerformanceMetric
#' @export
P40 <- P10
formals(P40)$Ref <- 0.4
class(P40) <- "PM"

#' @rdname PerformanceMetric
#' @export
P80 <- P10
formals(P80)$Ref <- 0.8
class(P80) <- "PM"

