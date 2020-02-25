merge_MSE <- function(..., check_slots = TRUE) {
  dots <- list(...)
  if(length(dots) == 1) dots <- dots[[1]]

  slots_identical <- function(slotname, x = dots, is_logical = FALSE) {
    res <- lapply(x, getElement, slotname)
    # res <- lapply(res, function(x) {row.names(x) <- NULL;x})
    is_identical <- all(vapply(res[-1], identical, logical(1), res[[1]]))
    if(is_logical) {
      return(is_identical)
    } else return(unique(do.call(c, res)))
  }

  slots_identical("Name")
  slots_identical("nyears")
  slots_identical("proyears")
  slots_identical("nsim")

  if (isTRUE(check_slots)) {
  stopifnot(slots_identical("OM", is_logical = TRUE))
  stopifnot(slots_identical("Obs", is_logical = TRUE))
  stopifnot(slots_identical("SSB_hist", is_logical = TRUE))
  stopifnot(slots_identical("CB_hist", is_logical = TRUE))
  stopifnot(slots_identical("FM_hist", is_logical = TRUE))
  }

  nMPs <- vapply(dots, getElement, numeric(1), "nMPs")

  slotvec <- c("B_BMSY", "F_FMSY", "B", "SSB", "VB", "FM", "C", "TAC", "Effort", "PAA", "CAA", "CAL")
  res <- list()
  for(i in 1:length(slotvec)) {
    mm <- c(lapply(dots, getElement, slotvec[i]), along = 2)
    res[[i]] <- do.call(abind::abind, mm)
  }
  names(res) <- slotvec

  Misc <- lapply(dots, slot, "Misc")
  names(Misc[[1]])

  Misc_identical <- function(x) all(vapply(x[-1], identical, logical(1), x[[1]]))

  Data <- do.call(c, lapply(Misc, getElement, "Data"))
  TryMP <- do.call(cbind, lapply(Misc, getElement, "TryMP"))

  Unfished <- lapply(Misc, getElement, "Unfished")
  if (isTRUE(check_slots)) stopifnot(Misc_identical(Unfished))

  MSYRefs <- lapply(Misc, getElement, "MSYRefs")
  if (isTRUE(check_slots)) stopifnot(Misc_identical(Unfished))

  slotvec_Misc <- c("LatEffort", "Revenue", "Cost", "TAE")
  Misc_new <- list(Data = Data, TryMP = TryMP, Unfished = Unfished, MSYRefs = MSYRefs)
  Misc2 <- list()
  for(i in 1:length(slotvec_Misc)) {
    mm <- c(lapply(Misc, getElement, slotvec_Misc[i]), along = 2)
    Misc2[[i]] <- do.call(abind::abind, mm)
  }
  names(Misc2) <- slotvec_Misc

  ## Create MSE Object ####
  MSEout <- new("MSE", Name = slots_identical("Name"), nyears = slots_identical("nyears"),
    proyears = slots_identical("proyears"), nMPs = length(slots_identical("MPs")),
    MPs = slots_identical("MPs"), nsim = slots_identical("nsim"),
    OM = dots[[1]]@OM, Obs = dots[[1]]@Obs, B_BMSY = res$B_BMSY, F_FMSY = res$F_FMSY, B = res$B, SSB = res$SSB,
    VB = res$VB, FM = res$FM, res$C, TAC = res$TAC, SSB_hist = dots[[1]]@SSB_hist, CB_hist = dots[[1]]@CB_hist,
    FM_hist = dots[[1]]@FM_hist, Effort = res$Effort, PAA = res$PAA, CAA = res$CAA, CAL = res$CAL,
    CALbins = slots_identical("CALbins"), Misc = c(Misc_new, Misc2))

  # Store MSE info
  attr(MSEout, "version") <- packageVersion("DLMtool")
  attr(MSEout, "date") <- date()
  attr(MSEout, "R.version") <- R.version

  MSEout
}
