#' Summary of MSE object - PBS-specific version overwrites DLMtool version
#'
#' @param object object of class MSE
#' @param ... a list of names of PM methods
#' @param silent Should summary be printed to console? Logical.
#' @param Refs An optional named list (matching the PM names) with numeric values to override the default `Ref` values. See examples.
#' @rdname summary-MSE
setMethod('summary', signature = "MSE", function(object, ..., Refs = NULL) {
  PMlist <- unlist(list(...))

  if(length(PMlist) == 0) PMlist <- c("PNOF", "P50", "AAVY", "LTY")
  if (class(PMlist) != 'character') stop("Must provide names of PM methods")
  # check
  for (X in seq_along(PMlist)){
    if (!PMlist[X] %in% avail("PM")) stop(PMlist[X], " is not a valid PM method")
  }
  storeMean <- vector('list', length(PMlist))
  storeName <- vector('list', length(PMlist))
  storeCap <- vector('list', length(PMlist))
  storeHeading <- vector('list', length(PMlist))
  storeMP <- vector('list', length(PMlist))
  for (X in 1:length(PMlist)) {
    ref <- Refs[[PMlist[X]]]
    if (is.null(ref)) {
      runPM <- eval(call(PMlist[X], object))
    } else {
      runPM <- eval(call(PMlist[X], object, Ref=ref))
    }
    storeMean[[X]] <- runPM@Mean
    storeName[[X]] <- runPM@Name
    storeCap[[X]] <- runPM@Caption
    storeMP[[X]] <- runPM@MPs
  }

  df <- data.frame('MP' = storeMP[[1]],
                   signif(do.call('cbind', storeMean),2), stringsAsFactors = FALSE)
  colnames(df)[2:(length(PMlist)+1)] <- PMlist #caps # gsub(" ", "", caps)
  df <- as_tibble(df)
  df[,-1] <- apply(df[,-1], c(1, 2), function(x){
    gfutilities::f(x, 2)
  })
  j <- as.vector(do.call('rbind', storeCap))

  #j <- paste0("$", colnames, "$")
  yrs <- stringr::str_extract(j, "\\(Year.*\\)$")
  yrs <- gsub("Years", "Yrs", yrs)
  yrs <- gsub(" - ", "-", yrs)

  probs <- regmatches(j, regexpr("(?<=Prob\\. ).*(?= \\()", j, perl = TRUE))
  probs <- gsub("MSY", "_{MSY}", probs)
  probs <- gsub("%", "\\\\%", probs)
  probs <- paste0("$", probs, "$")
  col.names <- c("Managment Proc.", kableExtra::linebreak(paste0(probs, "\n", yrs),
                                                          align = "c"))
  align <- c("l", rep("c", length(probs)))

  csasdown::csas_table(df,
                       align = align,
                       escape = FALSE,
                       col.names = col.names)
})
