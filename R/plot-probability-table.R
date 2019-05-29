#' Summary of probabilities of things from the MSE object in a colored tile table format
#'
#' @param object object of class MSE
#' @param ... a list of names of PM methods
#' @param refs An optional named list (matching the PM names) with numeric values to
#'  override the default `Ref` values. See examples in summary for MSE object.
#'  @param digits How many decimal places to show in the tiles for the values
#'  @param sort_by the table will be sorted by the first value in pm_list, this is either "increasing" or "decreasing"
#'  @param value_size font size for the values in the tiles
plot_probs <- function(object,
                       ...,
                       refs = NULL,
                       digits = 3,
                       sort_by = "decreasing",
                       value_size = 3.1){
  pm_list <- unlist(list(...))

  if(!length(pm_list)) pm_list <- c("PNOF", "P50", "AAVY", "LTY")
  if(class(pm_list) != 'character') stop("Must provide names of PM methods")

  for(X in seq_along(pm_list)){
    if (!pm_list[X] %in% avail("PM")) stop(pm_list[X], " is not a valid PM method")
  }
  storeMean <- storeName <- storeCap <- storeMP <- list()
  for (X in 1:length(pm_list)) {
    ref <- refs[[pm_list[X]]]
    if (is.null(ref)) {
      runPM <- eval(call(pm_list[X], object))
    } else {
      runPM <- eval(call(pm_list[X], object, Ref = ref))
    }
    storeMean[[X]] <- runPM@Mean
    storeName[[X]] <- runPM@Name
    storeCap[[X]] <- runPM@Caption
    storeMP[[X]] <- runPM@MPs
  }

  df <- data.frame('MP' = storeMP[[1]],
                   signif(do.call('cbind', storeMean),2), stringsAsFactors = FALSE)
  colnames(df)[2:(length(pm_list) + 1)] <- pm_list
  df <- as_tibble(df)
  df[,-1] <- apply(df[,-1], c(1, 2), function(x){
    gfutilities::f(x, digits)
  })

  if(sort_by == "decreasing"){
    df$MP <- factor(df$MP, levels = df$MP[order(df[2] %>% pull())])
  }else if(sort_by == "increasing"){
    df$MP <- factor(df$MP, levels = df$MP[rev(order(df[2] %>% pull()))])
  }else{
    stop("Error - sort_by must be either 'increasing' or 'decreasing'",
         call. = FALSE)
  }

  df <- reshape2::melt(df,
                        id.vars = "MP",
                        variable.name = "type",
                        value.name = "value")

  ## Set up expressions for tick labels
  j <- as.vector(do.call('rbind', storeCap))
  yrs <- stringr::str_extract(j, "\\(Year.*\\)$")
  yrs <- gsub("Years", "Yrs", yrs)
  yrs <- gsub(" - ", "-", yrs)

  probs <- regmatches(j, regexpr("(?<=Prob\\. ).*(?= \\()", j, perl = TRUE))
  probs <- gsub("MSY", "_{MSY}", probs)
  probs <- gsub("%", "\\\\%", probs)
  probs <- paste0("$", probs, "$")
  ## Note that yrs contains years string which is not shown on the plot
  ## All seem to be years 1-50 except LTY which is years 41-50
  #probs <- paste0(probs, "  ", yrs)
  probs <- unlist(lapply(probs, latex2exp::TeX))

  g <- ggplot(df, aes(x = type, y = MP)) +
    geom_tile(aes(fill = value), width = 0.95, height = 0.95) +
    gfplot::theme_pbs() +
    theme(panel.border=element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(face = "bold", size = 10),
          axis.text.y = element_text(face = "bold", size = 10)) +
    scale_colour_viridis_d() +
    guides(fill = FALSE) + xlab("") + ylab("") +
    geom_text(aes(x = type, label = value),
              colour = "black",
              size = value_size, alpha = 1) +
    scale_x_discrete(labels = parse(text = probs), position = "left")

  g
}
