#'Summary of probabilities of performance metrics from the MSE object
#'
#' @param object MSE object, output of the DLMtool runMSE() function
#' @param ... List of performace metrics
#' @param refs List containing the reference limits for each metric
#' @returns List of length 2, first item is a data frame of the output, second item is a list of
#'  captions for the metrics
get_probs <- function(object,
                      ...,
                      refs = NULL){

  if (class(object) != "MSE"){
    stop("object must be class `MSE`",
         call. = FALSE)
  }

  pm_list <- unlist(list(...))
  if(!length(pm_list)){
    warning("No PM's included. Using defaults")
    pm_list <- c("PNOF", "P50", "AAVY", "LTY")
  }
  if(class(pm_list) != 'character'){
    stop("Must provide names of PM methods",
         call. = FALSE)
  }
  for(X in seq_along(pm_list)){
    if (!pm_list[X] %in% avail("PM")){
      stop(pm_list[X], " is not a valid PM method",
           call. = FALSE)
    }
  }

  means <- names <- captions <- mps <- list()
  for (X in 1:length(pm_list)) {
    ref <- refs[[pm_list[X]]]
    if (is.null(ref)) {
      run_pm <- eval(call(pm_list[X], object))
    } else {
      run_pm <- eval(call(pm_list[X], object, Ref = ref))
    }
    means[[X]] <- run_pm@Mean
    names[[X]] <- run_pm@Name
    captions[[X]] <- run_pm@Caption
    mps[[X]] <- run_pm@MPs
  }

  df <- data.frame('MP' = mps[[1]],
                   signif(do.call('cbind', means),2), stringsAsFactors = FALSE)
  colnames(df)[2:(length(pm_list) + 1)] <- pm_list

  list(as_tibble(df), captions)
}

#' Summary of probabilities of things from the MSE object in a colored tile table format
#'
#'  @param probs_dat A list of length 2 - a data frame and another list of captions describing the
#'   columns of the data frame as returned from get_probs()
#'  @param digits How many decimal places to show in the tiles for the values
#'  @param value_size font size for the values in the tiles
plot_probs <- function(probs_dat,
                       digits = 2,
                       value_size = 4.1,
                       relative_max = FALSE,
                       scale_0_1 = FALSE,
                       sort_by = "decreasing"){

  df <- probs_dat[[1]]
  captions <- probs_dat[[2]]

  if(sort_by == "decreasing"){
    df$MP <- factor(df$MP, levels = df$MP[do.call(order, df[-1])])
  }else if(sort_by == "increasing"){
    df$MP <- factor(df$MP, levels = df$MP[rev(do.call(order, df[-1]))])
  }else{
    stop("sort_by must be either 'increasing' or 'decreasing'",
         call. = FALSE)
  }

  df <- reshape2::melt(df,
                        id.vars = "MP",
                        variable.name = "type",
                        value.name = "value")

  ## Set up expressions for tick labels
  j <- as.vector(do.call('rbind', captions))
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
  df$txt <- vapply(df$value, function(x){
    gfutilities::f(x, digits)
  }, FUN.VALUE = character(1L))
  if(relative_max){
    df <- group_by(df, type) %>%
      mutate(value = value / max(value)) %>%
      ungroup()
  }
  if(scale_0_1){
    df <- group_by(df, type) %>%
      mutate(value = (value - min(value)) / (max(value) - min(value))) %>%
      ungroup()
  }

  g <- ggplot(df, aes(x = type, y = MP)) +
    geom_tile(aes(fill = value), color = "white") +
    gfplot::theme_pbs() +
    theme(panel.border=element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(face = "bold", size = 10),
          axis.text.y = element_text(face = "bold", size = 10)) +
    scale_fill_gradient(low = "white", high = "grey50", limits = c(0, 1)) +
    guides(fill = FALSE) + xlab("") + ylab("") +
    geom_text(aes(x = type, label = txt),
              colour = "black",
              size = value_size, alpha = 1) +
    scale_x_discrete(labels = parse(text = probs), position = "left")

  g
}

#' Compare pairs of performance measures a(nd report TRUE if the probabaility
#'  of both performance measures together (logical AND) are greater than lim
#'  for each management procedure
#'
#' @param object MSE object, output of the DLMtool runMSE() function
#' @param pm_list List of performace metric names. Must be even length, with odd ones being compared
#'  to the even ones that follow them in pairs
#' @param refs List containing the reference limits for each metric
#' @param yrs Numeric vector of length 2 with year indices to summarize performance
#' @param lims  A numeric vector of acceptable risk/minimum probability thresholds
#'
#' @returns A data frame of the MP name, name and probability of x and y performance metrics,
#'  and pass/fail
trade_off <- function(object,
                      pm_list = NULL,
                      refs = NULL,
                      yrs = NULL,
                      lims = NULL){

  if(is.null(lims) | is.null(pm_list)){
    stop("Both pm_list and lims are required arguments.",
         call. = FALSE)
  }

  if(length(pm_list) %% 2){
    stop("pm_list must have an even length.",
         call. = FALSE)
  }

  if(length(lims) != length(pm_list) / 2){
    stop("lims must be half the length of pm_list.",
         call. = FALSE)
  }

  run_pm <- list()
  for(i in seq_along(pm_list)){
    ref <- refs[[pm_list[i]]]
    yr <- yrs[pm_list[i]]
    if(is.null(ref)){
      if(is.null(yr)){
        run_pm[[i]] <- eval(call(pm_list[[i]], object))
      }else{
        run_pm[[i]] <- eval(call(pm_list[[i]], object, Yrs = yr))
      }
    }else{
      if(is.null(yr)){
        run_pm[[i]] <- eval(call(pm_list[[i]], object, Ref = ref))
      }else{
        run_pm[[i]] <- eval(call(pm_list[[i]], object, Ref = ref, Yrs = yr))
      }
    }
  }

  xind <- seq(1, by = 2, length.out = length(pm_list) / 2)
  yind <- xind + 1
  out <- list()
  for(i in seq_len(length(pm_list) / 2)){
    xpm <- pm_list[[xind[i]]]
    xvals <- run_pm[[match(xpm, pm_list)]]@Mean
    xcap <-  run_pm[[match(xpm, pm_list)]]@Caption
    xname <-  run_pm[[match(xpm, pm_list)]]@Name
    #xline <- lims[match(xpm, pm_list)]
    xline <- lims[i]

    ypm <- pm_list[[yind[i]]]
    yvals <- run_pm[[match(ypm, pm_list)]]@Mean
    ycap <-  run_pm[[match(ypm, pm_list)]]@Caption
    yname <-  run_pm[[match(ypm, pm_list)]]@Name
    #yline <- lims[match(ypm, pm_list)]
    yline <- lims[i]

    out[[i]] <- as_tibble(data.frame(name = pm_list[[xind[i]]],
                                     x = xvals,
                                     y = yvals,
                                     pass = xvals > xline & yvals > yline,
                                     xpm = xpm,
                                     ypm = ypm))

  }
  do.call(rbind, out)
}
