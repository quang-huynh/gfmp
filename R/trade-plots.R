#' Radar plot for comparing management procedures and performance metrics
#'
#' @param fill_polys Fill the polygons with color?
#' @param fill_alpha Alpha to use if fill = TRUE.
#' @param ref_levels A vector of values between 0 and 1 to draw dotted lines for
#' @param inc_yrs Include years in the performance metrics labels?
#' @param df is the data frame output by [pm_pass()]
r_plot <- function(df,
                   fill_polys = FALSE,
                   fill_alpha = 0.05,
                   ref_levels = c(0.5, 0.975),
                   inc_yrs = TRUE){

  prob_labs <- cap_expr(as.character(unique(df$probcap)), inc_yrs)
  g <- ggplot(data = df,
              aes(x = pm,
                  y = prob,
                  group = mp,
                  color = mp,
                  fill = mp,
                  shape = class)) +
    geom_point(size = 5) +
    #scale_x_discrete(labels = parse(text = prob_labs)) +
    theme(panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          size = 0.5,
                                          linetype = "solid"),
          panel.grid.major = element_line(size = 0.5,
                                          linetype = "solid",
                                          colour = "grey40"),
          panel.grid.minor = element_line(size = 0.25,
                                          linetype = 'solid',
                                          colour = "grey40")) +
          #axis.text.x = element_text(size = 12, angle = c(0, 90, 0, 0, -90, 0))) +
    ggrepel::geom_text_repel(data = df, label = parse(text = prob_labs)) +
  ylim(0, 1) +
    xlab("") +
    ylab("") +
    coord_polar(clip = "off") +
    geom_hline(yintercept = ref_levels, lwd = 1, lty = 2)

    if(fill_polys){
      g <- g + geom_polygon(aes(fill = mp), alpha = fill_alpha)
    }else{
      g <- g + geom_polygon(fill = NA)
    }
  g
}

t_plot <- function(df,
                   label_size = 4,
                   Title = NULL,
                   Labels = NULL,
                   Satisficed = FALSE,
                   Show = 'both',
                   point.size = 2,
                   lab.size = 4,
                   axis.title.size = 12,
                   axis.text.size = 10,
                   legend.title.size = 12,
                   position = c("right", "bottom"),
                   cols = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"),
                   fill = "gray80",
                   alpha = 0.4,
                   PMlist = NULL,
                   Refs = NULL,
                   Yrs = NULL){

  xcap <- cap_expr(as.character(unique(df$xcap)), inc_yrs = TRUE)
  ycap <- cap_expr(as.character(unique(df$ycap)), inc_yrs = TRUE)
  num_id <- length(unique(df$id))
  if(length(ycap) < num_id){
    ycap <- rep(ycap[1], num_id)
  }

  plot_list <- list()
  for(i in seq_along(unique(df$id))){
    d <- df %>% filter(id == i)
    plot_list[[i]] <- ggplot(d,
                             aes(x,
                                 y,
                                 color = class,
                                 shape = class)) +
      geom_point() +
      scale_color_discrete(name = "MP type") +
      scale_shape_discrete(name = "MP type") +
      scale_x_continuous(limits = c(0, 1)) +
      scale_y_continuous(limits = c(0, 1)) +
      gfplot::theme_pbs() +
      xlab(parse(text = xcap[i])) +
      ylab(parse(text = ycap[i])) +
      ggrepel::geom_text_repel(data = d,
                               aes(x,
                                   y,
                                   color = class,
                                   label = name),
                               size = label_size,
                               na.rm = TRUE)
    if(i == 1){
      ## Extract legend to apply later as shared legend
      legend <- cowplot::get_legend(plot_list[[i]])
    }
    plot_list[[i]] <- plot_list[[i]] +
      theme(legend.position = "none")
  }
  pgrid <- cowplot::plot_grid(plotlist = plot_list)
  cowplot::plot_grid(pgrid, legend, ncol = 2, rel_widths = c(1, .1))
}

#' Pass/fail for performance measures based on their probabaility being
#'  greater than some limit for each management procedure
#'
#' @param object MSE object, output of the DLMtool [DLMtool::runMSE()] function
#' @param pm_list List of performace metric names
#' @param refs Optional. List containing the reference limits for each metric
#' @param yrs Numeric vector of length 2 with year indices to summarize performance
#' @param lims  A numeric vector of acceptable risk/minimum probability thresholds
#'
#' @returns A data frame of the MP name, name and probability of performance metrics,
#'  and pass/fail
pm_pass <- function(object,
                    pm_list = NULL,
                    refs = NULL,
                    yrs = NULL,
                    lims = NULL){

  if(is.null(lims) | is.null(pm_list)){
    stop("Both pm_list and lims are required arguments.",
         call. = FALSE)
  }

  if(length(lims) != length(pm_list)){
    stop("lims must be the length of pm_list.",
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

  out <- list()
  for(i in seq_along(pm_list)){
    pm <- pm_list[[i]]
    prob <- run_pm[[match(pm, pm_list)]]@Mean
    probcap <- run_pm[[match(pm, pm_list)]]@Caption
    name <- run_pm[[match(pm, pm_list)]]@Name
    line <- lims[i]

    mp_type <- MPtype(object@MPs)
    class <- mp_type[match(object@MPs, mp_type[,1]), 2]

    out[[i]] <- as_tibble(data.frame(id = i,
                                     mp = object@MPs,
                                     pm = pm,
                                     prob = prob,
                                     probcap = probcap,
                                     english = name,
                                     class = class,
                                     pass = prob > line))

  }
  do.call(rbind, out)
}
