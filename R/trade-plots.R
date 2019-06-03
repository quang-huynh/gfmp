#' Radar plot for management procedures and performance metrics
r_plot <- function(df,
                   fill_polys = FALSE,
                   ref_levels = c(0.5, 0.975)){
  df <- df %>%
    mutate(id = xcap) %>%
    select(id, name, x, y)

browser()
  g <- ggplot(data = df,
              aes(x = id,
                  y = x,
                  group = name,
                  color = name,
                  fill = name)) +
    geom_point(size = 5) +
    #theme(#panel.border = element_blank(),
          #panel.background = element_blank(),
          #axis.line = element_line(colour = "grey"),
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          #axis.title.x = element_blank(),
          #axis.title.y = element_blank())
    ylim(0, 1) +
    xlab("") +
    ylab("") +
    coord_polar() +
    #theme_bw() +
    geom_hline(yintercept = ref_levels, lwd = 1, lty = 2)

    if(fill_polys){
      g <- g + geom_polygon(aes(fill = name), alpha = 0.1)
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
    xline <- lims[i]

    ypm <- pm_list[[yind[i]]]
    yvals <- run_pm[[match(ypm, pm_list)]]@Mean
    ycap <-  run_pm[[match(ypm, pm_list)]]@Caption
    yname <-  run_pm[[match(ypm, pm_list)]]@Name
    yline <- lims[i]

    mp_type <- MPtype(object@MPs)
    class <- mp_type[match(object@MPs, mp_type[,1]),2]

    out[[i]] <- as_tibble(data.frame(id = i,
                                     name = object@MPs,
                                     x = xvals,
                                     y = yvals,
                                     xcap = xcap,
                                     ycap = ycap,
                                     xname = xname,
                                     yname = yname,
                                     class = class,
                                     pass = xvals > xline & yvals > yline,
                                     xpm = xpm,
                                     ypm = ypm))

  }
  do.call(rbind, out)
}
