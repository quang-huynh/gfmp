quantile_contour <- function(x, alpha = 0.8){
  zdens <- rev(sort(x$z))
  cumu_zdens <- cumsum(zdens)
  cumu_zdens <- (cumu_zdens / cumu_zdens[length(zdens)])
  crit.val <- zdens[max(which(cumu_zdens <= alpha))]
  b.full <- contourLines(x, levels = crit.val)
  list(x = b.full[[1]]$x, y = b.full[[1]]$y)
}

calc_contour_lines <- function(d,
                               alpha = c(0.025, 0.5, 0.975),
                               n = 200){
  d <- split(d, d$mp)
  lapply(alpha, function(j){
    lapply(seq_along(d), function(i){
      x <- d[[i]]$x
      y <- d[[i]]$y
      x_bw <- MASS::bandwidth.nrd(na.omit(x))
      y_bw <- MASS::bandwidth.nrd(na.omit(y))
      dens <- quantile_contour(MASS::kde2d(x,
                                           y,
                                           h = c(x_bw, y_bw),
                                           n = n),
                                   alpha = j)
      dens$alpha <- rep(j, length(dens$x))
      dens$mp <- rep(i, length(dens$x))
      dens
    }) %>%
      purrr::map_dfr(`[`, c("mp", "x", "y", "alpha"))
  }) %>%
    purrr::map_df(rbind)
}

plot_contours <- function(object,
                          yend = max(object@proyears),
                          dontshow_mp = NULL,
                          show_ref_pt_lines = FALSE,
                          alpha = c(0.2, 0.4, 0.6, 0.8),
                          n = 200){

  ffmsy <- object@F_FMSY[,,yend] %>%
    reshape2::melt() %>%
    rename(iter = Var1, mp = Var2, ffmsy = value)
  bbmsy <- object@B_BMSY[,,yend] %>%
    reshape2::melt() %>%
    rename(iter = Var1, mp = Var2, bbmsy = value)
  dl <- inner_join(ffmsy, bbmsy)
  dr <- data.frame(mp = seq_along(object@MPs), mp_name = object@MPs)
  d <- left_join(dl, dr) %>%
    filter(!mp_name %in% dontshow_mp)%>%
    select(-iter) %>%
    rename(x = bbmsy,
           y = ffmsy)

  mp_names <- d %>%
    distinct(mp_name) %>%
    rownames_to_column(var = "mp") %>%
    mutate(mp = as.integer(mp))

  contour_lines <- calc_contour_lines(d,
                                      alpha = alpha,
                                      n = n) %>%
    purrr::map_df(~.x) %>%
    left_join(mp_names)

  g <- ggplot(d, aes(x, y)) +
    geom_point(alpha = 0.2) +
    geom_path(data = contour_lines,
                 aes(color = alpha,
                     group = as.factor(alpha)),
                 alpha = 0.5) +
                 #fill = NA) +
    scale_color_viridis_c(end = 0.9) +
    ggsidekick::theme_sleek() +
    facet_wrap(~mp_name) +
    #scale_x_continuous(trans = "sqrt") +
    #scale_y_continuous(trans = "sqrt") +
    ylim(0, 3.5) +
    xlim(0, 3.5) +
    labs(colour = "Prob. density", x = expression(B/B[MSY]),
         y = expression(F/F[MSY])) +
    guides(colour = FALSE)

  if(show_ref_pt_lines){
    g <- g +
      geom_vline(xintercept = c(0.4, 0.8), alpha = 0.2) +
      geom_hline(yintercept = 1, alpha = 0.2)
  }
  g
}
