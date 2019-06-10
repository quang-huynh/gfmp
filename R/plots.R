calc_contour_lines <- function(d,
                               probs = c(0.025, 0.5, 0.975),
                               resolution = 100){
  d <- split(d, d$mp)
  lapply(probs, function(j){
    lapply(seq_along(d), function(i){
      dens <- MASS::kde2d(d[[i]]$bbmsy,
                          d[[i]]$ffmsy,
                          n = resolution,
                          lims = c(range(d[[i]]$bbmsy),
                                   range(d[[i]]$ffmsy)))
      lst <- contourLines(dens$x, dens$y, dens$z, levels = (max(dens$z) - min(dens$z)) * j)[[1]]
      lst$mp <- rep(i, length(lst$x))
      lst
    })  %>%
      purrr::map_dfr(`[`, c("mp", "x", "y")) %>%
      mutate(prob = j)
  }) %>%
    purrr::map_df(rbind)
}

plot_contours <- function(object,
                          yend = max(object@proyears),
                          dontshow_mp = NULL,
                          show_ref_pt_lines = FALSE,
                          probs = c(0.025, 0.5, 0.975),
                          resolution = 100){

  ffmsy <- object@F_FMSY[,,yend]
  bbmsy <- object@B_BMSY[,,yend]
  ffmsy <- reshape2::melt(ffmsy) %>%
    dplyr::rename(iter = Var1, mp = Var2, ffmsy = value)
  bbmsy <- reshape2::melt(bbmsy) %>%
    dplyr::rename(iter = Var1, mp = Var2, bbmsy = value)
  d <- dplyr::inner_join(ffmsy, bbmsy)
  dn <- data.frame(mp = seq_along(object@MPs), mp_name = object@MPs)
  d <- dplyr::left_join(d, dn) %>%
    dplyr::filter(!mp_name %in% dontshow_mp)

  contour_lines <- calc_contour_lines(d,
                                      probs = probs,
                                      resolution = resolution)

  g <- ggplot(d, aes(bbmsy, ffmsy)) +
    geom_path(data = contour_lines, aes(x, y, group = prob)) +
    viridis::scale_colour_viridis() +
    ggsidekick::theme_sleek() +
    facet_wrap(~mp) +
    scale_x_continuous(trans = "sqrt") +
    scale_y_continuous(trans = "sqrt") +
    #ylim(0, 3.5) + xlim(0, 3.5) +
    geom_point(alpha = 0.2) +
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
