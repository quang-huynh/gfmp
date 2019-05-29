plot_contours <- function(object,
                          yend = max(object@proyears),
                          dontshow_mp = NULL,
                          show_ref_pt_lines = FALSE,
                          bins = 5){

  ffmsy <- short_mse@F_FMSY[,,yend]
  bbmsy <- short_mse@B_BMSY[,,yend]
  ffmsy <- reshape2::melt(ffmsy) %>%
    dplyr::rename(iter = Var1, mp = Var2, ffmsy = value)
  bbmsy <- reshape2::melt(bbmsy) %>%
    dplyr::rename(iter = Var1, mp = Var2, bbmsy = value)
  d <- dplyr::inner_join(ffmsy, bbmsy)
  dn <- data.frame(mp = seq_along(short_mse@MPs), mp_name = short_mse@MPs)
  d <- dplyr::left_join(d, dn) %>%
    dplyr::filter(!mp_name %in% dontshow_mp)
  g <- ggplot(d, aes(bbmsy, ffmsy)) +
    geom_density_2d(aes(colour = ..level..), bins = bins) +
    # Cannot use breaks in geom_density_2d - Using breaks like in the following has no effect
    #stat_density2d(aes(colour = ..level..), breaks = c(.025, .5, .975), bins = bins, geom = "contour") +
    # Doing the following gives different contours than geom_density_2d but the different breaks produce the same results
    #stat_density2d(aes(colour = ..level..), bins = bins, geom = "contour") +
    #scale_alpha_continuous(breaks = c(.025, .5, .975)) +
    #scale_alpha_continuous(breaks = c(.25, .5, .75)) +
    viridis::scale_colour_viridis() +
    ggsidekick::theme_sleek() +
    facet_wrap(~mp_name) +
    ylim(0, 3.5) + xlim(0, 3.5) +
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
