make_table_plot <- function(scenario, ...) {
  p <- DLMtool::Sub(rex_mse[[scenario]], MPs = rex_satisficed_ref) %>%
    gfdlm::get_probs(PM)
  gfdlm::plot_probs(p, ...)
}

make_projection_plot <- function(scenario, MPs, mptype, height = 9.5,
  catch_breaks = NULL, catch_labels = NULL) {
  x <- DLMtool::Sub(rex_mse[[scenario]], MPs = MPs)
  g1 <- gfdlm::plot_projection_ts(x, type = c("SSB", "FM")) +
    coord_cartesian(expand = FALSE, ylim = c(0, 4.5)) +
    scale_y_continuous(breaks = c(1, 2, 3, 4)) +
    theme(strip.text.y = element_blank())

  g2 <- gfdlm::plot_projection_ts(x,
    type = "C", clip_ylim = 1.3,
    catch_reference = 1
  ) + theme(axis.title.y = element_blank())

  if (!is.null(catch_breaks) && !is.null(catch_labels)) {
    g2 <- g2 +
      scale_y_continuous(breaks = catch_breaks, labels = catch_labels)
  }

  g <- cowplot::plot_grid(g1, g2, rel_widths = c(2, 1.18), align = "h")
  ggsave(file.path(
    fig_dir,
    paste0("rex-projections-", mptype, "-", scenario, ".png")
  ),
    width = 8, height = height
  )
}

make_kobe_plot <- function(scenario, MPs, mptype, ...) {
  x <- DLMtool::Sub(rex_mse[[scenario]], MPs = MPs)
  g <- gfdlm::plot_contours(x,
    xlim = c(0, 3.5),
    ylim = c(0, 3.5), alpha = c(0.1, 0.25, 0.5, 0.75), ...
  )
  ggsave(file.path(
    fig_dir,
    paste0("rex-kobe-", mptype, "-", scenario, ".png")
  ),
    width = 8, height = 7.5
  )
}

make_spider <- function(scenario, MPs, mptype, save_plot = TRUE,
  custom_pal = NULL, legend = TRUE) {
  g <- DLMtool::Sub(rex_mse[[scenario]], MPs = MPs) %>%
    gfdlm::spider(pm_list = PM, palette = "Set2")
  if (length(MPs) > 8)
    g <- g + scale_color_viridis_d()

  if (!is.null(custom_pal)) {
    g <- g + scale_color_manual(values = custom_pal)
  }
  if (save_plot) {
    ggsave(file.path(
      fig_dir,
      paste0("rex-spider-", mptype, "-", scenario, ".png")
    ),
      width = 6, height = 6
    )
  }
  if (!legend) {
    g <- g + guides(colour = FALSE)
  }
  g
}

plot_grid_pbs <- function(plotlist, align = "hv",
  label_fontface = "bold", label_size = 12,
  hjust = 0, spider_margins = FALSE, ...) {
  out <- cowplot::plot_grid(
    plotlist = plotlist, align = align,
    label_fontface = label_fontface, hjust = hjust, label_size = label_size, ...
  )
  if (spider_margins)
    out <- out + theme(plot.margin = unit(c(0.2, 0.2, -0.7, 1.0), "lines"))
  out
}

spider_base <- function(x, ...) {
  x <- x %>%
    reshape2::melt(id.vars = "MP",
      value.name = "prob", variable.name = "pm")
  ggspider::spider_web(x,
    "MP",
    "pm",
    "prob",
    leg_main_title = "MP",
    leg_lty_title = "MP type",
    palette = "Set2",
    ...
  ) + ggplot2::labs(color = "MP")
}

make_neon_worm_plot <- function(scenario, MPs, mptype, type = c("SSB", "FM"), this_year = 2018,
  probs = c(0.2, 0.2)){
  ts <- DLMtool::Sub(rex_mse[[scenario]], MPs = MPs) %>%
    gfdlm:::get_ts(type = c("SSB", "FM"), this_year = 2018)
  ts_quantiles <- gfdlm:::get_ts_quantiles(ts, probs = c(0.2, 0.2))
  d <- filter(ts_quantiles, real_year >= 2019)

  now <- filter(ts_quantiles, real_year == 2018)

  m <- reshape2::dcast(d, mp_name + real_year ~ Type, value.var = "m") %>%
    rename(b_m = B_BMSY, f_m = F_FMSY)
  l <- reshape2::dcast(d, mp_name + real_year ~ Type, value.var = "l") %>%
    rename(b_l = B_BMSY, f_l = F_FMSY)
  u <- reshape2::dcast(d, mp_name + real_year ~ Type, value.var = "u") %>%
    rename(b_u = B_BMSY, f_u = F_FMSY)
  dd <- left_join(m, l, by = c("mp_name", "real_year")) %>%
    left_join(u, by = c("mp_name", "real_year"))

  poly_df <- split(dd, paste(dd$mp_name, dd$real_year)) %>%
    map_df(~ data.frame(
      x = c(.$b_m, .$b_l, .$b_m, .$b_u, .$b_m),
      y = c(.$f_l, .$f_m, .$f_u, .$f_m, .$f_l),
      real_year = unique(.$real_year),
      mp_name = unique(.$mp_name), stringsAsFactors = FALSE)
    )

  now_m <- reshape2::dcast(now, mp_name + real_year ~ Type, value.var = "m") %>%
    rename(b_m = B_BMSY, f_m = F_FMSY)
  end <- filter(ts_quantiles, real_year == max(ts_quantiles$real_year))
  end_m <- reshape2::dcast(end, mp_name + real_year ~ Type, value.var = "m") %>%
    rename(b_m = B_BMSY, f_m = F_FMSY)
  start <- filter(ts_quantiles, real_year == min(ts_quantiles$real_year))
  start_m <- reshape2::dcast(start, mp_name + real_year ~ Type, value.var = "m") %>%
    rename(b_m = B_BMSY, f_m = F_FMSY)
  other <- bind_rows(now_m, end_m) %>%
    bind_rows(start_m)

  g <- dd %>%
    mutate(b_rad = abs(b_u - b_l)/2) %>%
    mutate(f_rad = abs(f_u - f_l)/2) %>%
    ggplot(aes(b_m, f_m, colour = real_year)) +
    geom_polygon(aes(x = x, y = y, fill = real_year, group = real_year),
      data = poly_df, alpha = 0.2, inherit.aes = FALSE, colour = NA) +
    geom_path(lwd = 1.6, lineend = "round", linejoin = "bevel", colour = "white") +
    geom_path(lwd = 1.0, lineend = "round", linejoin = "bevel") +
    scale_color_viridis_c(option = "C", direction = -1) +
    scale_fill_viridis_c(option = "C", direction = -1) +
    gfplot::theme_pbs() + facet_wrap(~mp_name) +
    coord_fixed(xlim = c(0, 3), ylim = c(0, 3)) +
    geom_vline(xintercept = c(0.4, 0.8), lty = 2, alpha = 0.2, lwd = 0.5) +
    geom_hline(yintercept = 1, lty = 2, alpha = 0.2, lwd = 0.5) +
    labs(fill = "Year", colour = "Year", x = expression(B/B[MSY]), y = expression(F/F[MSY]), pch = "Year") +
    geom_point(data = other, mapping = aes(x = b_m, y = f_m, pch = as.factor(real_year)), inherit.aes = FALSE, col = "white", stroke = 1.6) +
    geom_point(data = other, mapping = aes(x = b_m, y = f_m, pch = as.factor(real_year)), inherit.aes = FALSE, col = "black", stroke = 1) +
    scale_shape_manual(values = c(2, 4, 21))

  ggsave(file.path(fig_dir, paste0("rex-neon-worms-", mptype, "-", scenario, ".png")), width = 8, height = 6.6)
}

