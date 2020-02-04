make_table_plot <- function(object, pm, ...) {
  p <- object %>% gfdlm::get_probs(pm)
  gfdlm::plot_probs(p, ...)
}

make_projection_plot <- function(scenario, MPs, mptype, height = 9.5,
                                 catch_breaks = NULL, catch_labels = NULL) {
  x <- DLMtool::Sub(mse[[scenario]], MPs = MPs)
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
  g
}
make_projection_plot2 <- function(x, scenario, mptype, height = 9.5,
                                  catch_breaks = NULL, catch_labels = NULL) {
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
  g
}

make_kobe_plot <- function(scenario, MPs, mptype, ...) {
  x <- DLMtool::Sub(mse[[scenario]], MPs = MPs)
  g <- gfdlm::plot_kobe(x, ...)
  ggsave(file.path(
    fig_dir,
    paste0("rex-kobe-", mptype, "-", scenario, ".png")
  ),
  width = 8, height = 7.5
  )
}

# FIXME: update for new function:
make_spider <- function(scenario, MPs, mptype, save_plot = TRUE,
                        custom_pal = NULL, legend = TRUE) {
  g <- DLMtool::Sub(mse[[scenario]], MPs = MPs) %>%
    gfdlm::plot_spider(pm_list = PM, palette = "Set2")
  if (length(MPs) > 8) {
    g <- g + scale_color_viridis_d()
  }

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
  if (spider_margins) {
    out <- out + theme(plot.margin = unit(c(0.2, 0.2, -0.7, 1.0), "lines"))
  }
  out
}

spider_base <- function(x, ...) {
  x <- x %>%
    reshape2::melt(
      id.vars = "MP",
      value.name = "prob", variable.name = "pm"
    )
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

make_neon_worm_plot <- function(scenario, object, MPs, this_year, prob = 0.5) {
  plot_worm(obj, this_year = this_year, prob = prob)
  ggsave(file.path(fig_dir, paste0("rex-neon-worms-", scenario, ".png")),
    width = 8, height = 6.6
  )
}
