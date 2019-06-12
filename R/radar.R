#' Title
# https://stackoverflow.com/a/46999174#'
#' @param mydf
#'
#' @return
#' @export
#'
#' @examples
calculate_radar <- function(mydf) {
  df <- cbind(mydf[, -1], mydf[,2])
  myvec <- c(t(df))
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / (ncol(df) - 1))
  xx <- myvec * sin(rep(c(angles[-ncol(df)], angles[1]), nrow(df)))
  yy <- myvec * cos(rep(c(angles[-ncol(df)], angles[1]), nrow(df)))
  graphData <- data.frame(group = rep(mydf[, 1], each = ncol(mydf)),
                          x = xx, y = yy)
  graphData
}

calculate_spokes <- function(mydf) {
  .n <- ncol(mydf) - 1
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / (.n))
  data.frame(x = 0, y = 0, xend = sin(angles[-1]), yend = cos(angles[-1]))
}

make_radar <- function(.species, .mp = NULL, top = TRUE) {
  dat <- wide_pm %>%
    filter(species == .species) %>%
    select(-1, -2)

  if (top)
    dat <- filter(dat, mp %in% top_top_pm_names)

  if (!is.null(.mp))
    dat <- filter(dat, mp %in% .mp)

  spokes_data <- calculate_spokes(dat)
  spokes_data$pm <- c(names(dat[, -1])[-1], names(dat[, -1])[1])
  radar_data <- calculate_radar(dat)
  label_data <- data.frame(x = 0, y = c(0.5, 0.75))

  ## for ggrepel labels:
  # radar_data$pm <- rep(c(names(dat[,-1]), ""), length(unique(dat[,1])))
  # labs <- filter(radar_data, pm %in% c("AAVY", "PNOF")) %>%
  #   group_by(group) %>%
  #   summarize(xx = approx(x = x, y = y, n = 7)$x[6],
  #     yy = approx(x = x, y = y, n = 7)$y[6])

  radar_data %>%
    ggplot(aes(x = x, y = y)) +
    geom_segment(
      data = spokes_data,
      aes(x = x, y = y, xend = xend, yend = yend), colour = "grey75", lty = 1
    ) +
    # FIXME: do this more elegantly in the data:
    geom_path(
      data = rbind(spokes_data, spokes_data[1, ]),
      aes(x = xend * 0.5, y = yend * 0.5), colour = "grey75", lty = 2
    ) +
    geom_path(
      data = rbind(spokes_data, spokes_data[1, ]),
      aes(x = xend * 0.75, y = yend * 0.75), colour = "grey75", lty = 2
    ) +
    geom_path(
      data = rbind(spokes_data, spokes_data[1, ]),
      aes(x = xend * 1, y = yend * 1), colour = "grey75", lty = 2
    ) +
    geom_path(aes(colour = as.factor(group)), lwd = 0.8) +
    coord_equal() +
    geom_text(data = spokes_data, aes(
      x = xend * 1.1, y = yend * 1.1,
      label = pm
    ), colour = "grey30") +
    geom_text(
      data = label_data,
      aes(x = x, y = y, label = y), colour = "grey50",
      nudge_y = 0.04, hjust = 0, nudge_x = 0.01
    ) +
    gfplot::theme_pbs() +
    labs(colour = "MP") +
    # scale_color_viridis_d() +
    scale_color_brewer(palette = "Set2") +
    # guides(colour = FALSE) +
    # ggrepel::geom_text_repel(data = labs,
    #   aes(x = xx, y = yy, label = group, colour = group),
    #   nudge_y = 0.1, nudge_x = -0.1) +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank()
    ) +
    theme(legend.position="right")

}

make_radar_plot <- function(mps, file_name = NULL, ...) {
  out <- lapply(species_names$species, make_radar, .mp = mps, ...)

  if (!is.null(file_name)) {
    pdf(file_name, width = 11, height = 12)
    on.exit(dev.off())
  }
  g <- cowplot::plot_grid(plotlist = out,
                          labels = gfsynopsis:::first_cap(species_names$species_full),
                          label_fontface = "plain", hjust = 0, label_x = 0.05, nrow = 3)
  g
}
