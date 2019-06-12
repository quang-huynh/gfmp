## Code used to make the functions found in plot-contours.R
## **Not used directly in this project**
##

get_quantile_contour <- function(x, alpha = 0.8) {
  zdens <- rev(sort(x$z))
  Czdens <- cumsum(zdens)
  Czdens <- (Czdens / Czdens[length(zdens)])
  crit.val <- zdens[max(which(Czdens <= alpha))]
  b.full <- contourLines(x, levels = crit.val)
  list(x = b.full[[1]]$x, y = b.full[[1]]$y)
}

add_dens_polygon <- function(x, y, col, alpha = c(0.25, 0.75), n = 200,
  add_pts = FALSE, add_poly = FALSE) {
  x_bw <- MASS::bandwidth.nrd(na.omit(x))
  y_bw <- MASS::bandwidth.nrd(na.omit(y))
  k <- list()
  for (i in seq_along(alpha)) {
    k[[i]] <- get_quantile_contour(MASS::kde2d(x, y,
      h = c(x_bw, y_bw), n = n), alpha = alpha[i])
    k[[i]]$alpha <- rep(alpha[i], length(k[[i]]$x))
    if (add_poly) {
      polygon(k[[i]]$x, k[[i]]$y, border = col,
        col = paste(col, alpha[i] * 100, sep = ""))
    }
  }
  if (add_pts) points(x, y, pch = 21, col = paste0(col, "60"))
  invisible(k)
}

set.seed(42)
x <- rnorm(1000)
y <- rnorm(1000)
plot(x, y)
k <- add_dens_polygon(x, y, col = "#FF0000", alpha = seq(0.2, 0.8, 0.2))
out <- purrr::map_df(k, ~.x)

library(ggplot2)
ggplot(out, aes(x, y, colour = as.factor(alpha))) +
  geom_point(
    data = data.frame(x = x, y = y), aes(x = x, y = y), inherit.aes = FALSE,
    alpha = 0.1
  ) +
  geom_polygon(alpha = 0.5, fill = NA) +
  scale_color_viridis_d(option = "C")
