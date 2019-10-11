library(dplyr)

get_ess_hat <- function(x) {

  get_R_hat <- function(C_t, mu_t_hat) {
    sum(C_t * mu_t_hat) / sum(C_t)
  }
  out <- dplyr::group_by(x, sample_id) %>%
    dplyr::summarise(C_t = n(), n_t = length(!is.na(length)),
      mean = mean(length, na.rm = TRUE))
  R_hat <- get_R_hat(out$C_t, mu_t_hat = out$mean)

  get_var_R_y_hat <- function(C_t, n_t, mu_t_hat, R_hat, .T) {
    sum(((C_t / n_t)^2) * ((mu_t_hat - R_hat) ^ 2), na.rm = TRUE) /
      (.T * (.T - 1)) # note typo in Hulson et al. 2011
  }

  var_R_y_hat <- get_var_R_y_hat(
    C_t = out$C_t, n_t = out$n_t,
    mu_t_hat = out$mean,
    R_hat = R_hat, .T = nrow(out))

  .d <- group_by(x, sample_id) %>%
    dplyr::transmute(
      n_t = length(!is.na(length)),
      C_t = n(),
      x_tj = length,
      R_hat = R_hat
    )
  sigma_y_hat_sq <- sum((.d$C_t / .d$n_t) *
      ((.d$x_tj - .d$R_hat) ^ 2), na.rm = TRUE) / (nrow(x) - 1)

  ess_hat <- sigma_y_hat_sq / var_R_y_hat
  ess_hat
}


# f <- "data/arrowtooth-flounder-survey-samples.rds"
# f <- "data/pacific-ocean-perch-survey-samples.rds"
f <- "data/rex-sole-survey-samples.rds"
if (!file.exists(f)) {
  # d <- gfdata::get_survey_samples("arrowtooth flounder")
  # d <- gfdata::get_survey_samples("pacific ocean perch")
  d <- gfdata::get_survey_samples("rex sole")
  saveRDS(d, file = f)
} else {
  d <- readRDS(f)
}

x <- filter(d, year == 2018, survey_series_id == 4) %>% # pick one
  select(length, sample_id)
length_bins <- seq(min(x$length) - 1, max(x$length) + 1, length.out = 25)
x$length_bin <- length_bins[findInterval(x$length, length_bins)]

x %>% filter(sample_id %in% sample(unique(x$sample_id), 24)) %>%
  group_by(sample_id, length_bin) %>%
  summarise(value = n()) %>%
  ggplot(aes(length_bin, value)) + facet_wrap(~sample_id, ncol = 4) + geom_col()

x %>% group_by(length_bin) %>%
  summarise(value = n()) %>%
  ggplot(aes(length_bin, value)) + geom_col()

get_ess_hat(x)

.out <- filter(d, survey_series_id %in% c(1, 3, 4, 16)) %>%
  group_split(year, survey_series_id) %>%
  purrr::map_dfr(~tibble::tibble(
    survey_abbrev = unique(.x$survey_abbrev),
    survey_series_id = unique(.x$survey_series_id),
    year = unique(.x$year),
    ess_hat = get_ess_hat(.x)))

ggplot(.out, aes(year, ess_hat)) + geom_col() + facet_wrap(~survey_abbrev)

# nsamp <- 500
# ess <- 50
# prob <- rep(0.1, 20)
# prob <- prob / sum(prob)
# samples <- 20
# fish_per_sample <- nsamp / samples
# # x <- purrr::map_df(ess, ~ tibble::tibble(ess = .x, bin = seq_along(prob),
# #   value = rmultinom(1, .x, prob) * nsamp/.x))
# # x <- purrr::pmap_df(x, function(ess, bin, value) {
# #   tibble::tibble(length = rep(bin, value))
# # })
# # x$sample_id <- sample(1:20, size = 500, replace = TRUE)
#
# x <- purrr::map_df(seq_len(samples),
#   ~ tibble::tibble(sample_id = .x,
#   bin = seq_along(prob),
#   value = rmultinom(1, 20/2, prob) * nsamp/.x))
#
#
# x <- MCMCpack::rdirichlet(10, rep(1, 16))
# # x <- apply(x, 2, function(.x) .x / sum(.x))
# # colSums(x)
# reshape2::melt(x) %>%
#   ggplot(aes(Var1, value)) + facet_wrap(~Var2) + geom_col()
