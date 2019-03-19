#' Create the data file for the given species by running gfplot routines
#'
#' @param species_name the name of the species or species code as described in gfplot
#' @param file the full path filename including extension .rds
#'
#' @export
#'
#' @examples
#' fetch_data()
#' fetch_data("yelloweye rockfish")
fetch_data <- function(species_name = "shortraker rockfish",
                       file = file.path(here::here("generated-data"),
                                        paste0(gsub(" ",
                                                    "-",
                                                    species_name),
                                               ".rds"))){
  d <- list()
  d$commercial_samples <- gfplot::get_commercial_samples(species_name)
  d$survey_samples <- gfplot::get_survey_samples(species_name)
  d$catch <- gfplot::get_catch(species_name)
  d$survey_index <- gfplot::get_survey_index(species_name)
  saveRDS(d, file)
}

#' Load the data in from the data file for the given species
#'
#' @param species_name the name of the species or species code as described in gfplot
#' @param file the full path filename including extension .rds
#'
#' @return the contents of the rds file as a list
#'
#' @importFrom rlang abort
#'
#' @export
#'
#' @examples
#' d <- load_data()
load_data <- function(species_name = "shortraker rockfish",
                      file = file.path(here::here("generated-data"),
                                       paste0(gsub(" ",
                                                   "-",
                                                   species_name),
                                              ".rds"))){
  if(!file.exists(file)){
    abort("Error, file ", file, " does not exist. To create it, run fetch_data().\n")
  }
  readRDS(file)
}

#' Does the data file exist or not for the given species
#'
#' @param species_name the name of the species or species code as described in gfplot
#' @param file the full path filename including extension .rds
#'
#' @return the contents of the rds file as a list
#' @export
#'
#' @examples
#' data_file_exists("shortraker rockfish")
data_file_exists <- function(species_name,
                             file = file.path(here::here("generated-data"),
                                              paste0(gsub(" ",
                                                          "-",
                                                          species_name),
                                                     ".rds"))){
  file.exists(file)
}

#' Convert groundfish PBS data to a DLMtool data object
#'
#' Takes the output from \pkg{gfplot} data fetching functions and converts them to a
#' DLMtool data object.
#'
#' @param dat A list object output from [gfplot::cache_pbs_data()] or a list
#'   object containing the elements `commercial_samples`, `survey_samples`,
#'   `catch`, `survey_index`.
#' @param name A name for the stock.
#' @param common_name A common name for the stock.
#' @param area The groundfish statistical area to subset the catch by.
#' @param survey A survey abbreviation designating which survey to use for the
#'   relative index of abundance.
#' @param max_year The most recent year of data to include. Default is the max
#'   year found in the data.
#' @param min_mean_length The minimum number of samples to include a mean length
#'   measurement for a given year.
#' @param length_bin_interval An interval for the length bins.
#' @param unsorted_only Include unsorted commercial samples only
#'
#' @importClassesFrom DLMtool Data
#' @importFrom gfplot tidy_catch tidy_survey_index bind_samples fit_mat_ogive
#' @importFrom gfplot fit_vb fit_length_weight
#' @importFrom dplyr filter mutate summarise select group_by n arrange ungroup
#' @importFrom dplyr inner_join left_join right_join anti_join full_join
#' @importFrom dplyr tibble rename
#' @importFrom rlang .data
#' @return An S4 object of class DLMtool Data.
#' @export
#'
#' @examples
#' \dontrun{
#' library(gfplot)
#' species <- "pacific cod"
#' d <- list()
#' d$commercial_samples <- get_commercial_samples(species)
#' d$survey_samples <- get_survey_samples(species)
#' d$catch <- get_catch(species)
#' d$survey_index <- get_survey_index(species)
#' pbs2dlm(d, name = "BC Pacific Cod", area = "3[CD]+")
#'
#' species <- "shortraker rockfish"
#' fetch_data(species)
#' d <- load_data(species)
#' pbs2dlm(d, name = "Shortraker Rockfish", area = "5[ABCD]+")
#' }
pbs2dlm <- function(dat,
                    name = "",
                    common_name = "",
                    area = "3[CD]+",
                    survey = "SYN WCVI",
                    max_year = max_year_from_all_data,
                    min_mean_length = 10,
                    length_bin_interval = 2,
                    unsorted_only = FALSE) {

  # Setup ----------
  obj <- methods::new("Data")
  obj@Name <- name
  obj@Common_Name <- common_name
  obj@Units <- "kg"

  max_year_from_all_data <- max(dat$survey_samples$year,
                                dat$commercial_samples$year,
                                dat$catch$year,
                                dat$survey_index$year)
  dat$commercial_samples <- filter(dat$commercial_samples, .data$year <= max_year)
  dat$survey_samples <- filter(dat$survey_samples, .data$year <= max_year)
  dat$catch <- filter(dat$catch, .data$year <= max_year)
  dat$survey_index <- filter(dat$survey_index, .data$year <= max_year)

  # Catch ----------
  catch <- tidy_catch(dat$catch, areas = area)
  catch <- catch %>%
    group_by(.data$year) %>%
    summarise(value = sum(.data$value)) %>%
    ungroup()
  last_year <- max_year
  ind <- tidy_survey_index(dat$survey_index, survey = survey)
  all_years <- tibble(year = seq(min(c(catch$year, ind$year)), last_year))
  catch <- left_join(all_years, catch, by = "year")

  obj@Cat <- t(matrix(catch$value))
  obj@Year <- catch$year
  obj@t <- length(obj@Cat)
  obj@AvC <- mean(obj@Cat, na.rm = TRUE)
  obj@CV_Cat <- stats::sd(catch$value, na.rm = TRUE) / mean(catch$value, na.rm = TRUE)

  # Index of abundance ----------
  ind <- left_join(all_years, ind, by = "year")
  ind <- t(matrix(ind$biomass))
  obj@Ind <- ind / mean(ind, na.rm = TRUE) # standardise

  # Maturity ----------
  # samps <- bind_samples(
    # dat_comm = dat$commercial_samples,
    # dat_survey = dat$survey_samples
  # )
  samps <- dat$survey_samples
  m_mat <- fit_mat_ogive(samps, type = "length")
  mat_perc <- extract_maturity_perc(stats::coef(m_mat$model))
  se_l50 <- delta_method(~ -(log((1/0.5) - 1) + x1 + x3) / (x2 + x4),
    mean = stats::coef(m_mat$model), cov = stats::vcov(m_mat$model))

  obj@L50 <- mat_perc$f.p0.5 # TODO, female only?
  obj@L95 <- mat_perc$f.p0.95
  obj@CV_L50 <- se_l50 / obj@L50

  # VB model ----------
  mvb <- suppressWarnings(fit_vb(samps, sex = "female"))
  .summary <- summary(TMB::sdreport(mvb$model))
  se <- .summary[,"Std. Error"]
  cv <- se / abs(.summary[,"Estimate"])

  obj@vbK <- mvb$pars[["k"]]
  obj@vbLinf <- mvb$pars[["linf"]]
  obj@vbt0 <- mvb$pars[["t0"]]
  obj@LenCV <- sd2cv(exp(mvb$pars[["log_sigma"]]))
  obj@CV_vbK <- cv[["k"]]
  obj@CV_vbLinf <- cv[["linf"]]
  obj@CV_vbt0 <- cv[["t0"]]

  # Length weight model ----------
  mlw <- fit_length_weight(samps, sex = "female")
  .summary <- summary(TMB::sdreport(mlw$model))
  se <- .summary[,"Std. Error"]

  obj@wla <- exp(mlw$pars[["log_a"]])
  obj@wlb <- mlw$pars[["b"]]
  obj@CV_wla <- sd2cv(se[["log_a"]]) # log scale
  obj@CV_wlb <- se[["b"]] / mlw$pars[["b"]]

  # Mean length timeseries ----------
  if(unsorted_only){
    dat$commercial_samples <- dat$commercial_samples %>%
      filter(sampling_desc == "UNSORTED")
  }
  ml <- tidy_mean_length(dat$commercial_samples) %>%
    filter(.data$n > min_mean_length, .data$year <= max_year) %>%
    right_join(all_years, by = "year")

  obj@ML <- t(matrix(ml$mean_length))

  # Catch at age ----------
  obj@CAA <- tidy_caa(dat$commercial_samples, yrs = all_years$year)
  obj@MaxAge <- ncol(obj@CAA[1, , ])

  # Catch at length ----------
  # CAL Catch-at-length data. An array with dimensions nsim x nyears
  # x length(CAL_bins). Non-negative integers
  length_bins <- seq(0, 1e4, length_bin_interval)
  obj@CAL <- dat$commercial_samples %>%
    select(-.data$age) %>%
    mutate(length_bin = length_bins[findInterval(.data$length, length_bins)]) %>%
    rename(age = .data$length_bin) %>% # hack
    tidy_caa(yrs = all_years$year, interval = length_bin_interval)
  # CAL_bins The values delimiting the length bins for the catch-at-length
  # data. Vector. Non-negative real numbers
  obj@CAL_bins <- seq(length_bin_interval, ncol(obj@CAL[1, , ]) *
    length_bin_interval) - length_bin_interval / 2 # mid points

  obj
}

#' Generate mean-length time series
#'
#' @param dat Commercial or biological samples from [get_commercial_samples()] or
#'   [get_survey_samples()].
#' @param unsorted_only Logical for whether to only include the unsorted samples.
#'   Only applies to the commercial data.
tidy_mean_length <- function(dat, unsorted_only = FALSE) {
  dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE]
  if ("sampling_desc" %in% names(dat) && unsorted_only) {
    dat <- filter(dat, .data$sampling_desc == "UNSORTED")
  }
  dat <- filter(dat, !is.na(.data$sex), !is.na(.data$length), .data$sex %in% 2) # female only
  group_by(dat, .data$year) %>%
    summarise(n = n(), mean_length = mean(.data$length)) %>%
    ungroup()
}

#' Generate catch-at-age or catch-at-length data
#'
#' @param dat Commercial or biological samples from [get_commercial_samples()] or
#'   [get_survey_samples()].
#' @param yrs A complete set of years to include in the matrix.
#' @param unsorted_only Logical for whether to only include the unsorted samples.
#'   Only applies to the commercial data.
#' @param interval Interval for the complete set of ages or lengths. For example,
#'   for length bins of interval 2, `interval = 2`.
#'
#' @return A catch at age or catch at length matrix as an array.
#'   1 x nyears x nage/nlength
tidy_caa <- function(dat, yrs, unsorted_only = FALSE, interval = 1,
  sex = c(1, 2)) {
  dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE]
  if ("sampling_desc" %in% names(dat) && unsorted_only) {
    dat <- filter(dat, .data$sampling_desc == "UNSORTED")
  }
  dat <- filter(dat, !is.na(.data$sex), !is.na(.data$age), .data$sex %in% sex)

  caa <- group_by(dat, .data$year, .data$age) %>%
    summarise(N = n()) %>%
    ungroup()

  caa <- left_join(
    expand.grid(age = seq(0, max(caa$age), interval), year = unique(caa$year)),
    caa,
    by = c("age", "year")
  )
  caa$N <- ifelse(is.na(caa$N), 0, caa$N)
  caa <- left_join(
    expand.grid(age = seq(0, max(caa$age), interval), year = yrs),
    caa,
    by = c("age", "year")
  )
  caa <- reshape2::dcast(caa, year ~ age, value.var = "N")[, -1L]
  array(as.numeric(as.matrix(caa)),
    dim = c(1L, nrow(caa), ncol(caa))
  ) # nsim x nyears x MaxAge
}


extract_maturity_perc <- function(object) {
  m.p0.5 <- logit_perc(a = object[[1]], b = object[[2]], perc = 0.5)
  m.p0.95 <- logit_perc(a = object[[1]], b = object[[2]], perc = 0.95)
  m.p0.05 <- logit_perc(a = object[[1]], b = object[[2]], perc = 0.05)

  f.p0.5 <- logit_perc(a = object[[1]] + object[[3]],
    b = object[[2]] + object[[4]], perc = 0.5)
  f.p0.95 <- logit_perc(a = object[[1]] + object[[3]],
    b = object[[2]] + object[[4]], perc = 0.95)
  f.p0.05 <- logit_perc(a = object[[1]] + object[[3]],
    b = object[[2]] + object[[4]], perc = 0.05)
  list(m.p0.5 = m.p0.5, m.p0.95 = m.p0.95, m.p0.05 = m.p0.05, f.p0.5 = f.p0.5,
    f.p0.95 = f.p0.95, f.p0.05 = f.p0.05)
}

delta_method <- function(g, mean, cov) {
  # simplified from msm::deltamethod
  cov <- as.matrix(cov)
  n <- length(mean)
  g <- list(g)
  syms <- paste0("x", seq_len(n))
  for (i in seq_len(n)) assign(syms[i], mean[i])
  gdashmu <- t(sapply(g, function(form) {
    as.numeric(attr(eval(stats::deriv(form, syms)), "gradient"))
  }))
  new.covar <- gdashmu %*% cov %*% t(gdashmu)
  sqrt(diag(new.covar))
}

sd2cv <- function(.sd) {
  sqrt(exp(.sd^2) - 1)
}

logit_perc <- function(a, b, perc = 0.5) {
  -(log((1 / perc) - 1) + a) / b
}
