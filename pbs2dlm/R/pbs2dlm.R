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

#' Create a Stock object for DLMtool from data and values, to be used in an Operating Model (OM)
#'
#' @param d An S4 object of class DLMtool Data. If NULL, default values from the statring_stock
#'   will be used in the returned object. If supplied, select values will be copied from the
#'   Data object into the returned Stock object.
#'
#' @return An S4 object of class DLMtool Stock.
#'
#' @export
#' #' \dontrun{
#' library(gfplot)
#' species <- "shortraker rockfish"
#' fetch_data(species)
#' d <- load_data(species)
#' dat <- create_dlm_data(d, name = "Shortraker Rockfish", area = "5[ABCD]+")
#' stk <- create_dlm_stock(dat, starting_stock = "Rockfish")
#' stk2 <- create_dlm_stock(starting_stock = "Rockfish")
#' stk3 <- create_dlm_stock()
#' }
create_dlm_stock <- function(dat = NULL,
                             starting_stock = NA,
                             name = obj@Name,
                             common_name = obj@Common_Name,
                             species = obj@Species,
                             maxage = obj@maxage,
                             r0 = obj@R0,
                             m = obj@M,
                             m2 = obj@M2,
                             mexp = obj@Mexp,
                             msd = obj@Msd,
                             mgrad = obj@Mgrad,
                             h = obj@h,
                             srrel = obj@SRrel,
                             perr = obj@Perr,
                             ac = obj@AC,
                             period = obj@Period,
                             amplitude = obj@Amplitude,
                             linf = obj@Linf,
                             k = obj@K,
                             t0 = obj@t0,
                             lencv = obj@LenCV,
                             ksd = obj@Ksd,
                             kgrad = obj@Kgrad,
                             linfsd = obj@Linfsd,
                             linfgrad = obj@Linfgrad,
                             l50 = obj@L50,
                             l50_95= obj@L50_95,
                             d = obj@D,
                             a = obj@a,
                             b = obj@b,
                             size_area_1 = obj@Size_area_1,
                             frac_area_1 = obj@Frac_area_1,
                             prob_staying = obj@Prob_staying,
                             fdisc = obj@Fdisc,
                             src = obj@Source){

  if(is.na(starting_stock)){
    obj <- methods::new("Stock")
  }else if(starting_stock %in% avail("Stock")){
    obj <- get(starting_stock)
  }else{
    stop("starting_stock '", starting_stock, "', doesn't exist. Use one of:\n",
         paste(avail("Stock"), collapse = "\n"))
  }

  obj@Name <- ifelse(is.null(dat), name, dat@Name)
  obj@Common_Name <- ifelse(is.null(dat), common_name, dat@Common_Name)
  obj@Species <- ifelse(is.null(dat), species, dat@Species)
  obj@maxage <- ifelse(is.null(dat), maxage, dat@MaxAge)
  obj@R0 <- r0
  obj@M <- m
  obj@M2 <- m2
  obj@Mexp <- mexp
  obj@Msd <- msd
  obj@Mgrad <- mgrad
  obj@h <- h
  obj@SRrel <- srrel
  obj@Perr <- perr
  obj@AC <- ac
  obj@Period <- period
  obj@Amplitude <- amplitude
  obj@Linf <- ifelse(is.null(dat), linf, dat@vbLinf)
  obj@K <- ifelse(is.null(dat), k, dat@vbK)
  obj@t0 <- ifelse(is.null(dat), t0, dat@vbt0)
  obj@LenCV <- lencv
  obj@Ksd <- ksd
  obj@Kgrad <- kgrad
  obj@Linfsd <- linfsd
  obj@Linfgrad <- linfgrad
  obj@L50 <- l50
  obj@L50_95<- l50_95
  obj@D <- d
  obj@a <- a
  obj@b <- b
  obj@Size_area_1 <- size_area_1
  obj@Frac_area_1 <- frac_area_1
  obj@Prob_staying <- prob_staying
  obj@Fdisc <- fdisc
  obj@Source <- src

  obj
}

#' Create a Fleet object for DLMtool from data and values, to be used in an Operating Model (OM)
#'
#' @param d An S4 object of class DLMtool Data. If NULL, default values from the statring_fleet
#'   will be used in the returned object. If supplied, select values will be copied from the
#'   Data object into the returned Fleet object.
#'
#' @return An S4 object of class DLMtool Fleet.
#'
#' @export
#' #' \dontrun{
#' library(gfplot)
#' species <- "shortraker rockfish"
#' fetch_data(species)
#' d <- load_data(species)
#' dat <- create_dlm_data(d, name = "Shortraker Rockfish", area = "5[ABCD]+")
#' flt <- create_dlm_fleet(dat, starting_fleet = "Generic_Fleet")
#' flt2 <- create_dlm_fleet(starting_fleet = "Generic_Fleet")
#' flt3 <- create_dlm_fleet()
#' }
create_dlm_fleet <- function(dat = NULL,
                             starting_fleet = NA,
                             name = obj@Name,
                             nyears = obj@nyears,
                             spat_targ = obj@Spat_targ,
                             effyears = obj@EffYears,
                             efflower = obj@EffLower,
                             esd = obj@Esd,
                             qinc = obj@qinc,
                             qcv = obj@qcv,
                             l5 = obj@L5,
                             lfs = obj@LFS,
                             vmaxlen = obj@Vmaxlen,
                             isrel = obj@isRel,
                             lr5 = obj@LR5,
                             lfr = obj@LFR,
                             rmaxlen = obj@Rmaxlen,
                             dr = obj@DR,
                             selyears = obj@SelYears,
                             absselyears = obj@AbsSelYears,
                             l5lower = obj@L5Lower,
                             l5upper = obj@L5Upper,
                             lfslower = obj@LFSLower,
                             lfsupper = obj@LFSUpper,
                             vmaxlower = obj@VmaxLower,
                             vmaxupper = obj@VmaxUpper,
                             currentyr = obj@CurrentYr,
                             mpa = obj@MPA){

  if(is.na(starting_fleet)){
    obj <- methods::new("Fleet")
  }else if(starting_fleet %in% avail("Fleet")){
    obj <- get(starting_fleet)
  }else{
    stop("starting_fleet '", starting_fleet, "', doesn't exist. Use one of:\n",
          paste(avail("Fleet"), collapse = "\n"))
  }

  obj@Name <- ifelse(is.null(dat), name, dat@Name)
  obj@nyears <- nyears
  obj@Spat_targ <- spat_targ
  obj@EffYears <- effyears
  obj@EffLower <- efflower
  obj@Esd <- esd
  obj@qinc <- qinc
  obj@qcv <- qcv
  obj@L5 <- l5
  obj@LFS <- lfs
  obj@Vmaxlen <- vmaxlen
  obj@isRel <- isrel
  obj@LR5 <- lr5
  obj@LFR <- lfr
  obj@Rmaxlen <- rmaxlen
  obj@DR <- dr
  obj@SelYears <- selyears
  obj@AbsSelYears <- absselyears
  obj@L5Lower <- l5lower
  obj@L5Upper <- l5upper
  obj@LFSLower <- lfslower
  obj@LFSUpper <- lfsupper
  obj@VmaxLower <- vmaxlower
  obj@VmaxUpper <- vmaxupper
  obj@CurrentYr <- currentyr
  obj@MPA <- mpa

  obj
}

#' Create an Obs object for DLMtool from data and values, to be used in an Operating Model (OM)
#'
#' @param d An S4 object of class DLMtool Data.
#'
#' @return An S4 object of class DLMtool Obs.
#' @export
#' #' \dontrun{
#' library(gfplot)
#' species <- "shortraker rockfish"
#' fetch_data(species)
#' d <- load_data(species)
#' dat <- create_dlm_data(d, name = "Shortraker Rockfish", area = "5[ABCD]+")
#' om <- create_dlm_obs(dat)
#' }
create_dlm_obs <- function(d){

  obj <- methods::new("Obs")
  obj@Name <- d@name

  obj
}

#' Create an Imp object for DLMtool from data and values, to be used in an Operating Model (OM)
#'
#' @param d An S4 object of class DLMtool Data.
#'
#' @return An S4 object of class DLMtool Imp.
#' @export
#' #' \dontrun{
#' library(gfplot)
#' species <- "shortraker rockfish"
#' fetch_data(species)
#' d <- load_data(species)
#' dat <- create_dlm_data(d, name = "Shortraker Rockfish", area = "5[ABCD]+")
#' om <- create_dlm_imp(dat)
#' }
create_dlm_imp <- function(d){

  obj <- methods::new("Imp")
  obj@Name <- d@name

  obj
}

#' Convert groundfish PBS data to a DLMtool Data object
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
#' create_dlm_data(d, name = "BC Pacific Cod", area = "3[CD]+")
#'
#' species <- "shortraker rockfish"
#' fetch_data(species)
#' d <- load_data(species)
#' create_dlm_data(d, name = "Shortraker Rockfish", area = "5[ABCD]+")
#' }
create_dlm_data <- function(dat,
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
  dat$commercial_samples <- dplyr::filter(dat$commercial_samples, year <= max_year)
  dat$survey_samples <- dplyr::filter(dat$survey_samples, year <= max_year)
  dat$catch <- dplyr::filter(dat$catch, year <= max_year)
  dat$survey_index <- dplyr::filter(dat$survey_index, year <= max_year)

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
