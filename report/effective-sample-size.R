# Pennington, M., Burmeister, L.-M., and Hjellvik, V. 2002.
# Assessing the precision of frequency distributions estimated
# from trawl-survey samples. Fish. Bull. 100: 74–80.

# d <- readRDS("../gfsynopsis/report/data-cache/pacific-ocean-perch.rds")
# d <- readRDS("../gfsynopsis/report/data-cache/redbanded-rockfish.rds")
# d <- readRDS("../gfsynopsis/report/data-cache/arrowtooth-flounder.rds")
# d <- readRDS("../gfsynopsis/report/data-cache/rex-sole.rds")
# survey_sets <- d$survey_sets
# d <- d$survey_samples

# You can get that same data frame with:
# d <- gfdata::get_survey_samples("arrowtooth flounder")

f <- "data/arrowtooth-flounder-survey-samples.rds"
if (!file.exists(f)) {
  d <- gfdata::get_survey_samples("arrowtooth flounder")
  saveRDS(d, file = f)
} else {
  d <- readRDS(f)
}

library(dplyr)
d <- filter(d, survey_series_id %in% 4) # Pick an example survey
glimpse(d)

# Pick an example year
dd <- dplyr::filter(d, year == 2018) %>%
  filter(!is.na(length)) %>%
  select(year, grouping_code, sample_id, length)

mean(dd$length)
nrow(dd)

r_hat <- group_by(dd, sample_id) %>%
  summarize(mu_hat = mean(length), M_i = n()) %>%
  ungroup() %>%
  summarize(numerator = sum(mu_hat * M_i), denominator = sum(M_i)) %>%
  mutate(r_hat = numerator / denominator) %>%
  pull(r_hat)
r_hat # mean length of fish across sampling events
mean(dd$length) # that was a fancy way of doing this because we don't have stations (M_i)

M_bar <- group_by(dd, sample_id) %>%
  summarize(M_i = n()) %>%
  ungroup() %>%
  summarise(M_bar = mean(M_i)) %>%
  pull(M_bar)
M_bar # mean number of fish sampled per sampling event

.n <- length(unique(dd$sample_id))
.n # number of fish sampled

var_r_hat <- group_by(dd, sample_id) %>%
  summarize(
    mu_hat_i = mean(length),
    M_i = n()
  ) %>%
  mutate(numerator = (M_i / M_bar)^2 * (mu_hat_i - r_hat)^2) %>%
  mutate(denominator = .n * (.n - 1)) %>%
  mutate(ratio = numerator / denominator) %>%
  ungroup() %>%
  summarize(var_r_hat = sum(ratio)) %>%
  pull(var_r_hat)
var_r_hat # estimated variance of the sampled mean length estimate

sigma_2 <- group_by(dd, sample_id) %>%
  # M_i = m_i; assuming that all fish are measured
  mutate(M_i = n(), m_i = M_i, x_ij = length) %>%
  ungroup() %>%
  summarize(
    result =
      sum((M_i / m_i) * (x_ij - r_hat)^2) / (nrow(dd) - 1)
  ) %>%
  pull(result)
sigma_2 # estimated variance of the population length distribution

# "if it were possible to sample m fish at random from the population, then the
# variance of the sample mean would be equal to sigma_2 / m"
sigma_2 / M_bar

# but we observed:
var_r_hat

# effective sample size ("the number of fish that would need to be sampled at
# random so that the sample mean would have the same precision as an estimate
# based on a sample of n clusters. An estimate of the effective sample size for
# a particular cluster sample can be derived by substituting the estimates from
# Equation 2 and either Equation 3 or 4 into the equation")
m_hat_eff <- sigma_2 / var_r_hat
m_hat_eff

# this is the effective sample size for a given sample

# the total effective number of fish across all samples then would be:
m_hat_eff * .n

# this is in comparison to the number of fish actually sampled:
nrow(dd)

# Alternatively, we can get there like this:

# The ratio of expected versus observed variance of mean length:
ratio_sigma <- (sigma_2 / M_bar) / var_r_hat
ratio_sigma

# Multiply that by the number of total fish sampled again indicates
# that the effective total number of fish is:
ratio_sigma * nrow(dd)

hist(d$length)
