# Pennington, M., Burmeister, L.-M., and Hjellvik, V. 2002.
# Assessing the precision of frequency distributions estimated
# from trawl-survey samples. Fish. Bull. 100: 74–80.

d <- readRDS("../gfsynopsis/report/data-cache/pacific-ocean-perch.rds")
d <- readRDS("../gfsynopsis/report/data-cache/redbanded-rockfish.rds")
d <- readRDS("../gfsynopsis/report/data-cache/arrowtooth-flounder.rds")
# survey_sets <- d$survey_sets
d <- d$survey_samples

# You can get that same data frame with:
# d <- gfdata::get_survey_samples("arrowtooth flounder")

library(dplyr)
d <- filter(d, survey_series_id %in% 1) # Pick an example survey
glimpse(d)

# weights <- group_by(survey_sets, sample_id) %>%
#   summarize(weight = sum(density_kgpm2) * 1000000)

# Pick an example year
dd <- filter(d, year == 2017) %>%
  filter(!is.na(length)) %>%
  select(year, grouping_code, sample_id, length) %>%
  left_join(weights)

mean(dd$length)
nrow(dd)

r_hat <- group_by(dd, sample_id) %>%
  summarize(mu_hat = mean(length), M_i = n()) %>%
  ungroup() %>%
  summarize(numerator = sum(mu_hat * M_i), denominator = sum(M_i)) %>%
  mutate(r_hat = numerator / denominator) %>%
  pull(r_hat)
r_hat # mean length of fish across sampling events
mean(dd$length) # that was a fancy way of doing this

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
      sum((M_i / m_i) * (x_ij - r_hat)^2) / (sum(M_i) - 1)
  ) %>%
  pull(result)
sigma_2 # estimated variance of the population length distribution

# if it were possible to sample m fish at random from the population, then the
# variance of the sample mean would be equal to sigma_2 / m

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

# presumably, this is the effective sample size for a given sample
# the total effective sample size across all samples than would be:
m_hat_eff * .n

# this is in comparison to the number of fish actually sampled:
nrow(dd)

# Alternatively, I think we can get there like this:

# The ratio of expected versus observed variance of mean length:
ratio_sigma <- (sigma_2 / M_bar) / var_r_hat
ratio_sigma

# Multiply that by the number of total fish sampled again indicates
# that the effective sample size is:
ratio_sigma * nrow(dd)

hist(d$length)
