# Pennington, M., Burmeister, L.-M., and Hjellvik, V. 2002.
# Assessing the precision of frequency distributions estimated
# from trawl-survey samples. Fish. Bull. 100: 74–80.

d <- readRDS("../gfsynopsis/report/data-cache/pacific-ocean-perch.rds")
d <- readRDS("../gfsynopsis/report/data-cache/redbanded-rockfish.rds")
d <- readRDS("../gfsynopsis/report/data-cache/arrowtooth-flounder.rds")
survey_sets <- d$survey_sets
d <- d$survey_samples

library(dplyr)
d <- filter(d, survey_series_id %in% 1) # Pick an example survey
glimpse(d)

weights <- group_by(survey_sets, sample_id) %>%
  summarize(weight = sum(density_kgpm2) * 1000000)

# Pick an example year
dd <- filter(d, year == 2017) %>%
  filter(!is.na(length)) %>%
  select(year, grouping_code, sample_id, length) %>%
  left_join(weights)

mean(dd$length)

r_hat <- group_by(dd, grouping_code) %>%
  summarize(mu_hat = mean(length), M_i = sum(weight)) %>%
  ungroup() %>%
  summarize(numerator = sum(mu_hat * M_i), denominator = sum(M_i)) %>%
  mutate(r_hat = numerator / denominator) %>%
  pull(r_hat)
r_hat

M_bar <- group_by(dd, grouping_code) %>%
  summarize(M_i = sum(weight)) %>%
  ungroup() %>%
  summarise(M_bar = sum(M_i / nrow(dd))) %>%
  pull(M_bar)
M_bar

var_r_hat <- group_by(dd, grouping_code) %>%
  summarize(mu_hat_i = mean(length), M_i = sum(weight)) %>%
  mutate(numerator = (M_i / M_bar)^2 * (mu_hat_i - r_hat)^2) %>%
  mutate(denominator = nrow(dd) * (nrow(dd) - 1)) %>%
  mutate(ratio = numerator / denominator) %>%
  ungroup() %>%
  summarize(var_r_hat = sum(ratio)) %>%
  pull(var_r_hat)
var_r_hat

sigma_2 <- group_by(dd, grouping_code) %>%
  mutate(M_i = sum(weight), m_i = M_i, x_ij = length) %>%
  ungroup() %>%
  summarize(result = sum((M_i / m_i) * (x_ij - r_hat)^2) / (nrow(dd) - 1)) %>%
  pull(result)
sigma_2

m_hat_eff <- sigma_2 / var_r_hat
m_hat_eff
