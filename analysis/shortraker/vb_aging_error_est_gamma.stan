data {
  int<lower=1> N;
  vector<lower=0>[N] length;
  vector<lower=0>[N] age;
}
parameters {
  real<lower=0> k;
  real<lower=0> linf;
  real<lower=0> cv_length;
  real<lower=0> cv_age;
  real t0;
  vector<lower=0>[N] age_true;
}
transformed parameters {
  real<lower=0> alpha_age;
  real<lower=0> alpha_length;
  alpha_age = inv(pow(cv_age, 2.0));
  alpha_length = inv(pow(cv_length, 2.0));
}
model {
  // priors:
  k ~ normal(0, 5);
  linf ~ normal(0, 300);
  t0 ~ normal(0, 50);
  cv_length ~ student_t(3, 0, 1);
  cv_age ~ gamma(1/0.05^2, 1/(0.2 * 0.05^2));

  // aging measurement error:
  age ~ gamma(alpha_age, alpha_age ./ age_true);

  // likelihood:
  length ~ gamma(alpha_length, alpha_length ./ (linf * (1 - exp(-k * (age_true - t0)))));
}
