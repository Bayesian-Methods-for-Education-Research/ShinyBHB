data {
  int<lower = 1> N;                    // number of students
  int<lower = 1> M;                    // number of covariates
  //int<lower = 1> C;                    // number of cycles
  int<lower = 1> NN;                   // number of students in the current cycle
  matrix[N, M] x;                      // covariates
  vector[N] y;                         // outcomes
  //int<lower = 1, upper = C> cycle[N];  // cycles
}

transformed data {
  vector[M] mu_x;
  vector[M] sd_x;
  matrix[NN, M] x_std;
  
  real mu_y = mean(y[1:NN]);
  real sd_y = sd(y[1:NN]);
  vector[NN] y_std = (y[1:NN] - mu_y) / sd_y;
  
  // x[, 1] is the intercept
  x_std[, 1] = x[1:NN, 1];
  for (m in 2:M) {
    mu_x[m] = mean(x[1:NN, m]);
    sd_x[m] = sd(x[1:NN, m]);
    x_std[, m] = (x[1:NN, m] - mu_x[m]) / sd_x[m];
  }
}

parameters {
  vector[M] beta_std;
  
  real<lower = 0> sigma_r_std;
}

model {
  beta_std ~ normal(0, SIGMA_BETA);
  
  for (n in 1:NN)
    y_std[n] ~ normal(x_std[n, ] * beta_std, sigma_r_std);
  sigma_r_std ~ SIGMA_R;
}

generated quantities {
  vector[M] beta;
  real<lower = 0> sigma_r = sigma_r_std * sd_y;
  vector[NN] log_lik;
  
  beta[1] = sd_y * beta_std[1] + mu_y;
  for (m in 2:M) {
    beta[m] = sd_y / sd_x[m] * beta_std[m];
    beta[1] -= beta[m] * mu_x[m];
  }
  
  for (n in 1:NN)
    log_lik[n] = normal_lpdf(y[n] | x[n, ] * beta, sigma_r);
}
