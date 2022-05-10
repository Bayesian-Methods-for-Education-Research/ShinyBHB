data {
  int<lower = 1> N;                         // number of students
  int<lower = 1> NN;                        // number of students in the current cycle
  int<lower = 1> M;                         // number of covariates
  int<lower = 1> MM;                        // number of covariates with random effects
  int<lower = 1> S;                         // number of schools
  int<lower = 1> SS;                        // number of schools in the current cycle
  int<lower = 1> C;                         // number of cycles
  matrix[N, M] x;                           // independent variables with fixed effects
  matrix[N, MM] xx;                         // independent variables with random effects
  vector[N] y;                              // depenendent variables
  int<lower = 1, upper = S> sch[N];         // schools
  int<lower = 1, upper = C> cycle[N];       // cycles of students
  int<lower = 1, upper = C> cycle_sch[S];   // cycles of schools
}

transformed data {
  vector[M] mu_x;
  vector[M] sd_x;
  matrix[N, M] x_std;
  vector[MM] mu_xx;
  vector[MM] sd_xx;
  matrix[N, MM] xx_std;
  
  real mu_y = mean(y);
  real sd_y = sd(y);
  vector[N] y_std = (y - mu_y) / sd_y;
  
  // x[, 1] is the intercept
  x_std[, 1] = x[, 1];
  for (m in 2:M) {
    mu_x[m] = mean(x[, m]);
    sd_x[m] = sd(x[, m]);
    x_std[, m] = (x[, m] - mu_x[m]) / sd_x[m];
  }
  xx_std[, 1] = xx[, 1];
  for (m in 2:MM) {
    mu_xx[m] = mean(xx[, m]);
    sd_xx[m] = sd(xx[, m]);
    xx_std[, m] = (xx[, m] - mu_xx[m]) / sd_xx[m];
  }
}

parameters {
  // fixed effects
  vector[M] beta_std[C];
  vector[M] mu_std;
  vector<lower = 0>[M] sigma_beta_std;
  
  // random effects
  vector[MM] u_std[S];
  cholesky_factor_corr[MM] L_omega_u;
  vector<lower = 0>[MM] tau_u_std;
  cov_matrix[MM] prec_u_std[C];

  real<lower = 0> sigma_r_std;
}

model {
  for (c in 1:C)
    beta_std[c] ~ normal(mu_std, SIGMA_BETA_STD);
  mu_std ~ normal(0, SIGMA_0_BETA);
  sigma_beta_std ~ SIGMA_BETA;
  
  for (s in 1:S)
    u_std[s] ~ multi_normal_prec(rep_vector(0, MM), prec_u_std[cycle_sch[s]]);
  {
    matrix[MM, MM] omega = inverse_spd(multiply_lower_tri_self_transpose(diag_pre_multiply(tau_u_std, L_omega_u)));
    for (c in 1:C)
      prec_u_std[c] ~ wishart(NU, NU * omega);
  }
  L_omega_u ~ OMEGA;
  tau_u_std ~ TAU;
  
  for (n in 1:N)
    y_std[n] ~ normal(x_std[n, ] * beta_std[cycle[n]] + xx_std[n, ] * u_std[sch[n]], sigma_r_std);
  sigma_r_std ~ SIGMA_R;
}

generated quantities {
  vector[M] beta;
  real<lower = 0> sigma_r = sigma_r_std * sd_y;
  vector[NN] log_lik;
  vector[SS] log_lik_sch = rep_vector(0, SS);
  
  matrix[MM, MM] sigma_u = inverse_spd(prec_u_std[1]) * (sd_y ^ 2);
  
  for (i in 2:MM) {
    sigma_u[1, i] /= sd_xx[i];
    sigma_u[1, 1] -= 2 * mu_xx[i] * sigma_u[1, i];
    for (j in 2:MM) {
      sigma_u[i, j] /= sd_xx[i] * sd_xx[j];
      sigma_u[1, i] -= sigma_u[i, j] * mu_xx[j];
      sigma_u[1, 1] += mu_xx[i] * mu_xx[j] * sigma_u[i, j];
    }
    sigma_u[i, 1] = sigma_u[1, i];
  }
  
  beta[1] = sd_y * beta_std[1, 1] + mu_y;
  for (m in 2:M) {
    beta[m] = sd_y / sd_x[m] * beta_std[1, m];
    beta[1] -= beta[m] * mu_x[m];
  }
  
  for (n in 1:NN) {
    log_lik[n] = normal_lpdf(y[n] | x[n, ] * beta + x[n, 1:MM] * (u_std[sch[n]] * sd_y), sigma_r);
    log_lik_sch[sch[n]] += log_lik[n];
  }
}
