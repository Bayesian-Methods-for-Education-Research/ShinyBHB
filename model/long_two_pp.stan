data {
  int<lower = 1> S;                      // number of observations
  int<lower = 1> SS;                     // number of observations in the current cycle
  int<lower = 1> N;                      // number of students
  int<lower = 1> NN;                     // number of students in the current cycle
  int<lower = 1> M;                      // number of schools
  int<lower = 1> MM;                     // number of schools in the current cycle
  int<lower = 1> C;                      // number of cycles
  int<lower = 1> K;                      // number of covariates
  int<lower = 1> K1;                     // number of covariates with random effects at student level
  int<lower = 1> K2;                     // number of covariates with random effects at school level
  matrix[S, K] x;                        // independent variables with fixed effects
  matrix[S, K1] x1;                      // independent variables with random effects at student level
  matrix[S, K2] x2;                      // independent variables with random effects at school level
  vector[S] y;                           // dependent variables
  int<lower = 1, upper = N> id[S];       // student id
  int<lower = 1, upper = M> sid[N];      // school id
  int<lower = 1, upper = C> cycle[M];    // cycle id
  vector[C] a;
}

transformed data {
  vector[K] mu_x;
  vector[K] sd_x;
  matrix[K, S] x_std;
  vector[K1] mu_x1;
  vector[K1] sd_x1;
  matrix[K1, S] x1_std;
  vector[K2] mu_x2;
  vector[K2] sd_x2;
  matrix[K2, S] x2_std;
  
  real mu_y = mean(y);
  real sd_y = sd(y);
  vector[S] y_std = (y - mu_y) / sd_y;
  
  x_std[1, ] = to_row_vector(x[, 1]);
  for (k in 2:K) {
    mu_x[k] = mean(x[, k]);
    sd_x[k] = sd(x[, k]);
    x_std[k, ] = to_row_vector(x[, k] - mu_x[k]) / sd_x[k];
  }
  x1_std[1, ] = to_row_vector(x1[, 1]);
  for (k in 2:K1) {
    mu_x1[k] = mean(x1[, k]);
    sd_x1[k] = sd(x1[, k]);
    x1_std[k, ] = to_row_vector(x1[, k] - mu_x1[k]) / sd_x1[k];
  }
  x2_std[1, ] = to_row_vector(x2[, 1]);
  for (k in 2:K2) {
    mu_x2[k] = mean(x2[, k]);
    sd_x2[k] = sd(x2[, k]);
    x2_std[k, ] = to_row_vector(x2[, k] - mu_x2[k]) / sd_x2[k];
  }
}

parameters {
  vector[K] beta_std;

  vector[K1] u_std[N];
  cholesky_factor_corr[K1] L_omega_u;
  vector<lower = 0>[K1] tau_u_std;

  vector[K2] v_std[M];
  cholesky_factor_corr[K2] L_omega_v;
  vector<lower = 0>[K2] tau_v_std;
  
  real<lower = 0> sigma_r_std;
}

model {
  beta_std ~ normal(0, SIGMA_BETA);

  {
    matrix[K1, K1] L = diag_pre_multiply(tau_u_std, L_omega_u);
    for (n in 1:N)
      u_std[n] ~ multi_normal_cholesky(rep_vector(0, K1), L);
  }
  {
    matrix[K2, K2] L = diag_pre_multiply(tau_v_std, L_omega_v);
    for (m in 1:M)
      v_std[m] ~ multi_normal_cholesky(rep_vector(0, K2), L);
  }

  L_omega_u ~ OMEGA;
  tau_u_std ~ TAU;
  L_omega_v ~ OMEGA2;
  tau_v_std ~ TAU2;
  
  for (s in 1:S) {
    target += a[cycle[sid[id[s]]]] * normal_lpdf(y_std[s] | dot_product(x_std[, s], beta_std) +
                                                            dot_product(x1_std[, s], u_std[id[s]]) +
                                                            dot_product(x2_std[, s], v_std[sid[id[s]]]), sigma_r_std);
  }
  sigma_r_std ~ SIGMA_R;
}

generated quantities {
  vector[K] beta;
  real sigma_r = sigma_r_std * sd_y;
  real Sigma_r = sigma_r ^ 2;
  
  vector[NN] log_lik = rep_vector(0, NN);
  vector[MM] log_lik_sch = rep_vector(0, MM);
  
  matrix[K1, K1] Sigma_u = multiply_lower_tri_self_transpose(diag_pre_multiply(tau_u_std, L_omega_u)) * sd_y ^ 2;
  matrix[K2, K2] Sigma_v = multiply_lower_tri_self_transpose(diag_pre_multiply(tau_v_std, L_omega_v)) * sd_y ^ 2;
  
  for (i in 2:K1) {
    Sigma_u[1, i] /= sd_x1[i];
    Sigma_u[1, 1] -= 2 * mu_x1[i] * Sigma_u[1, i];
    for (j in 2:K1) {
      Sigma_u[i, j] /= sd_x1[i] * sd_x1[j];
      Sigma_u[1, i] -= Sigma_u[i, j] * mu_x1[j];
      Sigma_u[1, 1] += mu_x1[i] * mu_x1[j] * Sigma_u[i, j];
    }
    Sigma_u[i, 1] = Sigma_u[1, i];
  }
  
  for (i in 2:K2) {
    Sigma_v[1, i] /= sd_x2[i];
    Sigma_v[1, 1] -= 2 * mu_x2[i] * Sigma_v[1, i];
    for (j in 2:K2) {
      Sigma_v[i, j] /= sd_x2[i] * sd_x2[j];
      Sigma_v[1, i] -= Sigma_v[i, j] * mu_x2[j];
      Sigma_v[1, 1] += mu_x2[i] * mu_x2[j] * Sigma_v[i, j];
    }
    Sigma_v[i, 1] = Sigma_v[1, i];
  }
  
  beta[1] = sd_y * beta_std[1] + mu_y;
  for (k in 2:K) {
    beta[k] = sd_y / sd_x[k] * beta_std[k];
    beta[1] -= beta[k] * mu_x[k];
  }
  
  for (s in 1:SS) {
    int n = id[s];
    int m = sid[n];
    real ll = normal_lpdf(y[s] | dot_product(x_std[, s], beta_std) +
                                 dot_product(x1_std[, s], u_std[n]) +
                                 dot_product(x2_std[, s], v_std[m]), sigma_r);
    log_lik[n] += ll;
    log_lik_sch[m] += ll;
  }
}
