// generated with brms 2.18.0
functions {
  real latent_lognormal_lpdf(real y, real mu, real sigma, real pwindow,
                             real swindow, real stime, real obs_t) {
    real p = y + pwindow;
    real s = stime + swindow;
    real d = s - p;
    real obs_time = obs_t - p;
    return lognormal_lpdf(d | mu, sigma)
           - lognormal_lcdf(obs_time | mu, sigma);
  }
}
data {
  int<lower=1> N; // total number of observations
  vector[N] Y; // response variable
  // data for custom real vectors
  array[N] real vreal1;
  // data for custom real vectors
  array[N] real vreal2;
  int<lower=1> K_pwindow; // number of population-level effects
  matrix[N, K_pwindow] X_pwindow; // population-level design matrix
  int<lower=1> K_swindow; // number of population-level effects
  matrix[N, K_swindow] X_swindow; // population-level design matrix
  int prior_only; // should the likelihood be ignored?
}
transformed data {
  
}
parameters {
  real Intercept; // temporary intercept for centered predictors
  real Intercept_sigma; // temporary intercept for centered predictors
  vector<lower=0, upper=1>[K_pwindow] b_pwindow; // population-level effects
  vector<lower=0, upper=1>[K_swindow] b_swindow; // population-level effects
}
transformed parameters {
  real lprior = 0; // prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept | 3, 1, 2.5);
  lprior += student_t_lpdf(Intercept_sigma | 3, 0, 2.5);
  lprior += uniform_lpdf(b_pwindow | 0, 1)
            - 1
              * log_diff_exp(uniform_lcdf(1 | 0, 1), uniform_lcdf(0 | 0, 1));
  lprior += uniform_lpdf(b_swindow | 0, 1)
            - 1
              * log_diff_exp(uniform_lcdf(1 | 0, 1), uniform_lcdf(0 | 0, 1));
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] sigma = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] pwindow = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] swindow = rep_vector(0.0, N);
    mu += Intercept;
    sigma += Intercept_sigma;
    pwindow += X_pwindow * b_pwindow;
    swindow += X_swindow * b_swindow;
    sigma = exp(sigma);
    for (n in 1 : N) {
      target += latent_lognormal_lpdf(Y[n] | mu[n], sigma[n], pwindow[n], swindow[n], vreal1[n], vreal2[n]);
    }
  }
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
  // actual population-level intercept
  real b_sigma_Intercept = Intercept_sigma;
}

