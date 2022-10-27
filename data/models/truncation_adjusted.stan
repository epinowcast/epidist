// generated with brms 2.18.0
functions {
  
}
data {
  int<lower=1> N; // total number of observations
  vector[N] Y; // response variable
  array[N] real lb; // lower truncation bounds;
  array[N] real ub; // upper truncation bounds
  int prior_only; // should the likelihood be ignored?
}
transformed data {
  
}
parameters {
  real Intercept; // temporary intercept for centered predictors
  real Intercept_sigma; // temporary intercept for centered predictors
}
transformed parameters {
  real lprior = 0; // prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept | 3, 0, 2.5);
  lprior += student_t_lpdf(Intercept_sigma | 3, 0, 2.5);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] sigma = rep_vector(0.0, N);
    mu += Intercept;
    sigma += Intercept_sigma;
    sigma = exp(sigma);
    for (n in 1 : N) {
      target += lognormal_lpdf(Y[n] | mu[n], sigma[n])
                - log_diff_exp(lognormal_lcdf(ub[n] | mu[n], sigma[n]),
                               lognormal_lcdf(lb[n] | mu[n], sigma[n]));
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

