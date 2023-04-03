// generated with brms 2.18.0
functions {
  
}
data {
  int<lower=1> N; // total number of observations
  vector[N] Y; // response variable
  array[N] int<lower=-1, upper=2> cens; // indicates censoring
  vector[N] rcens; // right censor points for interval censoring
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
      // special treatment of censored data
      if (cens[n] == 0) {
        target += lognormal_lpdf(Y[n] | mu[n], sigma[n])
                  - log_diff_exp(lognormal_lcdf(ub[n] | mu[n], sigma[n]),
                                 lognormal_lcdf(lb[n] | mu[n], sigma[n]));
      } else if (cens[n] == 1) {
        target += lognormal_lccdf(Y[n] | mu[n], sigma[n])
                  - log_diff_exp(lognormal_lcdf(ub[n] | mu[n], sigma[n]),
                                 lognormal_lcdf(lb[n] | mu[n], sigma[n]));
      } else if (cens[n] == -1) {
        target += lognormal_lcdf(Y[n] | mu[n], sigma[n])
                  - log_diff_exp(lognormal_lcdf(ub[n] | mu[n], sigma[n]),
                                 lognormal_lcdf(lb[n] | mu[n], sigma[n]));
      } else if (cens[n] == 2) {
        target += log_diff_exp(lognormal_lcdf(rcens[n] | mu[n], sigma[n]),
                               lognormal_lcdf(Y[n] | mu[n], sigma[n]))
                  - log_diff_exp(lognormal_lcdf(ub[n] | mu[n], sigma[n]),
                                 lognormal_lcdf(lb[n] | mu[n], sigma[n]));
      }
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

