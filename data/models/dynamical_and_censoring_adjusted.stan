// generated with brms 2.18.0
functions {
  
}
data {
  int<lower=1> N; // total number of observations
  vector[N] Y; // response variable
  array[N] int<lower=-1, upper=2> cens; // indicates censoring
  vector[N] rcens; // right censor points for interval censoring
  int prior_only; // should the likelihood be ignored?
  int tmin;
  int<lower=1> tlength; // time series length
  array[N] int stime_daily;
  array[tlength] real log_cases;
}
transformed data {
  
}
parameters {
  real Intercept; // temporary intercept for centered predictors
  real Intercept_sigma; // temporary intercept for centered predictors
}
transformed parameters {
  real lprior = 0; // prior contributions to the log posterior
  
  array[tlength] real cdenom;
  
  cdenom[1] = 0;
  
  for (i in 2 : tlength) {
    cdenom[i] = 0;
    for (j in 1 : (i - 1)) {
      if (j == 1) {
        cdenom[i] += exp(lognormal_lcdf(j | Intercept, exp(Intercept_sigma))
                         + log_cases[i - j]);
      } else {
        cdenom[i] += exp(log_diff_exp(lognormal_lcdf(j | Intercept, exp(Intercept_sigma)),
                                      lognormal_lcdf(j - 1 | Intercept, exp(Intercept_sigma)))
                         + log_cases[i - j]);
      }
    }
  }
  
  lprior += student_t_lpdf(Intercept | 3, 0, 2.5);
  lprior += student_t_lpdf(Intercept_sigma | 3, 0, 2.5);
}
model {
  if (!prior_only) {
    for (i in 1 : N) {
      target += -log(cdenom[stime_daily[i] - tmin + 1]);
    }
  }
  
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
        target += lognormal_lpdf(Y[n] | mu[n], sigma[n]);
      } else if (cens[n] == 1) {
        target += lognormal_lccdf(Y[n] | mu[n], sigma[n]);
      } else if (cens[n] == -1) {
        target += lognormal_lcdf(Y[n] | mu[n], sigma[n]);
      } else if (cens[n] == 2) {
        target += log_diff_exp(lognormal_lcdf(rcens[n] | mu[n], sigma[n]),
                               lognormal_lcdf(Y[n] | mu[n], sigma[n]));
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
  
  array[tlength] real backwardmean;
  
  for (i in 1 : tlength) {
    backwardmean[i] = 0;
    
    // this is quite approximate...
    // but sort of the best we can do without sacrificing a ton of
    // computational power
    for (j in 1 : (i - 1)) {
      backwardmean[i] += exp(log_diff_exp(lognormal_lcdf(j | Intercept, exp(Intercept_sigma)),
                                          lognormal_lcdf(j - 1 | Intercept, exp(Intercept_sigma)))
                             + log_cases[i - j] + log(j - 0.5)
                             - log(cdenom[i]));
    }
  }
}

