data {
  int<lower=1> N; // total number of observations
  int delay_lwr[N];
  int delay_upr[N];
  int stime_daily[N];
  
  int<lower=1> tlength; // time series length
  int tmin;
  
  int cases[tlength];
}

parameters {
  real Intercept;
  real Intercept_sigma;
}

transformed parameters {
  real cdenom[tlength];
  
  cdenom[1] = 0;
  
  for (i in 2:tlength) {
    cdenom[i] = 0;
    for (j in 1:(i-1)) {
      if (j==1) {
        cdenom[i] += exp(lognormal_lcdf(j | Intercept, exp(Intercept_sigma)) + log(cases[i-j]));
      } else {
        cdenom[i] += exp(
          log_diff_exp(lognormal_lcdf(j | Intercept, exp(Intercept_sigma)), 
          lognormal_lcdf(j - 1 | Intercept, exp(Intercept_sigma))) + 
          log(cases[i-j]));
      }
    }
  }
}

model {
  target += student_t_lpdf(Intercept | 3, 0, 2.5);
  target += student_t_lpdf(Intercept_sigma | 3, 0, 2.5);
  
  for (i in 1:N) {
    target += log_diff_exp(lognormal_lcdf(delay_upr[i] | Intercept, exp(Intercept_sigma)),
                               lognormal_lcdf(delay_lwr[i] | Intercept, exp(Intercept_sigma)));
    target += - log(cdenom[stime_daily[i] - tmin + 1]);
  }
}

generated quantities {
  real backwardmean[tlength];
  
  for (i in 1:tlength) {
    backwardmean[i] = 0;
    
    // this is quite approximate...
    // but sort of the best we can do without sacrificing a ton of computational power
    for (j in 1:(i-1)) {
      backwardmean[i] += exp(log_diff_exp(lognormal_lcdf(j | Intercept, exp(Intercept_sigma)), 
          lognormal_lcdf(j - 1 | Intercept, exp(Intercept_sigma))) + log(cases[i-j]) + log(j-0.5) - log(cdenom[i]));
    }
  }
}
