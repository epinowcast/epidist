data {
  int<lower=1> N; // total number of observations
  int delay_daily[N];
  int stime_daily[N];
  
  int<lower=1> tlength; // time series length
  int tmin;
  
  int incidence_p[tlength];
}

parameters {
  real mu;
  real<lower=0> sigma;
}

transformed parameters {
  real cdenom[tlength];
  
  for (i in 1:tlength) {
    cdenom[i] = 0;
    for (j in 1:(i-1)) {
      if (j==i) {
        cdenom[i] += exp(lognormal_lcdf(j | mu, sigma) + log(incidence_p[i-j]));
      } else {
        cdenom[i] += exp(log_diff_exp(lognormal_lcdf(j | mu, sigma), lognormal_lcdf(j-1 | mu, sigma)) + log(incidence_p[i-j]));
      }
    }
  }
}

model {
  mu ~ normal(0, 10);
  sigma ~ normal(0, 10);
  
  for (i in 1:N) {
    target += lognormal_lpdf(delay_daily[i] | mu, sigma) - log(cdenom[stime_daily[i] - tmin + 1]);
  }
}

generated quantities {
  real backwardmean[tlength];
  
  for (i in 1:tlength) {
    backwardmean[i] = 0;
    
    for (j in 1:(i-1)) {
      backwardmean[i] += exp(lognormal_lpdf(j | mu, sigma) + log(incidence_p[i-j]) + log(j) - log(cdenom[i]));
    }
  }
}
