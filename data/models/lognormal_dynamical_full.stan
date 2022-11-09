data {
  int<lower=1> N; // total number of observations
  int delay_daily[N];
  int stime_daily[N];
  
  int<lower=1> tlength; // time series length
  int tmin;
  
  int incidence_p[tlength];
}

parameters {
  real<lower=0> lambda;
}

transformed parameters {
  real cdenom[tlength];
  
  for (i in 1:tlength) {
    cdenom[i] = 0;
    for (j in 0:(i-1)) {
      cdenom[i] += exp(poisson_lpmf(j | lambda) + log(incidence_p[i-j]));
    }
  }
}

model {
  lambda ~ normal(0, 10);
  
  for (i in 1:N) {
    target += poisson_lpmf(delay_daily[i] | lambda) - log(cdenom[stime_daily[i] - tmin + 1]);
  }
}

generated quantities {
  real backwardmean[tlength];
  
  for (i in 1:tlength) {
    backwardmean[i] = 0;
    
    for (j in 0:(i-1)) {
      backwardmean[i] += exp(poisson_lpmf(j | lambda) + log(incidence_p[i-j]) + log(j) - log(cdenom[i]));
    }
  }
}
