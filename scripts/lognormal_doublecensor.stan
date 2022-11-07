data {
  int<lower=1> N; //number of data
  real end_t;
  vector[N] ptime_lwr; //lower bound for primary event time
  vector[N] ptime_upr; //upper bound for primary event time
  vector[N] stime_lwr; //lower bound for secondary event time
  vector[N] stime_upr; //upper bound for secondary event time
}

parameters {
  real mu;
  real logsigma;
  vector<lower=0, upper=1>[N] ptime_uniform;
  vector<lower=0, upper=1>[N] stime_uniform;
}

transformed parameters {
  vector[N] ptime;
  vector[N] stime;
  vector[N] delay;
  vector[N] obs_time;
  real<lower=0> sigma;
  
  ptime = ptime_uniform .* (ptime_upr-ptime_lwr) + ptime_lwr;
  stime = stime_uniform .* (stime_upr-stime_lwr) + stime_lwr;
  delay = stime-ptime;
  obs_time = end_t - ptime;
  sigma = exp(logsigma);
}

model {
  //priors
  ptime_uniform ~ uniform(0, 1);
  stime_uniform ~ uniform(0, 1);
  
  target += student_t_lpdf(mu | 3, 1, 2.5);
  target += student_t_lpdf(logsigma | 3, 0, 2.5);
  target += lognormal_lpdf(delay|mu, sigma) - lognormal_lcdf(obs_time| mu, sigma);
}

generated quantities {
  real lognormal_mean;
  
  lognormal_mean = exp(mu + sigma^2/2);
}
