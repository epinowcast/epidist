functions {
  real denom(real x, real xc, real[] theta,
                      real[] x_r, int[] x_i) {
    real r = theta[1];
    real mu = theta[2];
    real sigma = theta[3];
    
    return exp(lognormal_lpdf(x| mu, sigma) - r * x);                    
  }
  
  real dynamical_lognormal_lpdf(real y, real mu, real sigma, real r,
                             real max_delay, data real[] x_r, data int[] x_i) {
    // it looks like we can skip - r * y because that's just a contant value???
    return lognormal_lpdf(y | mu, sigma) -
       log(integrate_1d(denom, 0, max_delay, {r, mu, sigma}, x_r, x_i, 1e-4));
  }
}

data {
  real r;
  real max_delay;
  int<lower=1> N; // total number of observations
  vector[N] Y; // response variable
}

transformed data {
  real x_r[0];
  int x_i[0];
}

parameters {
  real mu;
  real<lower = 0> sigma;
}

model {
  mu ~ normal(0, 10);
  sigma ~ normal(0, 10) T[0,];
  
  for (n in 1 : N) {
      target += dynamical_lognormal_lpdf(
        Y[n] | mu, sigma, r, max_delay, x_r, x_i
      );
  }
}
