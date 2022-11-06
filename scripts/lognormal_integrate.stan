functions {
  real denom(real x, real xc, real[] theta,
                      real[] x_r, int[] x_i) {
    real r = theta[1];
    real mu = theta[2];
    real sigma = theta[3];
    
    return exp(lognormal_lpdf(x| mu, sigma) - r * x);                    
  }
}

data {
  real r;
  real mu;
  real sigma;
  real max_delay;
}

transformed data {
  real x_r[0];
  int x_i[0];
}

parameters {
  real<lower=0> z; // stan won't sample without a parameter... so adding a dummy parameter
}

model {
  z ~ uniform(0, 1);
}

generated quantities {
  real res = log(integrate_1d(denom, 0, max_delay, {r, mu, sigma}, x_r, x_i, 1e-8)); 
}
