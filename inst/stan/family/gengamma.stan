/**
  * Generalised gamma delay distribution
  *
  * The Prentice parameterisation used by flexsurv::dgengamma() in R, with the
  * distributional parameters in the order brms declares them for the epidist
  * gengamma() family: mu and sigma are the location and scale of the log
  * delay and Q > 0 is the shape. With w = (log(y) - mu) / sigma and
  * u = exp(Q * w) / Q^2, u is gamma distributed with shape 1 / Q^2 and unit
  * scale, so the Weibull (Q = 1) and gamma (Q = sigma) distributions are
  * special cases and the lognormal is the limit as Q goes to zero.
  *
  * brms evaluates the naive model density one observation at a time, and the
  * latent model calls the density and the distribution function on vectors
  * whose parameters are vectors where they have a model and reals otherwise,
  * so each function is provided in every form the models need. There is no
  * scalar gengamma_lcdf(), because no model calls one and primarycensored
  * defines gengamma_lcdf(real y, real shape, real scale, real k) for the
  * Stacy parameterisation, which the marginal and meta models use.
  *
  * @param y Delay (y > 0)
  * @param mu Location of the log delay
  * @param sigma Scale of the log delay
  * @param Q Shape parameter
  */
real gengamma_lpdf(real y, real mu, real sigma, real Q) {
  real q2 = inv_square(Q);
  real x = Q * (log(y) - mu) / sigma;
  // The density is log(Q) + q2 * log(q2) - lgamma(q2) + x * q2 -
  // q2 * exp(x) - log(sigma * y). It is rearranged so that it stays accurate
  // as Q goes to zero and the density to the lognormal: exp(x) - 1 - x is
  // taken from its series when x is small, and log(Q) + q2 * log(q2) -
  // lgamma(q2) - q2 from Stirling's series when q2 is large, where it is
  // -log(2 * pi) / 2 - 1 / (12 * q2) + 1 / (360 * q2^3).
  real excess;
  real log_norm;
  if (abs(x) < 1e-3) {
    excess = square(x) * (0.5 + x / 6 + square(x) / 24);
  } else {
    excess = expm1(x) - x;
  }
  if (q2 > 500) {
    log_norm = -0.5 * log(2 * pi()) - inv(12 * q2) + inv(360 * q2 ^ 3);
  } else {
    log_norm = log(Q) + q2 * log(q2) - lgamma(q2) - q2;
  }
  return log_norm - log(sigma) - log(y) - q2 * excess;
}

real gengamma_lpdf(vector y, vector mu, vector sigma, vector Q) {
  real lpdf = 0;
  for (n in 1:num_elements(y)) {
    lpdf += gengamma_lpdf(y[n] | mu[n], sigma[n], Q[n]);
  }
  return lpdf;
}

real gengamma_lpdf(vector y, vector mu, real sigma, real Q) {
  int n = num_elements(y);
  return gengamma_lpdf(y | mu, rep_vector(sigma, n), rep_vector(Q, n));
}

real gengamma_lpdf(vector y, vector mu, vector sigma, real Q) {
  return gengamma_lpdf(y | mu, sigma, rep_vector(Q, num_elements(y)));
}

real gengamma_lpdf(vector y, vector mu, real sigma, vector Q) {
  return gengamma_lpdf(y | mu, rep_vector(sigma, num_elements(y)), Q);
}

/**
  * Log of the regularised lower incomplete gamma function P(k, x), given
  * log(x). Deep in the lower tail P(k, x) underflows to zero, so there it is
  * computed from its series, x^k exp(-x) / Gamma(k + 1) times a sum that is
  * close to 1 / (1 - x / (k + 1)) when x is small against k. Without this a
  * log distribution function of minus infinity in the latent model's
  * truncation adjustment makes the log posterior plus infinity.
  *
  * @param log_x Log of the argument of the gamma distribution function
  * @param k Shape parameter of the gamma distribution
  */
real gengamma_log_gamma_p(real log_x, real k) {
  real x = exp(log_x);
  real log_lead = k * log_x - x - lgamma(k + 1);
  if (x < 0.5 * (k + 1) && log_lead < -600) {
    return log_lead - log1m(x / (k + 1));
  }
  return gamma_lcdf(x | k, 1);
}

/**
  * Log distribution function of the generalised gamma, the regularised lower
  * incomplete gamma function P(1 / Q^2, exp(Q * w) / Q^2), summed over the
  * vector.
  *
  * The gradient of gamma_lcdf() with respect to its shape is not a number
  * once the shape reaches about 1000, so where 1 / Q^2 is above 500 the
  * Wilson and Hilferty (1931) normal approximation to the gamma distribution
  * function is used instead. It is accurate to about 1e-6 in the bulk there
  * and tends to the lognormal distribution function as Q goes to zero.
  */
real gengamma_lcdf(vector y, vector mu, vector sigma, vector Q) {
  vector[num_elements(y)] q2 = inv_square(Q);
  vector[num_elements(y)] w = (log(y) - mu) ./ sigma;
  real lcdf = 0;
  for (n in 1:num_elements(y)) {
    if (q2[n] > 500) {
      real z = (expm1(Q[n] * w[n] / 3) + square(Q[n]) / 9) * 3 / Q[n];
      lcdf += std_normal_lcdf(z);
    } else {
      lcdf += gengamma_log_gamma_p(log(q2[n]) + Q[n] * w[n], q2[n]);
    }
  }
  return lcdf;
}

real gengamma_lcdf(vector y, vector mu, real sigma, real Q) {
  int n = num_elements(y);
  return gengamma_lcdf(y | mu, rep_vector(sigma, n), rep_vector(Q, n));
}

real gengamma_lcdf(vector y, vector mu, vector sigma, real Q) {
  return gengamma_lcdf(y | mu, sigma, rep_vector(Q, num_elements(y)));
}

real gengamma_lcdf(vector y, vector mu, real sigma, vector Q) {
  return gengamma_lcdf(y | mu, rep_vector(sigma, num_elements(y)), Q);
}
