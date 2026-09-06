/**
  * Generalised gamma delay distribution
  *
  * The Stacy parameterisation used by flexsurv::dgengamma.orig() in R and by
  * dist_id 5 of primarycensored in Stan, with the distributional parameters
  * in the order brms declares them for the epidist gengamma() family: mu is
  * the scale, shape the power parameter and k the shape of the underlying
  * gamma distribution. If G is gamma distributed with shape k and unit scale
  * then mu * G^(1 / shape) is generalised gamma, so the gamma (shape = 1) and
  * Weibull (k = 1) distributions are special cases.
  *
  * brms evaluates the naive model density one observation at a time, and the
  * latent model calls the density and the distribution function on vectors
  * whose parameters are vectors where they have a model and reals otherwise,
  * so each function is provided in every form the models need.
  *
  * @param y Delay (y > 0)
  * @param mu Scale parameter
  * @param shape Power parameter
  * @param k Shape parameter of the underlying gamma distribution
  */
real gengamma_lpdf(real y, real mu, real shape, real k) {
  real log_z = log(y / mu);
  return log(shape) - lgamma(k) + shape * k * log_z - log(y) -
    exp(shape * log_z);
}

real gengamma_lpdf(vector y, vector mu, vector shape, vector k) {
  vector[num_elements(y)] log_z = log(y ./ mu);
  return sum(log(shape) - lgamma(k) + shape .* k .* log_z - log(y) -
             exp(shape .* log_z));
}

real gengamma_lpdf(vector y, vector mu, real shape, real k) {
  int n = num_elements(y);
  return gengamma_lpdf(y | mu, rep_vector(shape, n), rep_vector(k, n));
}

real gengamma_lpdf(vector y, vector mu, vector shape, real k) {
  return gengamma_lpdf(y | mu, shape, rep_vector(k, num_elements(y)));
}

real gengamma_lpdf(vector y, vector mu, real shape, vector k) {
  return gengamma_lpdf(y | mu, rep_vector(shape, num_elements(y)), k);
}

/**
  * Log distribution function of the generalised gamma, the regularised lower
  * incomplete gamma function P(k, (y / mu)^shape), summed over the vector.
  */
real gengamma_lcdf(vector y, vector mu, vector shape, vector k) {
  return gamma_lcdf(exp(shape .* log(y ./ mu)) | k, 1);
}

real gengamma_lcdf(vector y, vector mu, real shape, real k) {
  int n = num_elements(y);
  return gengamma_lcdf(y | mu, rep_vector(shape, n), rep_vector(k, n));
}

real gengamma_lcdf(vector y, vector mu, vector shape, real k) {
  return gengamma_lcdf(y | mu, shape, rep_vector(k, num_elements(y)));
}

real gengamma_lcdf(vector y, vector mu, real shape, vector k) {
  return gengamma_lcdf(y | mu, rep_vector(shape, num_elements(y)), k);
}
