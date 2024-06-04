real latent_lognormal_lpdf(vector y, vector mu, vector sigma,
                           vector pwindow, vector swindow,
                           array[] real obs_t) {
  int n = num_elements(y);
  vector[n] d = y - pwindow + swindow;
  vector[n] obs_time = to_vector(obs_t) - pwindow;
  return lognormal_lpdf(d | mu, sigma) -
    lognormal_lcdf(obs_time | mu, sigma);
  }
