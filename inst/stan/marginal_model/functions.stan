real primarycensored_wrapper_lpmf(data int d, real mu, real sigma, data real pwindow) {
  int dist_id = 1; // lognormal
  array[2] real params = {mu, sigma};
  int d_upper = d + 1;
  int primary_id = 1; // Uniform
  array[0] real primary_params;
  return primarycensored_lpmf(d | dist_id, params, pwindow, d_upper, positive_infinity(), primary_id, primary_params);
}
