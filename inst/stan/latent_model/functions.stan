/**
  * Compute the log probability density function for a latent model with censoring
  *
  * This function is designed to be read into R where:
  * - 'family' is replaced with the target distribution (e.g., 'lognormal')
  * - 'dpars_A' is replaced with multiple distribution parameters in the format
  *   "vector|real paramname1, vector|real paramname2, ..." depending on whether
  *   each parameter has a model.
  * - 'dpars_B' is replaced with the same parameters as dpars_A but
  *   reparameterised according to the brms parameterisation for Stan.
  *
  * @param y Vector of observed values (delays)
  * @param dpars_A Distribution parameters (replaced via regex)
  * @param relative_obs_t Array of observation times relative to primary window
  * start
  * @param pwindow_width Array of primary window widths (actual time scale)
  * @param swindow_width Array of secondary window widths (actual time scale)
  * @param pwindow_raw Vector of primary window positions (0-1 scale)
  * @param swindow_raw Vector of secondary window positions (0-1 scale)
  * @param woverlap Array of indices for overlapping windows
  * @param wN Number of overlapping windows
  *
  * @return Log probability density with censoring adjustment for latent model
  */
real latent_family_lpdf(vector y, dpars_A,
                        array[] real relative_obs_t,
                        array[] real pwindow_width, array[] real swindow_width,
                        vector pwindow_raw, vector swindow_raw,
                        array[] int woverlap, int wN) {
  int n = num_elements(y);
  vector[n] pwindow = to_vector(pwindow_width) .* pwindow_raw;
  vector[n] swindow = to_vector(swindow_width) .* swindow_raw;

  if (wN) {
    pwindow[woverlap] = swindow[woverlap] .* pwindow_raw[woverlap];
  }
  vector[n] d = y - pwindow + swindow;
  vector[n] obs_time = to_vector(relative_obs_t) - pwindow;
  return family_lpdf(d | dpars_B) - family_lcdf(obs_time | dpars_B);
}
