/**
  * Compute the log probability density function for a latent model with censoring
  *
  * This function is designed to be read into R where:
  * - 'family' is replaced with the target distribution (e.g., 'lognormal')
  * - 'dpars_A' is replaced with multiple parameters in the format
  *   "vector|real paramname1, vector|real paramname2, ..." depending on whether
  *   each parameter has a model. This includes distribution parameters.
  * - 'pwindow' and 'swindow' are replaced with vectors of window positions
  *   on a 0-1 scale
  *
  * @param y Vector of observed values (delays)
  * @param dpars_A Distribution parameters (replaced via regex)
  * @param relative_obs_t Array of observation times relative to primary window
  * start
  * @param pwindow_width Array of primary window widths (actual time scale)
  * @param swindow_width Array of secondary window widths (actual time scale)
  * @param woverlap Array of indices for overlapping windows
  * @param wN Number of overlapping windows
  *
  * @return Log probability density with censoring adjustment
  */
real latent_family_lpdf(vector y, dpars_A,
                        array[] real relative_obs_t,
                        array[] real pwindow_width, array[] real swindow_width,
                        array[] int woverlap, int wN) {
  int n = num_elements(y);
  vector[n] pwindow_adj = to_vector(pwindow_width) .* pwindow;
  vector[n] swindow_adj = to_vector(swindow_width) .* swindow;

  if (wN) {
    pwindow_adj[woverlap] = swindow_adj[woverlap] .* pwindow[woverlap];
  }
  vector[n] d = y - pwindow_adj + swindow_adj;
  vector[n] obs_time = to_vector(relative_obs_t) - pwindow_adj;
  return family_lpdf(d | dpars_B) - family_lcdf(obs_time | dpars_B);
}
