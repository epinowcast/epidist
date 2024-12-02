/**
  * Compute the log probability mass function for a marginal model with censoring
  *
  * This function is designed to be read into R where:
  * - 'family' is replaced with the target distribution (e.g., 'lognormal')
  * - 'dpars_A' is replaced with multiple parameters in the format
  *   "vector|real paramname1, vector|real paramname2, ..." depending on whether
  *   each parameter has a model. This includes distribution parameters.
  * - 'dpars_B' is replaced with the same parameters as dpars_A but with window
  *   indices removed.
  *
  * @param y Real value of observed delay
  * @param dpars_A Distribution parameters (replaced via regex)
  * @param relative_obs_t Observation time relative to primary window start
  * @param pwindow_width Primary window width (actual time scale)
  * @param swindow_width Secondary window width (actual time scale)
  * @param y_upper Upper bound of delay interval
  * @param primary_params Array of parameters for primary distribution
  *
  * @return Log probability mass with censoring adjustment for marginal model
  */
  real marginal_family_lpmf(data int y, dpars_A, data real relative_obs_t,
                            data real pwindow_width, data real swindow_width,
                            data real y_upper, array[] real primary_params) {

  return primarycensored_lpmf(
      y | dist_id, {dpars_B}, pwindow_width, y_upper, relative_obs_t,
      primary_id, primary_params
    );
}
