// Here the strings
// * family
// * dpars_A
// * dpars_B
// are/have been replaced using regex

real latent_family_lpdf(vector y, dpars_A, vector pwindow,
                           vector swindow array[] real relative_obs_t,
                           array[] real pwindow_width,
                           array[] real swindow_width,
                           int noverlap, array[] int woverlap) {
  int n = num_elements(y);
  vector<lower = 0>[N] pwindow_adj;
  vector<lower = 0>[N] swindow_adj =
    to_vector(swindow_width) .* swindow;
  pwindow_adj[noverlap] =
    to_vector(pwindow_width[noverlap]) .* pwindow[noverlap];
  if (wN) {
    pwindow_adj[woverlap] = swindow_adj[woverlap] .* pwindow[woverlap];
  }
  vector[n] d = y - pwindow_adj + swindow_adj;
  vector[n] obs_time = to_vector(relative_obs_t) - pwindow_adj;
  return family_lpdf(d | dpars_B) - family_lcdf(obs_time | dpars_B);
}
