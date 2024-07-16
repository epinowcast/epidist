// Here the strings
// * family
// * dpars_A
// * dpars_B
// are to be replaced using regex

real latent_family_lpdf(vector y, vector mu, dpars_A, vector pwindow,
                           vector swindow, array[] real obs_t) {
  int n = num_elements(y);
  vector[n] d = y - pwindow + swindow;
  vector[n] obs_time = to_vector(obs_t) - pwindow;
  return family_lpdf(d | mu, dpars_B) - family_lcdf(obs_time | mu, dpars_B);
}
