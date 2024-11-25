// This function is a wrapper to primarycensored_lpmf
// Here the strings
// * family
// * dpars_A
// * dpars_1
// * dpars_2
// are/have been replaced using regex

real marginal_family_lpmf(data int d, dpars_A, data real pwindow) {
  int dist_id = input_dist_id;
  array[2] real params;
  params[1] = dpars_1;
  params[2] = dpars_2;
  int d_upper = d + 1;
  int primary_id = 1; // Fixed as uniform
  array[0] real primary_params;
  return primarycensored_lpmf(d | dist_id, params, pwindow, d_upper, positive_infinity(), primary_id, primary_params);
}
