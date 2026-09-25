/**
  * Parameters of the non-parametric hazard delay distribution, in the layout
  * primarycensored expects for dist_id 27 and 28: the K + 1 bin boundaries
  * followed by the K bin hazards, the last of which is 1. Mirrors
  * .np_hazards() in R.
  *
  * @param boundaries Bin boundaries, b_0 to b_K.
  * @param mu Logit hazard shared by every bin.
  * @param sigma Spread of the logit hazard offsets.
  * @param eps Standard normal innovations of the offsets.
  * @param rw 1 for a random walk, where the first bin has no shift and the
  *   shift of each later bin is the sum of the innovations up to it, and 0
  *   for independent random effects, where each bin has its own innovation.
  *
  * @return An array of length 2 * K + 1.
  */
  array[] real epidist_np_params(data array[] real boundaries, real mu,
                                 real sigma, array[] real eps, data int rw) {
    int K = size(boundaries) - 1;
    vector[K - 1] shift;
    array[2 * K + 1] real params;
    if (rw == 1) {
      shift[1] = 0;
      shift[2:(K - 1)] = cumulative_sum(to_vector(eps));
    } else {
      shift = to_vector(eps);
    }
    params[1:(K + 1)] = boundaries;
    params[(K + 2):(2 * K)] = to_array_1d(inv_logit(mu + sigma * shift));
    params[2 * K + 1] = 1;
    return params;
  }
