/**
  * Parameters of the non-parametric hazard delay distribution, in the layout
  * primarycensored expects for dist_id 27: the K + 1 bin boundaries followed
  * by the K bin hazards, the last of which is 1. The logit hazards of the
  * other bins are mu plus the basis of the hazard formula times its
  * coefficients. Mirrors .np_hazards() in R.
  *
  * @param boundaries Bin boundaries, b_0 to b_K.
  * @param mu Mean logit hazard over the bins.
  * @param basis Basis of the hazard formula, one row per bin but the last.
  * @param coefs Coefficients of the basis.
  *
  * @return An array of length 2 * K + 1.
  */
  array[] real epidist_np_params(data array[] real boundaries, real mu,
                                 data matrix basis, vector coefs) {
    int K = size(boundaries) - 1;
    array[2 * K + 1] real params;
    params[1:(K + 1)] = boundaries;
    params[(K + 2):(2 * K)] = to_array_1d(inv_logit(mu + basis * coefs));
    params[2 * K + 1] = 1;
    return params;
  }
