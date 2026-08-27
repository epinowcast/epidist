  real dot_expgrowth_raw_lpdf(vector raw, vector r, vector bound) {
    real total = 0;
    for (i in 1:num_elements(raw)) {
      total += expgrowth_lpdf(raw[i] | 0, 1, r[i] * bound[i]);
    }
    return total;
  }
