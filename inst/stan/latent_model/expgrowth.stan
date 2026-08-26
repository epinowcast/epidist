  // Unit scale offsets under an exponentially growing primary event. The
  // rate is scaled by each bound so the time scale distribution is right.
  // Follows the primarycensored paper.
  real dot_expgrowth_raw_lpdf(vector raw, vector r, vector bound) {
    real total = 0;
    for (i in 1:num_elements(raw)) {
      total += expgrowth_lpdf(raw[i] | 0, 1, r[i] * bound[i]);
    }
    return total;
  }
