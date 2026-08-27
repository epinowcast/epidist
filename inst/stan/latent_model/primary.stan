  real dot_primary_raw_lpdf(vector raw, int primary_id,
                            array[] vector params, vector bound) {
    real total = 0;
    for (i in 1:num_elements(raw)) {
      array[size(params)] real p;
      for (k in 1:size(params)) {
        p[k] = params[k][i];
      }
      total += primary_lpdf(raw[i] * bound[i] | primary_id, p, 0, bound[i])
        + log(bound[i]);
    }
    return total;
  }
