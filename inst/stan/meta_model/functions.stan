/**
  * Log probability mass functions for the meta model
  *
  * This file is designed to be read into R where:
  * - 'family' is replaced with the target distribution (e.g., 'lognormal')
  * - 'dist_id' is replaced with the primarycensored distribution identifier
  * - 'dpars_A' is replaced with multiple distribution parameters in the format
  *   "real paramname1, real paramname2, ...".
  * - 'dpars_B' is replaced with the same parameters as dpars_A but
  *   reparameterised according to the brms parameterisation for Stan.
  * - 'primary_id' is replaced with the primary distribution identifier used
  *   for individual level rows.
  * - 'n_quad_default' is replaced with the number of quadrature intervals
  *   used for truncated continuous moments, which is `.meta_n_quad()` in R.
  *
  * Summaries are returned as a vector of the mean, the standard deviation,
  * the kurtosis and the skewness of the biased estimand. The kurtosis is
  * needed because the sampling error of a reported standard deviation depends
  * on it, and the skewness because the sampling covariance of a reported mean
  * and a reported standard deviation from the same study depends on it.
  *
  * Every summary is conditioned on the delay exceeding `delay_min`, the
  * study's left truncation point, which is zero for a study that counted
  * every delay.
  */

  /** Package a mean and central moments into a summary vector. */
  vector meta_family_moment_vector(real delay_mean, real variance, real third,
                                   real fourth) {
    real safe_variance = fmax(variance, 1e-10);
    return [delay_mean, sqrt(safe_variance),
            fmax(fourth / safe_variance ^ 2, 1),
            third / pow(safe_variance, 1.5)]';
  }

  /** Convert the first four raw moments into a summary vector. */
  vector meta_family_central_from_raw(vector raw) {
    real m1 = raw[1];
    real variance = raw[2] - m1 ^ 2;
    real third = raw[3] - 3 * m1 * raw[2] + 2 * m1 ^ 3;
    real fourth = raw[4] - 4 * m1 * raw[3] + 6 * m1 ^ 2 * raw[2] - 3 * m1 ^ 4;
    return meta_family_moment_vector(m1, variance, third, fourth);
  }

  /** Difference of two exponentiated logs, guarded against underflow. */
  /**
    * The censoring adjustment whose estimand a code is built on. Midpoint
    * imputation of the secondary interval (3) shifts the naive discrete grid
    * of code 0, and midpoint imputation of the primary event (4) shifts the
    * primary censored estimand of code 2. Mirrors .meta_cens_base() in R.
    */
  int meta_family_cens_base(data int cens_adj) {
    if (cens_adj == 3) {
      return 0;
    }
    if (cens_adj == 4) {
      return 2;
    }
    return cens_adj;
  }

  /**
    * The delay a midpoint imputation moves the base estimand by. Mirrors
    * .meta_cens_shift() in R.
    */
  real meta_family_shift(data int cens_adj, data real pwindow_width,
                         data real swindow_width) {
    if (cens_adj == 3) {
      return swindow_width / 2;
    }
    if (cens_adj == 4) {
      return -pwindow_width / 2;
    }
    return 0;
  }

  real meta_family_diff_exp(real log_upper, real log_lower) {
    if (is_inf(log_upper)) {
      return 0;
    }
    if (is_inf(log_lower)) {
      return exp(log_upper);
    }
    if (log_upper <= log_lower) {
      return 0;
    }
    return exp(log_diff_exp(log_upper, log_lower));
  }

  /** Log follow up available to a delay under an accrual design. */
  real meta_family_log_accrual_weight(data real d, data real window,
                                      data real r) {
    real remaining = fmax(window - d, 0);
    if (remaining <= 0) {
      return negative_infinity();
    }
    if (r == 0) {
      return log(remaining);
    }
    if (r > 0) {
      real scaled = r * remaining;
      return scaled + log1m_exp(-scaled) - log(r);
    }
    return log1m_exp(r * remaining) - log(-r);
  }

  /** Reweight a distribution function for an accrual design. */
  vector meta_family_accrual_reweight(vector cdf, data real delay_min,
                                      data real cutoff, data real r,
                                      data real weight_offset) {
    int n_quad = num_elements(cdf) - 1;
    vector[n_quad] weight;
    vector[n_quad] mass;
    real total;
    if (r == 0) {
      for (i in 1:n_quad) {
        weight[i] = fmax(
          cutoff -
            (delay_min + (i - 0.5) * (cutoff - delay_min) / n_quad -
             weight_offset), 0
        );
      }
      if (max(weight) <= 0) {
        return cdf;
      }
      weight = weight / max(weight);
    } else {
      vector[n_quad] log_weight;
      for (i in 1:n_quad) {
        log_weight[i] = meta_family_log_accrual_weight(
          delay_min + (i - 0.5) * (cutoff - delay_min) / n_quad -
            weight_offset, cutoff, r
        );
      }
      weight = exp(log_weight - max(log_weight));
    }
    mass = fmax(cdf[2:(n_quad + 1)] - cdf[1:n_quad], 0) .* weight;
    total = sum(mass);
    if (total <= 0 || is_nan(total)) {
      return cdf;
    }
    return append_row(0, cumulative_sum(mass) / total);
  }

  /** Log primary censored distribution function, guarded against underflow. */
  real meta_family_pcens_lcdf(data real d, array[] real params,
                              data real pwindow_width, data int prim_id,
                              array[] real prim_params) {
    real log_cdf;
    if (d <= 0) {
      return negative_infinity();
    }
    log_cdf = primarycensored_lcdf(
      d | dist_id, params, pwindow_width, 0, positive_infinity(), prim_id,
      prim_params
    );
    if (is_nan(log_cdf)) {
      return negative_infinity();
    }
    return log_cdf;
  }

  /** Index of the first grid cell a left truncated study could have seen. */
  int meta_family_grid_first(data real delay_min, data real swindow_width) {
    return to_int(ceil(delay_min / swindow_width - 1e-9));
  }

  /**
    * Discrete delay distribution a study using date differences would
    * observe. Doubly interval censored delay distribution as in
    * primarycensored, following Park et al. (2024) and Charniga et al.
    * (2024). See the model guide vignette for the maths. Cells recording a
    * delay below `delay_min` are dropped and the rest renormalised, which
    * conditions the grid on the study's left truncation point.
    */
  vector meta_family_grid_pmf(array[] real params, data real delay_min,
                              data real cutoff, data real pwindow_width,
                              data real swindow_width, data int prim_id,
                              array[] real prim_params, data int accrual,
                              data real growth_rate) {
    int n_grid = to_int(floor(cutoff / swindow_width));
    int first = meta_family_grid_first(delay_min, swindow_width);
    int n_cell = n_grid - first;
    if (n_cell < 1) {
      reject("meta_family_grid_pmf: the study's grid holds no cells between ",
             "its minimum delay and its cutoff.");
    }
    {
      vector[n_cell + 1] log_cdf;
      vector[n_cell] log_mass;
      for (j in 0:n_cell) {
        log_cdf[j + 1] = meta_family_pcens_lcdf(
          (first + j) * swindow_width | params, pwindow_width, prim_id,
          prim_params
        );
      }
      for (j in 1:n_cell) {
        // Once the distribution function saturates its log stops increasing,
        // so the cell holds no mass a double can represent. Taking the
        // difference anyway would return NaN.
        if (log_cdf[j + 1] <= log_cdf[j]) {
          log_mass[j] = negative_infinity();
        } else {
          log_mass[j] = log_diff_exp(log_cdf[j + 1], log_cdf[j]);
        }
      }
      if (accrual == 0) {
        real total = meta_family_diff_exp(log_cdf[n_cell + 1], log_cdf[1]);
        if (total <= 0) {
          reject("meta_family_grid_pmf: every grid cell underflowed to zero ",
                 "probability; the delay distribution puts no mass within ",
                 "the study's grid.");
        }
        return exp(log_mass - log(total));
      }
      for (j in 1:n_cell) {
        log_mass[j] += meta_family_log_accrual_weight(
          (first + j - 1) * swindow_width, cutoff, growth_rate
        );
      }
      if (max(log_mass) == negative_infinity()) {
        reject("meta_family_grid_pmf: every grid cell underflowed to zero ",
               "probability; the delay distribution puts no mass within the ",
               "study's grid.");
      }
      return softmax(log_mass);
    }
  }

  /** Cohort grid distribution function at the two edges of one cell. */
  vector meta_family_grid_edges(data int cell, array[] real params,
                                data real delay_min, data real cutoff,
                                data real pwindow_width,
                                data real swindow_width, data int prim_id,
                                array[] real prim_params) {
    int n_grid = to_int(floor(cutoff / swindow_width));
    int first = meta_family_grid_first(delay_min, swindow_width);
    real log_top = meta_family_pcens_lcdf(
      n_grid * swindow_width | params, pwindow_width, prim_id, prim_params
    );
    real log_base = first > 0
      ? meta_family_pcens_lcdf(
          first * swindow_width | params, pwindow_width, prim_id, prim_params
        )
      : negative_infinity();
    real total = meta_family_diff_exp(log_top, log_base);
    real log_lower;
    real upper_mass;
    if (total <= 0) {
      reject("meta_family_grid_edges: the delay distribution puts no mass ",
             "within the study's grid.");
    }
    log_lower = cell > first
      ? meta_family_pcens_lcdf(
          cell * swindow_width | params, pwindow_width, prim_id, prim_params
        )
      : negative_infinity();
    upper_mass = cell + 1 >= n_grid
      ? total
      : meta_family_diff_exp(
          meta_family_pcens_lcdf(
            (cell + 1) * swindow_width | params, pwindow_width, prim_id,
            prim_params
          ),
          log_base
        );
    return [meta_family_diff_exp(log_lower, log_base) / total,
            upper_mass / total]';
  }

  /** Summaries of a discrete delay grid starting at `first_delay`. */
  vector meta_family_grid_moments(vector mass, data real first_delay,
                                  data real swindow_width) {
    int n_grid = num_elements(mass);
    vector[n_grid] delay = first_delay +
      linspaced_vector(n_grid, 0, n_grid - 1) * swindow_width;
    real m1 = dot_product(mass, delay);
    vector[n_grid] centred = delay - m1;
    vector[n_grid] squared = centred .* centred;
    real variance = dot_product(mass, squared);
    real third = dot_product(mass, squared .* centred);
    real fourth = dot_product(mass, squared .* squared);
    return meta_family_moment_vector(m1, variance, third, fourth);
  }

  /** Analytic summaries of the delay distribution. */
  vector meta_family_moments(array[] real params) {
    real delay_mean;
    real variance;
    real third;
    real fourth;
    if (dist_id == 1) {
      real var_log = params[2] ^ 2;
      delay_mean = exp(params[1] + var_log / 2);
      variance = delay_mean ^ 2 * expm1(var_log);
      third = (exp(var_log) + 2) * sqrt(expm1(var_log)) *
        pow(variance, 1.5);
      fourth = (exp(4 * var_log) + 2 * exp(3 * var_log) +
                3 * exp(2 * var_log) - 3) * variance ^ 2;
    } else if (dist_id == 2) {
      delay_mean = params[1] / params[2];
      variance = params[1] / params[2] ^ 2;
      third = 2 / sqrt(params[1]) * pow(variance, 1.5);
      fourth = (3 + 6 / params[1]) * variance ^ 2;
    } else if (dist_id == 3) {
      real g1 = tgamma(1 + 1 / params[1]);
      real g2 = tgamma(1 + 2 / params[1]);
      real g3 = tgamma(1 + 3 / params[1]);
      real g4 = tgamma(1 + 4 / params[1]);
      delay_mean = params[2] * g1;
      variance = params[2] ^ 2 * (g2 - g1 ^ 2);
      third = params[2] ^ 3 * (g3 - 3 * g1 * g2 + 2 * g1 ^ 3);
      fourth = params[2] ^ 4 *
        (g4 - 4 * g1 * g3 + 6 * g1 ^ 2 * g2 - 3 * g1 ^ 4);
    } else {
      reject("Meta model summary rows support lognormal, gamma and weibull ",
             "delay distributions only.");
    }
    return meta_family_moment_vector(delay_mean, variance, third, fourth);
  }

  /**
    * Summaries implied by a distribution function evaluated on a grid
    * running from `delay_min` to `cutoff`, by Simpson's rule on the truncated
    * survival integrals. Left truncation adds the boundary term
    * `delay_min ^ k (F(cutoff) - F(delay_min))` to each raw moment, which
    * vanishes when `delay_min` is zero. The grid must have an even number of
    * intervals.
    */
  vector meta_family_survival_moments(vector cdf, data real delay_min,
                                      data real cutoff) {
    int n_quad = num_elements(cdf) - 1;
    vector[n_quad + 1] grid = linspaced_vector(n_quad + 1, delay_min, cutoff);
    vector[n_quad + 1] weight = rep_vector(2, n_quad + 1);
    vector[n_quad + 1] tail_prob = cdf[n_quad + 1] - cdf;
    real denom = cdf[n_quad + 1] - cdf[1];
    vector[4] raw;
    weight[1] = 1;
    weight[n_quad + 1] = 1;
    for (i in 2:n_quad) {
      if (i % 2 == 0) {
        weight[i] = 4;
      }
    }
    if (denom <= 0) {
      reject("meta_family_survival_moments: the truncated distribution ",
             "function underflowed to zero over the study's delay range.");
    }
    for (k in 1:4) {
      raw[k] = pow(delay_min, k) +
        dot_product(weight, k * pow(grid, k - 1) .* tail_prob) *
        (cutoff - delay_min) / (3.0 * n_quad * denom);
    }
    return meta_family_central_from_raw(raw);
  }

  /** Summaries of a right truncated delay distribution. */
  vector meta_family_trunc_moments(array[] real params, data real delay_min,
                                   data real cutoff, data int n_quad,
                                   data int accrual, data real growth_rate) {
    vector[n_quad + 1] grid = linspaced_vector(n_quad + 1, delay_min, cutoff);
    vector[n_quad + 1] cdf;
    for (i in 1:(n_quad + 1)) {
      cdf[i] = grid[i] <= 0 ? 0 : exp(dist_lcdf(grid[i] | params, dist_id));
    }
    if (accrual == 1) {
      return meta_family_survival_moments(
        meta_family_accrual_reweight(cdf, delay_min, cutoff, growth_rate, 0),
        delay_min, cutoff
      );
    }
    return meta_family_survival_moments(cdf, delay_min, cutoff);
  }

  /** Summaries of a right truncated primary censored delay distribution. */
  vector meta_family_pcens_trunc_moments(array[] real params,
                                         data real delay_min,
                                         data real cutoff,
                                         data real pwindow_width,
                                         data int prim_id,
                                         array[] real prim_params,
                                         data int n_quad, data int accrual,
                                         data real growth_rate) {
    vector[n_quad + 1] cdf;
    for (i in 1:(n_quad + 1)) {
      cdf[i] = exp(meta_family_pcens_lcdf(
        delay_min + (i - 1) * (cutoff - delay_min) / n_quad | params,
        pwindow_width, prim_id, prim_params
      ));
    }
    if (accrual == 1) {
      return meta_family_survival_moments(
        meta_family_accrual_reweight(
          cdf, delay_min, cutoff, growth_rate, pwindow_width / 2
        ),
        delay_min, cutoff
      );
    }
    return meta_family_survival_moments(cdf, delay_min, cutoff);
  }

  /**
    * Add an independent uniform primary window to a set of summaries, exact
    * when the study also adjusted for right truncation.
    */
  vector meta_family_add_uniform(vector moments, data real pwindow_width) {
    real var_delay = moments[2] ^ 2;
    real third_delay = moments[4] * pow(var_delay, 1.5);
    real fourth_delay = moments[3] * var_delay ^ 2;
    real var_window = pwindow_width ^ 2 / 12;
    real fourth_window = pwindow_width ^ 4 / 80;
    return meta_family_moment_vector(
      moments[1] + pwindow_width / 2,
      var_delay + var_window,
      third_delay,
      fourth_delay + 6 * var_delay * var_window + fourth_window
    );
  }

  /** The summaries a study using a given procedure would report. */
  vector meta_family_implied_moments(array[] real params, data real delay_min,
                                     data real cutoff,
                                     data real pwindow_width,
                                     data real swindow_width,
                                     data int trunc_adj, data int cens_adj,
                                     data int prim_id,
                                     array[] real prim_params,
                                     data int accrual,
                                     data real growth_rate) {
    if (cens_adj == 3 || cens_adj == 4) {
      // Midpoint imputation moves the base estimand along the delay axis, so
      // its mean moves and every central moment is unchanged.
      vector[4] moments = meta_family_implied_moments(
        params, delay_min, cutoff, pwindow_width, swindow_width, trunc_adj,
        meta_family_cens_base(cens_adj), prim_id, prim_params, accrual,
        growth_rate
      );
      moments[1] += meta_family_shift(cens_adj, pwindow_width, swindow_width);
      return moments;
    }
    if (cens_adj == 0) {
      int first = meta_family_grid_first(delay_min, swindow_width);
      return meta_family_grid_moments(
        meta_family_grid_pmf(params, delay_min, cutoff, pwindow_width,
                             swindow_width, prim_id, prim_params, accrual,
                             growth_rate),
        first * swindow_width, swindow_width
      );
    }
    if (cens_adj == 2) {
      if (trunc_adj == 1 && prim_id == 1 && delay_min == 0) {
        return meta_family_add_uniform(meta_family_moments(params),
                                       pwindow_width);
      }
      return meta_family_pcens_trunc_moments(
        params, delay_min, cutoff, pwindow_width, prim_id, prim_params,
        n_quad_default, accrual, growth_rate
      );
    }
    if (trunc_adj == 1 && delay_min == 0) {
      return meta_family_moments(params);
    }
    return meta_family_trunc_moments(params, delay_min, cutoff, n_quad_default,
                                     accrual, growth_rate);
  }

  /**
    * Continuity corrected distribution function of a discrete delay grid,
    * interpolated linearly through the cell mid points.
    */
  real meta_family_grid_prob(data real y, array[] real params,
                             data real delay_min, data real cutoff,
                             data real pwindow_width,
                             data real swindow_width, data int prim_id,
                             array[] real prim_params, data int accrual,
                             data real growth_rate) {
    int n_grid = to_int(floor(cutoff / swindow_width));
    int first = meta_family_grid_first(delay_min, swindow_width);
    int cell = to_int(floor(y / swindow_width + 0.5));
    real frac = y / swindow_width + 0.5 - cell;
    if (cell < first) {
      return 0;
    }
    if (cell >= n_grid) {
      return 1;
    }
    if (accrual == 0) {
      vector[2] edges = meta_family_grid_edges(
        cell, params, delay_min, cutoff, pwindow_width, swindow_width, prim_id,
        prim_params
      );
      return edges[1] * (1 - frac) + edges[2] * frac;
    }
    {
      vector[n_grid - first + 1] cdf = append_row(0, cumulative_sum(
        meta_family_grid_pmf(params, delay_min, cutoff, pwindow_width,
                             swindow_width, prim_id, prim_params, accrual,
                             growth_rate)
      ));
      return cdf[cell - first + 1] * (1 - frac) +
        cdf[cell - first + 2] * frac;
    }
  }

  /** Accrual weighted distribution function on the quadrature grid. */
  vector meta_family_accrual_nodes(array[] real params, data real delay_min,
                                 data real cutoff,
                                 data real pwindow_width, data int cens_adj,
                                 data int prim_id, array[] real prim_params,
                                 data real growth_rate, data int n_quad) {
    vector[n_quad + 1] cdf;
    for (i in 1:(n_quad + 1)) {
      if (cens_adj == 2) {
        cdf[i] = exp(meta_family_pcens_lcdf(
          delay_min + (i - 1) * (cutoff - delay_min) / n_quad | params,
          pwindow_width, prim_id, prim_params
        ));
      } else if (delay_min + (i - 1) * (cutoff - delay_min) / n_quad <= 0) {
        cdf[i] = 0;
      } else {
        cdf[i] = exp(dist_lcdf(
          delay_min + (i - 1) * (cutoff - delay_min) / n_quad | params, dist_id
        ));
      }
    }
    return meta_family_accrual_reweight(
      cdf, delay_min, cutoff, growth_rate, cens_adj == 2 ? pwindow_width / 2 : 0
    );
  }

  /** Distribution function of a continuous estimand under an accrual design. */
  real meta_family_accrual_prob(data real y, array[] real params,
                                data real delay_min, data real cutoff,
                                data real pwindow_width,
                                data int cens_adj, data int prim_id,
                                array[] real prim_params,
                                data real growth_rate, data int n_quad) {
    int lower_node = to_int(
      floor((y - delay_min) / (cutoff - delay_min) * n_quad)
    );
    real frac = (y - delay_min) / (cutoff - delay_min) * n_quad - lower_node;
    if (y >= cutoff) {
      return 1;
    }
    if (y <= delay_min) {
      return 0;
    }
    {
      vector[n_quad + 1] weighted = meta_family_accrual_nodes(
        params, delay_min, cutoff, pwindow_width, cens_adj, prim_id,
        prim_params, growth_rate, n_quad
      );
      return weighted[lower_node + 1] * (1 - frac) +
        weighted[lower_node + 2] * frac;
    }
  }

  /** Density of a continuous estimand under an accrual design. */
  real meta_family_accrual_density(data real y, array[] real params,
                                   data real delay_min, data real cutoff,
                                   data real pwindow_width,
                                   data int cens_adj, data int prim_id,
                                   array[] real prim_params,
                                   data real growth_rate, data int n_quad) {
    int lower_node = to_int(
      floor((y - delay_min) / (cutoff - delay_min) * n_quad)
    );
    if (y >= cutoff || y <= delay_min) {
      return 0;
    }
    {
      vector[n_quad + 1] weighted = meta_family_accrual_nodes(
        params, delay_min, cutoff, pwindow_width, cens_adj, prim_id,
        prim_params, growth_rate, n_quad
      );
      return fmax(
        (weighted[lower_node + 2] - weighted[lower_node + 1]) * n_quad /
          (cutoff - delay_min),
        0
      );
    }
  }

  /** Cumulative probability a study using a given procedure would report. */
  real meta_family_implied_prob(data real y, array[] real params,
                                data real delay_min, data real cutoff,
                                data real pwindow_width,
                                data real swindow_width, data int trunc_adj,
                                data int cens_adj, data int prim_id,
                                array[] real prim_params, data int accrual,
                                data real growth_rate) {
    if (cens_adj == 3 || cens_adj == 4) {
      // Midpoint imputation moved every delay along the axis, so the base
      // estimand is evaluated at the reported delay moved back.
      return meta_family_implied_prob(
        y - meta_family_shift(cens_adj, pwindow_width, swindow_width), params,
        delay_min, cutoff, pwindow_width, swindow_width, trunc_adj,
        meta_family_cens_base(cens_adj), prim_id, prim_params, accrual,
        growth_rate
      );
    }
    if (cens_adj == 0) {
      return meta_family_grid_prob(
        y, params, delay_min, cutoff, pwindow_width, swindow_width, prim_id,
        prim_params, accrual, growth_rate
      );
    }
    if (y <= delay_min) {
      return 0;
    }
    if (accrual == 1) {
      return meta_family_accrual_prob(
        y, params, delay_min, cutoff, pwindow_width, cens_adj, prim_id,
        prim_params, growth_rate, n_quad_default
      );
    }
    if (cens_adj == 2) {
      real log_y = meta_family_pcens_lcdf(
        y | params, pwindow_width, prim_id, prim_params
      );
      real log_base = delay_min > 0
        ? meta_family_pcens_lcdf(
            delay_min | params, pwindow_width, prim_id, prim_params
          )
        : negative_infinity();
      real base = is_inf(log_base) ? 0 : exp(log_base);
      if (trunc_adj == 1) {
        if (base >= 1) {
          reject("meta_family_implied_prob: the distribution function leaves ",
                 "no mass above the study's minimum delay.");
        }
        return fmin(meta_family_diff_exp(log_y, log_base) / (1 - base), 1);
      }
      if (y >= cutoff) {
        return 1;
      }
      {
        real total = meta_family_diff_exp(
          meta_family_pcens_lcdf(
            cutoff | params, pwindow_width, prim_id, prim_params
          ),
          log_base
        );
        if (total <= 0) {
          reject("meta_family_implied_prob: the distribution function ",
                 "underflowed to zero over the study's delay range.");
        }
        return fmin(meta_family_diff_exp(log_y, log_base) / total, 1);
      }
    }
    {
      real log_base = delay_min > 0
        ? dist_lcdf(delay_min | params, dist_id) : negative_infinity();
      real numerator = meta_family_diff_exp(
        dist_lcdf(y | params, dist_id), log_base
      );
      real base = is_inf(log_base) ? 0 : exp(log_base);
      if (trunc_adj == 1) {
        if (base >= 1) {
          reject("meta_family_implied_prob: the distribution function leaves ",
                 "no mass above the study's minimum delay.");
        }
        return fmin(numerator / (1 - base), 1);
      }
      if (y >= cutoff) {
        return 1;
      }
      {
        real total = meta_family_diff_exp(
          dist_lcdf(cutoff | params, dist_id), log_base
        );
        if (total <= 0) {
          reject("meta_family_implied_prob: the distribution function ",
                 "underflowed to zero over the study's delay range.");
        }
        return fmin(numerator / total, 1);
      }
    }
  }

  /**
    * A central difference of the implied distribution function. The step is
    * an argument because Stan only treats function arguments declared data,
    * and expressions built from them, as data only.
    */
  real meta_family_central_difference(data real y, data real step,
                                      array[] real params, data real delay_min,
                                      data real cutoff,
                                      data real pwindow_width,
                                      data real swindow_width,
                                      data int trunc_adj, data int cens_adj,
                                      data int prim_id,
                                      array[] real prim_params,
                                      data int accrual,
                                      data real growth_rate) {
    real prob_upper = meta_family_implied_prob(
      y + step, params, delay_min, cutoff, pwindow_width, swindow_width,
      trunc_adj, cens_adj, prim_id, prim_params, accrual, growth_rate
    );
    real prob_lower = meta_family_implied_prob(
      fmax(y - step, delay_min), params, delay_min, cutoff, pwindow_width,
      swindow_width, trunc_adj, cens_adj, prim_id, prim_params, accrual,
      growth_rate
    );
    return fmax(
      (prob_upper - prob_lower) / (y + step - fmax(y - step, delay_min)), 0
    );
  }

  /** Density of the delay distribution. */
  real meta_family_density(data real y, array[] real params) {
    if (y <= 0) {
      return 0;
    }
    if (dist_id == 1) {
      return exp(lognormal_lpdf(y | params[1], params[2]));
    }
    if (dist_id == 2) {
      return exp(gamma_lpdf(y | params[1], params[2]));
    }
    if (dist_id == 3) {
      return exp(weibull_lpdf(y | params[1], params[2]));
    }
    reject("Meta model summary rows support lognormal, gamma and weibull ",
           "delay distributions only.");
  }

  /** Density of a delay censored by a uniform primary window. */
  real meta_family_uniform_pcens_density(data real y, array[] real params,
                                         data real pwindow_width) {
    real cdf_upper = exp(dist_lcdf(y | params, dist_id));
    real cdf_lower = y > pwindow_width
      ? exp(dist_lcdf(y - pwindow_width | params, dist_id)) : 0;
    return fmax(cdf_upper - cdf_lower, 0) / pwindow_width;
  }

  /**
    * Density of the biased estimand at a reported quantile value, used to
    * convert a delay scale quantile standard error to the probability scale
    * by the delta method. Falls back to a central difference where no closed
    * form density exists.
    */
  real meta_family_implied_density(data real y, array[] real params,
                                   data real delay_min, data real cutoff,
                                   data real pwindow_width,
                                   data real swindow_width, data int trunc_adj,
                                   data int cens_adj, data int prim_id,
                                   array[] real prim_params, data int accrual,
                                   data real growth_rate) {
    if (cens_adj == 3 || cens_adj == 4) {
      return meta_family_implied_density(
        y - meta_family_shift(cens_adj, pwindow_width, swindow_width), params,
        delay_min, cutoff, pwindow_width, swindow_width, trunc_adj,
        meta_family_cens_base(cens_adj), prim_id, prim_params, accrual,
        growth_rate
      );
    }
    if (cens_adj == 0) {
      int n_grid = to_int(floor(cutoff / swindow_width));
      int first = meta_family_grid_first(delay_min, swindow_width);
      int cell = to_int(floor(y / swindow_width + 0.5));
      if (cell < first || cell >= n_grid) {
        return 0;
      }
      if (accrual == 0) {
        vector[2] edges = meta_family_grid_edges(
          cell, params, delay_min, cutoff, pwindow_width, swindow_width,
          prim_id, prim_params
        );
        return (edges[2] - edges[1]) / swindow_width;
      }
      {
        vector[n_grid - first] mass = meta_family_grid_pmf(
          params, delay_min, cutoff, pwindow_width, swindow_width, prim_id,
          prim_params, accrual, growth_rate
        );
        return mass[cell - first + 1] / swindow_width;
      }
    }
    if (y <= delay_min || (trunc_adj != 1 && y >= cutoff)) {
      return 0;
    }
    if (accrual == 1) {
      return meta_family_accrual_density(
        y, params, delay_min, cutoff, pwindow_width, cens_adj, prim_id,
        prim_params, growth_rate, n_quad_default
      );
    }
    if (cens_adj == 2 && prim_id != 1) {
      return meta_family_central_difference(
        y, fmax(1e-6, 1e-4 * y), params, delay_min, cutoff, pwindow_width,
        swindow_width, trunc_adj, cens_adj, prim_id, prim_params, accrual,
        growth_rate
      );
    }
    {
      real density = cens_adj == 2
        ? meta_family_uniform_pcens_density(y, params, pwindow_width)
        : meta_family_density(y, params);
      real log_base = delay_min <= 0
        ? negative_infinity()
        : (cens_adj == 2
             ? meta_family_pcens_lcdf(delay_min | params, pwindow_width,
                                      prim_id, prim_params)
             : dist_lcdf(delay_min | params, dist_id));
      real base = is_inf(log_base) ? 0 : exp(log_base);
      real norm;
      if (trunc_adj == 1) {
        norm = 1 - base;
      } else {
        real log_top = cens_adj == 2
          ? meta_family_pcens_lcdf(cutoff | params, pwindow_width, prim_id,
                                   prim_params)
          : dist_lcdf(cutoff | params, dist_id);
        norm = meta_family_diff_exp(log_top, log_base);
      }
      if (norm <= 0) {
        reject("meta_family_implied_density: the distribution function ",
               "underflowed to zero over the study's delay range.");
      }
      return density / norm;
    }
  }

  /**
    * Implied distribution function at equally spaced delays, packed as
    * [origin, spacing, values]. Node `i` sits at `origin + (i - 1) * spacing`.
    * Used to read off an implied quantile on the delay scale, which the
    * multivariate normal reporting mode needs and which has no closed form
    * on the discrete grid.
    */
  vector meta_family_implied_nodes(array[] real params, data real delay_min,
                                   data real cutoff, data real pwindow_width,
                                   data real swindow_width, data int trunc_adj,
                                   data int cens_adj, data int prim_id,
                                   array[] real prim_params, data int accrual,
                                   data real growth_rate) {
    if (cens_adj == 3 || cens_adj == 4) {
      // The nodes are packed as [origin, spacing, values], so moving the
      // estimand along the delay axis moves the origin.
      vector[2 + (meta_family_cens_base(cens_adj) == 0
                  ? to_int(floor(cutoff / swindow_width)) -
                    meta_family_grid_first(delay_min, swindow_width) + 1
                  : n_quad_default + 1)] nodes =
        meta_family_implied_nodes(
          params, delay_min, cutoff, pwindow_width, swindow_width, trunc_adj,
          meta_family_cens_base(cens_adj), prim_id, prim_params, accrual,
          growth_rate
        );
      nodes[1] += meta_family_shift(cens_adj, pwindow_width, swindow_width);
      return nodes;
    }
    if (cens_adj == 0) {
      int first = meta_family_grid_first(delay_min, swindow_width);
      vector[to_int(floor(cutoff / swindow_width)) - first] mass =
        meta_family_grid_pmf(params, delay_min, cutoff, pwindow_width,
                             swindow_width, prim_id, prim_params, accrual,
                             growth_rate);
      real origin = (first - 0.5) * swindow_width;
      return append_row([origin, swindow_width]',
                        append_row(0, cumulative_sum(mass)));
    }
    {
      int n_quad = n_quad_default;
      vector[n_quad + 1] raw;
      for (i in 1:(n_quad + 1)) {
        if (cens_adj == 2) {
          raw[i] = exp(meta_family_pcens_lcdf(
            delay_min + (i - 1) * (cutoff - delay_min) / n_quad | params,
            pwindow_width, prim_id, prim_params
          ));
        } else if (delay_min + (i - 1) * (cutoff - delay_min) / n_quad <= 0) {
          raw[i] = 0;
        } else {
          raw[i] = exp(dist_lcdf(
            delay_min + (i - 1) * (cutoff - delay_min) / n_quad | params,
            dist_id
          ));
        }
      }
      if (accrual == 1) {
        return append_row(
          [delay_min, (cutoff - delay_min) / n_quad]',
          meta_family_accrual_reweight(
            raw, delay_min, cutoff, growth_rate,
            cens_adj == 2 ? pwindow_width / 2 : 0
          )
        );
      }
      {
        real base = raw[1];
        real top = trunc_adj == 1 ? 1 : raw[n_quad + 1];
        if (top - base <= 0) {
          reject("meta_family_implied_nodes: the distribution function ",
                 "underflowed to zero over the study's delay range.");
        }
        return append_row(
          [delay_min, (cutoff - delay_min) / n_quad]',
          (raw - base) / (top - base)
        );
      }
    }
  }

  /**
    * Delay at which a packed node distribution function reaches `p`, by
    * inverse linear interpolation between the nodes it brackets.
    */
  real meta_family_node_quantile(vector nodes, data real p) {
    real origin = nodes[1];
    real spacing = nodes[2];
    int n = num_elements(nodes) - 2;
    int j = 1;
    if (p <= nodes[3]) {
      return origin;
    }
    if (p >= nodes[n + 2]) {
      return origin + (n - 1) * spacing;
    }
    while (j < n - 1 && nodes[j + 3] < p) {
      j += 1;
    }
    {
      real low = nodes[j + 2];
      real high = nodes[j + 3];
      real frac = high > low ? (p - low) / (high - low) : 0;
      return origin + (j - 1 + frac) * spacing;
    }
  }

  /**
    * The vector of summaries a study would report, one entry per member of a
    * multivariate normal group. Member types are 1 for a mean, 2 for a
    * standard deviation and 3 for a quantile at the matching probability.
    */
  vector meta_family_implied_summary_vector(data array[] int types,
                                            data vector probs,
                                            array[] real params,
                                            data real delay_min,
                                            data real cutoff,
                                            data real pwindow_width,
                                            data real swindow_width,
                                            data int trunc_adj,
                                            data int cens_adj,
                                            data int prim_id,
                                            array[] real prim_params,
                                            data int accrual,
                                            data real growth_rate) {
    int k = num_elements(probs);
    vector[k] implied = rep_vector(0, k);
    int any_moment = 0;
    int any_quantile = 0;
    for (j in 1:k) {
      if (types[j] == 3) {
        any_quantile = 1;
      } else {
        any_moment = 1;
      }
    }
    if (any_moment == 1) {
      vector[4] moments = meta_family_implied_moments(
        params, delay_min, cutoff, pwindow_width, swindow_width, trunc_adj,
        cens_adj, prim_id, prim_params, accrual, growth_rate
      );
      for (j in 1:k) {
        if (types[j] == 1) {
          implied[j] = moments[1];
        } else if (types[j] == 2) {
          implied[j] = moments[2];
        }
      }
    }
    if (any_quantile == 1) {
      int n_node = meta_family_cens_base(cens_adj) == 0
        ? to_int(floor(cutoff / swindow_width)) -
          meta_family_grid_first(delay_min, swindow_width) + 1
        : n_quad_default + 1;
      vector[2 + n_node] nodes = meta_family_implied_nodes(
        params, delay_min, cutoff, pwindow_width, swindow_width, trunc_adj,
        cens_adj, prim_id, prim_params, accrual, growth_rate
      );
      for (j in 1:k) {
        if (types[j] == 3) {
          implied[j] = meta_family_node_quantile(nodes, probs[j]);
        }
      }
    }
    return implied;
  }

  /** Sampling standard error of a reported standard deviation. */
  real meta_family_sd_se(vector moments, data int study_n) {
    return moments[2] * sqrt(fmax(moments[3] - 1, 1e-10) / (4.0 * study_n));
  }

  /**
    * Joint log density of a mean and a standard deviation from one study,
    * as the asymptotic bivariate normal of the pair.
    */
  real meta_family_moment_pair_lpdf(data real y_mean, data real y_sd,
                                    vector moments, data int study_n) {
    real se_mean = moments[2] / sqrt(1.0 * study_n);
    real se_sd = meta_family_sd_se(moments, study_n);
    // Matches .meta_max_correlation() in R/meta_summaries.R.
    real limit = 1 - 1e-6;
    real rho = fmin(fmax(moments[4] / sqrt(fmax(moments[3] - 1, 1e-10)),
                         -limit), limit);
    real z_mean = (y_mean - moments[1]) / se_mean;
    real z_sd = (y_sd - moments[2]) / se_sd;
    real quadratic = z_mean ^ 2 - 2 * rho * z_mean * z_sd + z_sd ^ 2;
    return -log(2 * pi()) - log(se_mean) - log(se_sd) -
      0.5 * log1m(rho ^ 2) - quadratic / (2 * (1 - rho ^ 2));
  }

  /**
    * Joint log mass of a set of quantiles from one study, multinomial over
    * the cells the quantiles cut the delay axis into.
    */
  real meta_family_quantile_set_lpmf(data array[] int cum_count, data vector y,
                                     data int study_n, array[] real params,
                                     data real delay_min, data real cutoff,
                                     data real pwindow_width,
                                     data real swindow_width,
                                     data int trunc_adj, data int cens_adj,
                                     data int prim_id,
                                     array[] real prim_params,
                                     data int accrual,
                                     data real growth_rate) {
    int n_reported = num_elements(y);
    real lp = lgamma(study_n + 1);
    real previous_prob = 0;
    int previous_count = 0;
    for (j in 1:n_reported) {
      real prob = meta_family_implied_prob(
        y[j], params, delay_min, cutoff, pwindow_width, swindow_width,
        trunc_adj, cens_adj, prim_id, prim_params, accrual, growth_rate
      );
      int count = cum_count[j] - previous_count;
      lp -= lgamma(count + 1);
      if (count > 0) {
        real cell = prob - previous_prob;
        if (is_nan(cell) || cell <= 0) {
          return negative_infinity();
        }
        lp += count * log(cell);
      }
      previous_prob = prob;
      previous_count = cum_count[j];
    }
    {
      int count = study_n - previous_count;
      real cell = 1 - previous_prob;
      lp -= lgamma(count + 1);
      if (count > 0) {
        if (is_nan(cell) || cell <= 0) {
          return negative_infinity();
        }
        lp += count * log(cell);
      }
    }
    return lp;
  }

/**
  * Compute the log probability mass function for the meta model.
  * Individual level rows use the marginal likelihood from primarycensored.
  * Summary rows compare the reported value with the summary the study would
  * have converged to given the biases in its estimation procedure.
  * Summaries reported by the same study are fitted jointly, indexed into the
  * flat group_value, group_count, group_type and group_p arrays by
  * group_start and group_len. A study that reported a covariance matrix over
  * its summaries indexes its Cholesky factor into group_chol from chol_start,
  * which holds group_len * group_len entries in column major order.
  */
  real meta_family_lpmf(data int y, dpars_A, data int obs_type,
                        data int study_n, data int trunc_adj,
                        data int cens_adj, data int trunc_design,
                        data int group_start, data int group_len,
                        data int chol_start,
                        data real relative_obs_t,
                        data real pwindow_width, data real swindow_width,
                        data real y_upper, data real delay_min,
                        data real report_se,
                        data real quantile_p, data real growth_rate,
                        data vector group_value,
                        data array[] int group_count,
                        data array[] int group_type,
                        data vector group_p,
                        data vector group_chol,
                        array[] real primary_params) {

  if (obs_type == 1) {
    return primarycensored_lpmf(
      y | dist_id, {dpars_B}, pwindow_width, y_upper,
      delay_min, relative_obs_t, primary_id, primary_params
    );
  }

  int prim_id = growth_rate == 0 ? 1 : 2;
  array[growth_rate == 0 ? 0 : 1] real prim_params;
  // The truncation design only matters for a study that did not adjust for
  // right truncation, because a study that did has already removed it.
  int accrual = (trunc_adj != 1 && trunc_design == 1) ? 1 : 0;
  int last = group_start + group_len - 1;
  if (growth_rate != 0) {
    prim_params[1] = growth_rate;
  }

  if (obs_type == 7) {
    vector[group_len] implied = meta_family_implied_summary_vector(
      group_type[group_start:last], group_p[group_start:last], {dpars_B},
      delay_min, relative_obs_t, pwindow_width, swindow_width, trunc_adj,
      cens_adj, prim_id, prim_params, accrual, growth_rate
    );
    matrix[group_len, group_len] chol = to_matrix(
      group_chol[chol_start:(chol_start + group_len * group_len - 1)],
      group_len, group_len
    );
    return multi_normal_cholesky_lpdf(
      group_value[group_start:last] | implied, chol
    );
  }

  if (obs_type == 6) {
    return meta_family_quantile_set_lpmf(
      group_count[group_start:last] | group_value[group_start:last], study_n,
      {dpars_B}, delay_min, relative_obs_t, pwindow_width, swindow_width,
      trunc_adj, cens_adj, prim_id, prim_params, accrual, growth_rate
    );
  }

  if (obs_type == 4) {
    real implied = meta_family_implied_prob(
      y_upper, {dpars_B}, delay_min, relative_obs_t, pwindow_width,
      swindow_width, trunc_adj, cens_adj, prim_id, prim_params, accrual,
      growth_rate
    );
    real se;
    if (report_se > 0) {
      // A reported quantile standard error is on the delay scale, so convert
      // it to the probability scale by the delta method.
      real density = meta_family_implied_density(
        y_upper, {dpars_B}, delay_min, relative_obs_t, pwindow_width,
        swindow_width, trunc_adj, cens_adj, prim_id, prim_params, accrual,
        growth_rate
      );
      se = fmax(density * report_se, 1e-6);
    } else {
      se = sqrt(quantile_p * (1 - quantile_p) / study_n);
    }
    return normal_lpdf(quantile_p | implied, se);
  }

  vector[4] moments = meta_family_implied_moments(
    {dpars_B}, delay_min, relative_obs_t, pwindow_width, swindow_width,
    trunc_adj, cens_adj, prim_id, prim_params, accrual, growth_rate
  );

  if (obs_type == 2) {
    real se = report_se > 0 ? report_se : moments[2] / sqrt(study_n);
    return normal_lpdf(y_upper | moments[1], se);
  }
  if (obs_type == 3) {
    real se = report_se > 0 ? report_se
      : meta_family_sd_se(moments, study_n);
    return normal_lpdf(y_upper | moments[2], se);
  }
  if (obs_type == 5) {
    return meta_family_moment_pair_lpdf(
      group_value[group_start] | group_value[group_start + 1], moments,
      study_n
    );
  }
  reject("Unknown meta model observation type: ", obs_type);
}
