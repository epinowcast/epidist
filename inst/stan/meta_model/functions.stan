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
  *
  * Summaries are returned as a vector of the mean, the standard deviation and
  * the kurtosis of the biased estimand. The kurtosis is needed because the
  * sampling error of a reported standard deviation depends on it.
  */

  /**
    * Package a mean and central moments into a summary vector
    *
    * @param delay_mean Mean of the estimand
    * @param variance Variance of the estimand
    * @param fourth Fourth central moment of the estimand
    *
    * @return Vector of the mean, standard deviation and kurtosis
    */
  vector meta_family_moment_vector(real delay_mean, real variance,
                                   real fourth) {
    real safe_variance = fmax(variance, 1e-10);
    return [delay_mean, sqrt(safe_variance),
            fmax(fourth / safe_variance ^ 2, 1)]';
  }

  /**
    * Convert the first four raw moments into a summary vector
    *
    * @param raw Vector of the first four raw moments
    *
    * @return Vector of the mean, standard deviation and kurtosis
    */
  vector meta_family_central_from_raw(vector raw) {
    real m1 = raw[1];
    real variance = raw[2] - m1 ^ 2;
    real fourth = raw[4] - 4 * m1 * raw[3] + 6 * m1 ^ 2 * raw[2] - 3 * m1 ^ 4;
    return meta_family_moment_vector(m1, variance, fourth);
  }

  /**
    * The log follow up available to a delay under an accrual design
    *
    * A study that collected primary events over a window and stopped at its
    * calendar end could only observe a delay of d for the primary events that
    * occurred at least d before the stop. With primary events arriving at a
    * rate proportional to exp(r t) the amount of such follow up is
    * w(d) = (exp(r (window - d)) - 1) / r, which tends to window - d as r
    * tends to zero. Working on the log scale keeps the weight finite for a
    * fast growing epidemic observed over a long window.
    *
    * @param d Delay
    * @param window Length of the collection window
    * @param r Exponential growth rate of primary events
    *
    * @return Log follow up available to the delay
    */
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

  /**
    * Reweight a distribution function for an accrual design
    *
    * Weights the probability mass between consecutive quadrature nodes by the
    * follow up available at the midpoint of the interval, then renormalises,
    * so that the returned distribution function is that of the delays a study
    * collecting up to a calendar stop would have seen.
    *
    * Where the quantity being weighted already includes the offset of the
    * primary event within its censoring window, as it does for the uniform
    * single interval approximation, weight_offset shifts the weight back to
    * the primary event time. Averaging over the window makes the shift half
    * its width.
    *
    * @param cdf Distribution function at equally spaced nodes running from
    *   zero to the cutoff
    * @param cutoff Length of the collection window
    * @param r Exponential growth rate of primary events
    * @param weight_offset Amount by which the weighted quantity overstates the
    *   time from the primary event's censoring window to the secondary event
    *
    * @return Distribution function at the same nodes, running from zero to one
    */
  vector meta_family_accrual_reweight(vector cdf, data real cutoff,
                                      data real r,
                                      data real weight_offset) {
    int n_quad = num_elements(cdf) - 1;
    vector[n_quad] log_weight;
    vector[n_quad] mass;
    real total;
    for (i in 1:n_quad) {
      log_weight[i] = meta_family_log_accrual_weight(
        (i - 0.5) * cutoff / n_quad - weight_offset, cutoff, r
      );
    }
    mass = (cdf[2:(n_quad + 1)] - cdf[1:n_quad]) .*
      exp(log_weight - max(log_weight));
    total = sum(mass);
    if (total <= 0 || is_nan(total)) {
      return cdf;
    }
    return append_row(0, cumulative_sum(mass) / total);
  }

  /**
    * The log primary censored distribution function, guarded against underflow
    *
    * Primary distributions without an analytical solution are integrated
    * numerically, which can return a non positive cumulative probability deep
    * in the lower tail and so a log of not a number. Those cases carry
    * negligible probability and are treated as zero.
    *
    * @param d Delay
    * @param params Delay distribution parameters
    * @param pwindow_width Primary window width
    * @param prim_id Primary distribution identifier
    * @param prim_params Primary distribution parameters
    *
    * @return Log cumulative probability at the delay
    */
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

  /**
    * Discrete delay distribution a study using date differences would observe
    *
    * The grid runs over delays of 0, swindow_width, 2 * swindow_width and so
    * on, up to the largest multiple whose upper bound is within the cutoff.
    * Renormalising the grid conditions on the delay falling within it, which
    * is how the study's right truncation is applied.
    *
    * Under an accrual design the cell masses are additionally weighted by the
    * follow up available to the delay each cell records, which is the delay at
    * its lower edge, before renormalising.
    *
    * @param params Delay distribution parameters
    * @param cutoff Grid cutoff (observation time or maximum delay)
    * @param pwindow_width Primary window width
    * @param swindow_width Secondary window width and grid spacing
    * @param prim_id Primary distribution identifier
    * @param prim_params Primary distribution parameters
    * @param accrual 1 to apply the accrual weight, 0 otherwise
    * @param growth_rate Exponential growth rate of primary events
    *
    * @return Vector of grid probabilities summing to one
    */
  vector meta_family_grid_pmf(array[] real params, data real cutoff,
                              data real pwindow_width,
                              data real swindow_width, data int prim_id,
                              array[] real prim_params, data int accrual,
                              data real growth_rate) {
    int n_grid = to_int(floor(cutoff / swindow_width));
    vector[n_grid + 1] log_cdf;
    vector[n_grid] log_mass;
    log_cdf[1] = negative_infinity();
    for (j in 1:n_grid) {
      log_cdf[j + 1] = meta_family_pcens_lcdf(
        j * swindow_width | params, pwindow_width, prim_id, prim_params
      );
    }
    for (j in 1:n_grid) {
      if (is_inf(log_cdf[j + 1])) {
        log_mass[j] = negative_infinity();
      } else if (is_inf(log_cdf[j])) {
        log_mass[j] = log_cdf[j + 1];
      } else {
        log_mass[j] = log_diff_exp(log_cdf[j + 1], log_cdf[j]);
      }
      if (accrual == 1) {
        log_mass[j] += meta_family_log_accrual_weight(
          (j - 1) * swindow_width, cutoff, growth_rate
        );
      }
    }
    return softmax(log_mass);
  }

  /**
    * Summaries of a discrete delay grid
    *
    * @param mass Grid probabilities from meta_family_grid_pmf
    * @param swindow_width Grid spacing
    *
    * @return Vector of the mean, standard deviation and kurtosis
    */
  vector meta_family_grid_moments(vector mass, data real swindow_width) {
    int n_grid = num_elements(mass);
    vector[n_grid] delay = linspaced_vector(n_grid, 0, n_grid - 1) *
      swindow_width;
    real m1 = dot_product(mass, delay);
    vector[n_grid] centred = delay - m1;
    vector[n_grid] squared = centred .* centred;
    real variance = dot_product(mass, squared);
    real fourth = dot_product(mass, squared .* squared);
    return meta_family_moment_vector(m1, variance, fourth);
  }

  /**
    * Analytic summaries of the delay distribution
    *
    * @param params Delay distribution parameters
    *
    * @return Vector of the mean, standard deviation and kurtosis
    */
  vector meta_family_moments(array[] real params) {
    real delay_mean;
    real variance;
    real fourth;
    if (dist_id == 1) {
      real var_log = params[2] ^ 2;
      delay_mean = exp(params[1] + var_log / 2);
      variance = delay_mean ^ 2 * expm1(var_log);
      fourth = (exp(4 * var_log) + 2 * exp(3 * var_log) +
                3 * exp(2 * var_log) - 3) * variance ^ 2;
    } else if (dist_id == 2) {
      delay_mean = params[1] / params[2];
      variance = params[1] / params[2] ^ 2;
      fourth = (3 + 6 / params[1]) * variance ^ 2;
    } else if (dist_id == 3) {
      real g1 = tgamma(1 + 1 / params[1]);
      real g2 = tgamma(1 + 2 / params[1]);
      real g3 = tgamma(1 + 3 / params[1]);
      real g4 = tgamma(1 + 4 / params[1]);
      delay_mean = params[2] * g1;
      variance = params[2] ^ 2 * (g2 - g1 ^ 2);
      fourth = params[2] ^ 4 *
        (g4 - 4 * g1 * g3 + 6 * g1 ^ 2 * g2 - 3 * g1 ^ 4);
    } else {
      reject("Meta model summary rows support lognormal, gamma and weibull ",
             "delay distributions only.");
    }
    return meta_family_moment_vector(delay_mean, variance, fourth);
  }

  /**
    * Summaries implied by a distribution function evaluated on a grid
    *
    * Uses Simpson's rule on the survival integrals
    * int_0^D k t^(k - 1) (F(D) - F(t)) dt, which are the first four raw
    * moments of the distribution truncated at the cutoff. The grid must have
    * an even number of intervals.
    *
    * @param cdf Distribution function at n_quad + 1 equally spaced points
    *   running from zero to the cutoff
    * @param cutoff Right truncation point
    *
    * @return Vector of the mean, standard deviation and kurtosis
    */
  vector meta_family_survival_moments(vector cdf, data real cutoff) {
    int n_quad = num_elements(cdf) - 1;
    vector[n_quad + 1] grid = linspaced_vector(n_quad + 1, 0, cutoff);
    vector[n_quad + 1] weight = rep_vector(2, n_quad + 1);
    vector[n_quad + 1] tail_prob = cdf[n_quad + 1] - cdf;
    vector[4] raw;
    weight[1] = 1;
    weight[n_quad + 1] = 1;
    for (i in 2:n_quad) {
      if (i % 2 == 0) {
        weight[i] = 4;
      }
    }
    for (k in 1:4) {
      raw[k] = dot_product(weight, k * pow(grid, k - 1) .* tail_prob) *
        cutoff / (3.0 * n_quad * cdf[n_quad + 1]);
    }
    return meta_family_central_from_raw(raw);
  }

  /**
    * Summaries of a right truncated delay distribution
    *
    * @param params Delay distribution parameters
    * @param cutoff Right truncation point
    * @param n_quad Number of quadrature intervals
    * @param accrual 1 to apply the accrual weight, 0 otherwise
    * @param growth_rate Exponential growth rate of primary events
    *
    * @return Vector of the mean, standard deviation and kurtosis
    */
  vector meta_family_trunc_moments(array[] real params, data real cutoff,
                                   data int n_quad, data int accrual,
                                   data real growth_rate) {
    vector[n_quad + 1] grid = linspaced_vector(n_quad + 1, 0, cutoff);
    vector[n_quad + 1] cdf;
    cdf[1] = 0;
    for (i in 2:(n_quad + 1)) {
      cdf[i] = exp(dist_lcdf(grid[i] | params, dist_id));
    }
    if (accrual == 1) {
      return meta_family_survival_moments(
        meta_family_accrual_reweight(cdf, cutoff, growth_rate, 0), cutoff
      );
    }
    return meta_family_survival_moments(cdf, cutoff);
  }

  /**
    * Summaries of a right truncated primary censored delay distribution
    *
    * The estimand is the delay plus the primary event offset within its
    * window, conditioned on falling below the cutoff. Under an accrual design
    * the accrual weight is offset by half a primary window because the
    * estimand already includes that offset.
    *
    * @param params Delay distribution parameters
    * @param cutoff Right truncation point
    * @param pwindow_width Primary window width
    * @param prim_id Primary distribution identifier
    * @param prim_params Primary distribution parameters
    * @param n_quad Number of quadrature intervals
    * @param accrual 1 to apply the accrual weight, 0 otherwise
    * @param growth_rate Exponential growth rate of primary events
    *
    * @return Vector of the mean, standard deviation and kurtosis
    */
  vector meta_family_pcens_trunc_moments(array[] real params,
                                         data real cutoff,
                                         data real pwindow_width,
                                         data int prim_id,
                                         array[] real prim_params,
                                         data int n_quad, data int accrual,
                                         data real growth_rate) {
    vector[n_quad + 1] cdf;
    cdf[1] = 0;
    for (i in 2:(n_quad + 1)) {
      cdf[i] = exp(meta_family_pcens_lcdf(
        (i - 1) * cutoff / n_quad | params, pwindow_width, prim_id, prim_params
      ));
    }
    if (accrual == 1) {
      return meta_family_survival_moments(
        meta_family_accrual_reweight(
          cdf, cutoff, growth_rate, pwindow_width / 2
        ),
        cutoff
      );
    }
    return meta_family_survival_moments(cdf, cutoff);
  }

  /**
    * Add an independent uniform primary window to a set of summaries
    *
    * The uniform single interval approximation leaves the primary interval
    * uncorrected, so the study effectively summarised the delay plus an
    * independent uniform draw over the primary window. This convolution is
    * exact when the study also adjusted for right truncation and the primary
    * events were uniform within their window.
    *
    * @param moments Summary vector to correct
    * @param pwindow_width Primary window width
    *
    * @return Vector of the mean, standard deviation and kurtosis
    */
  vector meta_family_add_uniform(vector moments, data real pwindow_width) {
    real var_delay = moments[2] ^ 2;
    real fourth_delay = moments[3] * var_delay ^ 2;
    real var_window = pwindow_width ^ 2 / 12;
    real fourth_window = pwindow_width ^ 4 / 80;
    return meta_family_moment_vector(
      moments[1] + pwindow_width / 2,
      var_delay + var_window,
      fourth_delay + 6 * var_delay * var_window + fourth_window
    );
  }

  /**
    * The summaries a study using a given procedure would report
    *
    * Midpoint imputation shifts the naive discrete grid up by half a secondary
    * window, which moves the mean and leaves every central moment unchanged.
    *
    * @param params Delay distribution parameters
    * @param cutoff Grid cutoff (observation time or maximum delay)
    * @param pwindow_width Primary window width
    * @param swindow_width Secondary window width
    * @param trunc_adj 1 if the study adjusted for right truncation
    * @param cens_adj Censoring adjustment code (0, 1, 2 or 3)
    * @param prim_id Primary distribution identifier
    * @param prim_params Primary distribution parameters
    * @param accrual 1 to apply the accrual weight, 0 otherwise
    * @param growth_rate Exponential growth rate of primary events
    *
    * @return Vector of the mean, standard deviation and kurtosis
    */
  vector meta_family_implied_moments(array[] real params, data real cutoff,
                                     data real pwindow_width,
                                     data real swindow_width,
                                     data int trunc_adj, data int cens_adj,
                                     data int prim_id,
                                     array[] real prim_params,
                                     data int accrual,
                                     data real growth_rate) {
    if (cens_adj == 0 || cens_adj == 3) {
      vector[3] moments = meta_family_grid_moments(
        meta_family_grid_pmf(params, cutoff, pwindow_width, swindow_width,
                             prim_id, prim_params, accrual, growth_rate),
        swindow_width
      );
      if (cens_adj == 3) {
        moments[1] += swindow_width / 2;
      }
      return moments;
    }
    if (cens_adj == 2) {
      if (trunc_adj == 1 && prim_id == 1) {
        return meta_family_add_uniform(meta_family_moments(params),
                                       pwindow_width);
      }
      return meta_family_pcens_trunc_moments(
        params, cutoff, pwindow_width, prim_id, prim_params, 100, accrual,
        growth_rate
      );
    }
    if (trunc_adj == 1) {
      return meta_family_moments(params);
    }
    return meta_family_trunc_moments(params, cutoff, 100, accrual,
                                     growth_rate);
  }

  /**
    * The continuity corrected distribution function of a discrete delay grid
    *
    * The step distribution function of the grid is replaced by the version
    * that interpolates it linearly through the mid points of its cells. This
    * removes most of the bias that comes from a reported quantile of day
    * resolution data landing on a jump of the step function.
    *
    * @param y Delay to evaluate the distribution function at
    * @param params Delay distribution parameters
    * @param cutoff Grid cutoff (observation time or maximum delay)
    * @param pwindow_width Primary window width
    * @param swindow_width Secondary window width
    * @param prim_id Primary distribution identifier
    * @param prim_params Primary distribution parameters
    * @param accrual 1 to apply the accrual weight, 0 otherwise
    * @param growth_rate Exponential growth rate of primary events
    *
    * @return Cumulative probability of the grid at y
    */
  real meta_family_grid_prob(data real y, array[] real params,
                             data real cutoff, data real pwindow_width,
                             data real swindow_width, data int prim_id,
                             array[] real prim_params, data int accrual,
                             data real growth_rate) {
    int n_grid = to_int(floor(cutoff / swindow_width));
    int cell = to_int(floor(y / swindow_width + 0.5));
    real frac = y / swindow_width + 0.5 - cell;
    if (cell < 0) {
      return 0;
    }
    if (cell >= n_grid) {
      return 1;
    }
    vector[n_grid + 1] cdf = append_row(0, cumulative_sum(
      meta_family_grid_pmf(params, cutoff, pwindow_width, swindow_width,
                           prim_id, prim_params, accrual, growth_rate)
    ));
    return cdf[cell + 1] * (1 - frac) + cdf[cell + 2] * frac;
  }

  /**
    * The distribution function of a continuous estimand under an accrual
    * design
    *
    * Builds the accrual weighted distribution function on the quadrature grid
    * and interpolates it linearly at the reported value. The weight is offset
    * by half a primary window for the uniform single interval approximation,
    * matching meta_family_pcens_trunc_moments, so that a reported quantile and
    * a reported moment describe the same estimand.
    *
    * @param y Reported quantile value
    * @param params Delay distribution parameters
    * @param cutoff Length of the collection window
    * @param pwindow_width Primary window width
    * @param cens_adj Censoring adjustment code
    * @param prim_id Primary distribution identifier
    * @param prim_params Primary distribution parameters
    * @param growth_rate Exponential growth rate of primary events
    * @param n_quad Number of quadrature intervals
    *
    * @return Cumulative probability of the biased estimand at y
    */
  real meta_family_accrual_prob(data real y, array[] real params,
                                data real cutoff, data real pwindow_width,
                                data int cens_adj, data int prim_id,
                                array[] real prim_params,
                                data real growth_rate, data int n_quad) {
    vector[n_quad + 1] cdf;
    vector[n_quad + 1] weighted;
    int lower_node = to_int(floor(y / cutoff * n_quad));
    real frac = y / cutoff * n_quad - lower_node;
    if (y >= cutoff) {
      return 1;
    }
    cdf[1] = 0;
    for (i in 2:(n_quad + 1)) {
      if (cens_adj == 2) {
        cdf[i] = exp(meta_family_pcens_lcdf(
          (i - 1) * cutoff / n_quad | params, pwindow_width, prim_id,
          prim_params
        ));
      } else {
        cdf[i] = exp(dist_lcdf(
          (i - 1) * cutoff / n_quad | params, dist_id
        ));
      }
    }
    weighted = meta_family_accrual_reweight(
      cdf, cutoff, growth_rate, cens_adj == 2 ? pwindow_width / 2 : 0
    );
    return weighted[lower_node + 1] * (1 - frac) +
      weighted[lower_node + 2] * frac;
  }

  /**
    * The cumulative probability a study using a given procedure would report
    *
    * Working on the probability scale avoids inverting the distribution
    * function, which has no closed form on the discrete grid.
    *
    * Midpoint imputation shifted every reported delay up by half a secondary
    * window, so its distribution function is the naive grid one evaluated half
    * a window lower.
    *
    * @param y Reported quantile value
    * @param params Delay distribution parameters
    * @param cutoff Grid cutoff (observation time or maximum delay)
    * @param pwindow_width Primary window width
    * @param swindow_width Secondary window width
    * @param trunc_adj 1 if the study adjusted for right truncation
    * @param cens_adj Censoring adjustment code (0, 1, 2 or 3)
    * @param prim_id Primary distribution identifier
    * @param prim_params Primary distribution parameters
    * @param accrual 1 to apply the accrual weight, 0 otherwise
    * @param growth_rate Exponential growth rate of primary events
    *
    * @return Cumulative probability of the biased estimand at y
    */
  real meta_family_implied_prob(data real y, array[] real params,
                                data real cutoff, data real pwindow_width,
                                data real swindow_width, data int trunc_adj,
                                data int cens_adj, data int prim_id,
                                array[] real prim_params, data int accrual,
                                data real growth_rate) {
    if (cens_adj == 0 || cens_adj == 3) {
      // Midpoint imputation shifted every delay up by half a window.
      return meta_family_grid_prob(
        y - (cens_adj == 3 ? swindow_width / 2 : 0), params, cutoff,
        pwindow_width, swindow_width, prim_id, prim_params, accrual,
        growth_rate
      );
    }
    if (y <= 0) {
      return 0;
    }
    if (accrual == 1) {
      return meta_family_accrual_prob(
        y, params, cutoff, pwindow_width, cens_adj, prim_id, prim_params,
        growth_rate, 100
      );
    }
    if (cens_adj == 2) {
      real log_cdf_y = meta_family_pcens_lcdf(
        y | params, pwindow_width, prim_id, prim_params
      );
      if (is_inf(log_cdf_y)) {
        return 0;
      }
      if (trunc_adj == 1) {
        return exp(log_cdf_y);
      }
      if (y >= cutoff) {
        return 1;
      }
      return exp(log_cdf_y - meta_family_pcens_lcdf(
        cutoff | params, pwindow_width, prim_id, prim_params
      ));
    }
    if (trunc_adj == 1) {
      return exp(dist_lcdf(y | params, dist_id));
    }
    if (y >= cutoff) {
      return 1;
    }
    return exp(dist_lcdf(y | params, dist_id) -
               dist_lcdf(cutoff | params, dist_id));
  }

  /**
    * A central difference of the implied distribution function
    *
    * The step is taken as an argument rather than computed here because Stan
    * only treats function arguments declared data, and expressions built from
    * them, as data only.
    *
    * @param y Reported quantile value
    * @param step Half width of the difference
    * @param params Delay distribution parameters
    * @param cutoff Grid cutoff (observation time or maximum delay)
    * @param pwindow_width Primary window width
    * @param swindow_width Secondary window width
    * @param trunc_adj 1 if the study adjusted for right truncation
    * @param cens_adj Censoring adjustment code (0, 1, 2 or 3)
    * @param prim_id Primary distribution identifier
    * @param prim_params Primary distribution parameters
    * @param accrual 1 to apply the accrual weight, 0 otherwise
    * @param growth_rate Exponential growth rate of primary events
    *
    * @return Slope of the implied distribution function at y
    */
  real meta_family_central_difference(data real y, data real step,
                                      array[] real params, data real cutoff,
                                      data real pwindow_width,
                                      data real swindow_width,
                                      data int trunc_adj, data int cens_adj,
                                      data int prim_id,
                                      array[] real prim_params,
                                      data int accrual,
                                      data real growth_rate) {
    real prob_upper = meta_family_implied_prob(
      y + step, params, cutoff, pwindow_width, swindow_width, trunc_adj,
      cens_adj, prim_id, prim_params, accrual, growth_rate
    );
    real prob_lower = meta_family_implied_prob(
      fmax(y - step, 0), params, cutoff, pwindow_width, swindow_width,
      trunc_adj, cens_adj, prim_id, prim_params, accrual, growth_rate
    );
    return fmax(
      (prob_upper - prob_lower) / (y + step - fmax(y - step, 0)), 0
    );
  }

  /**
    * The density of the biased estimand at a reported quantile value
    *
    * Used to convert a quantile standard error reported on the delay scale to
    * the cumulative probability scale the model works on, by the delta method.
    * For a discrete estimand the density is the mass of the grid cell the
    * value falls in divided by the grid spacing, which is exactly the slope of
    * the continuity corrected distribution function there. For a continuous
    * estimand it is a central difference of the implied distribution function.
    *
    * @param y Reported quantile value
    * @param params Delay distribution parameters
    * @param cutoff Grid cutoff (observation time or maximum delay)
    * @param pwindow_width Primary window width
    * @param swindow_width Secondary window width
    * @param trunc_adj 1 if the study adjusted for right truncation
    * @param cens_adj Censoring adjustment code (0, 1, 2 or 3)
    * @param prim_id Primary distribution identifier
    * @param prim_params Primary distribution parameters
    * @param accrual 1 to apply the accrual weight, 0 otherwise
    * @param growth_rate Exponential growth rate of primary events
    *
    * @return Density of the biased estimand at y
    */
  real meta_family_implied_density(data real y, array[] real params,
                                   data real cutoff, data real pwindow_width,
                                   data real swindow_width, data int trunc_adj,
                                   data int cens_adj, data int prim_id,
                                   array[] real prim_params, data int accrual,
                                   data real growth_rate) {
    if (cens_adj == 0 || cens_adj == 3) {
      int n_grid = to_int(floor(cutoff / swindow_width));
      int cell = to_int(floor(
        (y - (cens_adj == 3 ? swindow_width / 2 : 0)) / swindow_width + 0.5
      ));
      if (cell < 0 || cell >= n_grid) {
        return 0;
      }
      vector[n_grid] mass = meta_family_grid_pmf(
        params, cutoff, pwindow_width, swindow_width, prim_id, prim_params,
        accrual, growth_rate
      );
      return mass[cell + 1] / swindow_width;
    }
    return meta_family_central_difference(
      y, fmax(1e-6, 1e-4 * y), params, cutoff, pwindow_width, swindow_width,
      trunc_adj, cens_adj, prim_id, prim_params, accrual, growth_rate
    );
  }

  /**
    * The sampling standard error of a reported standard deviation
    *
    * The asymptotic standard error of a sample standard deviation is
    * sigma sqrt((kappa - 1) / (4 n)), where kappa is the kurtosis of the
    * estimand. The normal theory expression sigma / sqrt(2 (n - 1)) is not
    * used because it is far too narrow for the skewed distributions that
    * delays usually follow.
    *
    * @param moments Summary vector of the biased estimand
    * @param study_n Number of delays the standard deviation was computed from
    *
    * @return Standard error of the reported standard deviation
    */
  real meta_family_sd_se(vector moments, data int study_n) {
    return moments[2] * sqrt(fmax(moments[3] - 1, 1e-10) / (4.0 * study_n));
  }

/**
  * Compute the log probability mass function for the meta model
  *
  * Individual level rows use the marginal likelihood from primarycensored.
  * Summary rows compare the reported value with the summary the study would
  * have converged to given the biases in its estimation procedure.
  *
  * @param y Integer delay for individual level rows, 0 for summary rows
  * @param dpars_A Distribution parameters (replaced via regex)
  * @param obs_type 1 individual, 2 mean, 3 standard deviation, 4 quantile
  * @param study_n Study sample size (0 for individual level rows)
  * @param trunc_adj 1 if the study adjusted for right truncation
  * @param cens_adj Censoring adjustment code (0, 1, 2 or 3)
  * @param trunc_design 0 for a cohort design, 1 for an accrual design
  * @param relative_obs_t Observation time for individual level rows, grid
  *   cutoff for summary rows
  * @param pwindow_width Primary window width
  * @param swindow_width Secondary window width
  * @param y_upper Upper bound of the delay interval for individual level
  *   rows, reported value for summary rows
  * @param report_se Reported standard error (0 to derive it from study_n),
  *   on the scale of the reported value
  * @param quantile_p Quantile probability (0 for other rows)
  * @param growth_rate Exponential growth rate of primary events
  * @param primary_params Array of parameters for primary distribution
  *
  * @return Log probability mass for the meta model
  */
  real meta_family_lpmf(data int y, dpars_A, data int obs_type,
                        data int study_n, data int trunc_adj,
                        data int cens_adj, data int trunc_design,
                        data real relative_obs_t,
                        data real pwindow_width, data real swindow_width,
                        data real y_upper, data real report_se,
                        data real quantile_p, data real growth_rate,
                        array[] real primary_params) {

  if (obs_type == 1) {
    return primarycensored_lpmf(
      y | dist_id, {dpars_B}, pwindow_width, y_upper,
      0, relative_obs_t, primary_id, primary_params
    );
  }

  int prim_id = growth_rate == 0 ? 1 : 2;
  array[growth_rate == 0 ? 0 : 1] real prim_params;
  // The truncation design only matters for a study that did not adjust for
  // right truncation, because a study that did has already removed it.
  int accrual = (trunc_adj != 1 && trunc_design == 1) ? 1 : 0;
  if (growth_rate != 0) {
    prim_params[1] = growth_rate;
  }

  if (obs_type == 4) {
    real implied = meta_family_implied_prob(
      y_upper, {dpars_B}, relative_obs_t, pwindow_width, swindow_width,
      trunc_adj, cens_adj, prim_id, prim_params, accrual, growth_rate
    );
    real se;
    if (report_se > 0) {
      // A reported quantile standard error is on the delay scale, so convert
      // it to the probability scale by the delta method.
      real density = meta_family_implied_density(
        y_upper, {dpars_B}, relative_obs_t, pwindow_width, swindow_width,
        trunc_adj, cens_adj, prim_id, prim_params, accrual, growth_rate
      );
      se = fmax(density * report_se, 1e-6);
    } else {
      se = sqrt(quantile_p * (1 - quantile_p) / study_n);
    }
    return normal_lpdf(quantile_p | implied, se);
  }

  vector[3] moments = meta_family_implied_moments(
    {dpars_B}, relative_obs_t, pwindow_width, swindow_width, trunc_adj,
    cens_adj, prim_id, prim_params, accrual, growth_rate
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
  reject("Unknown meta model observation type: ", obs_type);
}
