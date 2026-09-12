# fmt: skip file
test_that("epidist.epidist_meta_model Stan code has no syntax errors in the default case", { # nolint: line_length_linter.
  skip_on_cran()
  stancode <- suppressMessages(epidist(
    data = prep_meta_obs,
    fn = brms::make_stancode
  ))
  expect_no_error(rstan::stanc(model_code = stancode))
})

test_that("epidist.epidist_meta_model Stan code has no syntax errors for a gamma delay", { # nolint: line_length_linter.
  skip_on_cran()
  stancode <- suppressMessages(epidist(
    data = prep_meta_obs,
    family = Gamma(link = "log"),
    fn = brms::make_stancode
  ))
  expect_no_error(rstan::stanc(model_code = stancode))
})

test_that("epidist.epidist_meta_model Stan code has no syntax errors for a weibull delay", { # nolint: line_length_linter.
  skip_on_cran()
  stancode <- suppressMessages(epidist(
    data = prep_meta_obs,
    family = "weibull",
    fn = brms::make_stancode
  ))
  expect_no_error(rstan::stanc(model_code = stancode))
})

test_that("epidist.epidist_meta_model fits and the MCMC converges with summary estimates only", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_fits()
  expect_s3_class(fit_meta_estimates, "brmsfit")
  expect_s3_class(fit_meta_estimates, "epidist_fit")
  expect_convergence(fit_meta_estimates)
})

test_that("epidist.epidist_meta_model recovers the simulation settings from biased summary estimates", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_fits()
  set.seed(1)
  pred <- delay_parameter_draws(fit_meta_estimates)
  expect_equal(mean(pred$mu), meanlog, tolerance = 0.1)
  expect_equal(mean(pred$sigma), sdlog, tolerance = 0.15)
})

test_that("epidist.epidist_meta_model fits and the MCMC converges with mixed data", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_fits()
  expect_s3_class(fit_meta_mixed, "brmsfit")
  expect_s3_class(fit_meta_mixed, "epidist_fit")
  expect_convergence(fit_meta_mixed)
})

test_that("epidist.epidist_meta_model recovers the simulation settings from mixed data", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_fits()
  set.seed(1)
  pred <- delay_parameter_draws(fit_meta_mixed)
  expect_equal(mean(pred$mu), meanlog, tolerance = 0.1)
  expect_equal(mean(pred$sigma), sdlog, tolerance = 0.1)
})

test_that("epidist.epidist_meta_model log_lik and posterior_predict have the expected shapes", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_fits()
  set.seed(1)
  log_lik <- brms::log_lik(fit_meta_estimates)
  expect_identical(ncol(log_lik), nrow(prep_meta_biased))
  expect_true(all(is.finite(log_lik)))

  pred <- brms::posterior_predict(fit_meta_estimates)
  expect_identical(ncol(pred), nrow(prep_meta_biased))
  expect_true(all(is.finite(pred)))
})

test_that("epidist.epidist_meta_model predicts individual level rows on the delay scale", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_fits()
  set.seed(1)
  prep <- brms::prepare_predictions(fit_meta_mixed)
  individual <- which(prep$data$vint1 == 1L)
  expect_gt(length(individual), 0L)
  expect_gt(sum(prep$data$vint1 != 1L), 0L)

  log_lik <- brms::log_lik(fit_meta_mixed)
  expect_identical(ncol(log_lik), length(prep$data$vint1))
  expect_true(all(is.finite(log_lik)))

  pred <- brms::posterior_predict(fit_meta_mixed)
  expect_identical(ncol(pred), length(prep$data$vint1))
  expect_true(all(is.finite(pred)))
  # Individual level rows are handed to the marginal model generators, so
  # they predict a censored delay rather than a reported summary.
  delays <- pred[, individual, drop = FALSE]
  expect_identical(floor(delays), delays)
  expect_gte(min(delays), 0)
})

test_that("the R and Stan meta model log likelihoods agree for every observation type", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_fits()
  meta <- suppressMessages(
    as_epidist_meta_model(estimates = lockstep_estimates)
  )
  program <- meta_log_lik_program(meta)
  standata <- program$standata
  # Every observation type, censoring adjustment and truncation design must
  # be exercised, so that no branch is compared vacuously.
  expect_setequal(unique(standata$vint1), 2:8)
  expect_setequal(unique(standata$vint4), 0:4)
  expect_setequal(unique(standata$vint5), 0:1)
  expect_true(any(standata$vreal5 > 0))
  expect_true(any(standata$vreal6 > 0))
  expect_true(any(standata$vint9 > .meta_n_quad()))
  # Three parameter points, so that agreement at one is not luck.
  mu <- c(1.7, 2.0, 1.4)
  sigma <- c(0.55, 0.4, 0.8)
  stan_log_lik <- meta_stan_log_lik(program, mu, sigma)
  r_log_lik <- meta_r_log_lik(program, mu, sigma)
  expect_true(all(is.finite(stan_log_lik)))
  expect_true(all(is.finite(r_log_lik)))
  # A growing primary event has no analytic primary censored distribution
  # function. Stan integrates it as an ODE and R by adaptive quadrature, and
  # the two integrators agree to about 1e-4, which is their tolerance rather
  # than the meta model's. Every other row is the same arithmetic in both
  # languages.
  growth <- standata$vreal8 != 0
  expect_true(any(growth))
  for (d in seq_along(mu)) {
    expect_rows_close(stan_log_lik[d, !growth], r_log_lik[d, !growth], 1e-6)
    expect_rows_close(stan_log_lik[d, growth], r_log_lik[d, growth], 1e-4)
  }
})

test_that("a quantile far beyond a narrow fitted delay keeps R and Stan in step", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_fits()
  # Stan differences log distribution functions and R differences them on
  # the natural scale, so a cell far into the upper tail used to keep a tiny
  # mass in one and none in the other, giving a finite log likelihood in
  # Stan and -Inf in R. Both now floor the cell at the same value. The 0.99
  # quantile here sits six standard deviations beyond a lognormal with
  # meanlog 2 and sdlog 0.05, on the grid, the continuous and the accrual
  # paths.
  narrow_mean <- exp(2 + 0.05^2 / 2)
  narrow_sd <- narrow_mean * sqrt(expm1(0.05^2))
  far <- narrow_mean + 6 * narrow_sd
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = rep(c("grid", "continuous", "accrual"), each = 2),
    type = "quantile",
    value = rep(c(narrow_mean, far), 3),
    p = rep(c(0.5, 0.99), 3),
    n = 500,
    relative_obs_time = 30,
    trunc_adjusted = FALSE,
    trunc_design = rep(c("cohort", "cohort", "accrual"), each = 2),
    cens_adjusted = rep(c(0, 1, 0), each = 2),
    growth_rate = rep(c(0, 0, 0.1), each = 2),
    stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  program <- meta_log_lik_program(meta)
  stan_log_lik <- meta_stan_log_lik(program, 2, 0.05)
  r_log_lik <- meta_r_log_lik(program, 2, 0.05)
  expect_identical(is.finite(stan_log_lik), is.finite(r_log_lik))
  expect_true(all(is.finite(r_log_lik)))
  growth <- program$standata$vreal8 != 0
  expect_rows_close(stan_log_lik[, !growth], r_log_lik[, !growth], 1e-6)
  # The two integrators of the primary censored distribution function
  # under a growing primary event, see the agreement test, are further apart
  # for a delay this narrow.
  expect_rows_close(stan_log_lik[, growth], r_log_lik[, growth], 1e-3)
  # The rows are heavily penalised rather than rejected.
  expect_true(all(r_log_lik < -30))
})

test_that("brms::log_lik() matches the Stan log likelihood at posterior draws", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_fits()
  # Pins the R log likelihood generator to what was sampled rather than to a
  # hand picked parameter, over the naive cohort and accrual studies of
  # fit_meta_estimates.
  ids <- seq(5, 100, by = 5)
  log_lik <- brms::log_lik(fit_meta_estimates, draw_ids = ids)
  prep <- brms::prepare_predictions(fit_meta_estimates, draw_ids = ids)
  mu <- as.numeric(brms::get_dpar(prep, "mu", i = 1))
  sigma <- as.numeric(brms::get_dpar(prep, "sigma", i = 1))
  expect_length(mu, length(ids))
  program <- meta_log_lik_program(prep_meta_biased)
  stan_log_lik <- meta_stan_log_lik(program, mu, sigma)
  expect_identical(dim(log_lik), dim(stan_log_lik))
  growth <- program$standata$vreal8 != 0
  expect_true(any(growth))
  expect_rows_close(log_lik[, !growth], stan_log_lik[, !growth], 1e-6)
  # The accrual study with a growing primary event, see the agreement test.
  expect_rows_close(log_lik[, growth], stan_log_lik[, growth], 1e-4)
})

test_that("the Stan grid and truncated moments match independent references", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_fits()
  # The R and Stan implementations share their discretisation, so agreement
  # between them cannot catch a shared mistake. The Stan grid is compared
  # with primarycensored::dpcens(), with and without a left truncation
  # point, and the Stan truncated continuous moments with the closed form
  # truncated moments of each family, at a wide, a narrow and a heavy tailed
  # parameter set.
  cutoff <- 30
  n_quad <- 4000L
  families <- list(
    lognormal = list(
      dist = "plnorm", names = c("meanlog", "sdlog"),
      sets = list(c(1.6, 0.5), c(2, 0.05), c(1, 1.5)),
      heavy = c(FALSE, FALSE, TRUE),
      raw = function(k, args, cutoff) {
        z <- (log(cutoff) - args[1]) / args[2]
        return(
          exp(k * args[1] + k^2 * args[2]^2 / 2) * stats::pnorm(z - k * args[2])
        )
      }
    ),
    gamma = list(
      dist = "pgamma", names = c("shape", "rate"),
      sets = list(c(3, 0.5), c(0.5, 0.1), c(400, 80)),
      heavy = c(FALSE, TRUE, FALSE),
      raw = function(k, args, cutoff) {
        return(
          exp(lgamma(args[1] + k) - lgamma(args[1]) - k * log(args[2])) *
            stats::pgamma(cutoff, args[1] + k, rate = args[2])
        )
      }
    ),
    weibull = list(
      dist = "pweibull", names = c("shape", "scale"),
      sets = list(c(2, 7), c(0.5, 5), c(25, 8)),
      heavy = c(FALSE, TRUE, FALSE),
      raw = function(k, args, cutoff) {
        return(
          args[2]^k * gamma(1 + k / args[1]) *
            stats::pgamma((cutoff / args[2])^args[1], 1 + k / args[1])
        )
      }
    )
  )
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = "A", type = c("mean", "sd"), value = c(7, 3), n = 100,
    trunc_adjusted = TRUE, cens_adjusted = 1, stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  for (family_name in names(families)) {
    family_spec <- families[[family_name]]
    family <- epidist_family(meta, family = family_name)
    formula <- epidist_formula(meta, family, formula = bf(mu ~ 1))
    stanvars <- epidist_stancode(meta, family = family, formula = formula)
    fn <- function(x) paste0("meta_", family_name, "_", x)
    mod <- rstan::stan_model(model_code = paste0(
      "functions {\n", stanvars[[3]]$scode, "\n", stanvars[[2]]$scode,
      "\n}\n",
      "data {\n  int S;\n  array[S, 2] real params;\n  int n_quad;\n}\n",
      "generated quantities {\n  array[0] real primary_params;\n",
      "  array[S] vector[", cutoff, "] pmf0;\n",
      "  array[S] vector[", cutoff - 2, "] pmf2;\n",
      "  array[S] vector[4] moments;\n",
      "  for (s in 1:S) {\n",
      "    pmf0[s] = ", fn("grid_pmf"), "(params[s], 0, ", cutoff,
      ", 1, 1, 1, primary_params, 0, 0);\n",
      "    pmf2[s] = ", fn("grid_pmf"), "(params[s], 2, ", cutoff,
      ", 1, 1, 1, primary_params, 0, 0);\n",
      "    moments[s] = ", fn("implied_moments"), "(params[s], 0, ", cutoff,
      ", 1, 1, 0, 1, 1, primary_params, 0, 0, n_quad);\n",
      "  }\n}\n"
    ))
    fit <- rstan::sampling(
      mod,
      data = list(
        S = length(family_spec$sets),
        params = do.call(rbind, family_spec$sets), n_quad = n_quad
      ),
      algorithm = "Fixed_param", chains = 1, iter = 1, warmup = 0, refresh = 0
    )
    draws <- posterior::as_draws_matrix(fit)
    for (s in seq_along(family_spec$sets)) {
      set <- family_spec$sets[[s]]
      args <- stats::setNames(as.list(set), family_spec$names)
      pmf0 <- as.numeric(draws[1, paste0("pmf0[", s, ",", 1:cutoff, "]")])
      pmf2 <- as.numeric(
        draws[1, paste0("pmf2[", s, ",", seq_len(cutoff - 2), "]")]
      )
      reference0 <- do.call(primarycensored::dpcens, c(
        list(
          x = 0:(cutoff - 1), pdist = .pdist(family_spec$dist),
          pwindow = 1, swindow = 1, D = cutoff
        ),
        args
      ))
      reference2 <- do.call(primarycensored::dpcens, c(
        list(
          x = 2:(cutoff - 1), pdist = .pdist(family_spec$dist),
          pwindow = 1, swindow = 1, L = 2, D = cutoff
        ),
        args
      ))
      expect_lt(max(abs(pmf0 - reference0)), 1e-12)
      expect_lt(max(abs(pmf2 - reference2)), 1e-12)
      moments <- as.numeric(draws[1, paste0("moments[", s, ",", 1:4, "]")])
      total <- do.call(
        .pdist(family_spec$dist), c(list(q = cutoff), args)
      )
      exact <- .meta_central_from_raw(vapply(
        1:4, function(k) family_spec$raw(k, set, cutoff) / total, numeric(1)
      ))
      # Simpson's rule converges as the fourth power of the spacing on a
      # smooth integrand, and only as its three halves power on a heavy
      # tailed one whose distribution function has infinite slope at zero.
      # The kurtosis and skewness come from cancelling raw moments, which
      # amplifies the quadrature error by the fourth power of the mean over
      # the standard deviation.
      tolerance <- if (family_spec$heavy[s]) 1e-3 else 1e-8
      shape_tolerance <- if (family_spec$heavy[s]) 1e-3 else 1e-6
      expect_equal(moments[1], exact[["mean"]], tolerance = tolerance)
      expect_equal(moments[2], exact[["sd"]], tolerance = tolerance)
      expect_equal(
        moments[3], exact[["kurtosis"]],
        tolerance = shape_tolerance
      )
      expect_equal(
        moments[4], exact[["skewness"]],
        tolerance = shape_tolerance
      )
    }
  }
})

test_that("the Stan naive grid stays finite on a grid that runs into the tail", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_fits()
  # Over a wide grid the primary censored distribution function saturates at
  # one, so its log stops increasing and differencing it returns NaN. A
  # generous `max_delay` puts a truncation adjusted naive study in this
  # region, so the numbers Stan returns are checked here rather than the code
  # it was built from.
  meanlog <- 1
  sdlog <- 0.4
  cutoff <- c(30, 60, 100, 200)
  accrual <- c(0L, 0L, 0L, 1L)
  n_case <- length(cutoff)
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = "A", type = c("mean", "sd"), value = c(20, 12), n = 100,
    trunc_adjusted = TRUE, cens_adjusted = 0, max_delay = 400,
    stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  stanvars <- epidist_stancode(meta)
  mod <- rstan::stan_model(model_code = paste0(
    "functions {\n", stanvars[[3]]$scode, "\n", stanvars[[2]]$scode, "\n}\n",
    "data {\n  int<lower=1> N;\n  array[N] real cutoff;\n",
    "  array[N] int accrual;\n  real mu;\n  real sigma;\n}\n",
    "generated quantities {\n  array[0] real primary_params;\n",
    "  array[N] vector[4] moments;\n",
    "  array[N] real mass_min;\n  array[N] real mass_total;\n",
    "  for (n in 1:N) {\n",
    "    vector[to_int(floor(cutoff[n]))] mass = meta_lognormal_grid_pmf(\n",
    "      {mu, sigma}, 0, cutoff[n], 1, 1, 1, primary_params, accrual[n], 0\n",
    "    );\n",
    "    mass_min[n] = min(mass);\n    mass_total[n] = sum(mass);\n",
    "    moments[n] = meta_lognormal_implied_moments(\n",
    "      {mu, sigma}, 0, cutoff[n], 1, 1, 0, 0, 1, primary_params,\n",
    "      accrual[n], 0, ", .meta_n_quad(), "\n",
    "    );\n  }\n}\n"
  ))
  fit <- rstan::sampling(
    mod,
    data = list(
      N = n_case, cutoff = cutoff, accrual = accrual, mu = meanlog,
      sigma = sdlog
    ),
    algorithm = "Fixed_param", chains = 1, iter = 1, warmup = 0, refresh = 0
  )
  draws <- posterior::as_draws_matrix(fit)
  stan_moments <- vapply(
    seq_len(n_case),
    function(n) {
      return(as.numeric(draws[1, paste0("moments[", n, ",", 1:4, "]")]))
    },
    numeric(4)
  )
  expect_true(all(is.finite(stan_moments)))
  # The grid Stan built must still be a probability mass function.
  expect_gte(
    min(as.numeric(draws[1, paste0("mass_min[", seq_len(n_case), "]")])), 0
  )
  expect_equal(
    as.numeric(draws[1, paste0("mass_total[", seq_len(n_case), "]")]),
    rep(1, n_case),
    tolerance = 1e-9
  )
  r_moments <- vapply(
    seq_len(n_case),
    function(n) {
      return(unname(.meta_implied_moments(
        "plnorm", list(meanlog = meanlog, sdlog = sdlog),
        cutoff = cutoff[n], pwindow = 1, swindow = 1, trunc_adjusted = 0L,
        cens_adjusted = 0L, growth_rate = 0, trunc_design = accrual[n]
      )))
    },
    numeric(4)
  )
  # The mean and standard deviation are what a study reports. The higher
  # moments only set the sampling standard errors, and weight the grid tail
  # heavily enough to see the difference between summing the cells on the log
  # scale as Stan does and on the delay scale as R does.
  expect_equal(stan_moments[1:2, ], r_moments[1:2, ], tolerance = 1e-6)
  expect_equal(stan_moments, r_moments, tolerance = 1e-3)
})

test_that("the R and Stan implied quantiles agree for every family and design", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_fits()
  # The chord inverse is refined exactly through the family quantile function
  # for a lognormal or weibull delay, by Newton steps with the closed form
  # primary censored distribution function otherwise, and left alone on the
  # discrete grid and under an accrual design. Every branch is compared.
  # The last design is a left truncated midpoint code, whose refinement has
  # to normalise from the moved left truncation point of its base estimand.
  designs <- data.frame(
    cens = c(1, 1, 2, 4, 1, 2, 3, 0, 4),
    trunc_adj = c(1, 0, 1, 0, 0, 0, 0, 0, 0),
    design = c(0, 0, 0, 0, 1, 0, 0, 0, 0),
    lower = c(0, 2, 0, 0, 0, 1.5, 0, 0, 2),
    cutoff = c(80, 30, 60, 25, 30, 40, 20, 24, 30),
    growth = c(0, 0, 0, 0, 0.1, 0.05, 0, 0, 0)
  )
  probs <- c(0.1, 0.5, 0.9)
  families <- list(
    lognormal = list(
      dist = "plnorm", args = list(meanlog = 1.6, sdlog = 0.5)
    ),
    gamma = list(dist = "pgamma", args = list(shape = 3, rate = 0.5)),
    weibull = list(dist = "pweibull", args = list(shape = 2, scale = 7))
  )
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = "A", type = c("mean", "sd"), value = c(7, 3), n = 100,
    trunc_adjusted = TRUE, cens_adjusted = 1, stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  design_slots <- function(i) {
    return(list(
      lower = designs$lower[i], cutoff = designs$cutoff[i], pwindow = 1,
      swindow = 1, trunc_adjusted = designs$trunc_adj[i],
      cens_adjusted = designs$cens[i], growth_rate = designs$growth[i],
      trunc_design = designs$design[i]
    ))
  }
  for (family_name in names(families)) {
    dist <- families[[family_name]]$dist
    args <- families[[family_name]]$args
    family <- epidist_family(meta, family = family_name)
    formula <- epidist_formula(meta, family, formula = bf(mu ~ 1))
    stanvars <- epidist_stancode(meta, family = family, formula = formula)
    fn <- function(x) paste0("meta_", family_name, "_", x)
    mod <- rstan::stan_model(model_code = paste0(
      "functions {\n", stanvars[[3]]$scode, "\n", stanvars[[2]]$scode,
      "\n}\n",
      "data {\n  int N;\n  array[N] int cens;\n  array[N] int trunc_adj;\n",
      "  array[N] int design;\n  array[N] real delay_min;\n",
      "  array[N] real cutoff;\n  array[N] real growth;\n",
      "  array[N] int n_node;\n  int K;\n  vector[K] probs;\n",
      "  array[2] real params;\n  int n_quad;\n}\n",
      "generated quantities {\n  array[N] vector[K] q;\n",
      "  for (n in 1:N) {\n",
      "    int prim_id = growth[n] == 0 ? 1 : 2;\n",
      "    array[growth[n] == 0 ? 0 : 1] real prim_params;\n",
      "    int accrual = (trunc_adj[n] != 1 && design[n] == 1) ? 1 : 0;\n",
      "    vector[2 + n_node[n]] nodes;\n",
      "    if (growth[n] != 0) prim_params[1] = growth[n];\n",
      "    nodes = ", fn("implied_nodes"), "(params, delay_min[n], ",
      "cutoff[n], 1, 1, trunc_adj[n], cens[n], prim_id, prim_params, ",
      "accrual, growth[n], n_quad);\n",
      "    for (k in 1:K) {\n",
      "      q[n, k] = ", fn("node_quantile"), "(nodes, probs[k], params, ",
      "delay_min[n], cutoff[n], 1, 1, trunc_adj[n], cens[n], prim_id, ",
      "prim_params, accrual, growth[n]);\n",
      "    }\n  }\n}\n"
    ))
    n_node <- vapply(
      seq_len(nrow(designs)),
      function(i) {
        return(length(.meta_implied_nodes(dist, args, design_slots(i))$values))
      },
      numeric(1)
    )
    fit <- rstan::sampling(
      mod,
      data = list(
        N = nrow(designs), cens = designs$cens,
        trunc_adj = designs$trunc_adj, design = designs$design,
        delay_min = designs$lower, cutoff = designs$cutoff,
        growth = designs$growth, n_node = n_node, K = length(probs),
        probs = probs, params = unname(unlist(args)), n_quad = .meta_n_quad()
      ),
      algorithm = "Fixed_param", chains = 1, iter = 1, warmup = 0, refresh = 0
    )
    draws <- posterior::as_draws_matrix(fit)
    for (i in seq_len(nrow(designs))) {
      slots <- design_slots(i)
      nodes <- .meta_implied_nodes(dist, args, slots)
      r_quantile <- vapply(
        probs,
        function(p) .meta_node_quantile(nodes, p, dist, args, slots),
        numeric(1)
      )
      stan_quantile <- as.numeric(
        draws[1, paste0("q[", i, ",", seq_along(probs), "]")]
      )
      # A growing primary event evaluates the primary censored distribution
      # function differently in R and Stan, which is the chord's own
      # tolerance; every refined design agrees to machine precision.
      tolerance <- ifelse(designs$growth[i] == 0, 1e-10, 1e-6)
      expect_equal(stan_quantile, r_quantile, tolerance = tolerance)
      chord <- vapply(
        probs, function(p) .meta_node_quantile(nodes, p),
        numeric(1)
      )
      if (designs$cens[i] %in% c(0, 3) || designs$design[i] == 1 ||
        designs$growth[i] != 0) {
        expect_identical(r_quantile, chord)
      } else {
        expect_gt(max(abs(r_quantile - chord)), 1e-3)
      }
    }
  }
})

test_that("the meta model log density has finite gradients or rejects at narrow and wide delays", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_fits()
  # Every grid and quadrature path evaluates the primary censored
  # distribution function from the study's minimum delay upwards, so a
  # narrow delay reaches deep into its lower tail, where primarycensored's
  # Stan function returns a finite value with a non finite gradient. The
  # meta model severs nodes that cannot matter before calling it, and
  # rejects a draw whose analytic moments overflow, so that the sampler sees
  # a rejection rather than a chain that cannot start. The nine designs of
  # the meta vignette are checked at the vignette's log mean and at narrow
  # and wide log standard deviations.
  set.seed(2)
  n_pool <- 20000
  ptime <- stats::runif(n_pool, 0, 30)
  delay <- stats::rlnorm(n_pool, meanlog, sdlog)
  pool <- data.frame(
    ptime = ptime, stime = ptime + delay,
    delay_daily = floor(ptime + delay) - floor(ptime)
  )
  measured <- function(data, cens) {
    return(switch(as.character(cens),
      "0" = data$delay_daily,
      "1" = data$stime - data$ptime,
      "2" = data$stime - floor(data$ptime),
      "3" = data$delay_daily + 0.5,
      "4" = data$stime - floor(data$ptime) - 0.5
    ))
  }
  simulate_study <- function(study, report, probs, cens, trunc_adjusted,
                             obs_time, trunc_design, delay_min,
                             growth_rate, study_n) {
    cases <- pool
    if (!trunc_adjusted && trunc_design == "accrual") {
      cases <- cases[cases$ptime <= obs_time & cases$stime <= obs_time, ]
    } else if (!trunc_adjusted) {
      seen <- if (cens %in% c(0, 3)) {
        cases$delay_daily + 1 <= obs_time
      } else {
        measured(cases, cens) <= obs_time
      }
      cases <- cases[seen, ]
    }
    cases <- cases[measured(cases, cens) >= delay_min, ]
    delays <- measured(cases[sample.int(nrow(cases), study_n), ], cens)
    metadata <- list(
      pwindow = 1, swindow = 1, cens_adjusted = cens,
      trunc_adjusted = trunc_adjusted, relative_obs_time = obs_time,
      trunc_design = trunc_design, delay_min = delay_min,
      growth_rate = growth_rate, max_delay = 60
    )
    if (report == "multivariate") {
      estimate <- c(mean(log(delays)), stats::sd(log(delays)))
      se <- estimate[2] / sqrt(c(1, 2) * length(delays))
      draws <- cbind(
        meanlog = stats::rnorm(1000, estimate[1], se[1]),
        sdlog = stats::rnorm(1000, estimate[2], se[2])
      )
      return(do.call(as_epidist_estimates_data, c(
        list(
          as_epidist_multivariate(draws),
          study = study, family = "lognormal"
        ),
        metadata
      )))
    }
    rows <- if (report == "moments") {
      data.frame(
        type = c("mean", "sd"), value = c(mean(delays), stats::sd(delays)),
        p = NA_real_, n = length(delays), se = NA_real_,
        stringsAsFactors = FALSE
      )
    } else if (report == "mean_se") {
      data.frame(
        type = "mean", value = mean(delays), p = NA_real_, n = NA_real_,
        se = stats::sd(delays) / sqrt(length(delays)),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        type = "quantile",
        value = stats::quantile(delays, probs, names = FALSE), p = probs,
        n = length(delays), se = NA_real_, stringsAsFactors = FALSE
      )
    }
    rows$study <- study
    for (name in names(metadata)) {
      rows[[name]] <- metadata[[name]]
    }
    return(as_epidist_estimates_data(rows))
  }
  designs <- list(
    list("naive cohort", "moments", NA, 0, FALSE, 12, "cohort", 0, 0, 180),
    list(
      "naive IQR", "quantiles", c(0.25, 0.5, 0.75), 0, FALSE, 16, "cohort",
      0, 0, 55
    ),
    list(
      "calendar stop", "quantiles", c(0.2, 0.5, 0.8), 0, FALSE, 20,
      "accrual", 0, 0.14, 240
    ),
    list("uniform window", "moments", NA, 2, FALSE, 25, "cohort", 0, 0, 95),
    list(
      "midpoint", "quantiles", c(0.3, 0.6, 0.9), 3, FALSE, 30, "cohort",
      0, 0, 40
    ),
    list(
      "adjusted (MVN)", "multivariate", NA, 1, TRUE, Inf, "cohort", 0, 0,
      300
    ),
    list("delays over 2d", "moments", NA, 0, FALSE, 18, "cohort", 2, 0, 130),
    list("mean and se", "mean_se", NA, 0, FALSE, 22, "cohort", 0, 0, 25),
    list("midpoint window", "moments", NA, 4, FALSE, 26, "cohort", 0, 0, 110)
  )
  names(designs) <- vapply(designs, `[[`, character(1), 1)
  studies <- suppressMessages(lapply(designs, function(design) {
    return(do.call(simulate_study, design))
  }))
  models <- lapply(studies, function(study) {
    return(suppressMessages(as_epidist_meta_model(estimates = study)))
  })
  # The Stan program is the same for every design, so it is compiled once.
  stancode <- suppressMessages(
    epidist(models[[1]], fn = brms::make_stancode)
  )
  mod <- rstan::stan_model(model_code = stancode)
  # The analytic gradient of the log density, and a central finite difference
  # of the log density at the same unconstrained parameters to compare it
  # with. A rejected draw reaches R as an error naming the function that
  # rejected it.
  epsilon <- 1e-6
  diagnose <- function(fit, mu, sigma) {
    upars <- rstan::unconstrain_pars(fit, list(
      Intercept = mu, Intercept_sigma = log(sigma),
      primary_params = numeric(0)
    ))
    gradient <- tryCatch(
      rstan::grad_log_prob(fit, upars),
      error = function(e) {
        return(conditionMessage(e))
      }
    )
    if (is.character(gradient)) {
      rejected <- grepl("meta_lognormal_", gradient, fixed = TRUE)
      return(list(
        outcome = if (rejected) "reject" else "other", error = NA_real_
      ))
    }
    if (!all(is.finite(gradient))) {
      return(list(outcome = "gradient not finite", error = NA_real_))
    }
    difference <- tryCatch(
      vapply(
        seq_along(upars),
        function(i) {
          shift <- replace(numeric(length(upars)), i, epsilon)
          return((rstan::log_prob(fit, upars + shift) -
            rstan::log_prob(fit, upars - shift)) / (2 * epsilon))
        },
        numeric(1)
      ),
      error = function(e) {
        return(NA_real_)
      }
    )
    return(list(
      outcome = "ok",
      error = max(abs(gradient - difference) / pmax(abs(gradient), 1))
    ))
  }
  sigmas <- c(0.03, 0.05, 0.1, 5, 10)
  outcomes <- character(0)
  for (design in names(models)) {
    standata <- suppressMessages(
      epidist(models[[design]], fn = brms::make_standata)
    )
    fit <- suppressWarnings(suppressMessages(
      rstan::sampling(mod, data = standata, chains = 0)
    ))
    for (sigma in sigmas) {
      cell <- paste0(design, ":", sigma)
      result <- diagnose(fit, meanlog, sigma)
      outcomes[cell] <- result$outcome
      expect_true(outcomes[cell] %in% c("ok", "reject"), label = cell)
      # The finite difference only says anything where the log density is
      # smooth over a step of `epsilon`. At a narrow log standard deviation
      # the grid branch moves cells between the difference's two points, so
      # it is only compared at the plausible draws.
      if (sigma >= 1 && identical(result$outcome, "ok")) {
        expect_lt(result$error, 1e-2, label = cell)
      }
    }
  }
  # The narrow and the plausible draws must evaluate on every design, and
  # the covariance row must reject the overflowing draw rather than carry
  # its infinite gradient.
  narrow <- paste0(rep(names(models), each = 3), ":", c(0.03, 0.05, 0.1))
  expect_true(all(outcomes[narrow] == "ok"))
  expect_true(all(outcomes[paste0(names(models), ":5")] == "ok"))
  expect_identical(unname(outcomes["adjusted (MVN):10"]), "reject")
})

test_that("epidist.epidist_meta_model recovers known parameters from simulated grid summaries", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  # Every study reports integer date differences from a right truncated
  # cohort, so the Stan grid branch carries all of the likelihood.
  skip_on_cran()
  skip_if_no_fits()
  expect_true(all(prep_meta_grid$cens_adjusted == 0L))
  expect_true(all(prep_meta_grid$trunc_design == 0L))
  expect_true(all(prep_meta_grid$trunc_adjusted == 0L))
  expect_true(any(prep_meta_grid$obs_type == 5L))
  expect_true(any(prep_meta_grid$obs_type == 6L & prep_meta_grid$group_len > 1))

  expect_convergence(fit_meta_grid)

  set.seed(1)
  pred <- delay_parameter_draws(fit_meta_grid)
  expect_equal(mean(pred$mu), meanlog, tolerance = 0.05)
  expect_equal(mean(pred$sigma), sdlog, tolerance = 0.1)
  expect_lt(stats::quantile(pred$mu, 0.025, names = FALSE), meanlog)
  expect_gt(stats::quantile(pred$mu, 0.975, names = FALSE), meanlog)
  expect_lt(stats::quantile(pred$sigma, 0.025, names = FALSE), sdlog)
  expect_gt(stats::quantile(pred$sigma, 0.975, names = FALSE), sdlog)
})

test_that("epidist.epidist_meta_model recovers known parameters from reported fits and posterior draws", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  # Five studies published lognormal parameters fitted to their own naive
  # date differences with standard errors, which reach the fit as the delta
  # method covariance over the mean and standard deviation. A sixth published
  # posterior draws of the delay mean and standard deviation, so every study
  # contributes a covariance row.
  skip_on_cran()
  skip_if_no_fits()
  expect_convergence(fit_meta_reported)
  expect_named(
    .estimates_vcov(sim_reported_estimates),
    c(paste0("published_", 1:5), "posterior_draws")
  )
  expect_true(all(.estimates_vcov_rows(sim_reported_estimates)))
  expect_true(all(is.na(sim_reported_estimates$se)))
  expect_true(all(prep_meta_reported$obs_type == 7L))
  expect_identical(
    unique(sim_reported_estimates$type), c("mean", "sd")
  )

  set.seed(1)
  pred <- delay_parameter_draws(fit_meta_reported)
  expect_equal(mean(pred$mu), meanlog, tolerance = 0.05)
  expect_equal(mean(pred$sigma), sdlog, tolerance = 0.1)
  expect_lt(stats::quantile(pred$mu, 0.025, names = FALSE), meanlog)
  expect_gt(stats::quantile(pred$mu, 0.975, names = FALSE), meanlog)
  expect_lt(stats::quantile(pred$sigma, 0.025, names = FALSE), sdlog)
  expect_gt(stats::quantile(pred$sigma, 0.975, names = FALSE), sdlog)
})

test_that("as_epidist_multivariate round trips draws of a fitted model", {
  # An analyst publishes draws of the delay mean and standard deviation from
  # a fitted model, and those become a summary row of a downstream meta model.
  skip_on_cran()
  skip_if_no_fits()
  dpars <- dplyr::ungroup(add_summaries(delay_parameter_draws(fit_marginal)))
  dpars <- dpars[dpars$.row == 1, ]
  reported <- as_epidist_multivariate(dpars, params = c("mean", "sd"))
  expect_identical(reported$params, c("mean", "sd"))
  expect_equal(
    unname(reported$value[1]), mean(dpars$mean),
    tolerance = 1e-10
  )
  estimates <- suppressMessages(as_epidist_estimates_data(
    reported,
    study = "round_trip", trunc_adjusted = TRUE, cens_adjusted = 1
  ))
  expect_identical(estimates$type, c("mean", "sd"))
  prep <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  expect_s3_class(prep, "epidist_meta_model")
  # The two summaries are one multivariate normal observation.
  expect_identical(nrow(prep), 1L)
  expect_identical(prep$obs_type, 7L)
  standata <- suppressMessages(epidist(prep, fn = brms::make_standata))
  expect_length(standata$meta_group_chol, 4L)
  # The reported mean must sit close to the truth, because the study that
  # produced these draws adjusted for censoring and truncation.
  expect_equal(
    unname(reported$value[1]), exp(meanlog + sdlog^2 / 2),
    tolerance = 0.1
  )
})

test_that("epidist.epidist_meta_model Stan code has no syntax errors with an expgrowth primary event", { # nolint: line_length_linter.
  skip_on_cran()
  model <- suppressMessages(as_epidist_meta_model(
    sim_obs,
    estimates = sim_estimates, primary = "expgrowth"
  ))
  stancode <- suppressMessages(epidist(
    data = model,
    formula = bf(mu ~ 1, pgrowth ~ 1),
    fn = brms::make_stancode
  ))
  # The growth rate is passed to the individual level likelihood, and the
  # summary rows keep their growth_rate slot.
  expect_match(stancode, "2, {pgrowth}", fixed = TRUE)
  expect_match(stancode, "pgrowth[n], vint1[n]", fixed = TRUE)
  expect_no_error(rstan::stanc(model_code = stancode))
})

test_that("epidist.epidist_meta_model with an expgrowth primary event recovers the marginal model fit", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  # Individual level rows of the meta model share the marginal likelihood, so
  # with the same data, prior and seed the two fits should agree on the delay
  # and on the growth rate.
  skip_on_cran()
  skip_if_no_fits()
  growth_rate <- 0.5
  obs <- simulate_exponential_cases(
    r = growth_rate, sample_size = 500, seed = 101
  ) |>
    simulate_secondary(distspec::LogNormal(meanlog, sdlog)) |>
    simulate_dates(outbreak_start_date = as.Date("2024-01-01"))
  linelist <- suppressMessages(as_epidist_linelist_data(obs))
  growth_prior <- brms::prior(
    normal(0.5, 0.1),
    class = "Intercept", dpar = "pgrowth"
  )
  fit_meta_growth <- suppressMessages(epidist(
    data = as_epidist_meta_model(linelist, primary = "expgrowth"),
    formula = bf(mu ~ 1, pgrowth ~ 1),
    prior = growth_prior,
    seed = 1,
    chains = 2,
    cores = 2,
    silent = 2,
    refresh = 0,
    iter = 1000
  ))
  fit_marginal_growth <- suppressMessages(epidist(
    data = as_epidist_marginal_model(linelist, primary = "expgrowth"),
    formula = bf(mu ~ 1, pgrowth ~ 1),
    prior = growth_prior,
    seed = 1,
    chains = 2,
    cores = 2,
    silent = 2,
    refresh = 0,
    iter = 1000
  ))
  expect_convergence(fit_meta_growth)
  expect_true(all(fit_meta_growth$data$obs_type == 1L))

  set.seed(1)
  meta_draws <- delay_parameter_draws(fit_meta_growth)
  marginal_draws <- delay_parameter_draws(fit_marginal_growth)
  expect_true(hasName(meta_draws, "pgrowth"))
  expect_equal(mean(meta_draws$mu), mean(marginal_draws$mu), tolerance = 0.05)
  expect_equal(
    mean(meta_draws$sigma), mean(marginal_draws$sigma),
    tolerance = 0.05
  )
  expect_equal(
    mean(meta_draws$pgrowth), mean(marginal_draws$pgrowth),
    tolerance = 0.05
  )
  expect_equal(mean(meta_draws$mu), meanlog, tolerance = 0.1)
  expect_equal(mean(meta_draws$sigma), sdlog, tolerance = 0.15)
  expect_equal(mean(meta_draws$pgrowth), growth_rate, tolerance = 0.2)

  # The R log likelihood of the fit uses the fitted growth rate for its
  # individual level rows. The draws are pinned so that both calls see the
  # same ones.
  prep <- brms::prepare_predictions(fit_meta_growth, draw_ids = 1:4)
  log_lik <- brms::log_lik(fit_meta_growth, draw_ids = 1:4)
  rows <- seq_len(min(3L, prep$nobs))
  expected <- vapply(
    rows,
    function(i) {
      mu <- brms::get_dpar(prep, "mu", i = i)
      sdlog_draw <- brms::get_dpar(prep, "sigma", i = i)
      pgrowth <- brms::get_dpar(prep, "pgrowth", i = i)
      lpdf <- vapply(
        seq_len(prep$ndraws),
        function(draw) {
          return(do.call(primarycensored::dpcens, c(
            list(
              x = prep$data$Y[i],
              pdist = stats::plnorm,
              pwindow = prep$data$vreal2[i],
              swindow = prep$data$vreal3[i],
              L = prep$data$vreal5[i],
              D = prep$data$vreal1[i],
              dprimary = primarycensored::dexpgrowth,
              log = TRUE,
              meanlog = mu[draw],
              sdlog = sdlog_draw[draw]
            ),
            stats::setNames(list(list(r = pgrowth[draw])), .primary_args_name())
          )))
        },
        numeric(1)
      )
      return(lpdf * prep$data$weights[i])
    },
    numeric(4)
  )
  expect_equal(unname(log_lik[, rows]), unname(expected), tolerance = 1e-6)
})

test_that("the R and Stan meta model log likelihoods agree for summary rows that estimate their growth rate", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_fits()
  # Every path the growth rate takes: the accrual grid of a naive study
  # (GA, GB), the quadrature of a study that adjusted for truncation but not
  # for its growing primary event (GC), the density of a continuous
  # estimand with a reported standard error (GD), midpoint imputation under
  # accrual (GE) and a single quantile on the accrual grid (GF). GA, GC and
  # GE give no rate, and the others a rate with a standard deviation.
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = c(
      "GA", "GA", "GB", "GB", "GC", "GC", "GD", "GE", "GE", "GF"
    ),
    type = c(
      "mean", "sd", "quantile", "quantile", "mean", "sd", "quantile",
      "mean", "sd", "quantile"
    ),
    value = c(7.5, 3.6, 4.8, 8.6, 7.9, 3.4, 6.2, 7.4, 3.5, 5.8),
    se = c(NA, NA, NA, NA, NA, NA, 0.5, NA, NA, NA),
    p = c(NA, NA, 0.25, 0.75, NA, NA, 0.5, NA, NA, 0.5),
    n = c(120, 120, 60, 60, 80, 80, 70, 115, 115, 140),
    relative_obs_time = c(20, 20, 30, 30, Inf, Inf, 24, 26, 26, 28),
    trunc_adjusted = c(
      FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE
    ),
    trunc_design = c(
      "accrual", "accrual", "accrual", "accrual", "cohort", "cohort",
      "cohort", "accrual", "accrual", "accrual"
    ),
    cens_adjusted = c(0, 0, 0, 0, 2, 2, 2, 4, 4, 3),
    growth_rate = c(NA, NA, 0.1, 0.1, NA, NA, 0.05, NA, NA, 0.1),
    growth_rate_sd = c(NA, NA, 0.02, 0.02, NA, NA, 0.01, NA, NA, 0.03),
    stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  expect_true(all(meta$growth_known == 0L))
  program <- meta_log_lik_program(meta)
  expect_true(program$growth)
  expect_setequal(unique(program$standata$vint1), c(4L, 5L, 6L))
  mu <- c(1.7, 2.0, 1.4)
  sigma <- c(0.55, 0.4, 0.8)
  pgrowth <- c(0.08, 0.15, 0.03)
  stan_log_lik <- meta_stan_log_lik(program, mu, sigma, pgrowth)
  r_log_lik <- meta_r_log_lik(program, mu, sigma, pgrowth)
  expect_true(all(is.finite(stan_log_lik)))
  expect_true(all(is.finite(r_log_lik)))
  # The two integrators of the primary censored distribution function, see
  # the agreement test above.
  for (d in seq_along(mu)) {
    expect_rows_close(stan_log_lik[d, ], r_log_lik[d, ], 1e-4)
  }
  # The rate in the slot is not what is used. Every row moves with pgrowth,
  # and at the rate a known study holds the two are the same arithmetic.
  moved <- meta_stan_log_lik(program, mu, sigma, pgrowth + 0.1)
  expect_true(all(abs(moved - stan_log_lik) > 1e-6))
  known_estimates <- estimates
  known_estimates$growth_rate <- pgrowth[1]
  known_estimates$growth_rate_sd <- NA
  known <- suppressMessages(
    as_epidist_meta_model(estimates = known_estimates)
  )
  expect_true(all(known$growth_known == 1L))
  known_program <- meta_log_lik_program(known)
  expect_false(known_program$growth)
  known_log_lik <- meta_stan_log_lik(known_program, mu[1], sigma[1])
  expect_rows_close(known_log_lik, stan_log_lik[1, , drop = FALSE], 1e-10)
  expect_rows_close(
    meta_r_log_lik(known_program, mu[1], sigma[1]),
    r_log_lik[1, , drop = FALSE], 1e-10
  )
})

test_that("a summary row with an unknown growth rate is corrected with the pgrowth a line list informs", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  # The line list pins the delay, and the naive mean of a study that
  # stopped at a calendar date then pins the growth rate it accrued cases
  # under, which the study did not report.
  skip_on_cran()
  skip_if_no_fits()
  growth_rate <- 0.5
  obs <- simulate_exponential_cases(
    r = growth_rate, sample_size = 500, seed = 101
  ) |>
    simulate_secondary(distspec::LogNormal(meanlog, sdlog))
  # The windows are built on the simulation clock, so that the exact times
  # simulate_study() works from sit inside them.
  linelist <- as_epidist_linelist_data(
    floor(obs$ptime), floor(obs$ptime) + 1,
    floor(obs$stime), floor(obs$stime) + 1,
    obs_time = ceiling(max(obs$stime)) + 1
  )
  linelist$ptime <- obs$ptime
  linelist$stime <- obs$stime
  set.seed(101)
  study <- simulate_study(
    linelist, "calendar stop",
    cens_adjusted = 0, trunc_adjusted = FALSE, trunc_design = "accrual",
    relative_obs_time = 30, growth_rate = NA, n = 200
  )
  expect_true(all(is.na(study$growth_rate)))
  meta <- suppressMessages(as_epidist_meta_model(
    linelist,
    estimates = study, primary = "expgrowth"
  ))
  expect_identical(sum(meta$growth_known == 0L), 1L)
  fit <- suppressMessages(epidist(
    data = meta,
    formula = bf(mu ~ 1, pgrowth ~ 1),
    seed = 1,
    chains = 2,
    cores = 2,
    silent = 2,
    refresh = 0,
    iter = 1000
  ))
  expect_convergence(fit)
  # One rate is shared by the line list and the summary row, and it is
  # estimated rather than returned as its normal(0, 0.25) prior.
  set.seed(1)
  draws <- delay_parameter_draws(fit)
  expect_true(hasName(draws, "pgrowth"))
  expect_equal(mean(draws$pgrowth), growth_rate, tolerance = 0.2)
  expect_lt(stats::sd(draws$pgrowth), 0.15)
  expect_equal(mean(draws$mu), meanlog, tolerance = 0.1)
  expect_equal(mean(draws$sigma), sdlog, tolerance = 0.15)
  # The R log likelihood of the summary row uses the fitted rate of each
  # draw, and agrees with Stan at those draws.
  ids <- 1:4
  log_lik <- brms::log_lik(fit, draw_ids = ids)
  prep <- brms::prepare_predictions(fit, draw_ids = ids)
  summary_row <- which(prep$data$vint1 != 1L)
  expect_length(summary_row, 1)
  rates <- as.numeric(brms::get_dpar(prep, "pgrowth", i = summary_row))
  expect_length(unique(rates), 4)
  program <- meta_log_lik_program(meta)
  stan_log_lik <- meta_stan_log_lik(
    program,
    as.numeric(brms::get_dpar(prep, "mu", i = summary_row)),
    as.numeric(brms::get_dpar(prep, "sigma", i = summary_row)),
    rates
  )
  expect_rows_close(
    log_lik[, summary_row, drop = FALSE],
    stan_log_lik[, summary_row, drop = FALSE], 1e-4
  )
})

# Simulation and recovery checks that take several minutes to run. They are
# opted into with environment variables so that the ordinary test run stays
# quick. EPIDIST_META_RECOVERY=true fits one meta model per bias code and
# EPIDIST_META_CALIBRATION=true fits forty replicates of two designs.

# Delays a study with a given procedure would have summarised, from
# simulated primary events and lognormal delays at the truth of setup.R.
recovery_delays <- function(cens, trunc_adjusted, obs_time, trunc_design,
                            delay_min, growth_rate, size) {
  draw <- 20 * size
  if (!trunc_adjusted && trunc_design == "accrual") {
    u <- stats::runif(draw)
    ptime <- if (growth_rate == 0) {
      u * obs_time
    } else {
      log1p(u * expm1(growth_rate * obs_time)) / growth_rate
    }
  } else {
    ptime <- stats::runif(draw)
  }
  delay <- stats::rlnorm(draw, meanlog, sdlog)
  stime <- ptime + delay
  daily <- floor(stime) - floor(ptime)
  measured <- switch(as.character(cens),
    "0" = daily,
    "1" = delay,
    "2" = stime - floor(ptime),
    "3" = daily + 0.5,
    "4" = stime - floor(ptime) - 0.5
  )
  # A cohort study sees a case when the delay it works from is below its
  # observation time: the whole day of a date difference, and for code 4
  # the delay from the start of the primary window, which the estimand
  # truncates at the cutoff before moving it by half a window.
  seen <- if (trunc_adjusted) {
    rep(TRUE, draw)
  } else if (trunc_design == "accrual") {
    stime <= obs_time
  } else if (cens %in% c(0, 3)) {
    daily + 1 <= obs_time
  } else if (cens == 4) {
    stime - floor(ptime) <= obs_time
  } else {
    measured <= obs_time
  }
  seen <- seen & measured >= delay_min
  return(utils::head(measured[seen], size))
}

recovery_estimates <- function(design, obs_times, size = 2000) {
  studies <- lapply(seq_along(obs_times), function(i) {
    delays <- recovery_delays(
      design$cens, design$trunc_adjusted, obs_times[i], design$trunc_design,
      design$delay_min, design$growth_rate, size
    )
    metadata <- list(
      relative_obs_time = obs_times[i], trunc_adjusted = design$trunc_adjusted,
      trunc_design = design$trunc_design, cens_adjusted = design$cens,
      delay_min = design$delay_min, growth_rate = design$growth_rate,
      max_delay = 80
    )
    study <- paste0(design$name, "_", i)
    if (design$report == "quantile_draws") {
      # The study fitted a lognormal to its own delays and published draws
      # of two quantiles of the fitted distribution.
      estimate <- c(mean(log(delays)), stats::sd(log(delays)))
      se <- estimate[2] / sqrt(c(1, 2) * length(delays))
      m <- stats::rnorm(1000, estimate[1], se[1])
      s <- stats::rnorm(1000, estimate[2], se[2])
      draws <- cbind(q0.5 = exp(m), q0.9 = exp(m + stats::qnorm(0.9) * s))
      return(do.call(as_epidist_estimates_data, c(
        list(as_epidist_multivariate(draws), study = study), metadata
      )))
    }
    rows <- data.frame(
      study = study, type = c("mean", "sd"),
      value = c(mean(delays), stats::sd(delays)), p = NA_real_,
      n = length(delays), stringsAsFactors = FALSE
    )
    if (design$report == "moments_quartiles") {
      rows <- rbind(rows, data.frame(
        study = study, type = "quantile",
        value = stats::quantile(delays, c(0.25, 0.75), names = FALSE),
        p = c(0.25, 0.75), n = length(delays), stringsAsFactors = FALSE
      ))
    }
    for (name in names(metadata)) {
      rows[[name]] <- metadata[[name]]
    }
    return(as_epidist_estimates_data(rows))
  })
  return(as_epidist_estimates_data(studies))
}

recovery_design <- function(name, cens, trunc_adjusted, trunc_design,
                            delay_min = 0, growth_rate = 0,
                            report = "moments") {
  return(list(
    name = name, cens = cens, trunc_adjusted = trunc_adjusted,
    trunc_design = trunc_design, delay_min = delay_min,
    growth_rate = growth_rate, report = report
  ))
}

recovery_fit <- function(estimates, iter = 1000, seed = 1) {
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  return(suppressMessages(epidist(
    data = meta, seed = seed, chains = 2, cores = 2, silent = 2,
    refresh = 0, iter = iter
  )))
}

test_that("the meta model recovers the truth from every bias code", {
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_fits()
  skip_if_not(
    identical(Sys.getenv("EPIDIST_META_RECOVERY"), "true"),
    "Set EPIDIST_META_RECOVERY=true to run the recovery fits"
  )
  designs <- list(
    recovery_design("cens0_cohort", 0, FALSE, "cohort"),
    recovery_design("cens0_accrual", 0, FALSE, "accrual", growth_rate = 0.15),
    recovery_design("cens1_cohort", 1, FALSE, "cohort"),
    # A mean and standard deviation of delays above 2 from a study that
    # adjusted for truncation are matched by a second, much wider lognormal
    # left truncated at 2 as well, and two chains split between the modes.
    # The study therefore also reports its quartiles, as one would.
    recovery_design(
      "cens1_adjusted", 1, TRUE, "cohort",
      delay_min = 2,
      report = "moments_quartiles"
    ),
    recovery_design("cens2_cohort", 2, FALSE, "cohort"),
    recovery_design("cens2_accrual", 2, FALSE, "accrual", growth_rate = 0.15),
    recovery_design("cens3_cohort", 3, FALSE, "cohort"),
    recovery_design("cens4_cohort", 4, FALSE, "cohort"),
    recovery_design(
      "mvn_quantiles", 1, TRUE, "cohort",
      report = "quantile_draws"
    )
  )
  cohort_times <- c(12, 16, 20, 25, 30)
  accrual_times <- c(16, 20, 24, 28, 32)
  results <- lapply(designs, function(design) {
    set.seed(11)
    obs_times <- if (design$trunc_adjusted) {
      rep(Inf, 5)
    } else if (design$trunc_design == "accrual") {
      accrual_times
    } else {
      cohort_times
    }
    fit <- recovery_fit(suppressMessages(
      recovery_estimates(design, obs_times)
    ))
    expect_convergence(fit)
    pred <- delay_parameter_draws(fit)
    posterior <- c(
      mu = mean(pred$mu), mu_sd = stats::sd(pred$mu),
      mu_lower = stats::quantile(pred$mu, 0.005, names = FALSE),
      mu_upper = stats::quantile(pred$mu, 0.995, names = FALSE),
      sigma = mean(pred$sigma), sigma_sd = stats::sd(pred$sigma),
      sigma_lower = stats::quantile(pred$sigma, 0.005, names = FALSE),
      sigma_upper = stats::quantile(pred$sigma, 0.995, names = FALSE)
    )
    # The cohort designs share one simulated sample of ten thousand delays,
    # whose own sampling error is the posterior standard deviation, so the
    # truth is asked to sit within three of them and inside the 99%
    # interval rather than two and the 95% one, which eighteen checks on
    # one sample would fail by chance more often than not.
    expect_lt(abs(posterior[["mu"]] - meanlog), 3 * posterior[["mu_sd"]])
    expect_lt(
      abs(posterior[["sigma"]] - sdlog), 3 * posterior[["sigma_sd"]]
    )
    expect_lt(posterior[["mu_lower"]], meanlog)
    expect_gt(posterior[["mu_upper"]], meanlog)
    expect_lt(posterior[["sigma_lower"]], sdlog)
    expect_gt(posterior[["sigma_upper"]], sdlog)
    return(c(design = design$name, round(posterior, 3)))
  })
  message(paste(utils::capture.output(print(
    do.call(rbind, results),
    quote = FALSE
  )), collapse = "\n"))
})

test_that("the exact quantile set likelihood is calibrated over repeated studies", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_fits()
  skip_if_not(
    identical(Sys.getenv("EPIDIST_META_CALIBRATION"), "true"),
    "Set EPIDIST_META_CALIBRATION=true to run the calibration fits"
  )
  # A naive cohort study reporting the type 1 quartiles of its integer day
  # delays, fitted as the joint probability of the crossings they stand for.
  # The multinomial on the continuity corrected grid this replaces was
  # calibrated at thirty delays and claimed standard errors 0.55 to 0.8 of
  # the truth at a hundred.
  n_rep <- 40
  replicate_study <- function(size, obs_time) {
    delays <- recovery_delays(0, FALSE, obs_time, "cohort", 0, 0, size)
    probs <- c(0.25, 0.5, 0.75)
    rows <- data.frame(
      study = "A", type = "quantile",
      value = stats::quantile(delays, probs, type = 1, names = FALSE),
      p = probs, n = length(delays), relative_obs_time = obs_time,
      trunc_adjusted = FALSE, cens_adjusted = 0, stringsAsFactors = FALSE
    )
    return(suppressMessages(as_epidist_estimates_data(rows)))
  }
  fit_replicate <- function(seed, size, obs_time) {
    set.seed(seed)
    fit <- recovery_fit(
      replicate_study(size, obs_time),
      iter = 600, seed = seed
    )
    pred <- delay_parameter_draws(fit)
    return(c(
      mu_lower = stats::quantile(pred$mu, 0.05, names = FALSE),
      mu_upper = stats::quantile(pred$mu, 0.95, names = FALSE),
      mu_rank = mean(pred$mu < meanlog),
      sigma_lower = stats::quantile(pred$sigma, 0.05, names = FALSE),
      sigma_upper = stats::quantile(pred$sigma, 0.95, names = FALSE),
      sigma_rank = mean(pred$sigma < sdlog)
    ))
  }
  calibration <- function(size, obs_time) {
    # The first fit compiles the model before the rest run in parallel.
    first <- fit_replicate(1, size, obs_time)
    rest <- parallel::mclapply(
      seq_len(n_rep)[-1], fit_replicate,
      size = size, obs_time = obs_time, mc.cores = 4
    )
    return(do.call(rbind, c(list(first), rest)))
  }
  covered <- function(results, name, truth) {
    return(sum(
      results[, paste0(name, "_lower")] <= truth &
        results[, paste0(name, "_upper")] >= truth
    ))
  }
  small <- calibration(30, 22)
  expect_gte(covered(small, "mu", meanlog), 30)
  expect_gte(covered(small, "sigma", sdlog), 30)
  medium <- calibration(100, 22)
  expect_gte(covered(medium, "mu", meanlog), 30)
  expect_gte(covered(medium, "sigma", sdlog), 30)
  message(sprintf(
    paste0(
      "quartiles, n 30: mu covered %d of %d, sigma covered %d of %d; ",
      "n 100: mu covered %d of %d, sigma covered %d of %d"
    ),
    covered(small, "mu", meanlog), n_rep, covered(small, "sigma", sdlog),
    n_rep, covered(medium, "mu", meanlog), n_rep,
    covered(medium, "sigma", sdlog), n_rep
  ))
})

test_that("the meta model is calibrated over repeated studies", {
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_fits()
  skip_if_not(
    identical(Sys.getenv("EPIDIST_META_CALIBRATION"), "true"),
    "Set EPIDIST_META_CALIBRATION=true to run the calibration fits"
  )
  n_rep <- 40
  replicate_study <- function(report, size, obs_time) {
    delays <- recovery_delays(0, FALSE, obs_time, "cohort", 0, 0, size)
    rows <- if (report == "moments") {
      data.frame(
        study = "A", type = c("mean", "sd"),
        value = c(mean(delays), stats::sd(delays)), n = length(delays),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        study = "A", type = "mean", value = mean(delays),
        se = stats::sd(delays) / sqrt(length(delays)),
        stringsAsFactors = FALSE
      )
    }
    rows$relative_obs_time <- obs_time
    rows$trunc_adjusted <- FALSE
    rows$cens_adjusted <- 0
    return(suppressMessages(as_epidist_estimates_data(rows)))
  }
  fit_replicate <- function(seed, report, size, obs_time) {
    set.seed(seed)
    fit <- recovery_fit(
      replicate_study(report, size, obs_time),
      iter = 600, seed = seed
    )
    pred <- delay_parameter_draws(fit)
    return(c(
      mu_lower = stats::quantile(pred$mu, 0.05, names = FALSE),
      mu_upper = stats::quantile(pred$mu, 0.95, names = FALSE),
      mu_rank = mean(pred$mu < meanlog),
      sigma_lower = stats::quantile(pred$sigma, 0.05, names = FALSE),
      sigma_upper = stats::quantile(pred$sigma, 0.95, names = FALSE),
      sigma_rank = mean(pred$sigma < sdlog)
    ))
  }
  decile_uniformity <- function(rank) {
    counts <- tabulate(findInterval(rank, seq(0.1, 0.9, by = 0.1)) + 1, 10)
    return(stats::chisq.test(counts)$p.value)
  }
  calibration <- function(report, size, obs_time) {
    # The first fit compiles the model before the rest run in parallel.
    first <- fit_replicate(1, report, size, obs_time)
    rest <- parallel::mclapply(
      seq_len(n_rep)[-1], fit_replicate,
      report = report, size = size,
      obs_time = obs_time, mc.cores = 4
    )
    return(do.call(rbind, c(list(first), rest)))
  }
  covered <- function(results, name, truth) {
    return(sum(
      results[, paste0(name, "_lower")] <= truth &
        results[, paste0(name, "_upper")] >= truth
    ))
  }
  # A naive cohort study reporting a mean and a standard deviation from 500
  # delays, whose likelihood dominates the prior on both parameters, so the
  # ranks of the truth among the draws should be uniform.
  moments <- calibration("moments", 500, 15)
  expect_gte(covered(moments, "mu", meanlog), 30)
  expect_gte(covered(moments, "sigma", sdlog), 30)
  expect_gt(decile_uniformity(moments[, "mu_rank"]), 0.01)
  expect_gt(decile_uniformity(moments[, "sigma_rank"]), 0.01)
  # A study reporting a mean with its standard error from 25 delays says
  # little about the spread, so sigma is prior driven there and only the
  # coverage of the intervals is asked for.
  mean_se <- calibration("mean_se", 25, 22)
  expect_gte(covered(mean_se, "mu", meanlog), 30)
  expect_gte(covered(mean_se, "sigma", sdlog), 30)
  message(sprintf(
    paste0(
      "mean and sd, n 500: mu covered %d of %d (rank p %.2f), sigma ",
      "covered %d of %d (rank p %.2f); mean and se, n 25: mu covered %d of ",
      "%d (rank p %.2f), sigma covered %d of %d (rank p %.2f)"
    ),
    covered(moments, "mu", meanlog), n_rep,
    decile_uniformity(moments[, "mu_rank"]),
    covered(moments, "sigma", sdlog), n_rep,
    decile_uniformity(moments[, "sigma_rank"]),
    covered(mean_se, "mu", meanlog), n_rep,
    decile_uniformity(mean_se[, "mu_rank"]),
    covered(mean_se, "sigma", sdlog), n_rep,
    decile_uniformity(mean_se[, "sigma_rank"])
  ))
})
