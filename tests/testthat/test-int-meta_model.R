# fmt: skip file
test_that("epidist.epidist_meta_model Stan code has no syntax errors in the default case", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_cmdstanr()
  stancode <- suppressMessages(epidist(
    data = prep_meta_obs,
    fn = brms::make_stancode
  ))
  mod <- cmdstanr::cmdstan_model(
    stan_file = cmdstanr::write_stan_file(stancode), compile = FALSE
  )
  expect_true(mod$check_syntax())
})

test_that("epidist.epidist_meta_model Stan code has no syntax errors for a gamma delay", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_cmdstanr()
  stancode <- suppressMessages(epidist(
    data = prep_meta_obs,
    family = Gamma(link = "log"),
    fn = brms::make_stancode
  ))
  mod <- cmdstanr::cmdstan_model(
    stan_file = cmdstanr::write_stan_file(stancode), compile = FALSE
  )
  expect_true(mod$check_syntax())
})

test_that("epidist.epidist_meta_model Stan code has no syntax errors for a weibull delay", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_cmdstanr()
  stancode <- suppressMessages(epidist(
    data = prep_meta_obs,
    family = "weibull",
    fn = brms::make_stancode
  ))
  mod <- cmdstanr::cmdstan_model(
    stan_file = cmdstanr::write_stan_file(stancode), compile = FALSE
  )
  expect_true(mod$check_syntax())
})

test_that("epidist.epidist_meta_model fits and the MCMC converges with summary estimates only", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_cmdstanr()
  expect_s3_class(fit_meta_estimates, "brmsfit")
  expect_s3_class(fit_meta_estimates, "epidist_fit")
  expect_convergence(fit_meta_estimates)
})

test_that("epidist.epidist_meta_model recovers the simulation settings from biased summary estimates", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_cmdstanr()
  set.seed(1)
  pred <- delay_parameter_draws(fit_meta_estimates)
  expect_equal(mean(pred$mu), meanlog, tolerance = 0.1)
  expect_equal(mean(pred$sigma), sdlog, tolerance = 0.15)
})

test_that("epidist.epidist_meta_model fits and the MCMC converges with mixed data", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_cmdstanr()
  expect_s3_class(fit_meta_mixed, "brmsfit")
  expect_s3_class(fit_meta_mixed, "epidist_fit")
  expect_convergence(fit_meta_mixed)
})

test_that("epidist.epidist_meta_model recovers the simulation settings from mixed data", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  skip_on_cran()
  skip_if_no_cmdstanr()
  set.seed(1)
  pred <- delay_parameter_draws(fit_meta_mixed)
  expect_equal(mean(pred$mu), meanlog, tolerance = 0.1)
  expect_equal(mean(pred$sigma), sdlog, tolerance = 0.1)
})

test_that("epidist.epidist_meta_model log_lik and posterior_predict have the expected shapes", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_cmdstanr()
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
  skip_if_no_cmdstanr()
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
  skip_if_no_cmdstanr()
  meta <- suppressMessages(
    as_epidist_meta_model(estimates = lockstep_estimates)
  )
  family <- epidist_family(meta, family = lognormal())
  formula <- epidist_formula(meta, family, formula = bf(mu ~ 1))
  stanvars <- epidist_stancode(meta, family = family, formula = formula)
  standata <- suppressMessages(epidist(meta, fn = brms::make_standata))
  slots <- c(paste0("vint", 1:9), paste0("vreal", 1:8))
  mod <- cmdstanr::cmdstan_model(cmdstanr::write_stan_file(paste0(
    "functions {\n", stanvars[[3]]$scode, "\n", stanvars[[2]]$scode, "\n}\n",
    "data {\n  int N;\n  array[N] int Y;\n",
    paste0("  array[N] int ", slots[1:9], ";\n", collapse = ""),
    paste0("  array[N] real ", slots[10:17], ";\n", collapse = ""),
    "  int<lower=0> N_meta_group;\n",
    "  vector[N_meta_group] meta_group_value;\n",
    "  array[N_meta_group] int meta_group_count;\n",
    "  array[N_meta_group] int meta_group_type;\n",
    "  vector[N_meta_group] meta_group_p;\n",
    "  int<lower=0> N_meta_chol;\n",
    "  vector[N_meta_chol] meta_group_chol;\n",
    "  real mu;\n  real sigma;\n}\n",
    "generated quantities {\n  array[0] real primary_params;\n",
    "  vector[N] log_lik;\n  for (n in 1:N) {\n",
    "    log_lik[n] = meta_lognormal_lpmf(Y[n] | mu, sigma, ",
    paste0(slots, "[n]", collapse = ", "),
    ", meta_group_value, meta_group_count, meta_group_type, meta_group_p",
    ", meta_group_chol, primary_params);\n  }\n}\n"
  )))
  stan_data <- c(
    list(N = length(standata$Y), Y = as.integer(standata$Y)),
    lapply(standata[slots[1:9]], as.integer),
    lapply(standata[slots[10:17]], as.numeric),
    list(
      N_meta_group = standata$N_meta_group,
      meta_group_value = as.array(standata$meta_group_value),
      meta_group_count = as.array(standata$meta_group_count),
      meta_group_type = as.array(standata$meta_group_type),
      meta_group_p = as.array(standata$meta_group_p),
      N_meta_chol = standata$N_meta_chol,
      meta_group_chol = as.array(standata$meta_group_chol),
      mu = 1.7, sigma = 0.55
    )
  )
  fit <- mod$sample(
    data = stan_data, fixed_param = TRUE, chains = 1, iter_sampling = 1,
    iter_warmup = 0, refresh = 0, show_messages = FALSE
  )
  stan_log_lik <- as.numeric(posterior::as_draws_matrix(fit$draws("log_lik")))
  prep <- list(data = stan_data, ndraws = 1)
  args <- list(meanlog = stan_data$mu, sdlog = stan_data$sigma)
  r_log_lik <- vapply(
    seq_along(stan_log_lik),
    function(i) {
      return(.meta_row_log_lik(.meta_row_slots(i, prep), "plnorm", args))
    },
    numeric(1)
  )
  # Every observation type, censoring adjustment and truncation design must
  # be exercised, so that no branch is compared vacuously.
  expect_setequal(unique(standata$vint1), 2:7)
  expect_setequal(unique(standata$vint4), 0:4)
  expect_setequal(unique(standata$vint5), 0:1)
  expect_true(any(standata$vreal5 > 0))
  expect_true(any(standata$vreal6 > 0))
  expect_equal(stan_log_lik, r_log_lik, tolerance = 1e-6)
})

test_that("the Stan naive grid stays finite on a grid that runs into the tail", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_cmdstanr()
  # Over a wide grid the primary censored distribution function saturates at
  # one, so its log stops increasing and differencing it returns NaN. The
  # default `max_delay` puts a truncation adjusted naive study in this region,
  # so the numbers Stan returns are checked here rather than the code it was
  # built from.
  meanlog <- 1
  sdlog <- 0.4
  cutoff <- c(30, 60, 100, 200)
  accrual <- c(0L, 0L, 0L, 1L)
  n_case <- length(cutoff)
  estimates <- suppressMessages(as_epidist_estimates_data(data.frame(
    study = "A", type = c("mean", "sd"), value = c(20, 12), n = 100,
    trunc_adjusted = TRUE, cens_adjusted = 0, stringsAsFactors = FALSE
  )))
  meta <- suppressMessages(as_epidist_meta_model(estimates = estimates))
  # The documented default runs the grid far enough into the tail to fail.
  expect_gte(estimates$max_delay[1], max(cutoff))
  stanvars <- epidist_stancode(meta)
  mod <- cmdstanr::cmdstan_model(cmdstanr::write_stan_file(paste0(
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
  )))
  fit <- mod$sample(
    data = list(
      N = n_case, cutoff = cutoff, accrual = accrual, mu = meanlog,
      sigma = sdlog
    ),
    fixed_param = TRUE, chains = 1, iter_sampling = 1, iter_warmup = 0,
    refresh = 0, show_messages = FALSE
  )
  draws <- posterior::as_draws_matrix(fit$draws())
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
  skip_if_no_cmdstanr()
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
    mod <- cmdstanr::cmdstan_model(cmdstanr::write_stan_file(paste0(
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
    )))
    n_node <- vapply(
      seq_len(nrow(designs)),
      function(i) {
        return(length(.meta_implied_nodes(dist, args, design_slots(i))$values))
      },
      numeric(1)
    )
    fit <- mod$sample(
      data = list(
        N = nrow(designs), cens = designs$cens,
        trunc_adj = designs$trunc_adj, design = designs$design,
        delay_min = designs$lower, cutoff = designs$cutoff,
        growth = designs$growth, n_node = n_node, K = length(probs),
        probs = probs, params = unname(unlist(args)), n_quad = .meta_n_quad()
      ),
      fixed_param = TRUE, chains = 1, iter_sampling = 1, iter_warmup = 0,
      sig_figs = 18, refresh = 0, show_messages = FALSE
    )
    draws <- posterior::as_draws_matrix(fit$draws("q"))
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
      chord <- vapply(probs, function(p) .meta_node_quantile(nodes, p),
        numeric(1))
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
  skip_if_no_cmdstanr()
  # Every grid and quadrature path evaluates the primary censored
  # distribution function from the study's minimum delay upwards, so a
  # narrow delay reaches deep into its lower tail, where primarycensored's
  # Stan function returns a finite value with a non finite gradient. The
  # meta model severs nodes that cannot matter before calling it, and
  # rejects a draw whose analytic moments overflow, so that the sampler sees
  # a rejection rather than a chain that cannot start. The nine designs of
  # the meta vignette are checked with CmdStan's gradient diagnostic at the
  # vignette's log mean and at narrow and wide log standard deviations.
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
          as_epidist_multivariate(draws), study = study, family = "lognormal"
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
  stan_dir <- tempfile("meta_diagnose")
  dir.create(stan_dir)
  on.exit(unlink(stan_dir, recursive = TRUE), add = TRUE)
  mod <- cmdstanr::cmdstan_model(cmdstanr::write_stan_file(
    suppressMessages(epidist(models[[1]], fn = brms::make_stancode)),
    dir = stan_dir
  ))
  diagnose <- function(standata, mu, sigma) {
    data_file <- file.path(stan_dir, "data.json")
    init_file <- file.path(stan_dir, "init.json")
    cmdstanr::write_stan_json(standata, data_file)
    cmdstanr::write_stan_json(
      list(
        Intercept = mu, Intercept_sigma = log(sigma),
        primary_params = numeric(0)
      ),
      init_file
    )
    out <- suppressWarnings(system2(
      mod$exe_file(),
      c(
        "diagnose", "test=gradient", "epsilon=1e-6", "error=1e-2", "data",
        paste0("file=", data_file), paste0("init=", init_file), "output",
        paste0("file=", file.path(stan_dir, "diagnose.csv"))
      ),
      stdout = TRUE, stderr = TRUE
    ))
    if (any(grepl("Log probability=", out, fixed = TRUE))) {
      rows <- out[grepl("^ *[0-9]+ +-?[0-9.e+-]+ +", out)]
      gradient <- as.numeric(vapply(
        strsplit(trimws(rows), " +"), `[`, character(1), 3
      ))
      return(if (all(is.finite(gradient))) "ok" else "gradient not finite")
    }
    if (any(
      grepl("meta_lognormal_", out, fixed = TRUE) &
        grepl("Exception", out, fixed = TRUE)
    )) {
      return("reject")
    }
    if (any(grepl("not finite", out, fixed = TRUE))) {
      return("gradient not finite")
    }
    return("other")
  }
  sigmas <- c(0.03, 0.05, 0.1, 5, 10)
  outcomes <- character(0)
  for (design in names(models)) {
    standata <- suppressMessages(
      epidist(models[[design]], fn = brms::make_standata)
    )
    for (sigma in sigmas) {
      cell <- paste0(design, ":", sigma)
      outcomes[cell] <- diagnose(standata, meanlog, sigma)
      expect_true(outcomes[cell] %in% c("ok", "reject"), label = cell)
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
  skip_if_no_cmdstanr()
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
  skip_if_no_cmdstanr()
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
  skip_if_no_cmdstanr()
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
    study = "round_trip", cens_adjusted = 1
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
  skip_if_no_cmdstanr()
  model <- suppressMessages(as_epidist_meta_model(
    sim_obs, estimates = sim_estimates, primary = "expgrowth"
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
  mod <- cmdstanr::cmdstan_model(
    stan_file = cmdstanr::write_stan_file(stancode), compile = FALSE
  )
  expect_true(mod$check_syntax())
})

test_that("epidist.epidist_meta_model with an expgrowth primary event recovers the marginal model fit", { # nolint: line_length_linter.
  # Note: this test is stochastic. See note at the top of this script
  # Individual level rows of the meta model share the marginal likelihood, so
  # with the same data, prior and seed the two fits should agree on the delay
  # and on the growth rate.
  skip_on_cran()
  skip_if_no_cmdstanr()
  growth_rate <- 0.5
  obs <- simulate_exponential_cases(
    r = growth_rate, sample_size = 500, seed = 101
  ) |>
    simulate_secondary(meanlog = meanlog, sdlog = sdlog) |>
    simulate_dates(outbreak_start_date = as.Date("2024-01-01"))
  linelist <- suppressMessages(as_epidist_linelist_data(obs))
  growth_prior <- brms::prior(
    normal(0.5, 0.1), class = "Intercept", dpar = "pgrowth"
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
    iter = 1000,
    backend = "cmdstanr"
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
    iter = 1000,
    backend = "cmdstanr"
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
          return(primarycensored::dpcens(
            x = prep$data$Y[i],
            pdist = stats::plnorm,
            pwindow = prep$data$vreal2[i],
            swindow = prep$data$vreal3[i],
            L = prep$data$vreal5[i],
            D = prep$data$vreal1[i],
            dprimary = primarycensored::dexpgrowth,
            dprimary_args = list(r = pgrowth[draw]),
            log = TRUE,
            meanlog = mu[draw],
            sdlog = sdlog_draw[draw]
          ))
        },
        numeric(1)
      )
      return(lpdf * prep$data$weights[i])
    },
    numeric(4)
  )
  expect_equal(unname(log_lik[, rows]), unname(expected), tolerance = 1e-6)
})
