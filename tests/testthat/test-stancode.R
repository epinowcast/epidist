test_that("epidist_stancode.default returns NULL", { # nolint: line_length_linter.
  expect_null(epidist_stancode(data.frame()))
})

# Collapse Stan source to a single line so that regexes can span lines
.flatten_stan <- function(x) {
  return(gsub("\\s+", " ", paste(x, collapse = " ")))
}

# Split a Stan argument list into bare argument names
.stan_arg_names <- function(x) {
  parts <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  return(sub(".*[[:space:]]", "", parts))
}

# Extract the argument list of a call or declaration by name
.stan_call_args <- function(source, pattern) {
  source <- .flatten_stan(source)
  matched <- regmatches(source, regexpr(pattern, source))
  expect_length(matched, 1)
  return(.stan_arg_names(sub(pattern, "\\1", matched)))
}

test_that("the marginal model passes delay_min as L and relative_obs_t as D to primarycensored_lpmf", { # nolint: line_length_linter.
  # The argument order of primarycensored_lpmf is read from the installed
  # primarycensored Stan source rather than hard coded, so a change upstream
  # is caught here rather than silently changing the likelihood.
  pcd_args <- .stan_call_args(
    primarycensored::pcd_load_stan_functions("primarycensored_lpmf"),
    "real primarycensored_lpmf\\((.*?)\\) \\{"
  )
  expect_true(all(c("L", "D") %in% pcd_args))

  # The epidist call site, read before any regex substitution is applied.
  chunk <- .stan_chunk(file.path("marginal_model", "functions.stan"))
  call_args <- .stan_call_args(
    gsub("|", ",", chunk, fixed = TRUE),
    "return primarycensored_lpmf\\((.*?)\\);"
  )

  expect_length(call_args, length(pcd_args))
  expect_identical(call_args[which(pcd_args == "L")], "delay_min")
  expect_identical(call_args[which(pcd_args == "D")], "relative_obs_t")
})

test_that("the marginal model Stan signature matches the vreal order in the formula", { # nolint: line_length_linter.
  # brms passes vreal1 ... vreal5 positionally, in the order given by
  # epidist_formula_model.epidist_marginal_model(). Check that the Stan
  # signature lines up with that order.
  formula <- epidist_formula_model(
    prep_marginal_obs,
    brms::bf(mu ~ 1, sigma ~ 1)
  )
  vreal_args <- .stan_call_args(
    deparse(formula$formula),
    "vreal\\((.*?)\\)"
  )
  expect_identical(
    vreal_args,
    c("relative_obs_time", "pwindow", "swindow", "delay_upr", "delay_min")
  )

  chunk <- .stan_chunk(file.path("marginal_model", "functions.stan"))
  stan_args <- .stan_call_args(
    chunk,
    "real marginal_family_lpmf\\((.*?)\\) \\{"
  )
  # y and dpars_A come first, primary_params last
  expect_identical(
    stan_args[3:7],
    c(
      "relative_obs_t", "pwindow_width", "swindow_width", "y_upper",
      "delay_min"
    )
  )
})

# Every Stan function chunk a model adds, as one string
.stanvars_code <- function(stanvars) {
  return(.flatten_stan(vapply(stanvars, function(x) x$scode, character(1))))
}

test_that("the gengamma family reaches primarycensored as dist_id 5 in the Stacy form", { # nolint: line_length_linter.
  skip_if_not_installed("flexsurv")
  for (data in list(prep_marginal_obs, prep_meta_obs)) {
    family <- epidist_family(data, family = gengamma())
    formula <- epidist_formula(data, family, formula = bf(mu ~ 1))
    code <- .stanvars_code(epidist_stancode(data, family, formula))
    expect_true(grepl(
      "5, {Q / sigma, exp(mu + 2 * sigma * log(Q) / Q), inv_square(Q)}",
      code,
      fixed = TRUE
    ))
    expect_true(grepl("real gengamma_lcdf(real y", code, fixed = TRUE))
  }
})

test_that("the latent and naive models carry the gengamma Stan functions", {
  skip_if_not_installed("flexsurv")
  family <- epidist_family(prep_obs, family = gengamma())
  formula <- epidist_formula(prep_obs, family, formula = bf(mu ~ 1))
  code <- .stanvars_code(epidist_stancode(prep_obs, family, formula))
  expect_true(grepl("gengamma_lpdf(d | mu, sigma, Q)", code, fixed = TRUE))
  expect_true(grepl(
    "gengamma_lcdf(obs_time | mu, sigma, Q)", code,
    fixed = TRUE
  ))
  expect_true(grepl("real gengamma_lpdf(vector y", code, fixed = TRUE))

  family <- epidist_family(prep_naive_obs, family = gengamma())
  formula <- epidist_formula(prep_naive_obs, family, formula = bf(mu ~ 1))
  code <- .stanvars_code(epidist_stancode(prep_naive_obs, family, formula))
  expect_true(grepl("real gengamma_lpdf(real y", code, fixed = TRUE))
  # brms calls the density with the parameters in the order declared
  model <- .flatten_stan(epidist(
    prep_naive_obs,
    family = gengamma(), fn = brms::make_stancode
  ))
  expect_true(grepl(
    "gengamma_lpdf(Y[n] | mu[n], sigma[n], Q[n])", model,
    fixed = TRUE
  ))
  # A brms family adds nothing to the naive model
  expect_null(epidist_stancode(prep_naive_obs))
})

# Evaluate the gengamma Stan density and distribution function at each
# element of the Prentice parameters.
gengamma_stan_values <- function(y, mu, sigma, Q) {
  code <- paste(
    "functions {", .stan_chunk(file.path("family", "gengamma.stan")), "}",
    "data { int N; vector[N] y; vector[N] mu; vector[N] sigma;",
    "vector[N] Q; }",
    "generated quantities {",
    "vector[N] lpdf; vector[N] lcdf;",
    "for (n in 1:N) {",
    "lpdf[n] = gengamma_lpdf(y[n] | mu[n], sigma[n], Q[n]);",
    "lcdf[n] = gengamma_lcdf(y[n:n] | mu[n:n], sigma[n], Q[n]);",
    "}",
    "}",
    sep = "\n"
  )
  fit <- suppressMessages(rstan::sampling(
    rstan::stan_model(model_code = code),
    data = list(N = length(y), y = y, mu = mu, sigma = sigma, Q = Q),
    algorithm = "Fixed_param", chains = 1, iter = 1, warmup = 0, refresh = 0
  ))
  draws <- posterior::as_draws_matrix(fit)
  return(list(
    lpdf = as.numeric(draws[1, paste0("lpdf[", seq_along(y), "]")]),
    lcdf = as.numeric(draws[1, paste0("lcdf[", seq_along(y), "]")])
  ))
}

test_that("the gengamma Stan density and distribution function match flexsurv", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_fits()
  skip_if_not_installed("flexsurv")
  y <- c(0.5, 2, 6, 15)
  mu <- c(1.6, 1.1, 2, 0.7)
  sigma <- c(0.6, 0.7, 1.2, 0.4)
  Q <- c(0.9, 0.7, 0.3, 1.8)
  stan <- gengamma_stan_values(y, mu, sigma, Q)
  expect_equal(
    stan$lpdf,
    flexsurv::dgengamma(y, mu = mu, sigma = sigma, Q = Q, log = TRUE),
    tolerance = 1e-8
  )
  expect_equal(
    stan$lcdf,
    flexsurv::pgengamma(y, mu = mu, sigma = sigma, Q = Q, log.p = TRUE),
    tolerance = 1e-8
  )
  # Close to the lognormal limit the density uses Stirling's series and the
  # distribution function the normal approximation to the gamma distribution
  # function
  near_lognormal <- gengamma_stan_values(
    c(2, 6, 15), rep(1.8, 3), rep(0.5, 3), rep(0.02, 3)
  )
  expect_equal(
    near_lognormal$lpdf,
    flexsurv::dgengamma(c(2, 6, 15), 1.8, 0.5, 0.02, log = TRUE),
    tolerance = 1e-8
  )
  expect_equal(
    near_lognormal$lcdf,
    flexsurv::pgengamma(c(2, 6, 15), 1.8, 0.5, 0.02, log.p = TRUE),
    tolerance = 1e-5
  )
  # and both reach the lognormal where Q is far too small to use the gamma
  # function directly
  lognormal <- gengamma_stan_values(
    c(2, 6, 15), rep(1.8, 3), rep(0.5, 3), rep(1e-12, 3)
  )
  expect_equal(
    lognormal$lpdf, dlnorm(c(2, 6, 15), 1.8, 0.5, log = TRUE),
    tolerance = 1e-8
  )
  expect_equal(
    lognormal$lcdf, plnorm(c(2, 6, 15), 1.8, 0.5, log.p = TRUE),
    tolerance = 1e-8
  )
  # The Stacy form that the marginal and meta models pass to primarycensored
  stacy <- .gengamma_stacy(mu, sigma, Q)
  expect_equal(
    stan$lpdf,
    flexsurv::dgengamma.orig(
      y,
      shape = stacy$shape, scale = stacy$scale, k = stacy$k, log = TRUE
    ),
    tolerance = 1e-8
  )
})

test_that("the gengamma Stan distribution function stays finite deep in the lower tail", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_fits()
  skip_if_not_installed("flexsurv")
  # The distribution function underflows to zero at each of these points. A
  # log distribution function of minus infinity in the latent model's
  # truncation adjustment makes the log posterior plus infinity. The points
  # are given in the Stacy form, whose gamma distribution function argument
  # is (y / scale)^shape with shape parameter k.
  y <- c(2, 2, 2)
  scale <- c(5, 5, 5)
  shape <- c(5, 1, 50)
  k <- c(100, 400, 400)
  Q <- 1 / sqrt(k)
  sigma <- Q / shape
  mu <- log(scale) - 2 * sigma * log(Q) / Q
  lcdf <- gengamma_stan_values(y, mu, sigma, Q)$lcdf
  log_x <- shape * log(y / scale)
  expected <- c(
    pgamma(exp(log_x[1:2]), k[1:2], log.p = TRUE),
    # The limit as the argument of the gamma distribution function goes to 0
    k[3] * log_x[3] - lgamma(k[3] + 1)
  )
  expect_true(all(is.finite(lcdf)))
  expect_equal(lcdf, expected, tolerance = 1e-8)
})
