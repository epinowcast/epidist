# fmt: skip file
test_that("epidist_gen_posterior_predict returns a function that outputs positive integers with length equal to draws", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_cmdstanr()

  # Helper function to test predictions
  test_predictions <- function(fit, family) {
    prep <- brms::prepare_predictions(fit)
    i <- 1
    predict_fn <- epidist_gen_posterior_predict(family)
    pred_i <- predict_fn(i = i, prep)
    expect_identical(floor(pred_i), pred_i)
    expect_length(pred_i, prep$ndraws)
    return(expect_gte(min(pred_i), 0))
  }

  # Test lognormal - latent and marginal
  test_predictions(fit, lognormal())
  test_predictions(fit_marginal, lognormal())

  # Test gamma - latent and marginal
  test_predictions(fit_gamma, Gamma())
  test_predictions(fit_marginal_gamma, Gamma())
})

test_that("epidist_gen_posterior_predict returns a function that errors for i out of bounds", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_cmdstanr()

  # Helper function to test out of bounds errors
  test_out_of_bounds <- function(fit, family) {
    prep <- brms::prepare_predictions(fit)
    i_out_of_bounds <- length(prep$data$Y) + 1
    predict_fn <- epidist_gen_posterior_predict(family)
    return(expect_error(
      predict_fn(i = i_out_of_bounds, prep)
    ))
  }

  # Test lognormal - latent and marginal
  test_out_of_bounds(fit, lognormal())
  test_out_of_bounds(fit_marginal, lognormal())

  # Test gamma - latent and marginal
  test_out_of_bounds(fit_gamma, Gamma())
  test_out_of_bounds(fit_marginal_gamma, Gamma())
})

test_that("epidist_gen_posterior_predict returns a function that can generate predictions with no censoring", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_cmdstanr()

  # Helper function to test uncensored predictions
  test_uncensored <- function(fit, family) {
    predict_fn <- epidist_gen_posterior_predict(family)
    draws <- data.frame(
      relative_obs_time = Inf, pwindow = 0, swindow = 0,
      delay_upr = NA, delay_min = 0
    ) |>
      tidybayes::add_predicted_draws(fit, ndraws = 100)
    expect_identical(draws$.draw, 1:100)
    pred <- draws$.prediction
    expect_gte(min(pred), 0)
    return(expect_true(
      all(abs(pred - round(pred)) > .Machine$double.eps^0.5)
    ))
  }

  # Test lognormal - latent and marginal
  test_uncensored(fit, lognormal())
  test_uncensored(fit_marginal, lognormal())

  # Test gamma - latent and marginal
  test_uncensored(fit_gamma, Gamma())
  test_uncensored(fit_marginal_gamma, Gamma())
})

test_that("epidist_gen_posterior_predict returns a function that predicts delays in the 95% credible interval", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_cmdstanr()

  # Helper function to test credible intervals
  test_credible_intervals <- function(fit, family) {
    prep <- brms::prepare_predictions(fit)
    prep$ndraws <- 1000 # Down from the 4000 for time saving
    predict_fn <- epidist_gen_posterior_predict(family)
    quantiles <- purrr::map_vec(
      seq_along(prep$data$Y),
      function(i) {
        y <- predict_fn(i, prep)
        ecdf_fn <- ecdf(y)
        return(ecdf_fn(prep$data$Y[i]))
      }
    )
    expect_lt(quantile(quantiles, 0.1), 0.3)
    expect_gt(quantile(quantiles, 0.9), 0.7)
    expect_lt(min(quantiles), 0.1)
    expect_gt(max(quantiles), 0.9)
    expect_lt(mean(quantiles), 0.65)
    return(expect_gt(mean(quantiles), 0.35))
  }

  # Test lognormal - latent and marginal
  test_credible_intervals(fit, lognormal())
  test_credible_intervals(fit_marginal, lognormal())

  # Test gamma - latent and marginal
  test_credible_intervals(fit_gamma, Gamma())
  test_credible_intervals(fit_marginal_gamma, Gamma())
})

test_that("epidist_gen_posterior_epred returns a function that creates arrays with correct dimensions", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_no_cmdstanr()

  # Helper function to test epred
  test_epred <- function(fit, expected_mean) {
    epred <- prep_obs |>
      mutate(delay_upr = NA, delay_min = 0) |>
      tidybayes::add_epred_draws(fit)
    expect_equal(
      mean(epred$.epred), expected_mean,
      tolerance = 0.1
    )
    return(expect_gte(min(epred$.epred), 0))
  }

  # Test lognormal - latent and marginal
  test_epred(fit, 5.97)
  test_epred(fit_marginal, 5.97)

  # Test gamma - latent and marginal
  test_epred(fit_gamma, 6.56)
  test_epred(fit_marginal_gamma, 6.56)
})

test_that( # nolint: line_length_linter.
  "epidist_gen_log_lik returns a function that produces valid log likelihoods",
  {
    skip_on_cran()
    skip_if_no_cmdstanr()
    # Test lognormal
    prep <- brms::prepare_predictions(fit)
    prep$ndraws <- 10
    i <- 1
    log_lik_fn <- epidist_gen_log_lik(lognormal())
    log_lik <- log_lik_fn(i = i, prep)
    expect_length(log_lik, prep$ndraws)
    expect_false(anyNA(log_lik))
    expect_true(all(is.finite(log_lik)))

    # Test gamma
    prep_gamma <- brms::prepare_predictions(fit_gamma)
    prep$ndraws <- 10
    log_lik_fn_gamma <- epidist_gen_log_lik(Gamma())
    log_lik_gamma <- log_lik_fn_gamma(i = i, prep_gamma)
    expect_length(log_lik_gamma, prep_gamma$ndraws)
    expect_false(anyNA(log_lik_gamma))
    expect_true(all(is.finite(log_lik_gamma)))
  }
)

test_that( # nolint: line_length_linter.
  "epidist_gen_log_lik falls back to generic method for unsupported distributions", # nolint: line_length_linter.
  {
    skip_on_cran()
    skip_if_no_cmdstanr()

    # Test with normal distribution without analytical solution
    prep <- brms::prepare_predictions(fit)
    prep$ndraws <- 10
    i <- 1

    # Capture the message about falling back to generic method
    log_lik_fn <- suppressMessages(
      epidist_gen_log_lik(brms::brmsfamily("gaussian"))
    )

    # Test that the generic method produces valid log likelihoods
    log_lik <- log_lik_fn(i = i, prep)
    expect_length(log_lik, prep$ndraws)
    expect_false(anyNA(log_lik))
    expect_true(all(is.finite(log_lik)))
  }
)

test_that(
  "epidist_gen_log_lik passes delay_min as L and relative_obs_time as D",
  {
    skip_on_cran()

    # vreal1 (D) and vreal5 (L) are given distinct values so that a swap
    # between them changes the answer.
    prep <- brms::prepare_predictions(fit_marginal)
    prep$ndraws <- 5
    i <- 1
    prep$data$Y[i] <- 5
    prep$data$vreal1[i] <- 12
    prep$data$vreal2[i] <- 1
    prep$data$vreal3[i] <- 1
    prep$data$vreal4[i] <- 6
    prep$data$vreal5[i] <- 2
    prep$data$weights <- NULL

    log_lik_fn <- epidist_gen_log_lik(lognormal())
    log_lik <- log_lik_fn(i = i, prep)

    meanlog <- brms::get_dpar(prep, "mu", i = i)
    sdlog <- brms::get_dpar(prep, "sigma", i = i)
    expected <- vapply(
      seq_len(prep$ndraws),
      function(draw) {
        return(primarycensored::dpcens(
          x = 5,
          pdist = stats::plnorm,
          pwindow = 1,
          swindow = 1,
          L = 2,
          D = 12,
          dprimary = stats::dunif,
          log = TRUE,
          meanlog = meanlog[draw],
          sdlog = sdlog[draw]
        ))
      },
      numeric(1)
    )
    expect_equal(log_lik, expected, tolerance = 1e-8)

    # The same call with no left truncation must give a different answer,
    # otherwise this test would pass even if delay_min were ignored.
    prep$data$vreal5[i] <- 0
    expect_false(isTRUE(all.equal(log_lik_fn(i = i, prep), expected)))
  }
)

test_that( # nolint: line_length_linter.
  "epidist_gen_log_lik generic method agrees with the analytical method",
  {
    skip_on_cran()
    skip_if_no_cmdstanr()

    analytical <- epidist_gen_log_lik(lognormal())
    log_lik_brms <- .get_brms_fn("log_lik", lognormal())
    generic <- .generic_gen_log_lik(log_lik_brms)

    # The marginal fit is weighted, so this also checks that the observation
    # weights are applied once rather than folded into the cdf.
    for (model_fit in list(fit, fit_marginal)) {
      prep <- brms::prepare_predictions(model_fit)
      prep$ndraws <- 10
      for (i in 1:3) {
        expect_equal(generic(i, prep), analytical(i, prep), tolerance = 1e-5)
      }
    }
  }
)

test_that( # nolint: line_length_linter.
  "epidist_gen_log_lik generic method evaluates each delay once for all draws",
  {
    skip_on_cran()
    skip_if_no_cmdstanr()

    prep <- brms::prepare_predictions(fit)

    log_lik_brms <- .get_brms_fn("log_lik", lognormal())
    counter <- new.env(parent = emptyenv())
    counter$calls <- 0
    counting_log_lik <- function(i, prep) {
      counter$calls <- counter$calls + 1
      return(log_lik_brms(i, prep))
    }
    generic <- .generic_gen_log_lik(counting_log_lik)

    prep$ndraws <- 10
    generic(1, prep)
    calls_10 <- counter$calls

    counter$calls <- 0
    prep$ndraws <- 100
    generic(1, prep)
    calls_100 <- counter$calls

    # The number of brms evaluations is set by the quadrature nodes rather
    # than by the number of draws, so a ten fold increase in draws must not
    # scale the number of calls.
    expect_lt(calls_100, 2 * calls_10)
    expect_lt(calls_100, 100)
  }
)

test_that("the generic log likelihood rejects a delay beyond the observation time", { # nolint: line_length_linter.
  # dpcens() errors on this, and the refactor integrates with pcens_cdf()
  # instead, so the same guard has to be applied here. Without it the
  # truncation normalisation can return a density above one.
  skip_on_cran()

  expect_error(
    primarycensored::dpcens(
      x = 5, pdist = stats::plnorm, meanlog = 1.5, sdlog = 0.5,
      pwindow = 1, swindow = 1, D = 5.5, dprimary = stats::dunif
    ),
    "Upper truncation point is greater than D"
  )

  log_lik <- epidist_gen_log_lik(epidist_family(prep_obs))
  prep <- list(
    data = list(Y = 5, vreal1 = 5.5, vreal2 = 1, vreal3 = 1),
    ndraws = 1,
    nobs = 1
  )
  expect_error(
    log_lik(1, prep),
    "greater than the relative observation time"
  )
})

test_that("the post-processing uses the primary event distribution of the fit", { # nolint: line_length_linter.
  # A fit made with an exponentially growing primary event would otherwise be
  # post-processed as though the event were uniform.
  prep <- structure(
    list(
      data = list(
        Y = 5, vreal1 = 12, vreal2 = 1, vreal3 = 1, vreal4 = 6, vreal5 = 2
      ),
      dpars = list(
        mu = matrix(1.5, nrow = 3, ncol = 1),
        sigma = matrix(0.5, nrow = 3, ncol = 1),
        pgrowth = matrix(c(0.2, 0.4, 0.6), nrow = 3, ncol = 1)
      ),
      ndraws = 3,
      nobs = 1,
      family = list(primary = "expgrowth")
    ),
    class = "brmsprep"
  )

  log_lik <- epidist_gen_log_lik(lognormal())(i = 1, prep)

  expected <- vapply(
    seq_len(prep$ndraws),
    function(draw) {
      return(primarycensored::dpcens(
        x = 5,
        pdist = stats::plnorm,
        pwindow = 1,
        swindow = 1,
        L = 2,
        D = 12,
        dprimary = primarycensored::dexpgrowth,
        dprimary_args = list(r = prep$dpars$pgrowth[draw, 1]),
        log = TRUE,
        meanlog = 1.5,
        sdlog = 0.5
      ))
    },
    numeric(1)
  )
  expect_equal(log_lik, expected, tolerance = 1e-8)

  # Dropping the primary event distribution from the fit must change the
  # answer, otherwise this test would pass with it ignored.
  prep$family <- list()
  expect_false(isTRUE(all.equal(
    epidist_gen_log_lik(lognormal())(i = 1, prep),
    expected
  )))
})

test_that("posterior predictions use the primary event distribution", {
  prep <- structure(
    list(
      data = list(
        Y = 5, vreal1 = 30, vreal2 = 1, vreal3 = 1, vreal4 = 6, vreal5 = 0
      ),
      dpars = list(
        mu = matrix(1.5, nrow = 50, ncol = 1),
        sigma = matrix(0.5, nrow = 50, ncol = 1),
        pgrowth = matrix(5, nrow = 50, ncol = 1)
      ),
      ndraws = 50,
      nobs = 1,
      family = list(primary = "expgrowth")
    ),
    class = "brmsprep"
  )

  # A steep growth rate puts the primary event at the end of its window, so
  # the sampler must be given it rather than a uniform draw.
  set.seed(101)
  predict_fn <- epidist_gen_posterior_predict(lognormal())
  growing <- predict_fn(i = 1, prep)
  prep$family <- list()
  set.seed(101)
  uniform <- predict_fn(i = 1, prep)
  expect_false(isTRUE(all.equal(mean(growing), mean(uniform))))
})
