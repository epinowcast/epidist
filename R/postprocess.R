#' Posterior draws of the delay distribution parameters
#'
#' @description
#' Returns posterior draws of the parameters of the delay distribution in the
#' long format used by `tidybayes`. The delay parameters are the distributional
#' parameters of the `brms` family, evaluated on the response scale for each
#' row of `newdata`. For a lognormal model they are `mu` and `sigma`. They are
#' the parameters of the delay distribution itself, so they do not describe the
#' censoring or truncation of the observation process, and they are not the
#' natural scale mean and standard deviation of the delay. Use
#' [add_summaries()] to add those.
#'
#' `add_delay_parameter_draws()` is the same function with `newdata` first, for
#' use at the start of a pipeline as with [tidybayes::add_epred_draws()].
#'
#' @details
#' The returned columns follow the `tidybayes` conventions. The columns of
#' `newdata` come first, followed by `.row`, `.chain`, `.iteration` and
#' `.draw`, followed by one column per distributional parameter. The result is
#' grouped by the columns of `newdata` and by `.row`. `.chain` and `.iteration`
#' are `NA` when the draws have been subset, because the chain a subset draw
#' came from is not recoverable.
#'
#' Every row of `newdata` gets its own draws, so passing the data the model was
#' fitted to produces many identical draws when the model has few unique
#' combinations of predictors. [epidist_strata()] returns one row per unique
#' combination and is usually the better input.
#'
#' @param object A model fit with [epidist::epidist()].
#'
#' @param newdata A `data.frame` of data to predict for. If `NULL`, the default,
#'  the data the model was fitted to is used. The `brms` models `epidist` fits
#'  need the model variables as well as the predictors, so build `newdata` with
#'  [epidist_strata()] rather than from the predictors alone.
#'
#' @param ... Additional arguments passed to [brms::prepare_predictions()],
#'  such as `ndraws` or `re_formula`.
#'
#' @family postprocess
#' @returns A `tibble` of posterior draws of the delay distribution parameters,
#'  grouped by the columns of `newdata` and by `.row`.
#'
#' @seealso [add_summaries()] to add natural scale summaries of the delay,
#'  [epidist_strata()] to build `newdata`.
#'
#' @export
#' @examples
#' \dontrun{
#' # `fit` is a model fitted with `epidist()`
#' fit |>
#'   epidist_strata() |>
#'   add_delay_parameter_draws(fit) |>
#'   add_summaries(probs = c(0.05, 0.95))
#' }
delay_parameter_draws <- function(object, newdata = NULL, ...) {
  draws <- .dpar_draws(object, newdata = newdata, ...)
  pred_data <- newdata
  if (is.null(pred_data)) {
    pred_data <- object$data
  }
  pred_data <- tibble::as_tibble(pred_data)
  out <- dplyr::bind_cols(pred_data[draws$.row, , drop = FALSE], draws)
  # Matrix and list columns cannot be grouped by
  vector_cols <- vapply(
    pred_data,
    function(x) {
      return(is.atomic(x) && is.null(dim(x)))
    },
    logical(1)
  )
  group_cols <- c(names(pred_data)[vector_cols], ".row")
  out <- dplyr::group_by(out, dplyr::across(dplyr::all_of(group_cols)))
  attr(out, "epidist_family") <- .delay_family(object$family)
  return(out)
}

#' @rdname delay_parameter_draws
#' @export
add_delay_parameter_draws <- function(newdata, object, ...) {
  return(delay_parameter_draws(object, newdata = newdata, ...))
}

#' Draws of each distributional parameter in a long `data.frame`
#'
#' @inheritParams delay_parameter_draws
#'
#' @return A `data.frame` with columns `.row`, `.chain`, `.iteration`, `.draw`
#'  and one column per distributional parameter.
#'
#' @keywords internal
.dpar_draws <- function(object, newdata = NULL, ...) {
  pp <- brms::prepare_predictions(
    object,
    newdata = newdata,
    check_response = FALSE,
    ...
  )
  ndraws <- pp$ndraws
  n_obs <- pp$nobs
  out <- expand.grid(.draw = seq_len(ndraws), .row = seq_len(n_obs))
  ids <- .draw_chain_iteration(object, pp)
  out[[".chain"]] <- ids$chain[out$.draw]
  out[[".iteration"]] <- ids$iteration[out$.draw]
  out <- out[, c(".row", ".chain", ".iteration", ".draw")]
  # `mu` is a parameter of every `brms` family so it always comes first
  dpars <- c("mu", setdiff(names(pp$dpars), "mu"))
  for (dpar in dpars) {
    lp <- brms::get_dpar(pp, dpar = dpar, inv_link = TRUE)
    if (!is.matrix(lp)) {
      lp <- matrix(lp, nrow = ndraws, ncol = n_obs)
    }
    out[[dpar]] <- as.vector(lp)
  }
  return(tibble::as_tibble(out))
}

#' Chain and iteration index of each posterior draw
#'
#' `brms` stores draws ordered by chain, so the chain and iteration of a draw
#' follow from its position. A subset of draws cannot be placed this way, so
#' both are `NA` in that case, as they are in `tidybayes`.
#'
#' @inheritParams delay_parameter_draws
#'
#' @param pp A `brmsprep` object from [brms::prepare_predictions()].
#'
#' @return A list with `chain` and `iteration` elements.
#'
#' @keywords internal
.draw_chain_iteration <- function(object, pp) {
  na <- list(
    chain = rep(NA_integer_, pp$ndraws),
    iteration = rep(NA_integer_, pp$ndraws)
  )
  if (!is.null(pp$draw_ids)) {
    return(na)
  }
  nchains <- brms::nchains(object)
  niterations <- brms::niterations(object)
  if (nchains * niterations != pp$ndraws) {
    return(na)
  }
  draws <- seq_len(pp$ndraws)
  return(list(
    chain = as.integer((draws - 1) %/% niterations + 1),
    iteration = as.integer((draws - 1) %% niterations + 1)
  ))
}

#' Unique combinations of the predictors in a model
#'
#' @description
#' Returns one row of the model data for each unique combination of the
#' variables used to predict the delay distribution parameters. Passing this to
#' [delay_parameter_draws()] gives one set of draws per combination rather than
#' one per observation, which is the same result with far fewer draws.
#'
#' @details
#' The variables are taken from the right hand side of each distributional
#' parameter formula. The remaining columns are kept from the first row of the
#' model data in which each combination occurs. This keeps the model variables
#' that `brms` requires in `newdata`, such as the relative observation time and
#' the censoring windows for the latent and marginal models. Those variables do
#' not enter the distributional parameters, so the values kept do not change
#' the draws.
#'
#' A model with only an intercept has no predictors and so returns a single
#' row. A continuous predictor has as many combinations as it has distinct
#' values, so consider passing `vars` and a grid of your own instead.
#'
#' @inheritParams delay_parameter_draws
#'
#' @param vars A character vector of variables to take unique combinations of.
#'  If `NULL`, the default, the variables in the distributional parameter
#'  formulas are used.
#'
#' @family postprocess
#' @returns A `tibble` with one row per unique combination of `vars`, with the
#'  combination columns first.
#'
#' @export
#' @examples
#' \dontrun{
#' # `fit` is a model fitted with `epidist()`
#' epidist_strata(fit)
#' }
epidist_strata <- function(object, vars = NULL) {
  model_data <- object$data
  if (is.null(model_data)) {
    cli_abort("{.arg object} does not contain the data it was fitted to.")
  }
  model_data <- tibble::as_tibble(model_data)
  if (is.null(vars)) {
    vars <- intersect(.extract_dpar_terms(object$formula), names(model_data))
  } else {
    assert_character(vars, any.missing = FALSE)
    assert_names(names(model_data), must.include = vars)
  }
  if (length(vars) == 0) {
    return(model_data[1, , drop = FALSE])
  }
  strata <- dplyr::distinct(
    model_data,
    dplyr::across(dplyr::all_of(vars)),
    .keep_all = TRUE
  )
  return(dplyr::relocate(strata, dplyr::all_of(vars)))
}

#' Add natural scale summaries of the delay distribution
#'
#' @description
#' Adds the mean and standard deviation of the delay distribution implied by
#' each draw of the distributional parameters, and quantiles of that
#' distribution if `probs` is given. Analytic solutions are used for the
#' families that have one. Every other family is summarised by simulating
#' delays from it, which works for any family `brms` can predict from.
#'
#' @details
#' The summaries describe the delay distribution, not the posterior. A row of
#' `data` holds one draw of the distributional parameters, and the columns
#' added are the mean, standard deviation and quantiles of the delay
#' distribution those parameters define. Summarise the resulting columns across
#' draws to get posterior summaries of them.
#'
#' Quantile columns are named as in [posterior::quantile2()], so `probs = 0.05`
#' gives a `q5` column.
#'
#' Simulation adds Monte Carlo error to the summaries. The standard error of
#' the mean is the delay standard deviation divided by the square root of
#' `nsim`. Simulation is also memory hungry, because it draws `nsim` delays for
#' every row of `data`. Reduce the number of rows with [epidist_strata()], or
#' the number of draws with the `ndraws` argument of
#' [delay_parameter_draws()], if it is slow.
#'
#' @param data A `data.frame` of draws of the distributional parameters, as
#'  returned by [delay_parameter_draws()].
#'
#' @param family A model fit with [epidist::epidist()], a `brms` family, or
#'  the name of one, giving the delay distribution. If `NULL`, the default,
#'  the family is taken from `data`, which [delay_parameter_draws()] records
#'  on it. Some `dplyr` verbs drop that record, so pass the fit or the family
#'  if `data` has been through one of them.
#'
#' @param probs A numeric vector of probabilities to add quantiles of the delay
#'  distribution for. If `NULL`, the default, no quantiles are added.
#'
#' @param method Either `"auto"`, the default, which uses the analytic solution
#'  when there is one and simulates otherwise, `"analytic"`, which errors when
#'  there is no analytic solution, or `"sample"`, which always simulates.
#'
#' @param nsim The number of delays to simulate per row of `data`. Defaults to
#'  1000. Only used when simulating.
#'
#' @family postprocess
#' @returns The input with `mean` and `sd` columns added, and one column per
#'  element of `probs`.
#'
#' @export
#' @examples
#' draws <- data.frame(mu = c(1.8, 2.0), sigma = c(0.5, 0.4))
#' add_summaries(draws, family = "lognormal", probs = c(0.05, 0.95))
add_summaries <- function(
  data,
  family = NULL,
  probs = NULL,
  method = c("auto", "analytic", "sample"),
  nsim = 1000
) {
  method <- match.arg(method)
  assert_data_frame(data)
  assert_numeric(
    probs,
    lower = 0,
    upper = 1,
    any.missing = FALSE,
    null.ok = TRUE
  )
  assert_numeric(nsim, lower = 1, len = 1, any.missing = FALSE)
  family <- .resolve_delay_family(data, family)
  analytic <- .analytic_delay_summaries(family$name)
  has_analytic <- !is.null(analytic) && all(analytic$dpars %in% names(data))
  if (identical(method, "analytic") && !has_analytic) {
    cli_abort(c(
      "No analytic delay summaries are available for {.val {family$name}}.",
      i = "Use {.code method = \"sample\"} to summarise by simulation."
    ))
  }
  if (has_analytic && !identical(method, "sample")) {
    return(.analytic_summaries(data, analytic, probs))
  }
  return(.sample_summaries(data, family, probs, nsim))
}

#' Add summaries from an analytic solution
#'
#' @inheritParams add_summaries
#'
#' @param analytic A list of analytic solutions, as returned by
#'  `.analytic_delay_summaries()`.
#'
#' @return The input with summary columns added.
#'
#' @keywords internal
.analytic_summaries <- function(data, analytic, probs = NULL) {
  dpars <- as.list(data)[analytic$dpars]
  data[["mean"]] <- analytic$mean(dpars)
  data[["sd"]] <- analytic$sd(dpars)
  for (prob in probs) {
    data[[.quantile_name(prob)]] <- analytic$quantile(dpars, prob)
  }
  return(data)
}

#' Add summaries by simulating from the delay distribution
#'
#' @inheritParams add_summaries
#'
#' @return The input with summary columns added.
#'
#' @keywords internal
.sample_summaries <- function(data, family, probs = NULL, nsim = 1000) {
  missing_dpars <- setdiff(family$dpars, names(data))
  if (length(missing_dpars) > 0) {
    name <- family$name
    cli_abort(c(
      "{.arg data} is missing distributional parameters of the {.val {name}}
       family: {.val {missing_dpars}}.",
      i = "{.fn delay_parameter_draws} returns every distributional parameter."
    ))
  }
  dpars <- as.list(data)[family$dpars]
  samples <- .simulate_delays(family, dpars, nsim)
  data[["mean"]] <- rowMeans(samples)
  data[["sd"]] <- apply(samples, 1, stats::sd)
  for (prob in probs) {
    data[[.quantile_name(prob)]] <- apply(
      samples,
      1,
      stats::quantile,
      probs = prob,
      names = FALSE
    )
  }
  return(data)
}

#' Simulate delays from each draw of the distributional parameters
#'
#' Simulation goes through the `brms` posterior prediction function for the
#' family, so it works for any family `brms` can predict from. Rows are
#' simulated in chunks to bound the memory used.
#'
#' @inheritParams add_summaries
#'
#' @param dpars A named list of distributional parameter vectors.
#'
#' @return A matrix with one row per element of the vectors in `dpars` and
#'  `nsim` columns.
#'
#' @keywords internal
.simulate_delays <- function(family, dpars, nsim = 1000) {
  predict_fn <- .get_brms_fn("posterior_predict", list(family = family$name))
  n <- length(dpars[[1]])
  samples <- matrix(NA_real_, nrow = n, ncol = nsim)
  # A chunk holds `chunk * nsim` delays, so this bounds the memory used
  chunk <- max(1L, floor(1e6 / nsim))
  for (start in seq(1, n, by = chunk)) {
    rows <- seq(start, min(start + chunk - 1, n))
    prep <- list(
      ndraws = length(rows) * nsim,
      nobs = 1L,
      dpars = lapply(dpars, function(x) rep(x[rows], each = nsim)),
      data = list()
    )
    class(prep) <- "brmsprep"
    drawn <- try(predict_fn(i = 1, prep = prep), silent = TRUE)
    if (inherits(drawn, "try-error")) {
      cli_abort(c(
        "Could not simulate delays from the {.val {family$name}} family.",
        i = "The {.pkg brms} error was: {conditionMessage(attr(drawn, 'condition'))}" # nolint: line_length_linter.
      ))
    }
    samples[rows, ] <- matrix(
      as.vector(drawn),
      nrow = length(rows),
      ncol = nsim,
      byrow = TRUE
    )
  }
  return(samples)
}

#' Analytic delay summaries for the families that have them
#'
#' Each element gives the distributional parameters the solution needs and
#' functions of them returning the mean, the standard deviation and the
#' quantile function of the delay distribution. The parameters are the `brms`
#' parameters of the family.
#'
#' @param name The name of a delay distribution family.
#'
#' @return A list of solutions, or `NULL` when the family has none.
#'
#' @keywords internal
.analytic_delay_summaries <- function(name) {
  return(switch(name,
    lognormal = list(
      dpars = c("mu", "sigma"),
      mean = function(d) {
        return(exp(d$mu + d$sigma^2 / 2))
      },
      sd = function(d) {
        return(exp(d$mu + d$sigma^2 / 2) * sqrt(exp(d$sigma^2) - 1))
      },
      quantile = function(d, p) {
        return(stats::qlnorm(p, meanlog = d$mu, sdlog = d$sigma))
      }
    ),
    gamma = list(
      dpars = c("mu", "shape"),
      mean = function(d) {
        return(d$mu)
      },
      sd = function(d) {
        return(d$mu / sqrt(d$shape))
      },
      quantile = function(d, p) {
        return(stats::qgamma(p, shape = d$shape, rate = d$shape / d$mu))
      }
    ),
    weibull = list(
      dpars = c("mu", "shape"),
      mean = function(d) {
        return(d$mu)
      },
      sd = function(d) {
        return(
          d$mu * sqrt(gamma(1 + 2 / d$shape) / gamma(1 + 1 / d$shape)^2 - 1)
        )
      },
      quantile = function(d, p) {
        return(stats::qweibull(
          p,
          shape = d$shape,
          scale = d$mu / gamma(1 + 1 / d$shape)
        ))
      }
    ),
    NULL
  ))
}

#' Name a quantile column as `posterior` does
#'
#' @param prob A probability.
#'
#' @return A column name.
#'
#' @keywords internal
.quantile_name <- function(prob) {
  return(paste0("q", format(prob * 100, trim = TRUE, scientific = FALSE)))
}

#' The delay distribution family of a fitted model
#'
#' The families `epidist` builds are `brms` custom families named after the
#' model and the delay distribution, such as `latent_lognormal`. This returns
#' the delay distribution part, which is what the summaries and the simulation
#' need.
#'
#' @param family A `brms` family.
#'
#' @return A list with the delay distribution `name` and its distributional
#'  parameters `dpars`.
#'
#' @keywords internal
.delay_family <- function(family) {
  name <- family$name
  if (is.null(name)) {
    name <- family$family
  }
  name <- tolower(name)
  # Drop the model prefix `epidist` adds, keeping families whose own name
  # contains an underscore intact
  candidates <- unique(c(
    name,
    sub("^(latent|marginal)_", "", name),
    sub("^[^_]+_", "", name),
    sub(".*_", "", name)
  ))
  known <- vapply(
    candidates,
    function(x) {
      return(exists(
        paste0("posterior_predict_", x),
        envir = asNamespace("brms"),
        inherits = FALSE
      ))
    },
    logical(1)
  )
  name <- candidates[length(candidates)]
  if (any(known)) {
    name <- candidates[known][1]
  }
  return(list(name = name, dpars = family$dpars))
}

#' Resolve the delay distribution family of a `data.frame` of draws
#'
#' @inheritParams add_summaries
#'
#' @return A list with the delay distribution `name` and its distributional
#'  parameters `dpars`.
#'
#' @keywords internal
.resolve_delay_family <- function(data, family = NULL) {
  if (inherits(family, "brmsfit")) {
    family <- family$family
  }
  if (!is.null(family)) {
    return(.delay_family(.validate_family(family)))
  }
  recorded <- attr(data, "epidist_family")
  if (!is.null(recorded)) {
    return(recorded)
  }
  # `predict_delay_parameters()` recorded the family in the class instead
  samples_class <- grep("_samples$", class(data), value = TRUE)
  if (length(samples_class) > 0) {
    name <- sub("_samples$", "", samples_class[1])
    return(.delay_family(.validate_family(name)))
  }
  return(cli_abort(c(
    "Could not work out the delay distribution family of {.arg data}.",
    i = "Grouping verbs drop the family {.fn delay_parameter_draws} records,
         so pass it with the {.arg family} argument, as {.code family = fit}."
  )))
}
