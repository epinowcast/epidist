#' Export a fitted delay distribution as a `distspec` distribution
#'
#' @description
#' Summarises the posterior of a fitted delay distribution into an uncertain
#' `<dist_spec>` from the `distspec` package, for use in packages that take
#' their delay distributions in that form. The natural parameters of the delay
#' distribution are computed for each posterior draw and summarised into a
#' prior on each of them.
#'
#' @details
#' The lognormal, gamma and Weibull families are supported, as those are the
#' delay distributions `distspec` has. Their `brms` parameters are mapped to
#' the natural parameters of the matching `distspec` constructor for every
#' draw before summarising:
#'
#' * lognormal: `mu` is `meanlog` and `sigma` is `sdlog` of
#'   [distspec::LogNormal()].
#' * gamma: `shape` is `shape` and `shape / mu` is `rate` of
#'   [distspec::Gamma()].
#' * Weibull: `shape` is `shape` and `mu / gamma(1 + 1 / shape)` is `scale`
#'   of [distspec::Weibull()].
#'
#' Open an issue at <https://github.com/epiforecasts/distspec/issues> to ask
#' `distspec` for another distribution.
#'
#' Each natural parameter gets a [distspec::Normal()] prior with the mean and
#' standard deviation of its marginal posterior. The posterior correlation
#' between the parameters is not represented, so sampling from the result
#' gives a wider range of delay distributions than the posterior does.
#'
#' The default `newdata` is built with [epidist_newdata()] by expanding the
#' variables in the model formula into a grid, so it has one row per unique
#' combination of the predictors, and gives the delay distribution with no
#' censoring and no truncation. A model with only an intercept gets a single
#' row. A continuous predictor gets a row per distinct value, so pass
#' `newdata` for such a model.
#'
#' @inheritParams delay_parameter_draws
#'
#' @param x A model fit with [epidist()].
#'
#' @param newdata A `data.frame` of data to predict for, with one row per
#'  delay distribution wanted. If `NULL`, the default, [epidist_newdata()]
#'  builds one row per unique combination of the predictors, with no
#'  censoring and no truncation. See the details.
#'
#' @param max The maximum of the delay distribution, passed to
#'  [distspec::bound_dist()]. Defaults to `Inf`, which is no maximum.
#'  [simulate_secondary()] does not apply this bound when drawing delays, and
#'  warns when given a bounded distribution.
#'
#' @param cdf_max The cumulative probability to keep the delay distribution
#'  up to, passed to [distspec::bound_dist()]. Defaults to 1, which keeps the
#'  whole distribution. Not applied by [simulate_secondary()] either.
#'
#' @family postprocess
#' @returns A `<dist_spec>` when `newdata` has one row. A named list of them,
#'  one per row of `newdata`, when it has several. The names give the values
#'  of the columns of `newdata` that differ between rows, such as `"sex=0"`,
#'  and are the row numbers when no column differs.
#'
#' @seealso [delay_parameter_draws()] for the draws this summarises,
#'  [epidist_newdata()] to build `newdata`, and [simulate_secondary()] to
#'  simulate delays from the result.
#'
#' @exportS3Method distspec::as_dist_spec
#' @examples
#' \donttest{
#' fit <- sierra_leone_ebola_data |>
#'   as_epidist_linelist_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested"
#'   ) |>
#'   as_epidist_aggregate_data() |>
#'   as_epidist_marginal_model() |>
#'   epidist(chains = 2, cores = 2, refresh = ifelse(interactive(), 250, 0))
#'
#' dist <- as_dist_spec(fit)
#' dist
#'
#' # The delay distribution at the posterior mean of its parameters
#' distspec::fix_parameters(dist, strategy = "mean")
#'
#' # Simulate delays that carry the posterior uncertainty
#' simulate_gillespie(seed = 1) |>
#'   simulate_secondary(dist) |>
#'   head()
#'
#' # Bound the delay distribution at 60 days for a package that takes bounds
#' as_dist_spec(fit, max = 60)
#' }
as_dist_spec.epidist_fit <- function(
  x,
  newdata = NULL,
  max = Inf,
  cdf_max = 1,
  ...
) {
  if (is.null(newdata)) {
    newdata <- .fit_newdata(x)
  }
  spec_family <- .dist_spec_family(.delay_family(x$family)$name)
  draws <- delay_parameter_draws(x, newdata = newdata, ...)
  draws <- dplyr::ungroup(draws)
  dists <- lapply(
    split(draws, draws$.row),
    .dist_spec_from_draws,
    family = spec_family,
    max = max,
    cdf_max = cdf_max
  )
  if (length(dists) == 1) {
    return(dists[[1]])
  }
  names(dists) <- .stratum_names(newdata)
  return(dists)
}

#' The `distspec` constructor and natural parameters of a delay family
#'
#' Maps the name of a delay distribution family to the `distspec` constructor
#' that represents it and to a function turning draws of its `brms`
#' parameters into draws of the natural parameters of that constructor.
#'
#' @param name The name of a delay distribution family, as returned by
#'  `.delay_family()`.
#'
#' @return A list with the family `name`, the `constructor` name, the `brms`
#'  parameters `dpars` it needs, and a function `natural` taking a list of
#'  draws of them and returning a named list of draws of the natural
#'  parameters.
#'
#' @keywords internal
.dist_spec_family <- function(name) {
  families <- list(
    lognormal = list(
      constructor = "LogNormal",
      dpars = c("mu", "sigma"),
      natural = function(d) {
        return(list(meanlog = d$mu, sdlog = d$sigma))
      }
    ),
    gamma = list(
      constructor = "Gamma",
      dpars = c("mu", "shape"),
      natural = function(d) {
        return(list(shape = d$shape, rate = d$shape / d$mu))
      }
    ),
    weibull = list(
      constructor = "Weibull",
      dpars = c("mu", "shape"),
      natural = function(d) {
        return(list(shape = d$shape, scale = d$mu / gamma(1 + 1 / d$shape)))
      }
    )
  )
  if (!name %in% names(families)) {
    cli_abort(c(
      "The {.val {name}} family cannot be exported as a {.cls dist_spec}.",
      i = "The supported families are {.val {names(families)}}.",
      "*" = "Ask {.pkg distspec} for {.val {name}} at
             {.url https://github.com/epiforecasts/distspec/issues}."
    ))
  }
  return(c(list(name = name), families[[name]]))
}

#' Summarise draws of the delay parameters into a `<dist_spec>`
#'
#' @inheritParams as_dist_spec.epidist_fit
#'
#' @param draws A `data.frame` of draws of the `brms` parameters of the
#'  delay distribution for a single row of `newdata`, as one group of the
#'  result of [delay_parameter_draws()].
#'
#' @param family A list describing the family, as returned by
#'  `.dist_spec_family()`.
#'
#' @return A `<dist_spec>`.
#'
#' @keywords internal
.dist_spec_from_draws <- function(draws, family, max = Inf, cdf_max = 1) {
  .assert_dpars(draws, family$name, family$dpars)
  if (nrow(draws) < 2) {
    cli_abort(
      "At least two draws are needed to summarise the posterior, not
       {nrow(draws)}."
    )
  }
  natural <- family$natural(as.list(draws)[family$dpars])
  # Each parameter gets its own prior, so the posterior correlation between
  # them is lost. A joint prior needs a multivariate normal, which distspec
  # does not have yet: see epiforecasts/distspec#140.
  params <- lapply(natural, function(p) {
    return(distspec::Normal(mean = mean(p), sd = stats::sd(p)))
  })
  constructor <- get(family$constructor, envir = asNamespace("distspec"))
  return(do.call(constructor, c(params, list(max = max, cdf_max = cdf_max))))
}

#' Default `newdata` for a fitted model
#'
#' Expands the variables in the distributional parameter formulas into a grid
#' with [epidist_newdata()], so there is one row per unique combination of
#' the predictors with no censoring and no truncation. `brms` keeps the data
#' a model was fitted to as a plain `data.frame`, so the `epidist` class is
#' restored from the family name first.
#'
#' @param object A model fit with [epidist()].
#'
#' @return A [tibble::tibble()] of `newdata`.
#'
#' @keywords internal
.fit_newdata <- function(object) {
  name <- object$family$name
  if (is.null(name)) {
    name <- object$family$family
  }
  name <- tolower(name)
  model <- regmatches(name, regexpr("^(latent|marginal|meta)", name))
  if (length(model) == 0) {
    model <- "naive"
  }
  model_data <- .new_epidist_data(
    object$data,
    paste0("epidist_", model, "_model")
  )
  vars <- intersect(.extract_dpar_terms(object$formula), names(model_data))
  return(do.call(epidist_newdata, c(list(model_data), lapply(vars, as.name))))
}

#' Name the rows of `newdata` by the columns that differ between them
#'
#' @inheritParams as_dist_spec.epidist_fit
#'
#' @return A character vector with one element per row of `newdata`.
#'
#' @keywords internal
.stratum_names <- function(newdata) {
  newdata <- tibble::as_tibble(newdata)
  varying <- vapply(
    newdata,
    function(x) {
      return(is.atomic(x) && is.null(dim(x)) && length(unique(x)) > 1)
    },
    logical(1)
  )
  if (!any(varying)) {
    return(as.character(seq_len(nrow(newdata))))
  }
  parts <- lapply(names(newdata)[varying], function(col) {
    return(paste0(col, "=", as.character(newdata[[col]])))
  })
  return(do.call(paste, c(parts, list(sep = ", "))))
}
