# Primary event distributions.
#
# Each entry describes one distribution from `primarycensored`. Adding a new
# one here is all that is needed for both models, provided `primarycensored`
# implements it in `primary_lpdf()` and provides an R density and sampler.
#
# Distributional parameters are prefixed `p` and named after the `brms`
# parameter they correspond to, so a bounded normal would use `pmu` and
# `psigma`, avoiding a clash with the delay distribution's own parameters.

#' Supported primary event distributions
#'
#' @returns A named list, one entry per distribution, giving the
#'  `primarycensored` id, the distributional parameters it adds, their links
#'  and bounds, and the R density and sampler used in post-processing.
#'
#' @keywords internal
.primary_registry <- function() {
  return(list(
    uniform = list(
      id = 1L,
      dpars = character(0),
      links = character(0),
      bounds = list(),
      ddist = stats::dunif,
      rdist = stats::runif,
      args = character(0)
    ),
    expgrowth = list(
      id = 2L,
      dpars = "pgrowth",
      links = "identity",
      bounds = list(list(lb = NA, ub = NA)),
      ddist = primarycensored::dexpgrowth,
      rdist = primarycensored::rexpgrowth,
      args = "r"
    )
  ))
}

#' Names of the supported primary event distributions
#'
#' @returns A character vector, with the default first.
#'
#' @keywords internal
.primary_choices <- function() {
  return(names(.primary_registry()))
}

#' Look up a primary event distribution
#'
#' @param primary The distribution name.
#'
#' @returns The registry entry for `primary`.
#'
#' @keywords internal
.primary_spec <- function(primary) {
  registry <- .primary_registry()
  primary <- match.arg(primary, names(registry))
  return(registry[[primary]])
}

#' The primary event distribution of an `epidist` object
#'
#' Absent for objects made before this was configurable, which were uniform.
#'
#' @param data An `epidist` data object.
#'
#' @returns The primary event distribution as a string.
#'
#' @keywords internal
.primary_dist <- function(data) {
  primary <- attr(data, "primary")
  if (is.null(primary)) {
    return("uniform")
  }
  return(primary)
}

#' Add the distributional parameters a primary event distribution needs
#'
#' @param family A `brms` family object.
#'
#' @param data An `epidist` data object.
#'
#' @returns The family with any primary event parameters added.
#'
#' @keywords internal
.add_primary_dpars <- function(family, data) {
  spec <- .primary_spec(.primary_dist(data))
  if (length(spec$dpars) == 0) {
    return(family)
  }
  family$dpars <- c(family$dpars, spec$dpars)
  family$other_links <- c(family$other_links, spec$links)
  family$other_bounds <- c(family$other_bounds, spec$bounds)
  return(family)
}

#' Primary event arguments for post-processing
#'
#' The post-processing functions read distributional parameters by name, so
#' without this a fit made with a non-uniform primary event would be
#' post-processed as though it were uniform.
#'
#' @param prep A `brms` prep object.
#'
#' @param i The observation index.
#'
#' @param draw The posterior draw index, or `NULL` for all draws.
#'
#' @returns A list with `dprimary` and `dprimary_args`.
#'
#' @keywords internal
.primary_args <- function(prep, i, draw = NULL) {
  spec <- .primary_spec_from_prep(prep)
  if (length(spec$dpars) == 0) {
    return(list(dprimary = spec$ddist, dprimary_args = list()))
  }
  values <- lapply(spec$dpars, function(dpar) {
    value <- brms::get_dpar(prep, dpar, i = i)
    if (!is.null(draw)) {
      value <- value[[min(draw, length(value))]]
    }
    return(value)
  })
  names(values) <- spec$args
  return(list(dprimary = spec$ddist, dprimary_args = values))
}

#' Identify the primary event distribution from a prep object
#'
#' @param prep A `brms` prep object.
#'
#' @returns The registry entry whose parameters the fit carries.
#'
#' @keywords internal
.primary_spec_from_prep <- function(prep) {
  registry <- .primary_registry()
  for (name in names(registry)) {
    spec <- registry[[name]]
    if (length(spec$dpars) > 0 && all(spec$dpars %in% names(prep$dpars))) {
      return(spec)
    }
  }
  return(registry$uniform)
}
