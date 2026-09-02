# Primary event distributions.
#
# Each entry describes one distribution from `primarycensored`. Adding a new
# one here is all that is needed for both models, provided `primarycensored`
# implements it in `primary_lpdf()` and provides an R density and sampler
# that share their parameter names.
#
# Distributional parameters are prefixed `p` and named after the parameter
# they set, so a bounded normal would use `pmu` and `psigma`, avoiding a clash
# with the delay distribution's own parameters.

#' Supported primary event distributions
#'
#' @returns A named list, one entry per distribution. Each entry gives the
#'  `primarycensored` `id` used to dispatch in Stan, the `dpars` it adds to the
#'  family, their `links` and `bounds`, the R density `ddist` and sampler
#'  `rdist` used in post-processing, and the `args` of those two functions that
#'  the `dpars` supply.
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
#' Also records the distribution on the family so that the post-processing
#' functions built from it use the same one.
#'
#' @param family A `brms` family object.
#'
#' @param data An `epidist` data object.
#'
#' @returns The family with any primary event parameters added.
#'
#' @keywords internal
.add_primary_dpars <- function(family, data) {
  primary <- .primary_dist(data)
  spec <- .primary_spec(primary)
  family$primary <- primary
  if (length(spec$dpars) == 0) {
    return(family)
  }
  family$dpars <- c(family$dpars, spec$dpars)
  family$other_links <- c(family$other_links, spec$links)
  family$other_bounds <- c(family$other_bounds, spec$bounds)
  return(family)
}

#' The primary event distribution of a family
#'
#' Families built outside [epidist_family()], and those made before this was
#' configurable, carry no primary event distribution and were uniform.
#'
#' @param family A `brms` family object.
#'
#' @returns The registry entry for the family's primary event distribution.
#'
#' @keywords internal
.primary_spec_from_family <- function(family) {
  primary <- family$primary
  if (is.null(primary)) {
    primary <- "uniform"
  }
  return(.primary_spec(primary))
}

#' The primary event distribution a fit was made with
#'
#' The post-processing functions are built from a family, which the caller may
#' supply directly rather than taking it from the fit. The fit itself is
#' authoritative, so prefer what it carries and fall back on the family.
#'
#' @param prep A `brms` prep object.
#'
#' @param spec The registry entry of the family the caller supplied.
#'
#' @returns A registry entry.
#'
#' @keywords internal
.primary_spec_from_prep <- function(prep, spec) {
  if (is.null(prep$family$primary)) {
    return(spec)
  }
  return(.primary_spec(prep$family$primary))
}

#' Primary event arguments for post-processing
#'
#' The post-processing functions read distributional parameters by name, so
#' without this a fit made with a non-uniform primary event would be
#' post-processed as though it were uniform.
#'
#' @param spec A registry entry, as returned by [.primary_spec()].
#'
#' @param prep A `brms` prep object.
#'
#' @param i The observation index.
#'
#' @param draw The posterior draw index, or `NULL` for all draws.
#'
#' @returns A named list of arguments for `spec$ddist` and `spec$rdist`.
#'
#' @keywords internal
.primary_args <- function(spec, prep, i, draw = NULL) {
  values <- lapply(spec$dpars, function(dpar) {
    value <- brms::get_dpar(prep, dpar, i = i)
    if (!is.null(draw)) {
      value <- value[[min(draw, length(value))]]
    }
    return(value)
  })
  names(values) <- spec$args
  return(values)
}

#' The primary event distribution arguments for Stan
#'
#' @param spec A registry entry, as returned by [.primary_spec()].
#'
#' @param empty The Stan expression to pass when the distribution takes no
#'  parameters.
#'
#' @returns The distribution id and its parameters, as Stan code.
#'
#' @keywords internal
.primary_stancode_args <- function(spec, empty = "primary_params") {
  params <- if (length(spec$dpars) == 0) {
    empty
  } else {
    paste0("{", toString(spec$dpars), "}")
  }
  return(paste0(spec$id, ", ", params))
}
