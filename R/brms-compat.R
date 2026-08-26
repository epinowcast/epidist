# Compatibility helpers for internal 'brms' behaviour
#
# These helpers replace `brms:::validate_family()`,
# `brms:::validate_formula()`, `brms:::validate_data()`,
# `brms:::dpar_bounds()` and `brms:::log_lik_weight()`, because calls to
# unexported functions are flagged by `R CMD check --as-cran` and are not
# covered by the 'brms' interface guarantees. They reproduce only the narrow
# behaviour that 'epidist' depends on, written against the public 'brms'
# interface rather than copied from the 'brms' source.
#
# Behaviour was checked against 'brms' 2.23.0. The equivalence tests in
# `tests/testthat/test-brms-compat.R` compare each helper against the
# 'brms' internal it replaces, so drift in either package is caught.
#
# Credit for the original behaviour goes to the 'brms' authors.
# Upstream request to export these helpers:
# <https://github.com/paul-buerkner/brms/issues/1676>.
# 'epidist' tracking issue:
# <https://github.com/epinowcast/epidist/issues/420>.

#' Response families that `epidist` does not support
#'
#' Ordinal, categorical, mixture and Cox families need extra handling in
#' `brms` that the helpers in this file deliberately do not reproduce.
#'
#' @keywords internal
.unsupported_families <- function() {
  return(c(
    "cumulative", "sratio", "cratio", "acat",
    "categorical", "multinomial", "dirichlet", "dirichlet2",
    "logistic_normal", "cox"
  ))
}

#' Validate a response family
#'
#' Replaces `brms:::validate_family()`. Guarantees that a family given as a
#' family function, a `brms` family object, a `stats` family object or a
#' character string is returned as a `brmsfamily` object. Unlike the `brms`
#' internal it does not handle the `threshold` argument of ordinal
#' families, which `epidist` never sets.
#'
#' @param family A family function, a `brmsfamily` object, a `stats` family
#'  object, or a character string naming a `brms` family.
#'
#' @param link Optional character string giving the link function. Only used
#'  when `family` is a character string without a second element.
#'
#' @keywords internal
.validate_family <- function(family, link = NULL) {
  if (is.function(family)) {
    family <- family()
  }
  if (inherits(family, "brmsfamily")) {
    return(family)
  }
  if (inherits(family, "family")) {
    link <- family$link
    family <- family$family
  }
  if (!is.character(family)) {
    cli_abort("Argument {.arg family} is invalid.")
  }
  if (is.null(link)) {
    link <- family[2]
  }
  out <- brms::brmsfamily(family[1], link = link)
  # `brms::brmsfamily()` always records the default link of every
  # distributional parameter, whereas the internal constructor only keeps
  # links that were requested explicitly. Drop the defaults again so that
  # `.add_dpar_info()` sees the same fields either way.
  out[grep("^link_", names(out), value = TRUE)] <- NULL
  return(out)
}

#' Check that a family is one the `epidist` helpers support
#'
#' @param family A `brmsfamily` object.
#'
#' @keywords internal
.assert_supported_family <- function(family) {
  unsupported <- inherits(family, "mixfamily") ||
    isTRUE(family$family %in% .unsupported_families())
  if (unsupported) {
    cli_abort(c(
      "{.val {family$family}} is not a supported {.pkg epidist} family.",
      i = "Ordinal, categorical, mixture and Cox families are not supported."
    ))
  }
  return(invisible(family))
}

#' Expand a dot in a model formula
#'
#' Replaces the part of `brms:::expand_dot_formula()` that `epidist` needs.
#' A formula containing `.` on the right hand side is expanded against
#' `data`, keeping the attributes of the original formula.
#'
#' @param formula A formula object.
#'
#' @param data A `data.frame` used to expand `.`.
#'
#' @keywords internal
.expand_dot_formula <- function(formula, data = NULL) {
  if (!isTRUE("." %in% all.vars(formula))) {
    return(formula)
  }
  att <- attributes(formula)
  expanded <- try(stats::terms(formula, data = data), silent = TRUE)
  if (!inherits(expanded, "try-error")) {
    formula <- stats::formula(expanded)
  }
  attributes(formula) <- att
  return(formula)
}

#' Validate a model formula
#'
#' Replaces `brms:::validate_formula()` for the families `epidist`
#' supports. Guarantees that the result is a `brmsformula` object carrying
#' the validated family, with any `.` on the right hand side expanded
#' against `data` and with `mecor` set to its default of `TRUE`. The `brms`
#' internal additionally handles ordinal, categorical, mixture and Cox
#' families, and the deprecated `autocor`, `sparse` and `cov_ranef`
#' arguments. None of those are supported here.
#'
#' @param formula A formula or `brmsformula` object.
#'
#' @param family A description of the response distribution and link
#'  function.
#'
#' @param data A `data.frame` used to expand `.` in the formula.
#'
#' @keywords internal
.validate_formula <- function(formula, family = NULL, data = NULL) {
  out <- brms::bf(formula)
  if (is.null(out$family) && !is.null(family)) {
    out$family <- .validate_family(family)
  }
  .assert_supported_family(out$family)
  out$formula <- .expand_dot_formula(out$formula, data)
  for (i in seq_along(out$pforms)) {
    out$pforms[[i]] <- .expand_dot_formula(out$pforms[[i]], data)
  }
  if (is.null(out$mecor)) {
    out$mecor <- TRUE
  }
  return(brms::bf(out))
}

#' Validate model data
#'
#' Replaces the checks that `epidist` relies on from
#' `brms:::validate_data()`. `epidist` calls the `brms` internal only for
#' its errors and discards the returned model frame, so this helper checks
#' the same conditions and returns `data` invisibly. It errors when `data`
#' cannot be coerced to a `data.frame`, when it has no rows, when a
#' variable used in the formula is absent, when a column name contains a
#' double underscore or ends in an underscore, or when no complete case
#' remains. It warns when a used column contains infinite values.
#'
#' @param data A `data.frame` containing the model data.
#'
#' @param bterms An object returned by [brms::brmsterms()].
#'
#' @keywords internal
.validate_data <- function(data, bterms) {
  if (missing(data)) {
    cli_abort("Data must be specified using the {.arg data} argument.")
  }
  data <- try(as.data.frame(data), silent = TRUE)
  if (inherits(data, "try-error")) {
    cli_abort("Argument {.arg data} must be coercible to a data.frame.")
  }
  if (!isTRUE(nrow(data) > 0L)) {
    cli_abort("Argument {.arg data} does not contain observations.")
  }
  vars <- all.vars(bterms$allvars)
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    cli_abort(
      "The following variables can not be found in {.arg data}:
       {.val {missing_vars}}"
    )
  }
  if (any(grepl("__|_$", names(data)))) {
    cli_abort(
      "Variable names may not contain double underscores or underscores at
       the end."
    )
  }
  used <- data[, vars, drop = FALSE]
  if (!any(stats::complete.cases(used))) {
    cli_abort(
      "All observations in the data were removed, presumably because of
       {.val NA} values."
    )
  }
  is_inf <- vapply(used, function(x) any(is.infinite(x)), logical(1))
  if (any(is_inf)) {
    cli_warn(
      "Found infinite values in the data, which may cause issues for Stan."
    )
  }
  return(invisible(data))
}

#' Natural scale bounds of a distributional parameter
#'
#' Replaces `brms:::dpar_bounds()` for the non-mixture, non-custom families
#' that `epidist` supports. Guarantees a list with character elements `lb`
#' and `ub` giving the lower and upper bound of `dpar` on the natural
#' scale, where `""` means unbounded. Unlike the `brms` internal, an
#' unrecognised parameter is an error rather than `NULL`, because `epidist`
#' cannot generate Stan code without a bound.
#'
#' @param dpar A character string naming a distributional parameter.
#'
#' @param family Unused. Kept so that the signature matches the `brms`
#'  internal this helper replaces.
#'
#' @keywords internal
.dpar_bounds <- function(dpar, family = NULL) {
  bounds <- list(
    sigma = list(lb = "0", ub = ""),
    shape = list(lb = "0", ub = ""),
    nu = list(lb = "1", ub = ""),
    phi = list(lb = "0", ub = ""),
    kappa = list(lb = "0", ub = ""),
    beta = list(lb = "0", ub = ""),
    bs = list(lb = "0", ub = ""),
    disc = list(lb = "0", ub = ""),
    zi = list(lb = "0", ub = "1"),
    hu = list(lb = "0", ub = "1"),
    zoi = list(lb = "0", ub = "1"),
    coi = list(lb = "0", ub = "1"),
    bias = list(lb = "0", ub = "1"),
    quantile = list(lb = "0", ub = "1"),
    xi = list(lb = "", ub = ""),
    alpha = list(lb = "", ub = "")
  )
  out <- bounds[[dpar]]
  if (is.null(out)) {
    cli_abort(c(
      "Bounds for the distributional parameter {.val {dpar}} are unknown.",
      i = "Supported parameters are {.val {names(bounds)}}."
    ))
  }
  return(out)
}

#' Apply observation weights to a log likelihood
#'
#' Replaces `brms:::log_lik_weight()`. Multiplies `x` by the weight of
#' observation `i`, if the model has weights, and returns `x` unchanged
#' otherwise.
#'
#' @param x A numeric vector of log likelihood values.
#'
#' @param i The index of the observation.
#'
#' @param prep A `brms` prepared predictions object.
#'
#' @keywords internal
.log_lik_weight <- function(x, i, prep) {
  weight <- prep$data$weights[i]
  if (!is.null(weight)) {
    x <- x * weight
  }
  return(x)
}
