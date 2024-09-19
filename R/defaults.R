#' Default method for data validation
#'
#' @inheritParams epidist_validate
#' @param ... Additional arguments passed to method.
#' @family defaults
#' @export
epidist_validate.default <- function(data, ...) {
  cli_abort(
    "No epidist_validate method implemented for the class ", class(data), "\n",
    "See methods(epidist_validate) for available methods"
  )
}

#' Default method for defining a model specific formula
#'
#' @inheritParams epidist_formula
#' @param ... Additional arguments passed to method.
#' @family defaults
#' @export
epidist_formula.default <- function(data, ...) {
  cli_abort(
    "No epidist_formula method implemented for the class ", class(data), "\n",
    "See methods(epidist_formula) for available methods"
  )
}

#' Default method for defining a model specific family
#'
#' @inheritParams epidist_family
#' @param ... Additional arguments passed to method.
#' @family defaults
#' @export
epidist_family.default <- function(data, ...) {
  cli_abort(
    "No epidist_family method implemented for the class ", class(data), "\n",
    "See methods(epidist_family) for available methods"
  )
}

#' Default method for defining model specific Stan code
#'
#' @inheritParams epidist_stancode
#' @param ... Additional arguments passed to method.
#' @family defaults
#' @export
epidist_stancode.default <- function(data, ...) {
  cli_abort(
    "No epidist_stancode method implemented for the class ", class(data), "\n",
    "See methods(epidist_stancode) for available methods"
  )
}

#' Default method used for interface using `brms`
#'
#' @inheritParams epidist
#' @inheritParams epidist_formula
#' @rdname epidist.default
#' @method epidist default
#' @family defaults
#' @export
epidist.default <- function(data, formula = brms::bf(mu ~ 1),
                            family = "lognormal", prior = NULL,
                            backend = "cmdstanr", fn = brms::brm, ...) {
  epidist_validate(data)
  epidist_family <- epidist_family(data, family)
  epidist_formula <- epidist_formula(
    data = data, family = epidist_family, formula = formula
  )
  epidist_prior <- epidist_prior(
    data = data, family = family, formula = epidist_formula, prior
  )
  epidist_stancode <- epidist_stancode(
    data = data, family = epidist_family, formula = epidist_formula
  )
  fit <- fn(
    formula = epidist_formula, family = epidist_family, prior = epidist_prior,
    stanvars = epidist_stancode, backend = backend, data = data, ...
  )

  class(fit) <- c(class(fit), "epidist_fit")

  return(fit)
}
