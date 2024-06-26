#' Default method used when preparing data
#'
#' @param model Character string, model type to prepare to use.
#' Supported options are "latent_individual".
#' @param ... Additional arguments passed to model specific `epidist_prepare`
#' functions
#' @rdname epidist_prepare
#' @method epidist_prepare default
#' @family defaults
#' @export
epidist_prepare.default <- function(data, model, ...) {
  model <- match.arg(model, choices = c("latent_individual"))
  class(data) <- c(class(data), paste0("epidist_", model))
  epidist_prepare(data, ...)
}

#' Default method for defining a model specific formula
#'
#' @inheritParams epidist_formula
#' @param ... Additional arguments for method.
#' @family defaults
#' @export
epidist_formula.default <- function(data, ...) {
  stop(
    "No epidist_formula method implemented for the class ", class(data), "\n",
    "See methods(epidist_formula) for available methods"
  )
}

#' Default method for defining a model specific family
#'
#' @inheritParams epidist_family
#' @param ... Additional arguments for method.
#' @family defaults
#' @export
epidist_family.default <- function(data, ...) {
  stop(
    "No epidist_family method implemented for the class ", class(data), "\n",
    "See methods(epidist_family) for available methods"
  )
}

#' Default method for defining model specific priors
#'

#' @inheritParams epidist_prior
#' @param ... Additional arguments for method.
#' @family defaults
#' @export
epidist_prior.default <- function(data, ...) {
  stop(
    "No epidist_prior method implemented for the class ", class(data), "\n",
    "See methods(epidist_prior) for available methods"
  )
}

#' Default method for defining model specific Stan code
#'
#' @inheritParams epidist_stancode
#' @param ... Additional arguments for method.
#' @family defaults
#' @export
epidist_stancode.default <- function(data, ...) {
  stop(
    "No epidist_stancode method implemented for the class ", class(data), "\n",
    "See methods(epidist_stancode) for available methods"
  )
}

#' Default method used for interface using `brms`
#'
#' @inheritParams epidist
#' @rdname epidist.default
#' @method epidist default
#' @family defaults
#' @export
epidist.default <- function(data, formula = epidist_formula(data),
                            family = epidist_family(data),
                            prior = epidist_prior(data), fn = brms::brm, ...) {
  stancode <- epidist_stancode(data = data, family = family)
  fit <- fn(
    formula = formula, family = family, prior = prior, stanvars = stancode,
    backend = "cmdstanr", data = data, ...
  )

  class(fit) <- c(class(fit), "epidist_fit")

  return(fit)
}
