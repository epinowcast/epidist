#' Validate a data object for use with [epidist()]
#'
#' This function validates that the provided `data` is suitable to run a
#' particular `epidist` model. This may include checking the class of `data`,
#' and that it contains suitable columns.
#'
#' @param data A `data.frame` to be used for modelling.
#' @family generics
#' @export
epidist_validate <- function(data) {
  UseMethod("epidist_validate")
}

#' Define a model specific formula
#'
#' This function is used within [epidist()] to create the formula object passed
#' to `brms`. It is unlikely that as a user you will need this function, but we
#' export it nonetheless to be transparent about what exactly is happening
#' inside of a call to [epidist()].
#'
#' @inheritParams epidist_validate
#' @param ... Additional arguments passed to method.
#' @family generics
#' @export
epidist_formula <- function(data, ...) {
  UseMethod("epidist_formula")
}

#' Define model specific family
#'
#' This function is used within [epidist()] to create a model specific custom
#' `brms` family object. This object is passed to `brms`. It is unlikely that
#' as a user you will need this function, but we export it nonetheless to be
#' transparent about what exactly is happening inside of a call to [epidist()].
#'
#' @inheritParams epidist_validate
#' @param ... Additional arguments passed to method.
#' @family generics
#' @export
epidist_family <- function(data, ...) {
  UseMethod("epidist_family")
}

#' Define model specific Stan code
#'
#' This function is used within [epidist()] to create any custom Stan code which
#' is injected into `brms` via the `stanvars` argument. It is unlikely that
#' as a user you will need this function, but we export it nonetheless to be
#' transparent about what exactly is happening inside of a call to [epidist()].
#'
#' @inheritParams epidist_validate
#' @param ... Additional arguments passed to method.
#' @rdname epidist_stancode
#' @family generics
#' @export
epidist_stancode <- function(data, ...) {
  UseMethod("epidist_stancode")
}

#' Fit epidemiological delay distributions using a `brms` interface
#'
#' @inheritParams epidist_validate
#' @param formula A formula object created using `brms::bf`. A formula must be
#' provided for the distributional parameter `mu` common to all `brms` families.
#' Optionally, formulas may also be provided for additional distributional
#' parameters.
#' @param family A description of the response distribution and link function to
#' be used in the model. Every family function has a link argument allowing
#' users to specify the link function to be applied on the response variable.
#' If not specified, default links are used. For details of supported families
#' see [brmsfamily()].
#' @param prior One or more `brmsprior` objects created by [brms::set_prior()]
#' or related functions. These priors are passed to [epidist_prior()] in the
#' `prior` argument. We recommend caution and the use of prior predictive checks
#' for specifying prior distributions.
#' @param backend Character string naming the package to use as the backend for
#' fitting the Stan model. Options are `"rstan"` and `"cmdstanr"` (the default).
#' This option is passed directly through to `fn`.
#' @param fn The internal function to be called. By default this is `brms::brm`,
#' which performs inference for the specified model. Other options
#' `brms::make_stancode`, which returns the Stan code for the specified model,
#' and `brms::make_standata` which returns the data passed to Stan. These
#' options may be useful for model debugging and extensions.
#' @param ... Additional arguments for method.
#' @family generics
#' @export
epidist <- function(data, formula, family, prior, backend, fn, ...) {
  UseMethod("epidist")
}
