#' Fit epidemiological delay distributions using a `brms` interface
#'
#' @param data A `data.frame` containing line list data.
#' @param formula An object of class [stats::formula] or [brms::brmsformula]
#' (or one that can be coerced to those classes). A symbolic description of the
#' model to be fitted. A formula must be provided for the distributional
#' parameter `mu`, and may optionally be provided for other distributional
#' parameters.
#' @param family A description of the response distribution and link function to
#' be used in the model. Every family function has a link argument allowing
#' users to specify the link function to be applied on the response variable.
#' If not specified, default links are used. For details of supported families
#' see [brmsfamily()].
#' @param prior One or more `brmsprior` objects created by [brms::set_prior()]
#' or related functions. These priors are passed to [epidist_prior()] in the
#' `prior` argument.
#' @param backend Character string naming the package to use as the backend for
#' fitting the Stan model.
#' This option is passed directly through to `fn`.
#' @param fn The internal function to be called. By default this is
#' [brms::brm()] which performs inference for the specified model. Other options
#' are [brms::make_stancode()] which returns the Stan code for the specified
#' model, or [brms::make_standata()] which returns the data passed to Stan.
#' These two later options may be useful for model debugging and extensions.
#' @param ... Additional arguments passed to method.
#' @family fit
#' @export
epidist <- function(data, formula, family, prior, backend, fn, ...) {
  UseMethod("epidist")
}

#' Default method used for interface using `brms`
#'
#' @inheritParams epidist
#' @rdname epidist.default
#' @method epidist default
#' @family fit
#' @export
epidist.default <- function(data, formula = mu ~ 1,
                            family = "lognormal", prior = NULL,
                            backend = c("rstan", "cmdstanr"),
                            fn = brms::brm, ...) {
  backend <- match.arg(backend)
  epidist_validate_model(data)
  epidist_family <- epidist_family(data, family)
  epidist_formula <- epidist_formula(
    data = data, family = epidist_family, formula = formula
  )
  epidist_prior <- epidist_prior(
    data = data, family = epidist_family, formula = epidist_formula, prior
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
