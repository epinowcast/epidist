#' Define prior distributions using `brms` defaults, model specific priors,
#' family specific priors, and user provided priors
#'
#' This function obtains the `brms` default prior distributions for a particular
#' model, then replaces these prior distributions using:
#' 1. Model specific prior distributions from [epidist_model_prior()]
#' 2. Family specific prior distributions from [epidist_family_prior()]
#' 3. User provided prior distributions
#' Each element of this list overwrites previous elements, such that user
#' provided prior distribution have the highest priority. At the third stage,
#' if a prior distribution is provided which is not included in the model, then
#' a warning will be shown. To prevent this warning, do not pass prior
#' distributions for parameters which are not in the model.
#'
#' @param data A `data.frame` containing line list data
#' @param family Output of a call to `brms::brmsfamily()`
#' @param formula A formula object created using `brms::bf()`
#' @param prior User provided prior distribution created using `brms::prior()`
#' @rdname epidist_prior
#' @family prior
#' @export
epidist_prior <- function(data, family, formula, prior) {
  epidist_validate(data)
  family <- brms:::validate_family(family)
  class(family) <- c(class(family), family$family)
  default <- brms::default_prior(formula, data = data)
  model <- epidist_model_prior(data, formula)
  family <-  epidist_family_prior(family, formula)
  internal_prior <- Reduce(.replace_prior, list(default, model, family))
  prior <- .replace_prior(internal_prior, prior, abort = TRUE)
  return(prior)
}

#' Model specific prior distributions
#'
#' This function contains `brms` prior distributions which are specific to
#' particular `epidist` models e.g. the `latent_lognormal` model.
#'
#' @inheritParams epidist_prior
#' @param ... ...
#' @rdname epidist_model_prior
#' @family prior
#' @export
epidist_model_prior <- function(data, ...) {
  UseMethod("epidist_model_prior")
}

#' Default model specific prior distributions
#'
#' By default, we do not return any model specific prior distributions.
#'
#' @inheritParams epidist_prior
#' @param ... ...
#' @family prior
#' @export
epidist_model_prior.default <- function(data, formula, ...) {
  return(NULL)
}

#' Family specific prior distributions
#'
#' This function contains `brms` prior distributions which are specific to
#' particular likelihood families e.g. [brms::lognormal()].
#'
#' @inheritParams epidist_prior
#' @param ... ...
#' @rdname epidist_family_prior
#' @family prior
#' @export
epidist_family_prior <- function(family, ...) {
  UseMethod("epidist_family_prior")
}

#' Default family specific prior distributions
#'
#' By default, we do not return any family specific prior distributions.
#'
#' @inheritParams epidist_prior
#' @param ... ...
#' @family prior
#' @export
epidist_family_prior.default <- function(family, formula, ...) {
  return(NULL)
}

#' Family specific prior distributions for the lognormal family
#'
#' We suggest priors to overwrite the `brms` defaults for the lognormal family.
#'
#' @inheritParams epidist_prior
#' @param ... ...
#' @method epidist_family_prior lognormal
#' @family prior
#' @export
epidist_family_prior.lognormal <- function(family, formula, ...) {
  prior <- prior("normal(1, 1)", class = "Intercept")
  sigma_prior <- prior("normal(-0.7, 0.4)", class = "Intercept", dpar = "sigma")
  prior <- prior + sigma_prior
  prior$source <- "family"
  prior[is.na(prior)] <- "" # This is because brms likes empty over NA
  prior[prior == "NA"] <- NA # To keep particular NA
  return(prior)
}
