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
#' @inheritParams epidist
#' @param family A description of the response distribution and link function to
#' be used in the model created using [epidist_family()].
#' @param formula A symbolic description of the model to be fitted created using
#' [epidist_formula()].
#' @rdname epidist_prior
#' @family prior
#' @export
epidist_prior <- function(data, family, formula, prior) {
  epidist_validate(data)
  default <- brms::default_prior(formula, data = data)
  model <- epidist_model_prior(data, formula)
  family <-  epidist_family_prior(family, formula)
  if (!is.null(family)) {
    family$source <- "family"
    family[is.na(family)] <- "" # brms likes empty over NA
    family[family == "NA"] <- NA # To keep particular NA
  }
  internal_prior <- Reduce(.replace_prior, list(default, model, family))
  prior <- .replace_prior(internal_prior, prior, warn = TRUE)
  return(prior)
}

#' Model specific prior distributions
#'
#' This function contains `brms` prior distributions which are specific to
#' particular `epidist` models e.g. the `latent_lognormal` model.
#'
#' @inheritParams epidist
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
#' @inheritParams epidist
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
#' @inheritParams epidist
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
#' @inheritParams epidist
#' @family prior
#' @export
epidist_family_prior.default <- function(family, formula, ...) {
  return(NULL)
}

#' Family specific prior distributions for the lognormal family
#'
#' We suggest priors to overwrite the `brms` defaults for the lognormal family.
#'
#' @inheritParams epidist
#' @method epidist_family_prior lognormal
#' @family prior
#' @export
epidist_family_prior.lognormal <- function(family, formula, ...) {
  prior <- prior("normal(1, 1)", class = "Intercept")
  sigma_prior <- prior("normal(-0.7, 0.4)", class = "Intercept", dpar = "sigma")
  prior <- prior + sigma_prior
  return(prior)
}
