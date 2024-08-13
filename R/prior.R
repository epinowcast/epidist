#' Define prior from defaults, model, family, user
#'
#' @rdname epidist_prior
#' @family prior
#' @export
epidist_prior <- function(data, family, formula, prior) {
  epidist_validate(data)
  default <- brms::default_prior(formula, data = data)
  model <- epidist_model_prior(data, formula)
  family <-  epidist_family_prior(family, formula)
  prior <- Reduce(replace_prior, list(default, model, family, prior))
  return(prior)
}

#' Model specific prior
#'
#' @rdname epidist_model_prior
#' @family prior
#' @export
epidist_model_prior <- function(data, ...) {
  UseMethod("epidist_model_prior")
}

#' @family prior
#' @export
epidist_model_prior.default <- function(data, formula) {
  # Currently there are not model-specific priors
  # In future there might be, but we need to be careful about Stan code
  return(NULL)
}

#' Family specific prior
#'
#' @rdname epidist_family_prior
#' @family prior
#' @export
epidist_family_prior <- function(family, ...) {
  UseMethod("epidist_family_prior")
}

#' @family prior
#' @export
epidist_family_prior.default <- function(family, formula) {
  return(NULL)
}

#' @method epidist_family_prior lognormal
#' @family prior
#' @export
epidist_family_prior.lognormal <- function(family, formula) {
  prior <- brms::prior("normal(2, 0.5)", class = "Intercept") +
    brms::prior("normal(0, 0.5)", class = "Intercept", dpar = "sigma")
  prior$source <- "family"
  prior[is.na(prior)] <- "" # This is because brms likes empty over NA
  return(prior)
}
