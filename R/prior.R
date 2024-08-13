#' @family prior
#' @export
epidist_prior <- function(data, family, formula, prior) {
  epidist_validate(data)
  default <- brms::default_prior(data, family, formula)
  model <- epidist_model_prior(data, formula)
  family <  epidist_family_prior(family, formula)
  out <- replace_brms_prior(default, model)
  out <- replace_brms_prior(out, family)
  out <- replace_brms_prior(out, prior)
  return(out)
}

#' @family prior
#' @export
epidist_model_prior <- function(data, formula) {
  UseMethod("epidist_model_prior")
}

#' @family prior
#' @export
epidist_model_prior.default <- function(data, formula){
  return(NULL)
}


#' @family prior
#' @export
epidist_family_prior <- function(family, formula) {
  UseMethod("epidist_family_prior")
}

#' @family prior
#' @export
epidist_family_prior.default <- function(family, formula){
  return(NULL)
}

#' @family prior
#' @export
epidist_family_prior.lognormal <- function(family, formula){
  prior_mu <- brms::prior("normal(2, 0.5)", class = "Intercept")
  prior_sigma <- brms::prior(
    "normal(0, 0.5)", class = "Intercept", dpar = "sigma"
  )
  prior <- prior_mu + prior_sigma
  return(prior)
}
