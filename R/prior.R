#' @family prior
#' @export
epidist_prior <- function(data, family, formula, prior) {
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
  return(NULL)
}

#' Define priors for the model
#'
#' We provide suggested weakly informative priors.
#'
#' @inheritParams epidist_prior
#' @method epidist_prior epidist_latent_individual
#' @family latent_individual
#' @importFrom cli cli_inform
#' @export
epidist_prior.epidist_latent_individual <- function(data, family, formula,
                                                    ...) {
  epidist_validate(data)
  if (identical(family$dpars, c("mu", "sigma"))) {
    prior_mu <- brms::prior("normal(2, 0.5)", class = "Intercept")
    prior_sigma <- brms::prior(
      "normal(0, 0.5)", class = "Intercept", dpar = "sigma"
    )
    msg <- c(
      "i" = "The following priors have been set:",
      "*" = "normal(2, 0.5) on the intercept of distributional parameter mu",
      "*" = "normal(0, 0.5) on the intercept of distributional parameter sigma",
      "To alter priors, or set priors on other parameters, see ?epidist_prior."
    )
    cli::cli_inform(
      message = msg, .frequency = "regularly", .frequency_id = "prior-message"
    )
    priors <- prior_mu + prior_sigma
  } else {
    cli::cli_warn(c(
      "!" = "Priors not available for these distributional parameters.",
      "Using the default priors from brms."
    ))
    priors <- NULL
  }
  return(priors)
}
