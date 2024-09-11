#' Define prior distributions using `brms` defaults, model specific priors,
#' family specific priors, and user provided priors
#'
#' This function obtains the `brms` default prior distributions for a particular
#' model, then uses [replace_prior()] to update the prior distributions using:
#' 1. Model specific prior distributions from [epidist_model_prior()]
#' 2. Family specific prior distributions from [epidist_family_prior()]
#' 3. User provided prior distributions
#' Each element of this list overwrites previous elements, such that user
#' provided prior distribution have the highest priority.
#'
#' @param data ...
#' @param family ...
#' @param formula ...
#' @param prior ...
#' @rdname epidist_prior
#' @family prior
#' @export
epidist_prior <- function(data, family, formula, prior) {
  epidist_validate(data)
  class(family) <- c(class(family), family$family)
  default <- brms::default_prior(formula, data = data)
  model <- epidist_model_prior(data, formula)
  family <-  epidist_family_prior(family, formula)
  prior <- Reduce(replace_prior, list(default, model, family, prior))
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
  prior <- brms::prior("normal(1, 1)", class = "Intercept")
  if ("sigma" %in% names(formula$pforms)) {
    # Case with a model on sigma
    sigma_prior <- brms::prior(
      "normal(-0.7, 0.4)", class = "Intercept", dpar = "sigma"
    )
  } else if ("sigma" %in% names(formula$pfix)) {
    # Case with sigma fixed to a constant
    sigma_prior <- NULL
  } else {
    # Case with no model on sigma
    sigma_prior <- brms::prior(
      "lognormal(-0.7, 0.4)", class = "sigma", lb = 0, ub = "NA"
    )
  }
  prior <- prior + sigma_prior
  prior$source <- "family"
  prior[is.na(prior)] <- "" # This is because brms likes empty over NA
  prior[prior == "NA"] <- NA # To keep particular NA
  return(prior)
}

#' Replace `brms` prior distributions
#'
#' This function takes `old_prior` and replaces any prior distributions
#' contained in it by the corresponding prior distribution in `new_prior`.
#' If there is a prior distribution in `new_prior` with no match in `old_prior`
#' then the function will error and give the name of the new prior distribution
#' with no match.
#'
#' @param old_prior One or more prior distributions in the class `brmsprior`
#' @param new_prior One or more prior distributions in the class `brmsprior`
#' @family prior
#' @importFrom cli cli_inform
#' @importFrom utils capture.output
#' @autoglobal
#' @export
replace_prior <- function(old_prior, new_prior) {
  if (is.null(new_prior)) {
    return(old_prior)
  }
  cols <- c("class", "coef", "group", "resp", "dpar", "nlpar", "lb", "ub")
  prior <- dplyr::full_join(
    old_prior, new_prior, by = cols, suffix = c("_old", "_new")
  )

  if (any(is.na(prior$prior_old))) {
    missing_prior <- utils::capture.output(print(
      prior |>
        dplyr::filter(is.na(prior_old)) |>
        dplyr::select(
          prior = prior_new, dplyr::all_of(cols), source = source_new
        )
    ))
    msg <- c(
      "i" = "No available prior to replace in old_prior found for:",
      missing_prior
    )
    cli::cli_abort(message = msg)
  }

  prior <- prior |>
    dplyr::filter(!is.na(prior_old), !is.na(prior_new)) |>
    dplyr::select(prior = prior_new, dplyr::all_of(cols), source = source_new)

  return(prior)
}
