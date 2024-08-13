#' Define prior from defaults, model, family, user
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
  default <- brms::default_prior(formula, data = data)
  model <- epidist_model_prior(data, formula)
  family <-  epidist_family_prior(family, formula)
  prior <- Reduce(replace_prior, list(default, model, family, prior))
  return(prior)
}

#' Model specific prior
#'
#' @inheritParams epidist_prior
#' @param ... ...
#' @rdname epidist_model_prior
#' @family prior
#' @export
epidist_model_prior <- function(data, ...) {
  UseMethod("epidist_model_prior")
}

#' Default empty model specific prior
#'
#' @inheritParams epidist_prior
#' @param ... ...
#' @family prior
#' @export
epidist_model_prior.default <- function(data, formula, ...) {
  # Currently there are not model-specific priors
  # In future there might be, but we need to be careful about Stan code
  return(NULL)
}

#' Family specific prior
#'
#' @inheritParams epidist_prior
#' @param ... ...
#' @rdname epidist_family_prior
#' @family prior
#' @export
epidist_family_prior <- function(family, ...) {
  UseMethod("epidist_family_prior")
}

#' Default empty family specific prior
#'
#' @inheritParams epidist_prior
#' @param ... ...
#' @family prior
#' @export
epidist_family_prior.default <- function(family, formula, ...) {
  return(NULL)
}

#' Family specific prior for lognormal
#'
#' @inheritParams epidist_prior
#' @param ... ...
#' @method epidist_family_prior lognormal
#' @family prior
#' @export
epidist_family_prior.lognormal <- function(family, formula, ...) {
  prior <- brms::prior("normal(2, 0.5)", class = "Intercept") +
    brms::prior("normal(0, 0.5)", class = "Intercept", dpar = "sigma")
  prior$source <- "family"
  prior[is.na(prior)] <- "" # This is because brms likes empty over NA
  return(prior)
}

#' Replace a brms prior only if it exists
#'
#' @param old_prior One or more prior distributions in the class `brmsprior`
#' @param new_prior One prior distribution in the class `brmsprior`
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
