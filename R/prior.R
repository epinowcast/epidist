#' Define custom prior distributions for epidist models
#'
#' This function combines model specific prior distributions from
#' [epidist_model_prior()], family specific prior distributions from
#' [epidist_family_prior()], and user provided prior distributions into a single
#' set of custom priors. Each element overwrites previous elements, such that
#' user provided prior distributions have the highest priority. If a user prior
#' distribution is provided which is not included in the model, a warning will
#' be shown.
#'
#' Note that the matching of priors is imperfect as it does not use brms'
#' internal prior matching functionality. For example, it cannot distinguish
#' between a prior for all coefficients (class = "b") and a prior for a
#' specific coefficient (class = "b" and coef specified).
#'
#' @inheritParams epidist
#'
#' @param family A description of the response distribution and link function to
#'   be used in the model created using [epidist_family()].
#'
#' @param formula A symbolic description of the model to be fitted created using
#'   [epidist_formula()].
#'
#' @param merge If `TRUE` then merge new priors with existing ones, if `FALSE`
#'   only use new priors. Defaults to `TRUE`. This may be useful if the built in
#'   approaches for merging priors are not flexible enough for a particular use
#'   case.
#' @param enforce_presence If `TRUE` then only allow user priors that match
#'   existing default priors. If `FALSE` then allow user priors that are not
#'   present in the default set. Defaults to `FALSE`.
#'
#' @return A `brmsprior` object containing the combined custom prior
#'  distributions.
#'
#' @rdname epidist_prior
#' @family prior
#' @export
epidist_prior <- function(
  data,
  family,
  formula,
  prior,
  merge = TRUE,
  enforce_presence = FALSE
) {
  assert_epidist(data)
  .check_latent_priors(data, prior)
  default <- brms::default_prior(formula, data = data)
  model <- epidist_model_prior(data, formula)
  if (!is.null(model)) {
    model$source <- "model"
  }
  family <- epidist_family_prior(family, formula)
  if (!is.null(family)) {
    family$source <- "family"
  }
  custom <- .replace_prior(
    family,
    model,
    merge = TRUE,
    enforce_presence = FALSE
  )
  internal <- .replace_prior(default, custom, merge = TRUE)
  prior <- .replace_prior(
    internal,
    prior,
    warn = TRUE,
    merge = merge,
    enforce_presence = enforce_presence
  )

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

.check_latent_priors <- function(data, prior) {
  if (is_epidist_latent_model(data) && !is.null(prior)) {
    # Check swindow_raw
    swindow_rows <- which(
      prior$dpar == "swindow_raw" | grepl("swindow_raw", prior$prior)
    )
    if (length(swindow_rows) > 0) {
      for (i in swindow_rows) {
        p_str <- prior$prior[i]
        # Clean up string: remove spaces
        p_clean <- gsub("\\s+", "", p_str)
        # Check if it contains "uniform(0,1)" or matches "uniform(0,1)"
        # Default prior is "pwindow_raw ~ uniform(0, 1);"
        # User prior might be "uniform(0,1)" if dpar is set.

        is_uniform <- grepl("uniform\\(0,1\\)", p_clean)
        if (!is_uniform) {
          stop(
            "Priors for the secondary event window (swindow_raw) must be ",
            "uniform(0, 1).",
            call. = FALSE
          )
        }
      }
    }

    # Check pwindow_raw
    pwindow_rows <- which(
      prior$dpar == "pwindow_raw" | grepl("pwindow_raw", prior$prior)
    )
    if (length(pwindow_rows) > 0) {
      for (i in pwindow_rows) {
        p_str <- prior$prior[i]
        p_clean <- gsub("\\s+", "", p_str)

        is_uniform <- grepl("uniform\\(0,1\\)", p_clean)
        if (!is_uniform) {
          warning(
            "Non-uniform or non-IID priors for the primary event window ",
            "(pwindow_raw) are not fully supported and may lead to misleading ",
            "posterior predictions and log-likelihoods.",
            call. = FALSE
          )
        }
      }
    }
  }
}
