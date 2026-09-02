#' Define custom prior distributions for epidist models
#'
#' This function combines model specific prior distributions from
#' [epidist_model_prior()], family specific prior distributions from
#' [epidist_family_prior()], and user provided prior distributions into a single
#' set of custom priors. Each element overwrites previous elements, such that
#' user provided prior distributions have the highest priority. If a user prior
#' distribution is provided which is not a parameter of the model, a warning
#' will be shown.
#'
#' Note that the matching of priors is imperfect as it does not use brms'
#' internal prior matching functionality. For example, it cannot distinguish
#' between a prior for all coefficients (class = "b") and a prior for a
#' specific coefficient (class = "b" and coef specified).
#'
#' Some models add parameters which `brms` does not know about, such as the
#' event windows of the latent model. Priors for these are written using the
#' `parameter ~ distribution` syntax of [brms::set_prior()] and are passed to
#' Stan unchanged. A prior written this way replaces any existing prior for the
#' same parameter and is not checked against the parameters of the model. Note
#' that the latent model requires a `uniform(0, 1)` prior on its event windows.
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
  .check_model_prior(data, prior)

  if (!isTRUE(merge)) {
    return(prior)
  }

  default <- brms::default_prior(formula, data = data)
  internal <- .internal_prior(data, family, formula, default)
  .warn_unmatched_prior(prior, bind_rows(default, internal))

  return(.replace_prior(internal, prior, enforce_presence = enforce_presence))
}

#' Combine the model and family specific prior distributions
#'
#' Model specific priors from [epidist_model_prior()] overwrite family specific
#' priors from [epidist_family_prior()]. The result is then restricted to the
#' parameters `brms` recognises, so that priors for parameters which are not in
#' the model are dropped.
#'
#' @inheritParams epidist_prior
#'
#' @param default The default prior distributions from
#'   [brms::default_prior()].
#'
#' @returns A `brmsprior` object, or `NULL` when there are no internal priors.
#'
#' @keywords internal
.internal_prior <- function(data, family, formula, default) {
  model <- epidist_model_prior(data, formula)
  if (!is.null(model)) {
    model$source <- "model"
  }
  family <- epidist_family_prior(family, formula)
  if (!is.null(family)) {
    family$source <- "family"
  }
  custom <- .replace_prior(family, model, enforce_presence = FALSE)
  return(.replace_prior(default, custom))
}

#' Warn about user priors which are not parameters of the model
#'
#' Manually specified priors are passed to Stan unchanged and so are not
#' checked here.
#'
#' @inheritParams epidist_prior
#'
#' @param known One or more prior distributions in the class `brmsprior`
#'   covering the parameters of the model.
#'
#' @returns `NULL`, invisibly, called for the warning it may raise.
#'
#' @keywords internal
.warn_unmatched_prior <- function(prior, known) {
  if (is.null(prior) || is.null(known)) {
    return(invisible(NULL))
  }
  standard <- prior[!.is_manual_prior(prior), ]
  if (nrow(standard) == 0) {
    return(invisible(NULL))
  }
  unmatched <- dplyr::anti_join(standard, known, by = .prior_match_cols())
  if (nrow(unmatched) > 0) {
    msg <- c(
      "!" = "One or more priors have no match in existing parameters:",
      stats::setNames(.describe_prior(unmatched), rep("*", nrow(unmatched))),
      "i" = "To remove this warning consider changing prior specification." # nolint
    )
    cli_warn(message = msg)
  }
  return(invisible(NULL))
}

#' Describe prior distributions for use in messages
#'
#' Gives each prior as its distribution followed by the parameter it applies
#' to, dropping the matching columns which are empty. Braces are escaped so
#' that the result can be passed to `cli`.
#'
#' @inheritParams epidist_prior
#'
#' @returns A character vector with one entry per prior.
#'
#' @keywords internal
.describe_prior <- function(prior) {
  cols <- .prior_match_cols()
  described <- vapply(
    seq_len(nrow(prior)),
    function(i) {
      values <- unlist(prior[i, cols])
      values <- values[!is.na(values) & nzchar(values)]
      if (length(values) == 0) {
        return(prior$prior[i])
      }
      parameter <- paste0(names(values), " = ", values, collapse = ", ")
      return(paste0(prior$prior[i], " (", parameter, ")"))
    },
    character(1)
  )
  described <- gsub("{", "{{", described, fixed = TRUE)
  return(gsub("}", "}}", described, fixed = TRUE))
}

#' Model specific checks of user supplied prior distributions
#'
#' Dispatches on the class of `data` so that a model can reject or warn about
#' prior distributions it does not support. By default no checks are made.
#'
#' @inheritParams epidist_prior
#'
#' @returns `NULL`, invisibly, called for the messages it may raise.
#'
#' @keywords internal
.check_model_prior <- function(data, prior) {
  UseMethod(".check_model_prior")
}

#' @keywords internal
.check_model_prior.default <- function(data, prior) {
  return(invisible(NULL))
}

#' Model specific prior distributions
#'
#' This function contains `brms` prior distributions which are specific to
#' particular `epidist` models e.g. the `latent_lognormal` model.
#'
#' @inheritParams epidist
#' @rdname epidist_model_prior
#' @family prior
#' @returns A `brmsprior` object, or `NULL` when the model adds no priors.
#'
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
#' @returns A `brmsprior` object, or `NULL` when the model adds no priors.
#'
#' @export
epidist_model_prior.default <- function(data, formula, ...) {
  return(NULL)
}

#' Model specific prior distributions for the meta model
#'
#' The response column of a meta model is a placeholder on every summary row,
#' and `brms` centres its default prior for the intercept of `mu` on the
#' response. For a model fitted to summaries alone that default is centred on
#' a delay of zero. This method centres it instead on the log of the median
#' mean the studies reported, with a standard deviation of 1 on the log
#' scale, which is the scale of the lognormal family prior in
#' [epidist_family_prior()]. It does this for every family, so a Gamma or
#' Weibull meta fit gets a prior on the same scale as a lognormal one.
#'
#' Where no study reported a mean the median reported quantile is used, and
#' where no study reported either the mean of the individual level delays is
#' used. A model with individual level rows only adds no prior, so the family
#' or `brms` default applies as it does for the marginal model. The prior is
#' added where `mu` has a log or identity link, and on the identity link the
#' median is used as it is.
#'
#' The prior on the intercept of the other distributional parameters is left
#' to the family or to `brms`.
#'
#' @inheritParams epidist
#' @method epidist_model_prior epidist_meta_model
#' @family prior
#' @family meta_model
#' @returns A `brmsprior` object, or `NULL` when the model adds no priors.
#'
#' @export
epidist_model_prior.epidist_meta_model <- function(data, formula, ...) {
  location <- .meta_reported_location(data)
  link <- formula$family$link
  if (is.null(location) || is.null(link)) {
    return(NULL)
  }
  if (link == "log") {
    centre <- log(location)
  } else if (link == "identity") {
    centre <- location
  } else {
    return(NULL)
  }
  return(set_prior(
    sprintf("normal(%s, 1)", signif(centre, 3)),
    class = "Intercept"
  ))
}

#' The typical delay a meta model's data describe
#'
#' Used to centre the intercept prior of [epidist_model_prior()] for the meta
#' model. Reported means are taken first, then reported quantiles, then the
#' individual level delays. Joint summary rows carry their reported values in
#' the grouped members of the model rather than in the row itself, so both are
#' read.
#'
#' @param data An `epidist_meta_model` object.
#'
#' @returns A positive number, or `NULL` where the model has no summary rows
#'  or no summary gives a location.
#'
#' @keywords internal
.meta_reported_location <- function(data) {
  individual <- data$obs_type == 1L
  if (all(individual)) {
    return(NULL)
  }
  members <- .meta_members(data)
  means <- c(
    data$delay_upr[data$obs_type == 2L],
    members$value[members$type == 1L]
  )
  quantiles <- c(
    data$delay_upr[data$obs_type == 4L],
    members$value[members$type == 3L]
  )
  if (length(means) > 0) {
    location <- stats::median(means)
  } else if (length(quantiles) > 0) {
    location <- stats::median(quantiles)
  } else if (any(individual)) {
    location <- stats::weighted.mean(
      data$delay_lwr[individual] + data$swindow[individual] / 2,
      data$n[individual]
    )
  } else {
    return(NULL)
  }
  if (!is.finite(location) || location <= 0) {
    return(NULL)
  }
  return(location)
}

#' Family specific prior distributions
#'
#' This function contains `brms` prior distributions which are specific to
#' particular likelihood families e.g. [brms::lognormal()].
#'
#' @inheritParams epidist
#' @rdname epidist_family_prior
#' @family prior
#' @returns A `brmsprior` object, or `NULL` when the model adds no priors.
#'
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
#' @returns A `brmsprior` object, or `NULL` when the model adds no priors.
#'
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
#' @returns A `brmsprior` object, or `NULL` when the model adds no priors.
#'
#' @export
epidist_family_prior.lognormal <- function(family, formula, ...) {
  prior <- prior("normal(1, 1)", class = "Intercept")
  sigma_prior <- prior("normal(-0.7, 0.4)", class = "Intercept", dpar = "sigma")
  prior <- prior + sigma_prior
  return(prior)
}
