#' Define `epidist` family
#'
#' This function is used within [epidist()] to create a model specific custom
#' `brms` family object. This custom family is passed to `brms`. It is unlikely
#' that as a user you will need this function, but we export it nonetheless to
#' be transparent about what happens inside of a call to [epidist()].
#'
#' The family may be any `brms` family of a positive response, such as
#' [brms::lognormal()], `Gamma(link = "log")` or [brms::weibull()], or a
#' family `epidist` defines itself, such as [gengamma()].
#'
#' @inheritParams epidist
#' @family family
#' @returns A `brms` custom family object.
#'
#' @export
epidist_family <- function(data, family = lognormal(), ...) {
  assert_epidist(data)
  family <- .validate_family(family)
  class(family) <- c(.family_name(family), class(family))
  family <- .add_dpar_info(family)
  custom_family <- epidist_family_model(data, family, ...)
  class(custom_family) <- c(.family_name(family), class(custom_family))
  custom_family <- epidist_family_param(custom_family)
  return(custom_family)
}

#' The model-specific parts of an `epidist_family()` call
#'
#' @inheritParams epidist
#'
#' @param family Output of a call to `brms::brmsfamily()` with additional
#' information as provided by `.add_dpar_info()`
#'
#' @rdname epidist_family_model
#' @family family
#' @returns A `brms` custom family object.
#'
#' @export
epidist_family_model <- function(data, family, ...) {
  UseMethod("epidist_family_model")
}

#' Default method for defining a model specific family
#'
#' @inheritParams epidist_family_model
#' @family family
#' @returns A `brms` custom family object.
#'
#' @export
epidist_family_model.default <- function(data, family, ...) {
  return(family)
}

#' Reparameterise an `epidist` family to align `brms` and Stan
#'
#' Called by [epidist_family()]. A custom model supplies its family through
#' [epidist_family_model()] rather than by calling this.
#'
#' @inheritParams epidist_family
#' @rdname epidist_family_param
#' @family family
#' @keywords internal
epidist_family_param <- function(family, ...) {
  UseMethod("epidist_family_param")
}

#' Default method for families which do not require a reparameterisation
#'
#' This function extracts the Stan parameterisation for a given brms family by
#' creating a dummy model and parsing its Stan code. It looks for the log
#' probability density function (lpdf) call in the Stan code and extracts the
#' parameter order used by Stan. This is needed because brms and Stan may use
#' different parameter orderings for the same distribution.
#'
#' @param family A brms family object containing at minimum a `family` element
#'  specifying the distribution family name.
#'
#' @param ... Additional arguments passed to methods (not used)
#'
#' @details
#' The function works by:
#' 1. Creating a minimal dummy model using the specified family
#' 2. Extracting the Stan code for this model
#' 3. Finding the lpdf function call for the family
#' 4. Parsing out the parameter ordering used in Stan
#' 5. Adding this as the `param` element to the family object
#'
#' @return The input family object with an additional `param` element containing
#' the Stan parameter ordering as a string
#'
#' @family family
#' @importFrom brms make_stancode
#' @importFrom cli cli_abort
#' @export
epidist_family_param.default <- function(family, ...) {
  data_dummy <- data.frame(y = c(1, 2))
  dummy_mdl <- make_stancode(
    y ~ 1,
    data = data_dummy,
    family = class(family)[1]
  )

  # get the lowered family name
  family_name <- tolower(class(family)[1])

  # Extract the Stan parameterisation from the dummy model code
  lpdf_pattern <- paste0(
    "target \\+= ", # nolint
    family_name,
    "_(lpdf|lpmf)\\(Y \\| (.+?)\\)" # nolint
  )
  lpdf_match <- regexpr(lpdf_pattern, dummy_mdl)
  reparam <- if (lpdf_match > 0) {
    matches <- unlist(regmatches(dummy_mdl, lpdf_match))
    mu_matches <- matches[grepl("mu", matches, fixed = TRUE)]
    if (length(mu_matches) > 1) {
      cli_abort("Multiple Stan parameterisations found with 'mu' parameter.")
    } else if (length(mu_matches) == 0) {
      cli_abort("No Stan parameterisation found with 'mu' parameter.")
    }
    match_str <- mu_matches[1]
    param <- sub(
      paste0(
        "target \\+= ", # nolint
        family_name,
        "_(lpdf|lpmf)\\(Y \\| " # nolint
      ),
      "",
      match_str
    )
    param <- sub(")", "", param, fixed = TRUE)
    family$param <- param
  } else {
    cli_abort(
      "Unable to extract Stan parameterisation for {family_name}."
    )
  }
  return(family)
}

#' Generalised gamma delay distribution family
#'
#' A `brms` custom family for the generalised gamma distribution in the
#' parameterisation of Stacy (1962), as used by `flexsurv::dgengamma.orig()`
#' and by the `primarycensored` package.
#' It has three positive distributional parameters, modelled on the log scale
#' by default: `mu` is the scale, `shape` is the power parameter and `k` is
#' the shape of the underlying gamma distribution.
#' If \eqn{G} is gamma distributed with shape `k` and unit scale then
#' `mu * G^(1 / shape)` is generalised gamma, so the gamma (`shape = 1`) and
#' Weibull (`k = 1`) families are special cases and the lognormal is a limit.
#' The mean is `mu * gamma(k + 1 / shape) / gamma(k)`, and [add_summaries()]
#' gives the mean, the standard deviation and quantiles in closed form.
#'
#' `brms` has no generalised gamma family, so this one carries the Stan
#' density and distribution function and the R functions `brms` uses to
#' compute the log likelihood and to predict from a fit.
#' The `flexsurv` package must be installed to use it.
#' It works with every `epidist` model, including the meta model, and with
#' every primary event distribution.
#'
#' The default prior of [epidist_family_prior()] is weakly informative around
#' the gamma and Weibull special cases, because `shape` and `k` are only
#' weakly identified by a modest number of delays.
#'
#' @param link,link_shape,link_k The link functions of `mu`, `shape` and `k`.
#'  All default to `"log"`.
#'
#' @returns A `brms` custom family object.
#'
#' @references Stacy, E. W. (1962). A generalization of the gamma
#'  distribution. The Annals of Mathematical Statistics, 33(3), 1187-1192.
#'  \doi{10.1214/aoms/1177704481}
#'
#' @family family
#' @export
#' @examples
#' gengamma()
gengamma <- function(link = "log", link_shape = "log", link_k = "log") {
  .require_flexsurv()
  out <- brms::custom_family(
    "gengamma",
    dpars = c("mu", "shape", "k"),
    links = c(link, link_shape, link_k),
    lb = c(0, 0, 0),
    ub = c(NA, NA, NA),
    type = "real",
    log_lik = .gengamma_log_lik,
    posterior_predict = .gengamma_posterior_predict,
    posterior_epred = .gengamma_posterior_epred
  )
  # `brms::custom_family()` does not record the support of the response,
  # which the model families take from here
  out$ybounds <- c(0, Inf)
  return(out)
}

#' The families `epidist` defines itself
#'
#' `brms` names a custom family `"custom"` and keeps its own name in `name`,
#' so these are looked up by that name where `brms` would be asked for one of
#' its own families.
#'
#' @returns A named list of family constructors.
#'
#' @keywords internal
.epidist_families <- function() {
  return(list(gengamma = gengamma))
}

#' The name of a delay distribution family
#'
#' A family built with [brms::custom_family()], such as [gengamma()], is
#' named `"custom"` for `brms` to dispatch on and records its own name in
#' `name`. Every other family is named by `family`.
#'
#' @inheritParams epidist_family
#'
#' @returns A character string.
#'
#' @keywords internal
.family_name <- function(family) {
  if (identical(family$family, "custom")) {
    return(family$name)
  }
  return(family$family)
}

#' Stan functions a family defines itself
#'
#' A family `brms` does not have, such as [gengamma()], carries the Stan
#' density and distribution function `brms` and the latent model call. They
#' are read from the `stan/family/` folder of the installed package.
#'
#' @inheritParams epidist_family
#'
#' @returns A `brms` `stanvars` object, or `NULL` for a `brms` family.
#'
#' @keywords internal
.family_stanvars <- function(family) {
  name <- .delay_family(family)$name
  if (!name %in% names(.epidist_families())) {
    return(NULL)
  }
  return(brms::stanvar(
    block = "functions",
    scode = .stan_chunk(file.path("family", paste0(name, ".stan")))
  ))
}

#' Check that `flexsurv` is installed
#'
#' @returns `NULL`, invisibly, called for the error it may raise.
#'
#' @keywords internal
.require_flexsurv <- function() {
  if (!requireNamespace("flexsurv", quietly = TRUE)) {
    cli_abort(c(
      "The {.pkg flexsurv} package is needed for the generalised gamma family.",
      i = "Install it with {.code install.packages(\"flexsurv\")}."
    ))
  }
  return(invisible(NULL))
}

#' The generalised gamma parameters of a `brms` prep object
#'
#' @param prep A `brms` prep object.
#'
#' @param i The observation index, or `NULL` for every observation.
#'
#' @returns A named list of the `shape`, `scale` and `k` parameters in the
#'  parameterisation of `flexsurv::dgengamma.orig()`.
#'
#' @keywords internal
.gengamma_dpars <- function(prep, i = NULL) {
  return(list(
    shape = brms::get_dpar(prep, "shape", i = i),
    scale = brms::get_dpar(prep, "mu", i = i),
    k = brms::get_dpar(prep, "k", i = i)
  ))
}

#' The mean and standard deviation of the generalised gamma distribution
#'
#' From the raw moments \eqn{E[T^r] = \theta^r \Gamma(k + r / a) / \Gamma(k)},
#' with \eqn{\theta} the scale and \eqn{a} the `shape`.
#'
#' @param scale,shape,k Generalised gamma parameters in the parameterisation
#'  of `flexsurv::dgengamma.orig()`.
#'
#' @returns A numeric vector.
#'
#' @keywords internal
.gengamma_mean <- function(scale, shape, k) {
  return(scale * exp(lgamma(k + 1 / shape) - lgamma(k)))
}

#' @rdname dot-gengamma_mean
#' @keywords internal
.gengamma_sd <- function(scale, shape, k) {
  g1 <- exp(lgamma(k + 1 / shape) - lgamma(k))
  g2 <- exp(lgamma(k + 2 / shape) - lgamma(k))
  return(scale * sqrt(g2 - g1^2))
}

#' The `brms` post-processing functions of the [gengamma()] family
#'
#' Used by `brms` for a fit of the naive model, which passes the family
#' through unchanged, and by the generators in `R/gen.R` for the other models.
#'
#' @inheritParams .gengamma_dpars
#'
#' @param ... Not used.
#'
#' @returns The log likelihood of observation `i` for every draw, a delay
#'  drawn for observation `i` for every draw, and the mean of the delay for
#'  every draw and observation.
#'
#' @keywords internal
.gengamma_log_lik <- function(i, prep) {
  dpars <- .gengamma_dpars(prep, i)
  log_lik <- flexsurv::dgengamma.orig(
    prep$data$Y[i],
    shape = dpars$shape, scale = dpars$scale, k = dpars$k, log = TRUE
  )
  return(.log_lik_weight(log_lik, i = i, prep = prep))
}

#' @rdname dot-gengamma_log_lik
#' @keywords internal
.gengamma_posterior_predict <- function(i, prep, ...) {
  dpars <- .gengamma_dpars(prep, i)
  return(flexsurv::rgengamma.orig(
    prep$ndraws,
    shape = dpars$shape, scale = dpars$scale, k = dpars$k
  ))
}

#' @rdname dot-gengamma_log_lik
#' @keywords internal
.gengamma_posterior_epred <- function(prep) {
  dpars <- .gengamma_dpars(prep)
  return(as.matrix(.gengamma_mean(dpars$scale, dpars$shape, dpars$k)))
}

#' Method for the [gengamma()] family
#'
#' `brms` calls the Stan density of a custom family with its distributional
#' parameters in the order they are declared, which is the `param` the
#' latent model uses. `primarycensored` takes the generalised gamma as
#' `[shape, scale, k]`, so the marginal and meta models pass `pcd_param`.
#'
#' @inheritParams epidist_family_param
#' @family family
#' @export
epidist_family_param.gengamma <- function(family, ...) {
  family$param <- "mu, shape, k"
  family$pcd_param <- "shape, mu, k"
  return(family)
}
