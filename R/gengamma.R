#' Generalised gamma delay distribution family
#'
#' A `brms` custom family for the generalised gamma distribution in the
#' Prentice (1974) parameterisation of `flexsurv::dgengamma()`.
#' `mu` and `sigma` are the location and scale of the log delay and `Q` is a
#' positive shape.
#' The Weibull (`Q = 1`) and gamma (`Q = sigma`) families are special cases
#' and the lognormal is the limit as `Q` goes to zero.
#' It needs the `flexsurv` package and works with every `epidist` model.
#'
#' @param link,link_sigma,link_Q The link functions of `mu`, `sigma` and `Q`.
#'
#' @returns A `brms` custom family object.
#'
#' @references Prentice, R. L. (1974). A log gamma model and its maximum
#'  likelihood estimation. Biometrika, 61(3), 539-544.
#'  \doi{10.1093/biomet/61.3.539}
#'
#' @family family
#' @export
#' @examples
#' gengamma()
gengamma <- function(link = "identity", link_sigma = "log", link_Q = "log") {
  .require_flexsurv()
  out <- brms::custom_family(
    "gengamma",
    dpars = c("mu", "sigma", "Q"),
    links = c(link, link_sigma, link_Q),
    lb = c(NA, 0, 0),
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

#' Convert the generalised gamma from the Prentice to the Stacy form
#'
#' `primarycensored` and `flexsurv::dgengamma.orig()` use the Stacy (1962)
#' form with `shape`, `scale` and `k`. It has no counterpart for `Q` of zero
#' or below, which is why [gengamma()] keeps `Q` positive.
#'
#' @param mu,sigma,Q Parameters of `flexsurv::dgengamma()`.
#'
#' @returns A named list of `shape`, `scale` and `k`.
#'
#' @keywords internal
.gengamma_stacy <- function(mu, sigma, Q) {
  return(list(
    shape = Q / sigma,
    scale = exp(mu + 2 * sigma * log(Q) / Q),
    k = 1 / Q^2
  ))
}

#' The Stacy parameters of a `brms` prep object for [gengamma()]
#'
#' @param prep A `brms` prep object.
#'
#' @param i The observation index, or `NULL` for every observation.
#'
#' @inherit .gengamma_stacy return
#'
#' @keywords internal
.gengamma_dpars <- function(prep, i = NULL) {
  return(.gengamma_stacy(
    mu = brms::get_dpar(prep, "mu", i = i),
    sigma = brms::get_dpar(prep, "sigma", i = i),
    Q = brms::get_dpar(prep, "Q", i = i)
  ))
}

#' The mean and standard deviation of the generalised gamma distribution
#'
#' From the raw moments \eqn{E[T^r] = scale^r \Gamma(k + r / shape) /
#' \Gamma(k)} of the Stacy form.
#'
#' @param scale,shape,k Parameters of `flexsurv::dgengamma.orig()`.
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
#' The latent and naive models call the Stan density in the order `brms`
#' declares the parameters. The marginal and meta models pass `pcd_param`,
#' the Stacy form `primarycensored` takes.
#'
#' @inheritParams epidist_family_param
#' @family family
#' @returns The family with `param` and `pcd_param` elements.
#'
#' @export
epidist_family_param.gengamma <- function(family, ...) {
  family$param <- "mu, sigma, Q"
  family$pcd_param <- paste0(
    "Q / sigma, exp(mu + 2 * sigma * log(Q) / Q), ", "inv_square(Q)"
  )
  return(family)
}

#' Family specific prior distributions for the generalised gamma family
#'
#' `mu` and `sigma` get the intercept priors of the lognormal family. The
#' intercept of `Q`, on the log scale, gets `normal(0, 0.5)`, centred on the
#' Weibull case.
#'
#' @inheritParams epidist
#' @method epidist_family_prior gengamma
#' @family prior
#' @returns A `brmsprior` object.
#'
#' @export
epidist_family_prior.gengamma <- function(family, formula, ...) {
  prior <- prior("normal(1, 1)", class = "Intercept") +
    prior("normal(-0.7, 0.4)", class = "Intercept", dpar = "sigma") +
    prior("normal(0, 0.5)", class = "Intercept", dpar = "Q")
  return(prior)
}
