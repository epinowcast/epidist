#' Create a function to draw from the posterior predictive distribution for a
#' latent model
#'
#' This function creates a function that draws from the posterior predictive
#' distribution for a latent model using [primarycensored::rpcens()] to handle
#' censoring and truncation. The returned function takes a prep argument from
#' `brms` and returns posterior predictions. This is used internally by
#' [brms::posterior_predict()] to generate predictions for latent models.
#'
#' @inheritParams epidist_family_model
#'
#' @return A function that takes a prep argument from brms and returns a matrix
#' of posterior predictions, with one row per posterior draw and one column
#' per observation. The prep object must have the following variables:
#' * `vreal1`: relative observation time
#' * `vreal2`: primary event window
#' * `vreal3`: secondary event window
#'
#' @seealso [brms::posterior_predict()] for details on how this is used within
#' `brms`, [primarycensored::rpcens()] for details on the censoring approach
#' @autoglobal
#' @family gen
#' @export
epidist_gen_posterior_predict <- function(family) {
  dist_fn <- get(
    paste0("posterior_predict_", family$family),
    asNamespace("brms")
  )

  rdist <- function(n, i, prep, ...) {
    prep$ndraws <- n
    do.call(dist_fn, list(i = i, prep = prep))
  }

  .predict <- function(i, prep, ...) {
    relative_obs_time <- prep$data$vreal1[i]
    pwindow <- prep$data$vreal2[i]
    swindow <- prep$data$vreal3[i]

    as.matrix(primarycensored::rpcens(
      n = prep$ndraws,
      rdist = rdist,
      rprimary = stats::runif,
      pwindow = prep$data$vreal2[i],
      swindow = prep$data$vreal3[i],
      D = prep$data$vreal1[i],
      i = i,
      prep = prep
    ))
  }
  return(.predict)
}

#' Create a function to draw from the expected value of the posterior predictive
#' distribution for a latent model
#'
#' This function creates a function that calculates the expected value of the
#' posterior predictive distribution for a latent model. The returned function
#' takes a prep argument (from brms) and returns posterior expected values.
#' This is used internally by [brms::posterior_epred()] to calculate expected
#' values for latent models.
#'
#' @inheritParams epidist_family_model
#'
#' @return A function that takes a prep argument from brms and returns a matrix
#' of posterior expected values, with one row per posterior draw and one column
#' per observation.
#'
#' @seealso [brms::posterior_epred()] for details on how this is used within
#' `brms`.
#' @autoglobal
#' @family gen
#' @export
epidist_gen_posterior_epred <- function(family) {
  get(
    paste0("posterior_epred_", family$family),
    asNamespace("brms")
  )
}
