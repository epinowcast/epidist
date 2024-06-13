#' Calculate the cohort-based or cumulative mean
#'
#' @param data ...
#' @param type ...
#' @param by ...
#' @param obs_at ...
#' @family plot
#' @export
calculate_cohort_mean <- function(data, type = c("cohort", "cumulative"),
                                  by = c(), obs_at) {
  type <- match.arg(type)
  out <- copy(data)
  out <- out[, list(mean = mean(delay_daily), n = .N),
             by = c("ptime_daily", by)]
  out <- out[order(rank(ptime_daily))]

  if (type == "cumulative") {
    out[, mean := cumsum(mean * n) / cumsum(n), by = by]
    out[, n := cumsum(n), by = by]
  }

  if (!missing(obs_at)) {
    out[, ptime_daily := ptime_daily - obs_at]
  }

  return(out[])
}

#' Calculate the truncated mean by observation horizon
#'
#' @param draws ...
#' @param obs_at ...
#' @param ptime ...
#' @param distribution ...
#' @family plot
#' @export
calculate_truncated_means <- function(draws, obs_at, ptime,
                                      distribution = function(x, y, z) {
                                        dlnorm(x, meanlog = y, sdlog = z)
                                      }) {
  if (length(ptime) != 2) {
    stop("ptime must be a vector of length 2.")
  }

  integrate_for_trunc_mean <- function(x, m, s) {
    numer <- integrate(
      function(y) {
        y * distribution(y, m, s)
      },
      lower = 0, upper = abs(x)
    )[[1]]

    denom <- integrate(
      function(y) {
        distribution(y, m, s)
      },
      lower = 0, upper = abs(x)
    )[[1]]

    return(numer / denom)
  }
  safe_integrate_for_trunc_mean <- purrr::possibly(
    integrate_for_trunc_mean, otherwise = NA_real_
  )

  trunc_mean <- data.table::copy(draws)
  trunc_mean[, obs_horizon := list(seq(ptime[1] - obs_at, ptime[2] - obs_at))]
  trunc_mean <- trunc_mean[,
                           list(obs_horizon = unlist(obs_horizon)),
                           by = setdiff(colnames(draws), "obs_horizon")]
  trunc_mean[,
             trunc_mean := purrr::pmap_dbl(
               list(x = obs_horizon, m = meanlog, s = sdlog),
               safe_integrate_for_trunc_mean,
               .progress = TRUE
             )]

  return(trunc_mean)
}
