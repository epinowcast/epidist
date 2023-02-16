#' Calculate the cohort-based or cumulative mean
#' @export
calculate_cohort_mean <- function(data, type = c("cohort", "cumulative"),
                                  by = c(), obs_at) {
  type <- match.arg(type)

  out <- data |>
    copy() |>
    DT(, .(
      mean = mean(delay_daily), n = .N),
      by = c("ptime_daily", by)
     ) |>
    DT(order(rank(ptime_daily)))

  if (type == "cumulative") {
    out[, mean := cumsum(mean * n) / cumsum(n), by = by]
    out[, n := cumsum(n), by = by]
  }

  if (!missing(obs_at)) {
    out <- out |>
      DT(, ptime_daily := ptime_daily - obs_at)
  }

  return(out[])
}

#' Calculate the truncated mean by observation horizon
#' @export
calculate_truncated_means <- function(draws, obs_at, ptime,
                                      distribution = function(x, y, z) {
                                        dlnorm(x, meanlog = y, sdlog = z)
                                      }) {
  if (length(ptime) != 2) {
    stop("ptime must be a vector of length 2.")
  }
  trunc_mean <- draws |>
    copy() |>
    DT(,
      obs_horizon := list(seq(ptime[1] - obs_at, ptime[2] - obs_at))
    ) |>
    DT(,
      .(obs_horizon = unlist(obs_horizon)),
      by = setdiff(colnames(draws), "obs_horizon")
    ) |>
    DT(,
      trunc_mean := purrr::pmap_dbl(
        list(x = obs_horizon, m = meanlog, s = sdlog),
        \(x, m, s) {
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
        },
        .progress = TRUE
      )
    )
  return(trunc_mean)
}