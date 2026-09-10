#' Simulate cases from a uniform distribution
#'
#' This function simulates cases from a uniform distribution, where the primary
#' event times are uniformly distributed between 0 and `t`.
#'
#' @param sample_size The number of cases to simulate.
#'
#' @param t Upper bound of the uniform distribution to generate primary event
#'  times.
#'
#' @returns A `data.frame` with two columns: `case` (case number) and `ptime`
#'  (primary event time).
#'
#' @family simulate
#' @export
simulate_uniform_cases <- function(sample_size = 1000, t = 60) {
  return(data.frame(
    case = 1:sample_size,
    ptime = stats::runif(sample_size, 0, t)
  ))
}

#' Simulate exponential cases
#'
#' This function simulates cases from an exponential distribution. The user may
#' specify the rate parameter `r`, the sample size, and the upper bound of the
#' survival time. If the rate parameter is 0, then this function defaults to the
#' uniform distribution.
#'
#' @param r The exponential growth rate parameter. Defaults to 0.2.
#'
#' @param sample_size The number of cases to simulate. Defaults to 10000.
#'
#' @param seed The random seed to be used in the simulation process. Defaults
#'  to `NULL`, which leaves the random number generator as it is.
#'
#' @param t Upper bound of the survival time. Defaults to 30.
#'
#' @returns A `data.frame` with two columns: `case` (case number) and `ptime`
#'  (primary event time).
#'
#' @family simulate
#' @export
simulate_exponential_cases <- function(
  r = 0.2,
  sample_size = 10000,
  seed = NULL,
  t = 30
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  quant <- stats::runif(sample_size, 0, 1)

  if (r == 0) {
    ptime <- quant * t
  } else {
    ptime <- log(1 + quant * (exp(r * t) - 1)) / r
  }

  cases <- data.frame(case = seq_along(ptime), ptime = ptime)
  return(cases)
}

#' Simulate cases from a stochastic SIR model
#'
#' This function simulates cases from an stochastic SIR model. The user may
#' specify the initial epidemic growth rate \eqn{r}, the rate of recovery gamma
#' \eqn{\gamma}, the initial number of infected cases \eqn{I_0}, and the total
#' population size \eqn{N}.
#'
#' @param r The initial epidemic growth rate. Defaults to 0.2.
#'
#' @param gamma The rate of recovery. Defaults to 1/7.
#'
#' @param I0 The initial number of infected people. Defaults to 50.
#'
#' @param N The total population size. Defaults to 10000.
#'
#' @param seed The random seed to be used in the simulation process. Defaults
#'  to `NULL`, which leaves the random number generator as it is.
#'
#' @returns A `data.frame` with two columns: `case` (case number) and `ptime`
#' (primary event time).
#'
#' @family simulate
#' @export
simulate_gillespie <- function(
  r = 0.2,
  gamma = 1 / 7,
  I0 = 50, # nolint: object_name_linter
  N = 10000, # nolint: object_name_linter
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  current_time <- 0
  state <- c(N - I0, I0, 0)
  transmission_rate <- r + gamma
  go <- TRUE
  ptime <- NULL

  while (go) {
    rates <- c(transmission_rate * state[1] * state[2] / N, gamma * state[2])
    srates <- sum(rates)

    if (srates > 0) {
      time_increment <- stats::rexp(1, rate = srates)
      current_time <- current_time + time_increment
      wevent <- sample(seq_along(rates), size = 1, prob = rates)

      if (wevent == 1) {
        state <- c(state[1] - 1, state[2] + 1, state[3])
        ptime <- c(ptime, current_time)
      } else {
        state <- c(state[1], state[2] - 1, state[3] + 1)
      }
    } else {
      go <- FALSE
    }
  }

  cases <- data.frame(case = seq_along(ptime), ptime = ptime)
  return(cases)
}

#' Simulate secondary events based on a delay distribution
#'
#' This function simulates secondary events from a delay distribution given as
#' a `<dist_spec>` from the `distspec` package. The input dataset should have
#' the primary event times in a column named `ptime`.
#'
#' @details
#' Delays are drawn with [distspec::sample_dist()]. When `dist` has uncertain
#' parameters, as returned by [as_dist_spec.epidist_fit()], the parameters are
#' resolved once per row with [distspec::fix_parameters()] before the delay is
#' drawn. The simulated delays then carry the parameter uncertainty as well as
#' the spread of the delay distribution itself.
#'
#' [distspec::sample_dist()] does not apply the bounds set with `max` or
#' `cdf_max`, so a bounded `dist` gives the same delays as an unbounded one.
#' Passing a bounded `dist` warns for that reason.
#'
#' `distspec` supports a fixed set of distributions. Open an issue at
#' <https://github.com/epiforecasts/distspec/issues> to ask for another one.
#'
#' @param data A data frame with the primary event times.
#'
#' @param dist The delay distribution to be used, as a `<dist_spec>` built with
#'  a `distspec` constructor such as [distspec::LogNormal()], or returned by
#'  [as_dist_spec.epidist_fit()].
#'
#' @returns A `data.frame` that augments `data` with two new columns: `delay`
#'  (secondary event latency) and `stime` (the time of the secondary event).
#'
#' @family simulate
#' @autoglobal
#' @importFrom dplyr mutate
#' @importFrom checkmate assert_class
#' @export
#' @examples
#' simulate_gillespie(seed = 1) |>
#'   simulate_secondary(distspec::LogNormal(meanlog = 1.8, sdlog = 0.5)) |>
#'   head()
#'
#' # An uncertain delay distribution, such as one exported from a fit with
#' # as_dist_spec(), is resolved once per row
#' simulate_gillespie(seed = 1) |>
#'   simulate_secondary(
#'     distspec::LogNormal(
#'       meanlog = distspec::Normal(mean = 1.8, sd = 0.1),
#'       sdlog = 0.5
#'     )
#'   ) |>
#'   head()
simulate_secondary <- function(data, dist) {
  if (is.function(dist)) {
    cli::cli_abort(c(
      "{.arg dist} must be a {.cls dist_spec}, not a function.",
      i = "Build one with a {.pkg distspec} constructor, so
           {.code distspec::LogNormal(meanlog = 1.8, sdlog = 0.5)} in place of
           {.code dist = rlnorm, meanlog = 1.8, sdlog = 0.5}."
    ))
  }
  assert_class(dist, "dist_spec")
  if (distspec::ndist(dist) != 1) {
    cli::cli_abort(
      "{.arg dist} must be a single delay distribution, not
       {distspec::ndist(dist)}."
    )
  }
  .warn_ignored_bounds(dist)
  sim_data <- data |>
    mutate(
      delay = .sample_delays(dist, dplyr::n()),
      stime = .data$ptime + .data$delay
    )
  return(sim_data)
}

#' Warn that the bounds of a `<dist_spec>` are not applied when sampling
#'
#' [distspec::sample_dist()] ignores the `max` and `cdf_max` bounds of a
#' `<dist_spec>`, so delays drawn from a bounded distribution can fall outside
#' its bounds.
#'
#' @inheritParams simulate_secondary
#'
#' @return `dist`, invisibly.
#'
#' @keywords internal
.warn_ignored_bounds <- function(dist) {
  dist_max <- attr(dist, "max")
  dist_cdf_max <- attr(dist, "cdf_max")
  bounded <- (!is.null(dist_max) && any(is.finite(dist_max))) ||
    (!is.null(dist_cdf_max) && any(dist_cdf_max < 1))
  if (bounded) {
    cli_warn(c(
      "The bounds of {.arg dist} are not applied when drawing delays, so some
       delays will fall outside them.",
      i = "{.fn distspec::sample_dist} ignores {.arg max} and {.arg cdf_max}.",
      "*" = "Drop the bounds, or discard the delays outside them yourself."
    ))
  }
  return(invisible(dist))
}

#' Draw delays from a `<dist_spec>`
#'
#' Draws `n` delays from `dist`. An uncertain `dist` has its parameters
#' resolved once per delay, so the draws carry the parameter uncertainty.
#'
#' @inheritParams simulate_secondary
#'
#' @param n The number of delays to draw.
#'
#' @return A numeric vector of length `n`.
#'
#' @keywords internal
.sample_delays <- function(dist, n) {
  if (n == 0) {
    return(numeric(0))
  }
  if (!distspec::has_uncertainty(dist)) {
    return(as.numeric(distspec::sample_dist(dist, n)))
  }
  delays <- vapply(
    seq_len(n),
    function(i) {
      fixed <- distspec::fix_parameters(dist, strategy = "sample")
      return(as.numeric(distspec::sample_dist(fixed, 1)))
    },
    numeric(1)
  )
  return(delays)
}

#' Convert simulated event times to dates
#'
#' Takes the continuous event times produced by [simulate_gillespie()] and
#' [simulate_secondary()] and returns the dates an analyst would actually
#' receive. Event times are floored to their reporting window, so each event is
#' known only by the window it fell in, and are then offset from
#' `outbreak_start_date`.
#'
#' The returned columns are named to match [as_epidist_linelist_data()], so the
#' output can be passed straight to it.
#'
#' @param data A `data.frame` with numeric `ptime` and `stime` columns, as
#'  returned by [simulate_secondary()].
#'
#' @param outbreak_start_date The date the outbreak started, corresponding to
#'  time zero.
#'
#' @param primary_window Width of the primary event reporting window in days.
#'  Either a single value used for every observation, or one value per row of
#'  `data`. The default of 1 gives daily reporting. Use 7 for weekly.
#'
#' @param secondary_window Width of the secondary event reporting window in
#'  days, in the same form as `primary_window`. Defaults to `primary_window`,
#'  so the two events share a reporting interval unless you say otherwise.
#'
#' @param obs_time Optional numeric observation time, in the same units as
#'  `ptime` and `stime`. When supplied an `obs_date` column is added. When
#'  `NULL`, the default, no observation date is added and
#'  [as_epidist_linelist_data()] falls back to the day after the last
#'  secondary event.
#'
#' @param keep_times Whether to keep the underlying numeric times. Useful when
#'  comparing estimates against the values used to simulate.
#'
#' @returns A `data.frame` with `pdate_lwr`, `pdate_upr`, `sdate_lwr` and
#'  `sdate_upr` columns, and `obs_date` when `obs_time` is supplied.
#'
#' @family simulate
#' @autoglobal
#' @importFrom dplyr mutate select all_of
#' @importFrom checkmate assert_names assert_date assert_number
#' @importFrom checkmate assert_integerish
#' @export
#' @examples
#' simulate_gillespie(seed = 1) |>
#'   simulate_secondary(distspec::LogNormal(meanlog = 1.8, sdlog = 0.5)) |>
#'   simulate_dates(outbreak_start_date = as.Date("2024-02-01")) |>
#'   head()
simulate_dates <- function(
  data,
  outbreak_start_date = as.Date("2024-01-01"),
  primary_window = 1,
  secondary_window = primary_window,
  obs_time = NULL,
  keep_times = FALSE
) {
  assert_names(names(data), must.include = c("ptime", "stime"))
  assert_date(outbreak_start_date, len = 1, any.missing = FALSE)
  primary_window <- .assert_window(primary_window, nrow(data), "primary_window")
  secondary_window <- .assert_window(
    secondary_window, nrow(data), "secondary_window"
  )
  if (!is.null(obs_time)) {
    assert_number(obs_time, lower = 0, finite = TRUE)
  }

  sim_data <- data |>
    mutate(
      pdate_lwr = outbreak_start_date +
        primary_window * floor(.data$ptime / primary_window),
      pdate_upr = .data$pdate_lwr + primary_window,
      sdate_lwr = outbreak_start_date +
        secondary_window * floor(.data$stime / secondary_window),
      sdate_upr = .data$sdate_lwr + secondary_window
    )

  if (!is.null(obs_time)) {
    sim_data <- mutate(
      sim_data,
      obs_date = outbreak_start_date + floor(obs_time)
    )
  }

  if (!keep_times) {
    sim_data <- select(sim_data, -all_of(intersect(
      c("ptime", "stime", "delay"), names(sim_data)
    )))
  }

  return(sim_data)
}

#' Check a reporting window argument
#'
#' Accepts a single value or one value per observation, and recycles a single
#' value so the caller can treat them the same.
#'
#' @param window The reporting window argument to check.
#'
#' @param n The number of observations it has to cover.
#'
#' @param name The argument name, used in error messages.
#'
#' @returns The window as a vector of length `n`.
#'
#' @keywords internal
.assert_window <- function(window, n, name) {
  assert_integerish(
    window,
    lower = 1,
    any.missing = FALSE,
    .var.name = name
  )
  if (length(window) == 1) {
    return(rep(window, n))
  }
  if (length(window) != n) {
    cli::cli_abort(
      "{.arg {name}} must be a single value or one per observation
       ({n}), not {length(window)}."
    )
  }
  return(window)
}
