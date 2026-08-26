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
#' @return A `data.frame` with two columns: `case` (case number) and `ptime`
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
#' @param seed The random seed to be used in the simulation process.
#'
#' @param t Upper bound of the survival time. Defaults to 30.
#'
#' @return A `data.frame` with two columns: `case` (case number) and `ptime`
#'  (primary event time).
#'
#' @family simulate
#' @export
simulate_exponential_cases <- function(
  r = 0.2,
  sample_size = 10000,
  seed,
  t = 30
) {
  if (!missing(seed)) {
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
#' @param seed The random seed to be used in the simulation process.
#'
#' @return A `data.frame` with two columns: `case` (case number) and `ptime`
#' (primary event time).
#'
#' @family simulate
#' @export
simulate_gillespie <- function(
  r = 0.2,
  gamma = 1 / 7,
  I0 = 50, # nolint: object_name_linter
  N = 10000, # nolint: object_name_linter
  seed
) {
  if (!missing(seed)) {
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
#' This function simulates secondary events based on a given delay
#' distribution. The input dataset should have the primary event times in a
#' column named `ptime`.
#'
#' @param data A data frame with the primary event times.
#'
#' @param dist The delay distribution to be used. Defaults to [rlnorm()].
#'
#' @param ... Arguments to be passed to the delay distribution function.
#'
#' @return A `data.frame` that augments `data` with two new columns: `delay`
#'  (secondary event latency) and `stime` (the time of the secondary event).
#'
#' @family simulate
#' @autoglobal
#' @importFrom dplyr mutate
#' @export
simulate_secondary <- function(data, dist = rlnorm, ...) {
  sim_data <- data |>
    mutate(
      delay = dist(dplyr::n(), ...),
      stime = .data$ptime + .data$delay
    )
  return(sim_data)
}

#' Convert simulated event times to dates
#'
#' Takes the continuous event times produced by [simulate_gillespie()] and
#' [simulate_secondary()] and returns the dates an analyst would actually
#' receive. Event times are floored to the reporting window, so each event is
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
#' @param censoring_window Width of the reporting window in days. The default
#'  of 1 gives daily reporting. Use 7 for weekly reporting.
#'
#' @param obs_time Optional numeric observation time, in the same units as
#'  `ptime` and `stime`. When supplied an `obs_date` column is added. When
#'  `NULL`, the default, no observation date is added and
#'  [as_epidist_linelist_data()] will assume the last secondary event.
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
#'   simulate_secondary(meanlog = 1.8, sdlog = 0.5) |>
#'   simulate_dates(outbreak_start_date = as.Date("2024-02-01")) |>
#'   head()
simulate_dates <- function(
  data,
  outbreak_start_date = as.Date("2024-01-01"),
  censoring_window = 1,
  obs_time = NULL,
  keep_times = FALSE
) {
  assert_names(names(data), must.include = c("ptime", "stime"))
  assert_date(outbreak_start_date, len = 1, any.missing = FALSE)
  assert_integerish(censoring_window, lower = 1, len = 1, any.missing = FALSE)
  if (!is.null(obs_time)) {
    assert_number(obs_time, lower = 0, finite = TRUE)
  }

  sim_data <- data |>
    mutate(
      pdate_lwr = outbreak_start_date +
        censoring_window * floor(.data$ptime / censoring_window),
      pdate_upr = .data$pdate_lwr + censoring_window,
      sdate_lwr = outbreak_start_date +
        censoring_window * floor(.data$stime / censoring_window),
      sdate_upr = .data$sdate_lwr + censoring_window
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
