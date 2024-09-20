#' Simulate cases from a uniform distribution
#'
#' This function simulates cases from a uniform distribution, where the primary
#' event times are uniformly distributed between 0 and `t`.
#'
#' @param sample_size The number of cases to simulate.
#' @param t Upper bound of the uniform distribution to generate primary event
#' times.
#'
#' @return A `data.frame` with two columns: `case` (case number) and `ptime`
#' (primary event time).
#'
#' @family simulate
#' @export
simulate_uniform_cases <- function(sample_size = 1000, t = 60) {
  data.frame(
    case = 1:sample_size, ptime = stats::runif(sample_size, 0, t)
  )
}

#' Simulate exponential cases
#'
#' This function simulates cases from an exponential distribution. The user may
#' specify the rate parameter `r`, the sample size, and the upper bound of the
#' survival time. If the rate parameter is 0, then this function defaults to the
#' uniform distribution.
#'
#' @param r The exponential growth rate parameter. Defaults to 0.2.
#' @param sample_size The number of cases to simulate. Defaults to 10000.
#' @param seed The random seed to be used in the simulation process.
#' @param t Upper bound of the survival time. Defaults to 30.
#'
#' @return A `data.frame` with two columns: `case` (case number) and `ptime`
#' (primary event time).
#'
#' @family simulate
#' @export
simulate_exponential_cases <- function(r = 0.2,
                                       sample_size = 10000,
                                       seed,
                                       t = 30) {
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
#' @param gamma The rate of recovery. Defaults to 1/7.
#' @param I0 The initial number of infected people. Defaults to 50.
#' @param N The total population size. Defaults to 10000.
#' @param seed The random seed to be used in the simulation process.
#'
#' @return A `data.frame` with two columns: `case` (case number) and `ptime`
#' (primary event time).
#'
#' @family simulate
#' @export
simulate_gillespie <- function(r = 0.2,
                               gamma = 1 / 7,
                               I0 = 50, # nolint: object_name_linter
                               N = 10000, # nolint: object_name_linter
                               seed) {
  if (!missing(seed)) {
    set.seed(seed)
  }
  t <- 0
  state <- c(N - I0, I0, 0)
  beta <- r + gamma
  go <- TRUE
  ptime <- NULL

  while (go) {
    rates <- c(beta * state[1] * state[2] / N, gamma * state[2])
    srates <- sum(rates)

    if (srates > 0) {
      deltat <- stats::rexp(1, rate = srates)
      t <- t + deltat
      wevent <- sample(seq_along(rates), size = 1, prob = rates)

      if (wevent == 1) {
        state <- c(state[1] - 1, state[2] + 1, state[3])
        ptime <- c(ptime, t)
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
#' @param linelist A data frame with the primary event times.
#' @param dist The delay distribution to be used. Defaults to [rlnorm()].
#' @param ... Arguments to be passed to the delay distribution function.
#'
#' @return A `data.frame` that augments `linelist` with two new columns: `delay`
#' (secondary event latency) and `stime` (the time of the secondary event).
#'
#' @family simulate
#' @autoglobal
#' @importFrom dplyr mutate
#' @export
simulate_secondary <- function(linelist, dist = rlnorm, ...) {
  linelist |>
    mutate(
      delay = dist(dplyr::n(), ...),
      stime = .data$ptime + .data$delay
    )
}
