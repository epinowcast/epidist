#' Simulate cases from a uniform distribution
#'
#' This function simulates cases from a uniform distribution, where the primary
#' event times are uniformly distributed between 0 and `t`.
#'
#' @param sample_size The number of cases to simulate.
#' @param t Upper bound of the uniform distribution to generate primary event
#' times.
#'
#' @return A `data.table` with two columns: `case` (case number) and `ptime`
#' (primary event time).
#'
#' @family simulate
#' @export
simulate_uniform_cases <- function(sample_size = 1000, t = 60) {
  data.table::data.table(
    case = 1:sample_size, ptime = runif(sample_size, 0, t)
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
#' @return A `data.table` with two columns: `case` (case number) and `ptime`
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
  quant <- runif(sample_size, 0, 1)

  if (r == 0) {
    ptime <- quant * t
  } else {
    ptime <- log(1 + quant * (exp(r * t) - 1)) / r
  }

  cases <- data.table::data.table(
    case = seq_along(ptime),
    ptime = ptime
  )
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
#' @return A `data.table` with two columns: `case` (case number) and `ptime`
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
      deltat <- rexp(1, rate = srates)
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

  cases <- data.table::data.table(
    case = seq_along(ptime),
    ptime = ptime
  )

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
#' @return A `data.table` that augments `linelist` with two new columns: `delay`
#' (secondary event latency) and `stime` (the time of the secondary event).
#'
#' @family simulate
#' @export
simulate_secondary <- function(linelist, dist = rlnorm, ...) {
  delay <- ptime <- stime <- NULL
  obs <- data.table::copy(linelist)

  obs[, delay := dist(.N, ...)]
  obs[, stime := ptime + delay]

  return(obs)
}

#' Simulate a censored PMF
#'
#' This function simulates a double-censored probability mass function (PMF).
#' The user may specify the `alpha`, `beta`, and upper bound of the event times.
#' Additionally, the user can specify the random number generator functions for
#' primary events, secondary events, and delays.
#'
#' @param alpha The shape parameter of the delay distribution.
#' @param beta The scale parameter of the delay distribution.
#' @param max The upper bound of event time.
#' @param n The total number of cases to simulate.
#' @param rprimary Random number generator function for primary events.
#' Defaults to runif.
#' @param rdelay Random number generator function for delays. Defaults to
#' [rlnorm()].
#' @param delay_obs_process Observation process for delays. Defaults to
#' using the `floor` function to round both primary and secondary events to the
#' nearest integer. Internally the delay is also bounded to be non-negative.
#'
#' @return A probability mass function that represents the distribution of the
#' delays.
#'
#' @family simulate
#' @export
simulate_double_censored_pmf <- function(
  alpha, beta, max, n = 1000,
  rprimary = \(x) (runif(x, 0, 1)), rdelay = rlnorm,
  delay_obs_process = \(s, p) (floor(s) - floor(p))
) {
  primary <- rprimary(n)
  secondary <- primary + rdelay(n, alpha, beta)
  delay <- delay_obs_process(secondary, primary)
  delay <- pmax(delay, 0)
  cdf <- ecdf(delay)(0:max)
  pmf <- c(cdf[1], diff(cdf))
  return(pmf)
}
