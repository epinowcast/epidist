simulate_uniform_cases <- function(sample_size = 1000, t = 60) {
  data.table::data.table(
    case = 1:sample_size, ptime = runif(sample_size, 0, t)
  )
}

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
    ptime <- log(1 + quant * (exp(r * t) - 1))/r
  }
  
  cases <- data.table::data.table(
    case = seq_along(ptime),
    ptime = ptime
  )
  return(cases)
}

simulate_gillespie <- function(r = 0.2,
                               gamma = 1 / 7,
                               init_I = 50, ## to avoid extinction
                               n = 10000,
                               seed) {
  if (!missing(seed)) {
    set.seed(seed)
  }
  t <- 0
  state <- c(n - init_I, init_I, 0)
  beta <- r + gamma
  go <- TRUE
  ptime <- NULL

  while (go) {
    rates <- c(beta * state[1] * state[2] / n, gamma * state[2])
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

simulate_secondary <- function(linelist, dist = rlnorm, ...) {
  obs <- linelist |>
    data.table::copy() |>
    DT(, delay := dist(.N, ...)) |>
    # When the second event actually happens
    DT(, stime := ptime + delay)
  return(obs)
}
