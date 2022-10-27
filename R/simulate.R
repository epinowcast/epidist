simulate_uniform_cases <- function(sample_size = 1000, t = 60) {
  data.table::data.table(
    case = 1:sample_size, ptime = runif(sample_size, 0, t)
  )
}

simulate_secondary <- function(linelist, dist = rlnorm, ...) {
  obs <- linelist |>
    data.table::copy() |>
    DT(, delay := dist(.N, ...)) |>
    # When the second event actually happens
    DT(, stime := ptime + delay)
  return(obs)
}
