simulate_gillespie <- function(r=0.2,
                               gamma=1/7,
                               I0=50, ## to avoid extinction
                               N=10000) {
  t <- 0
  
  state <- c(N-I0, I0, 0)
  
  beta <- r+gamma
  
  go <- TRUE
  
  ptime <- NULL
  
  while(go) {
    rates <- c(beta * state[1] * state[2]/N, gamma * state[2])
    srates <- sum(rates)
    
    if (srates>0) {
      deltat <- rexp(1, rate=srates)
      
      t <- t + deltat
      
      wevent <- sample(1:length(rates),size=1,prob=rates)
      
      if(wevent==1) {
        state <- c(state[1]-1, state[2]+1, state[3])
        ptime <- c(ptime, t)
      } else {
        state <- c(state[1], state[2]-1, state[3]+1)
      }
    } else {
      go <- FALSE
    }
    
  }
  
  data.table::data.table(
    id=1:length(ptime),
    ptime=ptime
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

simulate_observations <- function(linelist) {
  clinelist <- linelist |>
    data.table::copy() |>
    DT(, ptime_daily := floor(ptime)) |>
    DT(, ptime_lwr := ptime_daily) |>
    DT(, ptime_upr := ptime_daily + 1) |>
    # How the second event would be recorded in the data
    DT(, stime_daily := floor(stime)) |>
    DT(, stime_lwr := stime_daily) |>
    DT(, stime_upr := stime_daily + 1) |>
    # How would we observe the delay distribution
    DT(, delay_daily := floor(delay)) |>
    DT(, delay_lwr := max(0, delay_daily - 1)) |>
    DT(, delay_upr := delay_daily + 1)
  return(clinelist)
}

filter_obs_by_obs_time <- function(linelist, obs_time) {
  truncated_linelist <- linelist |>
    data.table::copy() |>
    DT(, obs_time := obs_time - ptime) |>
    DT(, censored_obs_time := obs_time - ptime_daily) |>
    DT(, censored := "interval") |>
    DT(stime <= obs_time)
  return(truncated_linelist)
}