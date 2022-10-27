simulate_uniform_cases <- function(sample_size = 1000, t = 60) {
  data.table::data.table(
    case = 1:sample_size, ptime = runif(sample_size, 0, t)

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
