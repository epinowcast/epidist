library(data.table)
library(rstan)
load("data_exponential.rda")

options(mc.cores=4)

model <- stan_model("lognormal_doublecensor.stan")

rvec <- seq(-0.2, 0.2, length.out=11)

reslist <- vector('list', length(rvec))

for (i in 1:length(rvec)) {
  print(i)
  tt <- truncated_obs[r==rvec[i]]
  
  standata <- list(
    N=nrow(tt),
    ptime_lwr=tt$ptime_lwr,
    ptime_upr=tt$ptime_upr,
    stime_lwr=tt$stime_lwr,
    stime_upr=tt$stime_upr,
    end_t=20
  )
  
  fit <- sampling(model,standata,iter=2000,chains=4)
  
  ss <- summary(fit)
  
  sslm <- ss$summary[rownames(ss$summary) == "lognormal_mean",]
  
  reslist[[i]] <- data.frame(
    estimate=sslm[[1]],
    lwr=sslm[[4]],
    upr=sslm[[8]],
    r=rvec[i]
  )
} 

fit_exponential_doublecensor <- do.call("rbind", reslist)

save("fit_exponential_doublecensor", file="fit_exponential_doublecensor.rda")
