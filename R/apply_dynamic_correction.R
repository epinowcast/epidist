##' @param x brms object
##' @param r growth rate
##' @param subsample number of subsamples
apply_dynamic_correction <- function(x, r, subsample, correct = FALSE) {
  pp <- posterior_samples(x)
  
  npost <- nrow(pp)
  
  ss <- sample(1:npost, subsample, replace=FALSE)
  
  reslist <- vector('list', length(r))
  
  for (j in 1:length(r)) {
    meanvec <- rep(NA, length(ss))
    
    for (i in ss) {
      logmean <- pp[i, j]
      logsd <- exp(pp[i, j+length(r)])
      
      if (correct) {
        
        ii1 <- integrate(function(z) {
          dlnorm(z, meanlog=logmean, sdlog=logsd) * exp(r[j] * z)
        }, 0, 100, subdivisions = 100L)[[1]]
        
        ii2 <- integrate(function(z) {
          dlnorm(z, meanlog=logmean, sdlog=logsd) * exp(r[j] * z) * z
        }, 0, 100, subdivisions = 100L)[[1]]
        
        meanvec[match(i, ss)] <- ii2/ii1
      } else {
        meanvec[match(i, ss)] <- exp(logmean + logsd^2/2)
      }
      
    }
    
    reslist[[j]] <- data.frame(
      estimate=mean(meanvec),
      lwr=quantile(meanvec, 0.025),
      upr=quantile(meanvec, 0.975),
      r=r[j]
    )
  }
  
  out <- do.call("rbind", reslist)
  
  out
}
