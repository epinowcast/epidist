make_standata_target <- function(stan_model, ...){
  if(!("brmsmodel" %in% class(stan_model)))
    stop("Expecting an object of class brmsmodel in stan_model argument")
  
  standata <- brms::make_standata(...)
  return(unclass(standata))
}