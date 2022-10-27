
# Adjusting for common biases in infectious disease data when estimating distributions.

## Methods

Our goal is to estimate $f_s(\tau)$, where $s$ represents cohort time
(i.e., the timing of the primaruy event), from truncated data. Five
fits: \* Naive: $f_s(\tau)$ \* Right truncation:
$f_s(\tau)/F_s(t_{max}-s)$ \* Dynamical correction:
$b(\tau) = \exp(-r\tau) f_s(\tau)/(\int \exp(-r\tau) f_s(\tau) d\tau)$.
This is a bit trickier to fit because we have to do some integration.
Instead, we fit a lognormal distribution that represents $b(\tau)$ and
calculate $f_s(\tau)$ after the fit for each posterior sample. \* Right
truncation+dynamical correction: $b(\tau)/B(t_{max}-s)$ \* Left
trunction+dynamical correction: $b(\tau)/B(s-t_{min})$. Again, we fit a
single lognormal distribution with left truncation and then correct for
underlying dynamics after the fit

## Results

![](figures/figure_exponential.png)<!-- -->

- Naive fits are not too bad for $r < 0$ because not much right
  truncation
- Dynamical correction alone is not so good for $r < 0$ because we’re
  missing left truncation (because we’re starting from high incidence at
  $t=0$ and neglecting infections starting $t < 0$)
- Right truncation is good
- Right truncation + dynamical correction overcompensates and goes crazy
- Left truncation + dynamical correction is as good as right truncation
  but starts to give super wide CIs for high $r$ values. Need to check
  integration step.

![](figures/figure_doublecensor.png)<!-- -->

- Doubly censored likelihood performs just as well as if we knew exact
  time points
- Couldn’t try negative binomial with truncation because it takes
  forever

## Flow

Makefile currently doesn’t work because I had to switch to my Windows pc
to run brms. I’m saving rda files to the rdacache directory so that I
can move between machines (which you don’t need to update)…

- `param.R` sets parameters
- `data_exponential.R` generates data
- `fit_exponential.R` performs fits
- `figure_exponential.R` generates the main figure

## A simple example

First load required packages and functions.

``` r
library(data.table)
library(purrr, quietly = TRUE)
library(here)
library(brms)
functions <- list.files(here("R"), full.names = TRUE)
walk(functions, source)
```

Now simulate data from an outbreak, simulate an observation process for
a secondary event using a lognormal distribution, and finally simulate
observing this event.

``` r
truncated_obs <- simulate_uniform_cases(sample_size = 1000, t = 60) |>
  simulate_secondary(
    meanlog = 1.8,
    sdlog = 0.3
  ) |>
  observe_process() |>
  filter_obs_by_obs_time(obs_time = 45)
```

First fit a naive lognormal model with no adjustment.

``` r
naive_fit <- naive_delay(data = truncated_obs, cores = 4)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 1 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 1 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 1 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 1 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 1 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 1 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 1 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 1 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 2 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 2 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 2 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 2 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 2 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 2 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 2 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 2 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 2 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 2 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 2 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 2 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 2 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 2 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 3 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 3 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 3 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 3 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 3 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 3 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 3 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 3 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 3 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 3 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 3 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 3 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 3 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 3 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 3 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 3 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 3 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 4 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 4 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 4 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 4 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 4 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 4 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 4 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 4 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 4 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 4 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 1 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 1 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 1 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 1 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 1 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 1 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 1 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 1 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 2 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 2 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 2 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 2 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 2 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 3 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 3 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 3 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 3 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 3 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 4 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 4 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 4 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 4 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 4 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 1 finished in 0.4 seconds.
#> Chain 2 finished in 0.4 seconds.
#> Chain 3 finished in 0.4 seconds.
#> Chain 4 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 4 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 4 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 4 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 4 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 4 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 4 finished in 0.5 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 0.4 seconds.
#> Total execution time: 0.7 seconds.
summary(naive_fit)
#>  Family: lognormal 
#>   Links: mu = identity; sigma = log 
#> Formula: delay_daily ~ 1 
#>          sigma ~ 1
#>    Data: data (Number of observations: 317) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Population-Level Effects: 
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept           1.69      0.02     1.65     1.72 1.00     3699     2845
#> sigma_Intercept    -1.11      0.04    -1.18    -1.03 1.00     3259     2486
#> 
#> Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

Adjust for date censoring.

``` r
censored_fit <- censoring_adjusted_delay(data = truncated_obs, cores = 4)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 1 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 2 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 2 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 3 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 3 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 4 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 4 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 1 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 1 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 1 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 2 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 2 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 3 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 3 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 4 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 1 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 2 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 2 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 3 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 4 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 1 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 1 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 2 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 2 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 3 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 3 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 4 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 4 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 1 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 2 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 3 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 3 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 4 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 1 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 2 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 2 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 3 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 3 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 3 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 4 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 4 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 1 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 1 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 2 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 3 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 4 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 4 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 2 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 3 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 4 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 1 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 2 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 2 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 3 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 3 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 4 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 1 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 2 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 2 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 3 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 3 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 4 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 4 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 1 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 1 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 2 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 3 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 4 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 1 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 2 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 3 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 3 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 4 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 4 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 2 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 3 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 4 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 1 finished in 1.4 seconds.
#> Chain 2 finished in 1.5 seconds.
#> Chain 3 finished in 1.4 seconds.
#> Chain 4 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 4 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 4 finished in 1.5 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 1.5 seconds.
#> Total execution time: 1.7 seconds.
summary(censored_fit)
#>  Family: lognormal 
#>   Links: mu = identity; sigma = log 
#> Formula: delay_lwr | cens(censored, delay_upr) ~ 1 
#>          sigma ~ 1
#>    Data: data (Number of observations: 317) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Population-Level Effects: 
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept           1.70      0.02     1.66     1.73 1.00     2762     2332
#> sigma_Intercept    -1.19      0.05    -1.28    -1.10 1.00     3465     3000
#> 
#> Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

Adjust for right truncation.

``` r
truncation_fit <- truncation_adjusted_delay(data = truncated_obs, cores = 4)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 1 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 2 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 2 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 3 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 3 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 4 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 4 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 1 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 1 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 2 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 2 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 3 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 4 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 1 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 2 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 3 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 3 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 4 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 1 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 1 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 2 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 2 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 3 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 4 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 1 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 2 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 3 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 4 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 4 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 1 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 1 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 2 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 2 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 3 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 3 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 4 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 3 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 4 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 4 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 1 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 2 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 2 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 3 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 3 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 3 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 4 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 2 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 2 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 3 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 4 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 1 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 2 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 3 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 4 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 1 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 2 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 2 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 3 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 3 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 4 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 4 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 1 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 2 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 3 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 4 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 1 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 2 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 3 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 4 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 2 finished in 1.4 seconds.
#> Chain 1 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 3 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 4 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 1 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 3 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 3 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 4 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 4 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 1 finished in 1.7 seconds.
#> Chain 3 finished in 1.6 seconds.
#> Chain 4 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 4 finished in 1.6 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 1.6 seconds.
#> Total execution time: 1.8 seconds.
summary(truncation_fit)
#>  Family: lognormal 
#>   Links: mu = identity; sigma = log 
#> Formula: delay_daily | trunc(lb = 1, ub = censored_obs_time) ~ 1 
#>          sigma ~ 1
#>    Data: data (Number of observations: 317) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Population-Level Effects: 
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept           1.70      0.02     1.66     1.74 1.00     3474     2492
#> sigma_Intercept    -1.09      0.04    -1.17    -1.01 1.00     3248     2288
#> 
#> Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

Adjust for right truncation and date censoring.

``` r
truncation_censoring_fit <- truncation_censoring_adjusted_delay(
  data = truncated_obs, cores = 4
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 2 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 3 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 4 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 1 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 2 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 3 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 4 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 1 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 2 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 3 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 1 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 2 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 3 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 4 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 1 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 2 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 3 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 4 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 1 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 2 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 3 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 4 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 1 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 2 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 3 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 1 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 2 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 3 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 4 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 1 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 2 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 3 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 4 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 1 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 2 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 3 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 4 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 3 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 3 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 4 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 4 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 1 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 2 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 3 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 4 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 3 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 2 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 3 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 4 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 1 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 2 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 3 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 4 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 1 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 3 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 2 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 3 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 4 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 2 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 3 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 4 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 1 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 3 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 1 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 2 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 4 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 2 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 3 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 4 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 1 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 3 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 4 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 3 finished in 2.6 seconds.
#> Chain 1 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 2 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 2 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 4 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 4 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 1 finished in 3.0 seconds.
#> Chain 2 finished in 3.0 seconds.
#> Chain 4 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 4 finished in 3.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 2.9 seconds.
#> Total execution time: 3.2 seconds.
summary(truncation_censoring_fit)
#>  Family: lognormal 
#>   Links: mu = identity; sigma = log 
#> Formula: delay_lwr | cens(censored, delay_upr) + trunc(lb = 1, ub = censored_obs_time) ~ 1 
#>          sigma ~ 1
#>    Data: data (Number of observations: 317) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Population-Level Effects: 
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept           1.71      0.02     1.67     1.74 1.00     3192     2347
#> sigma_Intercept    -1.18      0.05    -1.27    -1.08 1.00     3038     2128
#> 
#> Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

Adjust for right truncation and date censoring using a latent variable
approach.

``` r
latent_truncation_censoring_fit <- latent_truncation_censoring_adjusted_delay(
  data = truncated_obs, cores = 4
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 2 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 3 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 4 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 2 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 3 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 2 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 1 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 3 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 4 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 2 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 3 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 1 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 4 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 2 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 3 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 1 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 4 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 2 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 3 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 1 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 4 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 2 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 3 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 1 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 3 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 2 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 4 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 1 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 3 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 2 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 4 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 3 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 1 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 2 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 4 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 1 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 3 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 3 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 4 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 1 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 2 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 3 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 4 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 3 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 4 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 2 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 3 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 4 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 1 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 2 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 3 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 4 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 1 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 2 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 3 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 4 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 1 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 2 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 3 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 4 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 2 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 3 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 4 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 1 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 2 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 3 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 4 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 1 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 2 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 3 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 4 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 1 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 3 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 2 finished in 53.7 seconds.
#> Chain 3 finished in 53.7 seconds.
#> Chain 4 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 1 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 4 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 1 finished in 54.9 seconds.
#> Chain 4 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 4 finished in 55.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 54.3 seconds.
#> Total execution time: 55.2 seconds.
summary(latent_truncation_censoring_fit)
#>  Family: latent_lognormal 
#>   Links: mu = identity; sigma = log; pwindow = identity; swindow = identity 
#> Formula: ptime | vreal(stime, obs_at) ~ 1 
#>          sigma ~ 1
#>          pwindow ~ 0 + as.factor(id)
#>          swindow ~ 0 + as.factor(id)
#>    Data: data (Number of observations: 317) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Population-Level Effects: 
#>                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> Intercept                  1.79      0.02     1.76     1.82 1.00     5116
#> sigma_Intercept           -1.28      0.04    -1.36    -1.20 1.00     5169
#> pwindow_as.factorid1       0.54      0.29     0.03     0.98 1.00     6369
#> pwindow_as.factorid2       0.51      0.29     0.02     0.98 1.00     5664
#> pwindow_as.factorid3       0.53      0.29     0.03     0.98 1.00     5092
#> pwindow_as.factorid4       0.52      0.29     0.03     0.98 1.00     5509
#> pwindow_as.factorid5       0.46      0.29     0.02     0.97 1.00     5907
#> pwindow_as.factorid6       0.51      0.29     0.03     0.97 1.00     6275
#> pwindow_as.factorid7       0.38      0.26     0.01     0.93 1.00     5446
#> pwindow_as.factorid8       0.50      0.29     0.02     0.98 1.00     6339
#> pwindow_as.factorid9       0.53      0.29     0.03     0.98 1.00     6462
#> pwindow_as.factorid10      0.50      0.29     0.02     0.97 1.00     5429
#> pwindow_as.factorid11      0.57      0.29     0.04     0.99 1.00     5985
#> pwindow_as.factorid12      0.39      0.28     0.01     0.95 1.00     5901
#> pwindow_as.factorid13      0.55      0.28     0.03     0.98 1.00     6333
#> pwindow_as.factorid14      0.56      0.28     0.04     0.98 1.01     5914
#> pwindow_as.factorid15      0.44      0.28     0.02     0.96 1.00     4939
#> pwindow_as.factorid16      0.55      0.29     0.03     0.98 1.00     6287
#> pwindow_as.factorid17      0.54      0.28     0.04     0.98 1.00     6685
#> pwindow_as.factorid18      0.52      0.29     0.03     0.97 1.00     4737
#> pwindow_as.factorid19      0.53      0.29     0.03     0.97 1.00     5434
#> pwindow_as.factorid20      0.49      0.29     0.02     0.97 1.00     5840
#> pwindow_as.factorid21      0.56      0.29     0.03     0.99 1.00     5312
#> pwindow_as.factorid22      0.54      0.28     0.03     0.98 1.00     7460
#> pwindow_as.factorid23      0.56      0.28     0.04     0.98 1.00     5367
#> pwindow_as.factorid24      0.50      0.29     0.03     0.97 1.00     6232
#> pwindow_as.factorid25      0.53      0.28     0.04     0.97 1.00     4943
#> pwindow_as.factorid26      0.49      0.29     0.03     0.97 1.00     5460
#> pwindow_as.factorid27      0.54      0.28     0.03     0.98 1.00     5613
#> pwindow_as.factorid28      0.55      0.29     0.03     0.98 1.00     6111
#> pwindow_as.factorid29      0.43      0.28     0.02     0.96 1.01     5514
#> pwindow_as.factorid30      0.54      0.28     0.03     0.98 1.00     6232
#> pwindow_as.factorid31      0.51      0.29     0.03     0.98 1.00     5516
#> pwindow_as.factorid32      0.42      0.28     0.02     0.95 1.00     6669
#> pwindow_as.factorid33      0.51      0.28     0.04     0.97 1.00     5514
#> pwindow_as.factorid34      0.41      0.28     0.02     0.95 1.00     6871
#> pwindow_as.factorid35      0.55      0.29     0.03     0.98 1.00     6227
#> pwindow_as.factorid36      0.54      0.28     0.03     0.98 1.00     7527
#> pwindow_as.factorid37      0.35      0.26     0.01     0.92 1.00     6240
#> pwindow_as.factorid38      0.51      0.29     0.03     0.97 1.00     6309
#> pwindow_as.factorid39      0.56      0.29     0.03     0.98 1.00     6211
#> pwindow_as.factorid40      0.51      0.28     0.03     0.97 1.00     6086
#> pwindow_as.factorid41      0.56      0.29     0.04     0.98 1.00     6888
#> pwindow_as.factorid42      0.52      0.29     0.03     0.97 1.00     6650
#> pwindow_as.factorid43      0.42      0.28     0.02     0.95 1.01     5689
#> pwindow_as.factorid44      0.51      0.29     0.02     0.97 1.00     4867
#> pwindow_as.factorid45      0.55      0.29     0.04     0.98 1.00     6501
#> pwindow_as.factorid46      0.55      0.29     0.03     0.98 1.00     4878
#> pwindow_as.factorid47      0.52      0.29     0.02     0.98 1.00     5847
#> pwindow_as.factorid48      0.47      0.29     0.02     0.97 1.00     6397
#> pwindow_as.factorid49      0.47      0.29     0.02     0.96 1.00     7177
#> pwindow_as.factorid50      0.52      0.28     0.03     0.98 1.00     5055
#> pwindow_as.factorid51      0.32      0.25     0.01     0.87 1.00     6243
#> pwindow_as.factorid52      0.51      0.29     0.02     0.98 1.00     5184
#> pwindow_as.factorid53      0.55      0.29     0.03     0.99 1.00     5220
#> pwindow_as.factorid54      0.47      0.29     0.02     0.97 1.00     6236
#> pwindow_as.factorid55      0.56      0.29     0.04     0.98 1.00     7056
#> pwindow_as.factorid56      0.56      0.28     0.04     0.98 1.00     6620
#> pwindow_as.factorid57      0.46      0.29     0.02     0.97 1.00     5233
#> pwindow_as.factorid58      0.52      0.29     0.03     0.97 1.00     5495
#> pwindow_as.factorid59      0.48      0.29     0.02     0.97 1.00     6113
#> pwindow_as.factorid60      0.48      0.28     0.03     0.96 1.00     7150
#> pwindow_as.factorid61      0.52      0.29     0.03     0.98 1.00     6221
#> pwindow_as.factorid62      0.55      0.28     0.04     0.98 1.01     6075
#> pwindow_as.factorid63      0.41      0.28     0.02     0.95 1.00     5110
#> pwindow_as.factorid64      0.48      0.28     0.02     0.97 1.00     5753
#> pwindow_as.factorid65      0.48      0.29     0.02     0.97 1.00     6135
#> pwindow_as.factorid66      0.52      0.28     0.03     0.98 1.00     5067
#> pwindow_as.factorid67      0.53      0.28     0.04     0.98 1.01     6441
#> pwindow_as.factorid68      0.54      0.29     0.03     0.98 1.00     5127
#> pwindow_as.factorid69      0.46      0.29     0.01     0.97 1.00     5397
#> pwindow_as.factorid70      0.46      0.28     0.02     0.97 1.00     6270
#> pwindow_as.factorid71      0.52      0.29     0.03     0.98 1.00     6502
#> pwindow_as.factorid72      0.54      0.28     0.03     0.97 1.01     7278
#> pwindow_as.factorid73      0.38      0.27     0.01     0.93 1.00     4993
#> pwindow_as.factorid74      0.54      0.29     0.03     0.98 1.00     5808
#> pwindow_as.factorid75      0.48      0.29     0.03     0.97 1.00     5172
#> pwindow_as.factorid76      0.56      0.29     0.04     0.98 1.00     6166
#> pwindow_as.factorid77      0.54      0.28     0.04     0.98 1.00     6138
#> pwindow_as.factorid78      0.56      0.28     0.03     0.98 1.00     5916
#> pwindow_as.factorid79      0.49      0.28     0.02     0.97 1.00     5377
#> pwindow_as.factorid80      0.56      0.28     0.03     0.98 1.00     5523
#> pwindow_as.factorid81      0.50      0.28     0.03     0.97 1.00     6358
#> pwindow_as.factorid82      0.51      0.29     0.03     0.98 1.00     5535
#> pwindow_as.factorid83      0.55      0.28     0.03     0.98 1.00     5095
#> pwindow_as.factorid84      0.57      0.28     0.04     0.98 1.00     5288
#> pwindow_as.factorid85      0.42      0.28     0.02     0.95 1.00     4687
#> pwindow_as.factorid86      0.54      0.29     0.04     0.98 1.00     6632
#> pwindow_as.factorid87      0.54      0.29     0.03     0.98 1.00     5098
#> pwindow_as.factorid88      0.54      0.29     0.03     0.98 1.00     5906
#> pwindow_as.factorid89      0.56      0.29     0.04     0.98 1.00     5700
#> pwindow_as.factorid90      0.55      0.29     0.03     0.98 1.00     5862
#> pwindow_as.factorid91      0.48      0.29     0.02     0.97 1.00     5455
#> pwindow_as.factorid92      0.56      0.29     0.03     0.98 1.00     5166
#> pwindow_as.factorid93      0.54      0.28     0.04     0.98 1.00     5538
#> pwindow_as.factorid94      0.49      0.29     0.03     0.97 1.00     5459
#> pwindow_as.factorid95      0.54      0.28     0.04     0.98 1.00     6381
#> pwindow_as.factorid96      0.49      0.29     0.02     0.97 1.00     6114
#> pwindow_as.factorid97      0.50      0.29     0.03     0.98 1.00     6164
#> pwindow_as.factorid98      0.53      0.29     0.03     0.98 1.00     6184
#> pwindow_as.factorid99      0.46      0.29     0.02     0.97 1.00     6048
#> pwindow_as.factorid100     0.31      0.24     0.01     0.88 1.00     6263
#> pwindow_as.factorid101     0.49      0.28     0.03     0.97 1.00     5436
#> pwindow_as.factorid102     0.54      0.29     0.03     0.98 1.00     4883
#> pwindow_as.factorid103     0.48      0.29     0.02     0.97 1.00     4687
#> pwindow_as.factorid104     0.53      0.28     0.04     0.98 1.00     7088
#> pwindow_as.factorid105     0.46      0.28     0.02     0.97 1.00     5079
#> pwindow_as.factorid106     0.53      0.29     0.03     0.98 1.00     5373
#> pwindow_as.factorid107     0.48      0.29     0.02     0.97 1.00     6744
#> pwindow_as.factorid108     0.44      0.28     0.02     0.96 1.00     5452
#> pwindow_as.factorid109     0.51      0.29     0.03     0.98 1.00     4759
#> pwindow_as.factorid110     0.40      0.27     0.02     0.94 1.00     5361
#> pwindow_as.factorid111     0.55      0.28     0.04     0.98 1.00     5844
#> pwindow_as.factorid112     0.53      0.29     0.03     0.98 1.00     6477
#> pwindow_as.factorid113     0.50      0.29     0.02     0.98 1.00     4922
#> pwindow_as.factorid114     0.53      0.29     0.03     0.98 1.00     5169
#> pwindow_as.factorid115     0.51      0.29     0.03     0.98 1.00     5354
#> pwindow_as.factorid116     0.52      0.29     0.03     0.98 1.00     4960
#> pwindow_as.factorid117     0.48      0.28     0.03     0.97 1.00     7088
#> pwindow_as.factorid118     0.44      0.28     0.02     0.96 1.00     5222
#> pwindow_as.factorid119     0.56      0.28     0.04     0.98 1.00     5545
#> pwindow_as.factorid120     0.52      0.29     0.03     0.98 1.00     5220
#> pwindow_as.factorid121     0.54      0.29     0.03     0.98 1.00     6045
#> pwindow_as.factorid122     0.53      0.29     0.03     0.98 1.00     6067
#> pwindow_as.factorid123     0.56      0.29     0.03     0.98 1.00     4701
#> pwindow_as.factorid124     0.35      0.25     0.01     0.91 1.00     6912
#> pwindow_as.factorid125     0.54      0.28     0.03     0.98 1.00     5658
#> pwindow_as.factorid126     0.56      0.28     0.04     0.98 1.00     5907
#> pwindow_as.factorid127     0.41      0.27     0.02     0.94 1.00     6001
#> pwindow_as.factorid128     0.47      0.28     0.02     0.96 1.00     4810
#> pwindow_as.factorid129     0.46      0.29     0.02     0.97 1.00     6175
#> pwindow_as.factorid130     0.53      0.29     0.03     0.98 1.00     7005
#> pwindow_as.factorid131     0.47      0.29     0.02     0.97 1.00     5893
#> pwindow_as.factorid132     0.39      0.28     0.01     0.95 1.00     5507
#> pwindow_as.factorid133     0.48      0.28     0.03     0.97 1.00     6650
#> pwindow_as.factorid134     0.47      0.29     0.02     0.96 1.00     6246
#> pwindow_as.factorid135     0.44      0.28     0.02     0.96 1.00     5789
#> pwindow_as.factorid136     0.50      0.28     0.03     0.97 1.00     5109
#> pwindow_as.factorid137     0.54      0.28     0.03     0.98 1.01     5850
#> pwindow_as.factorid138     0.56      0.29     0.04     0.98 1.00     5968
#> pwindow_as.factorid139     0.56      0.29     0.03     0.98 1.00     6041
#> pwindow_as.factorid140     0.50      0.29     0.02     0.97 1.00     4801
#> pwindow_as.factorid141     0.52      0.29     0.03     0.98 1.00     6503
#> pwindow_as.factorid142     0.50      0.29     0.03     0.97 1.00     5302
#> pwindow_as.factorid143     0.49      0.29     0.02     0.98 1.00     6750
#> pwindow_as.factorid144     0.52      0.29     0.03     0.98 1.00     5325
#> pwindow_as.factorid145     0.53      0.29     0.03     0.98 1.00     6714
#> pwindow_as.factorid146     0.31      0.24     0.01     0.88 1.00     5238
#> pwindow_as.factorid147     0.40      0.27     0.02     0.93 1.00     5997
#> pwindow_as.factorid148     0.48      0.29     0.02     0.97 1.00     6231
#> pwindow_as.factorid149     0.51      0.28     0.03     0.97 1.00     5380
#> pwindow_as.factorid150     0.55      0.29     0.04     0.98 1.00     6946
#> pwindow_as.factorid151     0.49      0.28     0.03     0.97 1.00     6585
#> pwindow_as.factorid152     0.41      0.27     0.02     0.94 1.00     5877
#> pwindow_as.factorid153     0.57      0.29     0.04     0.98 1.00     5863
#> pwindow_as.factorid154     0.46      0.29     0.02     0.97 1.00     5989
#> pwindow_as.factorid155     0.56      0.28     0.04     0.98 1.00     5109
#> pwindow_as.factorid156     0.47      0.28     0.02     0.97 1.00     6203
#> pwindow_as.factorid157     0.47      0.28     0.03     0.97 1.00     5008
#> pwindow_as.factorid158     0.44      0.29     0.02     0.96 1.00     5973
#> pwindow_as.factorid159     0.53      0.29     0.03     0.98 1.00     5172
#> pwindow_as.factorid160     0.50      0.29     0.03     0.97 1.00     6573
#> pwindow_as.factorid161     0.57      0.29     0.04     0.98 1.00     6427
#> pwindow_as.factorid162     0.48      0.28     0.03     0.97 1.00     4289
#> pwindow_as.factorid163     0.50      0.29     0.03     0.97 1.00     5777
#> pwindow_as.factorid164     0.43      0.28     0.02     0.95 1.01     4866
#> pwindow_as.factorid165     0.52      0.28     0.03     0.98 1.00     6404
#> pwindow_as.factorid166     0.54      0.28     0.03     0.98 1.01     5905
#> pwindow_as.factorid167     0.55      0.28     0.04     0.98 1.00     5774
#> pwindow_as.factorid168     0.45      0.28     0.02     0.96 1.00     7590
#> pwindow_as.factorid169     0.55      0.29     0.04     0.98 1.00     5567
#> pwindow_as.factorid170     0.57      0.28     0.04     0.98 1.00     5140
#> pwindow_as.factorid171     0.44      0.29     0.01     0.97 1.00     5959
#> pwindow_as.factorid172     0.54      0.29     0.03     0.98 1.00     6502
#> pwindow_as.factorid173     0.49      0.30     0.02     0.97 1.00     5885
#> pwindow_as.factorid174     0.41      0.28     0.02     0.95 1.00     5427
#> pwindow_as.factorid175     0.54      0.28     0.04     0.98 1.00     6174
#> pwindow_as.factorid176     0.46      0.28     0.02     0.97 1.00     4972
#> pwindow_as.factorid177     0.56      0.28     0.04     0.98 1.00     5543
#> pwindow_as.factorid178     0.56      0.29     0.03     0.98 1.00     5647
#> pwindow_as.factorid179     0.52      0.29     0.02     0.98 1.00     5453
#> pwindow_as.factorid180     0.48      0.28     0.03     0.97 1.00     6187
#> pwindow_as.factorid181     0.42      0.28     0.02     0.96 1.00     6364
#> pwindow_as.factorid182     0.53      0.28     0.04     0.97 1.00     5251
#> pwindow_as.factorid183     0.48      0.29     0.02     0.97 1.00     6282
#> pwindow_as.factorid184     0.42      0.28     0.02     0.95 1.00     6473
#> pwindow_as.factorid185     0.52      0.29     0.02     0.98 1.00     6011
#> pwindow_as.factorid186     0.52      0.29     0.03     0.98 1.00     5383
#> pwindow_as.factorid187     0.44      0.28     0.02     0.96 1.00     5939
#> pwindow_as.factorid188     0.51      0.28     0.03     0.97 1.00     5838
#> pwindow_as.factorid189     0.56      0.28     0.04     0.98 1.00     5733
#> pwindow_as.factorid190     0.32      0.24     0.01     0.89 1.00     7247
#> pwindow_as.factorid191     0.57      0.28     0.04     0.98 1.00     5698
#> pwindow_as.factorid192     0.53      0.28     0.03     0.98 1.00     5420
#> pwindow_as.factorid193     0.52      0.29     0.03     0.98 1.00     5509
#> pwindow_as.factorid194     0.52      0.29     0.03     0.97 1.00     6230
#> pwindow_as.factorid195     0.57      0.28     0.04     0.98 1.00     5607
#> pwindow_as.factorid196     0.55      0.28     0.04     0.98 1.00     5714
#> pwindow_as.factorid197     0.47      0.29     0.03     0.96 1.00     6914
#> pwindow_as.factorid198     0.43      0.28     0.01     0.96 1.00     5783
#> pwindow_as.factorid199     0.49      0.30     0.02     0.97 1.00     5758
#> pwindow_as.factorid200     0.54      0.29     0.03     0.98 1.00     7064
#> pwindow_as.factorid201     0.55      0.28     0.03     0.98 1.00     5105
#> pwindow_as.factorid202     0.54      0.28     0.04     0.98 1.00     5512
#> pwindow_as.factorid203     0.54      0.29     0.03     0.98 1.00     6565
#> pwindow_as.factorid204     0.45      0.28     0.02     0.97 1.00     4739
#> pwindow_as.factorid205     0.53      0.29     0.03     0.98 1.00     6008
#> pwindow_as.factorid206     0.52      0.28     0.03     0.97 1.00     4622
#> pwindow_as.factorid207     0.51      0.29     0.03     0.97 1.00     5619
#> pwindow_as.factorid208     0.48      0.29     0.02     0.97 1.00     5500
#> pwindow_as.factorid209     0.47      0.28     0.02     0.97 1.00     5746
#> pwindow_as.factorid210     0.54      0.29     0.03     0.98 1.00     5555
#> pwindow_as.factorid211     0.50      0.28     0.03     0.97 1.00     6040
#> pwindow_as.factorid212     0.54      0.29     0.03     0.98 1.00     6398
#> pwindow_as.factorid213     0.53      0.28     0.03     0.98 1.00     6989
#> pwindow_as.factorid214     0.56      0.29     0.04     0.98 1.00     5827
#> pwindow_as.factorid215     0.52      0.29     0.03     0.97 1.00     4690
#> pwindow_as.factorid216     0.54      0.28     0.03     0.98 1.00     4747
#> pwindow_as.factorid217     0.25      0.21     0.01     0.78 1.00     5744
#> pwindow_as.factorid218     0.48      0.29     0.03     0.98 1.00     5648
#> pwindow_as.factorid219     0.53      0.29     0.04     0.98 1.00     5626
#> pwindow_as.factorid220     0.53      0.29     0.03     0.98 1.00     5372
#> pwindow_as.factorid221     0.48      0.28     0.03     0.96 1.00     6881
#> pwindow_as.factorid222     0.44      0.28     0.02     0.97 1.00     6767
#> pwindow_as.factorid223     0.55      0.29     0.03     0.98 1.01     5566
#> pwindow_as.factorid224     0.51      0.29     0.03     0.98 1.00     5531
#> pwindow_as.factorid225     0.49      0.29     0.02     0.97 1.00     6485
#> pwindow_as.factorid226     0.47      0.29     0.02     0.97 1.00     6290
#> pwindow_as.factorid227     0.46      0.28     0.02     0.97 1.00     6068
#> pwindow_as.factorid228     0.56      0.29     0.03     0.98 1.00     5774
#> pwindow_as.factorid229     0.51      0.30     0.03     0.98 1.00     5423
#> pwindow_as.factorid230     0.49      0.28     0.03     0.97 1.00     6482
#> pwindow_as.factorid231     0.48      0.28     0.03     0.97 1.00     5897
#> pwindow_as.factorid232     0.51      0.29     0.03     0.97 1.00     5739
#> pwindow_as.factorid233     0.49      0.28     0.03     0.97 1.00     5839
#> pwindow_as.factorid234     0.55      0.29     0.03     0.98 1.00     6437
#> pwindow_as.factorid235     0.47      0.28     0.02     0.96 1.00     5412
#> pwindow_as.factorid236     0.50      0.29     0.03     0.97 1.00     6358
#> pwindow_as.factorid237     0.55      0.28     0.04     0.98 1.00     4833
#> pwindow_as.factorid238     0.52      0.29     0.03     0.98 1.00     5326
#> pwindow_as.factorid239     0.56      0.29     0.04     0.98 1.00     5744
#> pwindow_as.factorid240     0.49      0.29     0.02     0.97 1.01     4669
#> pwindow_as.factorid241     0.53      0.28     0.03     0.98 1.00     6230
#> pwindow_as.factorid242     0.44      0.28     0.02     0.96 1.00     4929
#> pwindow_as.factorid243     0.55      0.28     0.04     0.98 1.00     5675
#> pwindow_as.factorid244     0.51      0.29     0.02     0.98 1.00     5371
#> pwindow_as.factorid245     0.57      0.28     0.03     0.99 1.00     5685
#> pwindow_as.factorid246     0.56      0.29     0.04     0.98 1.00     5436
#> pwindow_as.factorid247     0.48      0.28     0.02     0.97 1.00     5876
#> pwindow_as.factorid248     0.48      0.29     0.02     0.98 1.00     6115
#> pwindow_as.factorid249     0.55      0.28     0.04     0.98 1.00     5678
#> pwindow_as.factorid250     0.54      0.29     0.03     0.98 1.00     6630
#> pwindow_as.factorid251     0.50      0.29     0.03     0.97 1.00     6440
#> pwindow_as.factorid252     0.42      0.28     0.02     0.95 1.00     4697
#> pwindow_as.factorid253     0.45      0.28     0.02     0.96 1.00     5195
#> pwindow_as.factorid254     0.48      0.28     0.03     0.96 1.00     6947
#> pwindow_as.factorid255     0.53      0.29     0.03     0.98 1.00     5993
#> pwindow_as.factorid256     0.48      0.29     0.02     0.97 1.00     4750
#> pwindow_as.factorid257     0.46      0.28     0.02     0.96 1.00     6750
#> pwindow_as.factorid258     0.51      0.29     0.02     0.98 1.00     6753
#> pwindow_as.factorid259     0.51      0.28     0.03     0.97 1.00     6030
#> pwindow_as.factorid260     0.46      0.29     0.02     0.96 1.00     6291
#> pwindow_as.factorid261     0.50      0.29     0.03     0.98 1.01     5136
#> pwindow_as.factorid262     0.56      0.29     0.04     0.98 1.00     6352
#> pwindow_as.factorid263     0.54      0.28     0.04     0.98 1.00     6614
#> pwindow_as.factorid264     0.49      0.29     0.02     0.97 1.00     4645
#> pwindow_as.factorid265     0.51      0.29     0.04     0.98 1.00     5843
#> pwindow_as.factorid266     0.51      0.29     0.03     0.98 1.00     5618
#> pwindow_as.factorid267     0.54      0.29     0.03     0.98 1.00     7178
#> pwindow_as.factorid268     0.40      0.27     0.02     0.94 1.00     5438
#> pwindow_as.factorid269     0.50      0.29     0.03     0.97 1.00     5759
#> pwindow_as.factorid270     0.45      0.28     0.02     0.96 1.00     5637
#> pwindow_as.factorid271     0.47      0.28     0.02     0.96 1.00     5560
#> pwindow_as.factorid272     0.55      0.28     0.04     0.98 1.00     6955
#> pwindow_as.factorid273     0.52      0.29     0.03     0.97 1.00     6699
#> pwindow_as.factorid274     0.49      0.28     0.03     0.97 1.00     5580
#> pwindow_as.factorid275     0.54      0.28     0.04     0.97 1.00     6714
#> pwindow_as.factorid276     0.51      0.28     0.03     0.98 1.00     4316
#> pwindow_as.factorid277     0.50      0.28     0.03     0.97 1.00     5973
#> pwindow_as.factorid278     0.56      0.28     0.04     0.98 1.00     6070
#> pwindow_as.factorid279     0.39      0.27     0.02     0.94 1.00     5336
#> pwindow_as.factorid280     0.44      0.28     0.02     0.96 1.00     5002
#> pwindow_as.factorid281     0.51      0.29     0.03     0.97 1.00     5452
#> pwindow_as.factorid282     0.51      0.29     0.02     0.98 1.00     5447
#> pwindow_as.factorid283     0.54      0.29     0.03     0.98 1.00     5794
#> pwindow_as.factorid284     0.52      0.29     0.04     0.98 1.00     6499
#> pwindow_as.factorid285     0.51      0.29     0.02     0.98 1.00     5455
#> pwindow_as.factorid286     0.55      0.29     0.03     0.98 1.00     5266
#> pwindow_as.factorid287     0.48      0.29     0.02     0.97 1.00     6829
#> pwindow_as.factorid288     0.49      0.28     0.03     0.97 1.00     4749
#> pwindow_as.factorid289     0.52      0.29     0.03     0.97 1.00     5311
#> pwindow_as.factorid290     0.51      0.29     0.02     0.98 1.00     4941
#> pwindow_as.factorid291     0.48      0.29     0.03     0.97 1.00     5478
#> pwindow_as.factorid292     0.50      0.29     0.03     0.97 1.00     6534
#> pwindow_as.factorid293     0.53      0.29     0.03     0.97 1.00     4914
#> pwindow_as.factorid294     0.45      0.27     0.02     0.96 1.00     6242
#> pwindow_as.factorid295     0.53      0.28     0.03     0.98 1.00     6225
#> pwindow_as.factorid296     0.50      0.29     0.03     0.97 1.00     5038
#> pwindow_as.factorid297     0.50      0.28     0.03     0.97 1.00     5135
#> pwindow_as.factorid298     0.43      0.28     0.02     0.96 1.00     6313
#> pwindow_as.factorid299     0.51      0.29     0.03     0.97 1.00     7343
#> pwindow_as.factorid300     0.57      0.28     0.05     0.98 1.00     5954
#> pwindow_as.factorid301     0.46      0.28     0.03     0.97 1.00     5005
#> pwindow_as.factorid302     0.49      0.29     0.03     0.97 1.00     5541
#> pwindow_as.factorid303     0.57      0.29     0.03     0.98 1.00     6423
#> pwindow_as.factorid304     0.51      0.29     0.03     0.97 1.00     5138
#> pwindow_as.factorid305     0.50      0.28     0.03     0.97 1.00     5447
#> pwindow_as.factorid306     0.52      0.28     0.03     0.97 1.00     6134
#> pwindow_as.factorid307     0.44      0.28     0.02     0.97 1.00     5562
#> pwindow_as.factorid308     0.48      0.29     0.02     0.97 1.00     5792
#> pwindow_as.factorid309     0.42      0.27     0.02     0.95 1.00     5744
#> pwindow_as.factorid310     0.54      0.29     0.03     0.98 1.00     5553
#> pwindow_as.factorid311     0.47      0.29     0.02     0.97 1.01     3747
#> pwindow_as.factorid312     0.55      0.29     0.03     0.98 1.00     4550
#> pwindow_as.factorid313     0.35      0.26     0.01     0.92 1.00     4827
#> pwindow_as.factorid314     0.54      0.29     0.03     0.98 1.00     5073
#> pwindow_as.factorid315     0.51      0.28     0.03     0.97 1.00     5482
#> pwindow_as.factorid316     0.53      0.30     0.03     0.98 1.00     6453
#> pwindow_as.factorid317     0.40      0.28     0.01     0.95 1.00     5810
#> swindow_as.factorid1       0.46      0.28     0.02     0.97 1.00     5278
#> swindow_as.factorid2       0.49      0.29     0.02     0.97 1.00     5349
#> swindow_as.factorid3       0.48      0.29     0.02     0.98 1.00     5019
#> swindow_as.factorid4       0.48      0.29     0.02     0.97 1.00     6032
#> swindow_as.factorid5       0.54      0.28     0.03     0.98 1.00     6167
#> swindow_as.factorid6       0.49      0.29     0.02     0.97 1.00     6151
#> swindow_as.factorid7       0.62      0.26     0.06     0.99 1.00     5126
#> swindow_as.factorid8       0.50      0.28     0.03     0.97 1.00     4542
#> swindow_as.factorid9       0.47      0.29     0.03     0.97 1.00     5294
#> swindow_as.factorid10      0.51      0.29     0.03     0.98 1.00     5591
#> swindow_as.factorid11      0.45      0.28     0.02     0.97 1.00     6550
#> swindow_as.factorid12      0.60      0.27     0.06     0.98 1.00     6705
#> swindow_as.factorid13      0.44      0.28     0.02     0.96 1.00     5289
#> swindow_as.factorid14      0.45      0.28     0.02     0.97 1.00     5645
#> swindow_as.factorid15      0.55      0.28     0.04     0.98 1.00     6384
#> swindow_as.factorid16      0.45      0.29     0.02     0.97 1.00     4631
#> swindow_as.factorid17      0.47      0.29     0.02     0.97 1.00     6391
#> swindow_as.factorid18      0.48      0.29     0.02     0.97 1.00     5705
#> swindow_as.factorid19      0.46      0.29     0.02     0.97 1.00     6585
#> swindow_as.factorid20      0.50      0.29     0.02     0.98 1.00     6238
#> swindow_as.factorid21      0.44      0.28     0.02     0.96 1.00     7110
#> swindow_as.factorid22      0.46      0.29     0.02     0.97 1.00     5533
#> swindow_as.factorid23      0.44      0.29     0.02     0.96 1.00     6652
#> swindow_as.factorid24      0.49      0.29     0.02     0.98 1.00     6171
#> swindow_as.factorid25      0.48      0.28     0.03     0.97 1.00     5274
#> swindow_as.factorid26      0.51      0.29     0.03     0.97 1.00     6264
#> swindow_as.factorid27      0.46      0.29     0.02     0.97 1.00     5721
#> swindow_as.factorid28      0.46      0.28     0.02     0.96 1.00     6649
#> swindow_as.factorid29      0.57      0.28     0.04     0.98 1.00     4690
#> swindow_as.factorid30      0.46      0.29     0.02     0.98 1.00     5551
#> swindow_as.factorid31      0.49      0.28     0.03     0.97 1.01     5367
#> swindow_as.factorid32      0.58      0.27     0.05     0.98 1.00     6519
#> swindow_as.factorid33      0.48      0.29     0.03     0.97 1.00     7030
#> swindow_as.factorid34      0.59      0.28     0.04     0.99 1.00     4973
#> swindow_as.factorid35      0.45      0.28     0.02     0.97 1.00     5622
#> swindow_as.factorid36      0.45      0.29     0.02     0.97 1.00     7043
#> swindow_as.factorid37      0.65      0.26     0.08     0.99 1.00     4834
#> swindow_as.factorid38      0.50      0.29     0.02     0.97 1.00     6138
#> swindow_as.factorid39      0.44      0.29     0.02     0.96 1.00     4689
#> swindow_as.factorid40      0.49      0.29     0.03     0.97 1.00     5338
#> swindow_as.factorid41      0.44      0.28     0.02     0.96 1.00     5684
#> swindow_as.factorid42      0.48      0.30     0.02     0.97 1.00     5937
#> swindow_as.factorid43      0.57      0.29     0.04     0.98 1.00     5800
#> swindow_as.factorid44      0.49      0.29     0.02     0.97 1.00     5656
#> swindow_as.factorid45      0.46      0.29     0.02     0.97 1.00     6418
#> swindow_as.factorid46      0.45      0.28     0.02     0.96 1.00     6975
#> swindow_as.factorid47      0.47      0.28     0.02     0.97 1.00     6142
#> swindow_as.factorid48      0.53      0.28     0.03     0.98 1.00     5513
#> swindow_as.factorid49      0.52      0.29     0.03     0.98 1.01     5585
#> swindow_as.factorid50      0.48      0.29     0.02     0.98 1.00     6143
#> swindow_as.factorid51      0.68      0.25     0.10     0.99 1.00     5100
#> swindow_as.factorid52      0.50      0.29     0.02     0.98 1.00     7412
#> swindow_as.factorid53      0.46      0.29     0.02     0.97 1.00     5311
#> swindow_as.factorid54      0.53      0.28     0.03     0.98 1.00     5938
#> swindow_as.factorid55      0.44      0.28     0.02     0.96 1.00     6360
#> swindow_as.factorid56      0.45      0.28     0.02     0.96 1.00     4617
#> swindow_as.factorid57      0.53      0.28     0.03     0.98 1.00     5633
#> swindow_as.factorid58      0.48      0.28     0.02     0.97 1.00     5912
#> swindow_as.factorid59      0.53      0.29     0.03     0.98 1.00     5142
#> swindow_as.factorid60      0.52      0.29     0.03     0.97 1.00     6039
#> swindow_as.factorid61      0.49      0.29     0.02     0.97 1.00     5452
#> swindow_as.factorid62      0.45      0.29     0.02     0.96 1.00     6245
#> swindow_as.factorid63      0.58      0.28     0.04     0.98 1.00     5348
#> swindow_as.factorid64      0.52      0.29     0.02     0.98 1.00     5103
#> swindow_as.factorid65      0.52      0.29     0.03     0.98 1.00     5600
#> swindow_as.factorid66      0.48      0.29     0.02     0.97 1.00     6067
#> swindow_as.factorid67      0.47      0.29     0.02     0.97 1.00     7140
#> swindow_as.factorid68      0.46      0.28     0.02     0.97 1.00     5700
#> swindow_as.factorid69      0.54      0.28     0.04     0.98 1.00     6368
#> swindow_as.factorid70      0.55      0.28     0.04     0.97 1.00     6033
#> swindow_as.factorid71      0.48      0.29     0.03     0.97 1.00     5212
#> swindow_as.factorid72      0.46      0.29     0.02     0.98 1.00     7182
#> swindow_as.factorid73      0.62      0.27     0.05     0.99 1.00     4899
#> swindow_as.factorid74      0.46      0.29     0.02     0.97 1.00     5224
#> swindow_as.factorid75      0.51      0.28     0.03     0.97 1.00     6882
#> swindow_as.factorid76      0.45      0.29     0.02     0.97 1.00     6359
#> swindow_as.factorid77      0.45      0.28     0.02     0.97 1.00     5664
#> swindow_as.factorid78      0.44      0.29     0.02     0.97 1.00     5509
#> swindow_as.factorid79      0.52      0.29     0.03     0.97 1.00     7095
#> swindow_as.factorid80      0.44      0.28     0.02     0.97 1.00     5774
#> swindow_as.factorid81      0.50      0.29     0.03     0.97 1.00     6039
#> swindow_as.factorid82      0.49      0.29     0.03     0.98 1.00     6580
#> swindow_as.factorid83      0.45      0.29     0.02     0.97 1.00     6662
#> swindow_as.factorid84      0.44      0.29     0.02     0.96 1.00     6409
#> swindow_as.factorid85      0.58      0.28     0.04     0.98 1.00     6045
#> swindow_as.factorid86      0.46      0.28     0.02     0.97 1.00     6221
#> swindow_as.factorid87      0.46      0.28     0.02     0.97 1.00     6858
#> swindow_as.factorid88      0.45      0.29     0.02     0.97 1.00     7714
#> swindow_as.factorid89      0.44      0.29     0.02     0.97 1.00     5421
#> swindow_as.factorid90      0.45      0.29     0.02     0.97 1.00     5685
#> swindow_as.factorid91      0.52      0.28     0.03     0.98 1.00     5981
#> swindow_as.factorid92      0.43      0.29     0.02     0.97 1.00     6355
#> swindow_as.factorid93      0.46      0.29     0.02     0.97 1.00     6120
#> swindow_as.factorid94      0.51      0.29     0.02     0.98 1.00     5469
#> swindow_as.factorid95      0.46      0.29     0.02     0.97 1.00     5631
#> swindow_as.factorid96      0.51      0.29     0.03     0.97 1.00     5873
#> swindow_as.factorid97      0.49      0.29     0.02     0.98 1.00     5708
#> swindow_as.factorid98      0.46      0.28     0.02     0.96 1.00     6225
#> swindow_as.factorid99      0.54      0.28     0.04     0.98 1.00     5189
#> swindow_as.factorid100     0.69      0.24     0.11     0.99 1.00     5428
#> swindow_as.factorid101     0.52      0.29     0.02     0.98 1.00     5980
#> swindow_as.factorid102     0.46      0.28     0.02     0.97 1.00     5700
#> swindow_as.factorid103     0.52      0.29     0.03     0.98 1.00     5343
#> swindow_as.factorid104     0.47      0.29     0.02     0.97 1.00     5840
#> swindow_as.factorid105     0.53      0.28     0.03     0.98 1.00     6004
#> swindow_as.factorid106     0.48      0.29     0.02     0.97 1.00     5646
#> swindow_as.factorid107     0.52      0.28     0.03     0.98 1.00     5419
#> swindow_as.factorid108     0.57      0.27     0.04     0.98 1.00     6003
#> swindow_as.factorid109     0.50      0.29     0.03     0.98 1.00     6436
#> swindow_as.factorid110     0.60      0.28     0.05     0.99 1.00     4326
#> swindow_as.factorid111     0.44      0.28     0.02     0.96 1.00     5876
#> swindow_as.factorid112     0.47      0.29     0.02     0.97 1.00     5844
#> swindow_as.factorid113     0.50      0.28     0.03     0.97 1.00     7283
#> swindow_as.factorid114     0.47      0.28     0.02     0.96 1.00     5562
#> swindow_as.factorid115     0.49      0.29     0.02     0.97 1.00     5793
#> swindow_as.factorid116     0.48      0.29     0.02     0.97 1.00     7023
#> swindow_as.factorid117     0.52      0.28     0.03     0.98 1.00     4385
#> swindow_as.factorid118     0.55      0.28     0.04     0.98 1.00     4785
#> swindow_as.factorid119     0.44      0.29     0.02     0.96 1.00     5394
#> swindow_as.factorid120     0.47      0.29     0.02     0.97 1.01     4856
#> swindow_as.factorid121     0.46      0.29     0.02     0.97 1.00     6005
#> swindow_as.factorid122     0.48      0.29     0.03     0.97 1.00     6566
#> swindow_as.factorid123     0.45      0.28     0.02     0.96 1.00     6448
#> swindow_as.factorid124     0.65      0.25     0.10     0.99 1.00     6850
#> swindow_as.factorid125     0.46      0.29     0.02     0.97 1.00     5605
#> swindow_as.factorid126     0.45      0.29     0.02     0.97 1.00     5984
#> swindow_as.factorid127     0.60      0.28     0.05     0.99 1.00     5938
#> swindow_as.factorid128     0.53      0.29     0.03     0.98 1.00     5129
#> swindow_as.factorid129     0.55      0.28     0.04     0.98 1.00     5446
#> swindow_as.factorid130     0.48      0.29     0.02     0.97 1.00     5057
#> swindow_as.factorid131     0.54      0.29     0.03     0.98 1.00     6118
#> swindow_as.factorid132     0.61      0.27     0.06     0.98 1.00     6174
#> swindow_as.factorid133     0.51      0.28     0.03     0.98 1.00     5571
#> swindow_as.factorid134     0.53      0.29     0.03     0.98 1.00     6237
#> swindow_as.factorid135     0.56      0.28     0.05     0.98 1.00     5174
#> swindow_as.factorid136     0.50      0.28     0.03     0.98 1.00     5715
#> swindow_as.factorid137     0.46      0.28     0.02     0.96 1.00     6618
#> swindow_as.factorid138     0.45      0.29     0.02     0.97 1.00     4996
#> swindow_as.factorid139     0.43      0.28     0.02     0.97 1.00     5924
#> swindow_as.factorid140     0.50      0.29     0.02     0.98 1.00     5952
#> swindow_as.factorid141     0.48      0.29     0.02     0.97 1.00     6167
#> swindow_as.factorid142     0.50      0.28     0.02     0.98 1.00     5806
#> swindow_as.factorid143     0.51      0.29     0.03     0.98 1.00     6508
#> swindow_as.factorid144     0.49      0.28     0.03     0.97 1.00     5669
#> swindow_as.factorid145     0.46      0.29     0.02     0.97 1.00     6964
#> swindow_as.factorid146     0.69      0.24     0.10     0.99 1.00     5004
#> swindow_as.factorid147     0.60      0.27     0.05     0.98 1.00     6159
#> swindow_as.factorid148     0.52      0.29     0.03     0.98 1.00     5339
#> swindow_as.factorid149     0.49      0.29     0.02     0.97 1.00     5581
#> swindow_as.factorid150     0.45      0.29     0.02     0.97 1.00     5125
#> swindow_as.factorid151     0.50      0.29     0.02     0.98 1.00     5053
#> swindow_as.factorid152     0.58      0.27     0.05     0.98 1.00     5233
#> swindow_as.factorid153     0.44      0.28     0.02     0.96 1.00     5058
#> swindow_as.factorid154     0.55      0.28     0.03     0.98 1.00     5037
#> swindow_as.factorid155     0.45      0.29     0.02     0.97 1.00     5825
#> swindow_as.factorid156     0.52      0.29     0.03     0.97 1.00     5524
#> swindow_as.factorid157     0.54      0.28     0.03     0.98 1.00     7365
#> swindow_as.factorid158     0.57      0.28     0.04     0.98 1.00     4338
#> swindow_as.factorid159     0.47      0.29     0.02     0.97 1.00     6002
#> swindow_as.factorid160     0.50      0.28     0.03     0.98 1.00     5376
#> swindow_as.factorid161     0.44      0.28     0.02     0.96 1.00     7464
#> swindow_as.factorid162     0.52      0.29     0.03     0.98 1.00     6296
#> swindow_as.factorid163     0.50      0.29     0.02     0.98 1.00     6275
#> swindow_as.factorid164     0.58      0.28     0.05     0.98 1.00     6603
#> swindow_as.factorid165     0.49      0.29     0.02     0.98 1.00     4905
#> swindow_as.factorid166     0.45      0.29     0.02     0.97 1.00     5290
#> swindow_as.factorid167     0.44      0.28     0.02     0.96 1.00     5736
#> swindow_as.factorid168     0.55      0.28     0.04     0.98 1.00     5260
#> swindow_as.factorid169     0.45      0.28     0.02     0.96 1.00     5641
#> swindow_as.factorid170     0.44      0.29     0.02     0.97 1.00     5681
#> swindow_as.factorid171     0.55      0.28     0.03     0.98 1.00     5438
#> swindow_as.factorid172     0.47      0.29     0.02     0.97 1.00     5247
#> swindow_as.factorid173     0.52      0.28     0.03     0.97 1.00     7103
#> swindow_as.factorid174     0.59      0.28     0.05     0.98 1.00     7580
#> swindow_as.factorid175     0.45      0.28     0.02     0.96 1.00     5997
#> swindow_as.factorid176     0.53      0.29     0.03     0.98 1.00     6033
#> swindow_as.factorid177     0.44      0.28     0.02     0.96 1.00     4708
#> swindow_as.factorid178     0.43      0.28     0.02     0.96 1.00     6127
#> swindow_as.factorid179     0.48      0.29     0.02     0.97 1.00     7797
#> swindow_as.factorid180     0.53      0.28     0.03     0.98 1.00     5977
#> swindow_as.factorid181     0.58      0.28     0.04     0.99 1.01     6296
#> swindow_as.factorid182     0.47      0.29     0.02     0.97 1.00     6366
#> swindow_as.factorid183     0.52      0.29     0.03     0.98 1.00     6256
#> swindow_as.factorid184     0.57      0.28     0.04     0.98 1.00     5059
#> swindow_as.factorid185     0.48      0.29     0.02     0.97 1.00     5657
#> swindow_as.factorid186     0.47      0.29     0.02     0.97 1.00     5662
#> swindow_as.factorid187     0.56      0.28     0.04     0.98 1.00     6015
#> swindow_as.factorid188     0.49      0.29     0.03     0.97 1.00     6144
#> swindow_as.factorid189     0.45      0.29     0.02     0.97 1.00     5127
#> swindow_as.factorid190     0.67      0.25     0.09     0.99 1.00     3870
#> swindow_as.factorid191     0.43      0.28     0.02     0.96 1.00     5997
#> swindow_as.factorid192     0.47      0.29     0.02     0.97 1.00     5337
#> swindow_as.factorid193     0.48      0.29     0.02     0.97 1.00     6310
#> swindow_as.factorid194     0.48      0.29     0.02     0.97 1.00     6014
#> swindow_as.factorid195     0.44      0.28     0.02     0.96 1.00     5909
#> swindow_as.factorid196     0.46      0.29     0.03     0.97 1.00     6306
#> swindow_as.factorid197     0.53      0.29     0.03     0.98 1.00     5494
#> swindow_as.factorid198     0.57      0.28     0.04     0.99 1.00     4805
#> swindow_as.factorid199     0.51      0.28     0.02     0.98 1.00     6825
#> swindow_as.factorid200     0.45      0.28     0.02     0.96 1.00     6787
#> swindow_as.factorid201     0.45      0.29     0.02     0.97 1.00     5346
#> swindow_as.factorid202     0.47      0.29     0.02     0.97 1.00     6546
#> swindow_as.factorid203     0.46      0.29     0.02     0.97 1.00     6238
#> swindow_as.factorid204     0.55      0.29     0.04     0.98 1.00     4814
#> swindow_as.factorid205     0.48      0.29     0.02     0.97 1.00     7443
#> swindow_as.factorid206     0.48      0.28     0.03     0.96 1.00     5273
#> swindow_as.factorid207     0.49      0.28     0.03     0.97 1.00     5368
#> swindow_as.factorid208     0.52      0.28     0.04     0.97 1.00     6189
#> swindow_as.factorid209     0.54      0.28     0.03     0.98 1.00     5218
#> swindow_as.factorid210     0.46      0.28     0.02     0.96 1.00     5454
#> swindow_as.factorid211     0.50      0.29     0.03     0.97 1.00     4986
#> swindow_as.factorid212     0.46      0.28     0.03     0.96 1.00     6023
#> swindow_as.factorid213     0.48      0.28     0.02     0.97 1.00     6054
#> swindow_as.factorid214     0.44      0.28     0.02     0.96 1.00     5278
#> swindow_as.factorid215     0.48      0.29     0.02     0.98 1.00     5477
#> swindow_as.factorid216     0.46      0.28     0.02     0.97 1.00     4504
#> swindow_as.factorid217     0.75      0.21     0.21     0.99 1.00     5762
#> swindow_as.factorid218     0.53      0.28     0.04     0.97 1.00     6903
#> swindow_as.factorid219     0.47      0.29     0.02     0.97 1.00     6555
#> swindow_as.factorid220     0.47      0.29     0.02     0.97 1.00     5628
#> swindow_as.factorid221     0.52      0.29     0.02     0.98 1.00     5614
#> swindow_as.factorid222     0.57      0.28     0.05     0.98 1.00     4604
#> swindow_as.factorid223     0.46      0.29     0.02     0.97 1.00     5048
#> swindow_as.factorid224     0.49      0.28     0.03     0.98 1.00     5341
#> swindow_as.factorid225     0.50      0.28     0.03     0.97 1.00     5087
#> swindow_as.factorid226     0.54      0.28     0.03     0.98 1.00     4785
#> swindow_as.factorid227     0.54      0.28     0.04     0.98 1.00     5507
#> swindow_as.factorid228     0.44      0.29     0.02     0.96 1.00     5304
#> swindow_as.factorid229     0.49      0.29     0.02     0.98 1.00     5984
#> swindow_as.factorid230     0.53      0.29     0.03     0.98 1.00     5182
#> swindow_as.factorid231     0.52      0.29     0.03     0.98 1.00     4947
#> swindow_as.factorid232     0.50      0.29     0.02     0.97 1.00     5491
#> swindow_as.factorid233     0.50      0.29     0.03     0.97 1.00     5165
#> swindow_as.factorid234     0.46      0.28     0.02     0.96 1.00     5190
#> swindow_as.factorid235     0.52      0.29     0.03     0.98 1.00     5872
#> swindow_as.factorid236     0.50      0.28     0.03     0.98 1.00     5594
#> swindow_as.factorid237     0.46      0.28     0.02     0.96 1.00     6757
#> swindow_as.factorid238     0.48      0.28     0.03     0.97 1.00     6460
#> swindow_as.factorid239     0.44      0.29     0.02     0.96 1.00     5242
#> swindow_as.factorid240     0.51      0.28     0.03     0.98 1.00     5934
#> swindow_as.factorid241     0.47      0.29     0.02     0.97 1.00     5302
#> swindow_as.factorid242     0.56      0.28     0.05     0.97 1.00     5953
#> swindow_as.factorid243     0.45      0.29     0.02     0.97 1.00     6110
#> swindow_as.factorid244     0.49      0.29     0.02     0.98 1.00     4751
#> swindow_as.factorid245     0.43      0.28     0.02     0.96 1.00     5726
#> swindow_as.factorid246     0.45      0.29     0.02     0.97 1.00     5653
#> swindow_as.factorid247     0.51      0.29     0.03     0.98 1.00     6715
#> swindow_as.factorid248     0.50      0.29     0.02     0.97 1.00     5685
#> swindow_as.factorid249     0.45      0.29     0.02     0.97 1.00     6256
#> swindow_as.factorid250     0.46      0.29     0.02     0.97 1.00     5491
#> swindow_as.factorid251     0.51      0.28     0.03     0.98 1.00     6245
#> swindow_as.factorid252     0.58      0.28     0.04     0.98 1.00     4924
#> swindow_as.factorid253     0.55      0.28     0.04     0.98 1.00     7418
#> swindow_as.factorid254     0.52      0.28     0.03     0.97 1.00     6213
#> swindow_as.factorid255     0.47      0.29     0.02     0.97 1.00     5927
#> swindow_as.factorid256     0.51      0.28     0.03     0.97 1.00     6072
#> swindow_as.factorid257     0.55      0.28     0.04     0.98 1.00     8721
#> swindow_as.factorid258     0.48      0.29     0.03     0.97 1.00     7138
#> swindow_as.factorid259     0.49      0.29     0.02     0.98 1.00     4563
#> swindow_as.factorid260     0.53      0.29     0.03     0.98 1.00     5165
#> swindow_as.factorid261     0.50      0.29     0.03     0.97 1.00     6342
#> swindow_as.factorid262     0.43      0.28     0.02     0.95 1.00     5784
#> swindow_as.factorid263     0.46      0.29     0.02     0.97 1.00     5996
#> swindow_as.factorid264     0.50      0.29     0.03     0.97 1.01     5180
#> swindow_as.factorid265     0.48      0.29     0.03     0.97 1.00     6743
#> swindow_as.factorid266     0.50      0.28     0.03     0.97 1.00     5955
#> swindow_as.factorid267     0.46      0.28     0.02     0.97 1.00     5204
#> swindow_as.factorid268     0.60      0.28     0.05     0.99 1.00     6930
#> swindow_as.factorid269     0.51      0.28     0.03     0.97 1.00     5282
#> swindow_as.factorid270     0.54      0.28     0.03     0.98 1.00     6154
#> swindow_as.factorid271     0.54      0.29     0.03     0.98 1.00     5892
#> swindow_as.factorid272     0.45      0.28     0.02     0.96 1.00     4616
#> swindow_as.factorid273     0.49      0.29     0.02     0.97 1.00     6934
#> swindow_as.factorid274     0.51      0.28     0.03     0.97 1.00     4582
#> swindow_as.factorid275     0.46      0.28     0.02     0.96 1.00     5499
#> swindow_as.factorid276     0.50      0.28     0.03     0.97 1.00     5315
#> swindow_as.factorid277     0.51      0.28     0.03     0.97 1.00     5976
#> swindow_as.factorid278     0.44      0.28     0.02     0.95 1.00     6737
#> swindow_as.factorid279     0.61      0.27     0.07     0.99 1.00     4968
#> swindow_as.factorid280     0.56      0.28     0.04     0.99 1.00     5882
#> swindow_as.factorid281     0.51      0.29     0.03     0.98 1.00     6123
#> swindow_as.factorid282     0.50      0.29     0.02     0.97 1.00     5941
#> swindow_as.factorid283     0.45      0.29     0.01     0.97 1.00     4778
#> swindow_as.factorid284     0.48      0.28     0.02     0.96 1.00     5528
#> swindow_as.factorid285     0.49      0.28     0.03     0.98 1.00     6260
#> swindow_as.factorid286     0.45      0.28     0.02     0.96 1.00     7131
#> swindow_as.factorid287     0.52      0.29     0.03     0.97 1.00     5329
#> swindow_as.factorid288     0.52      0.28     0.03     0.97 1.00     5448
#> swindow_as.factorid289     0.47      0.29     0.02     0.97 1.00     5858
#> swindow_as.factorid290     0.48      0.28     0.02     0.97 1.01     6350
#> swindow_as.factorid291     0.51      0.29     0.02     0.98 1.00     5557
#> swindow_as.factorid292     0.49      0.29     0.03     0.97 1.00     5351
#> swindow_as.factorid293     0.47      0.30     0.02     0.97 1.00     4982
#> swindow_as.factorid294     0.54      0.29     0.03     0.98 1.00     5291
#> swindow_as.factorid295     0.47      0.29     0.02     0.97 1.00     6152
#> swindow_as.factorid296     0.50      0.28     0.03     0.97 1.00     5967
#> swindow_as.factorid297     0.51      0.28     0.02     0.98 1.00     6222
#> swindow_as.factorid298     0.57      0.28     0.04     0.99 1.00     5355
#> swindow_as.factorid299     0.50      0.28     0.03     0.97 1.00     5635
#> swindow_as.factorid300     0.44      0.29     0.02     0.97 1.00     5948
#> swindow_as.factorid301     0.55      0.28     0.04     0.98 1.00     6025
#> swindow_as.factorid302     0.51      0.28     0.03     0.97 1.00     5634
#> swindow_as.factorid303     0.43      0.28     0.02     0.96 1.00     5908
#> swindow_as.factorid304     0.50      0.29     0.02     0.97 1.00     6331
#> swindow_as.factorid305     0.51      0.29     0.02     0.98 1.00     6502
#> swindow_as.factorid306     0.48      0.28     0.02     0.97 1.00     6200
#> swindow_as.factorid307     0.57      0.28     0.05     0.98 1.00     5203
#> swindow_as.factorid308     0.53      0.29     0.03     0.98 1.00     6771
#> swindow_as.factorid309     0.58      0.28     0.05     0.99 1.00     5490
#> swindow_as.factorid310     0.46      0.28     0.03     0.96 1.00     6927
#> swindow_as.factorid311     0.52      0.29     0.03     0.98 1.00     5411
#> swindow_as.factorid312     0.46      0.29     0.02     0.97 1.00     6041
#> swindow_as.factorid313     0.66      0.26     0.07     0.99 1.00     5161
#> swindow_as.factorid314     0.46      0.29     0.02     0.97 1.00     6721
#> swindow_as.factorid315     0.49      0.29     0.02     0.97 1.01     5950
#> swindow_as.factorid316     0.46      0.28     0.02     0.96 1.00     6043
#> swindow_as.factorid317     0.60      0.27     0.06     0.98 1.00     6642
#>                        Tail_ESS
#> Intercept                  3159
#> sigma_Intercept            2864
#> pwindow_as.factorid1       2388
#> pwindow_as.factorid2       2347
#> pwindow_as.factorid3       2276
#> pwindow_as.factorid4       2143
#> pwindow_as.factorid5       2519
#> pwindow_as.factorid6       2128
#> pwindow_as.factorid7       2408
#> pwindow_as.factorid8       2477
#> pwindow_as.factorid9       2616
#> pwindow_as.factorid10      2042
#> pwindow_as.factorid11      2443
#> pwindow_as.factorid12      2077
#> pwindow_as.factorid13      2151
#> pwindow_as.factorid14      2559
#> pwindow_as.factorid15      2640
#> pwindow_as.factorid16      2223
#> pwindow_as.factorid17      2410
#> pwindow_as.factorid18      2097
#> pwindow_as.factorid19      2317
#> pwindow_as.factorid20      2598
#> pwindow_as.factorid21      2203
#> pwindow_as.factorid22      2161
#> pwindow_as.factorid23      2101
#> pwindow_as.factorid24      2557
#> pwindow_as.factorid25      1818
#> pwindow_as.factorid26      2591
#> pwindow_as.factorid27      2543
#> pwindow_as.factorid28      2258
#> pwindow_as.factorid29      2037
#> pwindow_as.factorid30      2422
#> pwindow_as.factorid31      2458
#> pwindow_as.factorid32      2425
#> pwindow_as.factorid33      2783
#> pwindow_as.factorid34      2361
#> pwindow_as.factorid35      2688
#> pwindow_as.factorid36      2476
#> pwindow_as.factorid37      2486
#> pwindow_as.factorid38      2618
#> pwindow_as.factorid39      2354
#> pwindow_as.factorid40      2442
#> pwindow_as.factorid41      2382
#> pwindow_as.factorid42      2062
#> pwindow_as.factorid43      2357
#> pwindow_as.factorid44      2232
#> pwindow_as.factorid45      2522
#> pwindow_as.factorid46      1871
#> pwindow_as.factorid47      2245
#> pwindow_as.factorid48      2031
#> pwindow_as.factorid49      2577
#> pwindow_as.factorid50      2295
#> pwindow_as.factorid51      3092
#> pwindow_as.factorid52      2291
#> pwindow_as.factorid53      2101
#> pwindow_as.factorid54      2605
#> pwindow_as.factorid55      2489
#> pwindow_as.factorid56      2316
#> pwindow_as.factorid57      2775
#> pwindow_as.factorid58      2590
#> pwindow_as.factorid59      2155
#> pwindow_as.factorid60      2207
#> pwindow_as.factorid61      2123
#> pwindow_as.factorid62      2357
#> pwindow_as.factorid63      2674
#> pwindow_as.factorid64      2628
#> pwindow_as.factorid65      2223
#> pwindow_as.factorid66      2261
#> pwindow_as.factorid67      2725
#> pwindow_as.factorid68      2511
#> pwindow_as.factorid69      2400
#> pwindow_as.factorid70      2300
#> pwindow_as.factorid71      2271
#> pwindow_as.factorid72      2720
#> pwindow_as.factorid73      2191
#> pwindow_as.factorid74      2562
#> pwindow_as.factorid75      2265
#> pwindow_as.factorid76      2489
#> pwindow_as.factorid77      2580
#> pwindow_as.factorid78      2049
#> pwindow_as.factorid79      2208
#> pwindow_as.factorid80      2329
#> pwindow_as.factorid81      2399
#> pwindow_as.factorid82      2537
#> pwindow_as.factorid83      2221
#> pwindow_as.factorid84      2665
#> pwindow_as.factorid85      2044
#> pwindow_as.factorid86      2385
#> pwindow_as.factorid87      2287
#> pwindow_as.factorid88      2217
#> pwindow_as.factorid89      2373
#> pwindow_as.factorid90      2510
#> pwindow_as.factorid91      2422
#> pwindow_as.factorid92      2083
#> pwindow_as.factorid93      2361
#> pwindow_as.factorid94      2435
#> pwindow_as.factorid95      2211
#> pwindow_as.factorid96      2398
#> pwindow_as.factorid97      2541
#> pwindow_as.factorid98      2496
#> pwindow_as.factorid99      2010
#> pwindow_as.factorid100     2304
#> pwindow_as.factorid101     2357
#> pwindow_as.factorid102     2079
#> pwindow_as.factorid103     2277
#> pwindow_as.factorid104     2665
#> pwindow_as.factorid105     2478
#> pwindow_as.factorid106     2038
#> pwindow_as.factorid107     2515
#> pwindow_as.factorid108     1835
#> pwindow_as.factorid109     2268
#> pwindow_as.factorid110     2495
#> pwindow_as.factorid111     2602
#> pwindow_as.factorid112     2960
#> pwindow_as.factorid113     1920
#> pwindow_as.factorid114     2269
#> pwindow_as.factorid115     2385
#> pwindow_as.factorid116     2532
#> pwindow_as.factorid117     2574
#> pwindow_as.factorid118     1980
#> pwindow_as.factorid119     2142
#> pwindow_as.factorid120     2583
#> pwindow_as.factorid121     2610
#> pwindow_as.factorid122     2495
#> pwindow_as.factorid123     2061
#> pwindow_as.factorid124     2243
#> pwindow_as.factorid125     2264
#> pwindow_as.factorid126     2537
#> pwindow_as.factorid127     2355
#> pwindow_as.factorid128     2471
#> pwindow_as.factorid129     2352
#> pwindow_as.factorid130     2376
#> pwindow_as.factorid131     2186
#> pwindow_as.factorid132     2572
#> pwindow_as.factorid133     2451
#> pwindow_as.factorid134     2553
#> pwindow_as.factorid135     2262
#> pwindow_as.factorid136     2358
#> pwindow_as.factorid137     2100
#> pwindow_as.factorid138     2203
#> pwindow_as.factorid139     2468
#> pwindow_as.factorid140     2256
#> pwindow_as.factorid141     2176
#> pwindow_as.factorid142     2231
#> pwindow_as.factorid143     2377
#> pwindow_as.factorid144     1831
#> pwindow_as.factorid145     2372
#> pwindow_as.factorid146     2342
#> pwindow_as.factorid147     2620
#> pwindow_as.factorid148     2379
#> pwindow_as.factorid149     2264
#> pwindow_as.factorid150     2308
#> pwindow_as.factorid151     2655
#> pwindow_as.factorid152     2597
#> pwindow_as.factorid153     2091
#> pwindow_as.factorid154     2490
#> pwindow_as.factorid155     2183
#> pwindow_as.factorid156     2197
#> pwindow_as.factorid157     2424
#> pwindow_as.factorid158     2846
#> pwindow_as.factorid159     2569
#> pwindow_as.factorid160     2234
#> pwindow_as.factorid161     2254
#> pwindow_as.factorid162     2044
#> pwindow_as.factorid163     2325
#> pwindow_as.factorid164     2200
#> pwindow_as.factorid165     2206
#> pwindow_as.factorid166     2585
#> pwindow_as.factorid167     2243
#> pwindow_as.factorid168     2450
#> pwindow_as.factorid169     2681
#> pwindow_as.factorid170     2475
#> pwindow_as.factorid171     2158
#> pwindow_as.factorid172     2615
#> pwindow_as.factorid173     2462
#> pwindow_as.factorid174     2573
#> pwindow_as.factorid175     2147
#> pwindow_as.factorid176     2440
#> pwindow_as.factorid177     2418
#> pwindow_as.factorid178     2344
#> pwindow_as.factorid179     2400
#> pwindow_as.factorid180     2591
#> pwindow_as.factorid181     2617
#> pwindow_as.factorid182     2517
#> pwindow_as.factorid183     2451
#> pwindow_as.factorid184     2359
#> pwindow_as.factorid185     2446
#> pwindow_as.factorid186     2562
#> pwindow_as.factorid187     1989
#> pwindow_as.factorid188     2414
#> pwindow_as.factorid189     2602
#> pwindow_as.factorid190     2349
#> pwindow_as.factorid191     2521
#> pwindow_as.factorid192     2523
#> pwindow_as.factorid193     1586
#> pwindow_as.factorid194     2624
#> pwindow_as.factorid195     2580
#> pwindow_as.factorid196     2053
#> pwindow_as.factorid197     2700
#> pwindow_as.factorid198     2429
#> pwindow_as.factorid199     2171
#> pwindow_as.factorid200     2364
#> pwindow_as.factorid201     2243
#> pwindow_as.factorid202     2604
#> pwindow_as.factorid203     2475
#> pwindow_as.factorid204     2305
#> pwindow_as.factorid205     2503
#> pwindow_as.factorid206     2442
#> pwindow_as.factorid207     2306
#> pwindow_as.factorid208     2266
#> pwindow_as.factorid209     2532
#> pwindow_as.factorid210     2221
#> pwindow_as.factorid211     2556
#> pwindow_as.factorid212     2429
#> pwindow_as.factorid213     2321
#> pwindow_as.factorid214     2260
#> pwindow_as.factorid215     2134
#> pwindow_as.factorid216     2163
#> pwindow_as.factorid217     2671
#> pwindow_as.factorid218     1919
#> pwindow_as.factorid219     2544
#> pwindow_as.factorid220     2257
#> pwindow_as.factorid221     2365
#> pwindow_as.factorid222     2319
#> pwindow_as.factorid223     2693
#> pwindow_as.factorid224     2635
#> pwindow_as.factorid225     2575
#> pwindow_as.factorid226     2346
#> pwindow_as.factorid227     2544
#> pwindow_as.factorid228     2230
#> pwindow_as.factorid229     2397
#> pwindow_as.factorid230     2592
#> pwindow_as.factorid231     2494
#> pwindow_as.factorid232     2255
#> pwindow_as.factorid233     2529
#> pwindow_as.factorid234     2739
#> pwindow_as.factorid235     2304
#> pwindow_as.factorid236     2682
#> pwindow_as.factorid237     2625
#> pwindow_as.factorid238     2396
#> pwindow_as.factorid239     2392
#> pwindow_as.factorid240     2427
#> pwindow_as.factorid241     2386
#> pwindow_as.factorid242     2363
#> pwindow_as.factorid243     2530
#> pwindow_as.factorid244     2639
#> pwindow_as.factorid245     2060
#> pwindow_as.factorid246     2117
#> pwindow_as.factorid247     2456
#> pwindow_as.factorid248     2462
#> pwindow_as.factorid249     2664
#> pwindow_as.factorid250     2191
#> pwindow_as.factorid251     2560
#> pwindow_as.factorid252     2272
#> pwindow_as.factorid253     2310
#> pwindow_as.factorid254     2492
#> pwindow_as.factorid255     2284
#> pwindow_as.factorid256     2372
#> pwindow_as.factorid257     2705
#> pwindow_as.factorid258     2498
#> pwindow_as.factorid259     1895
#> pwindow_as.factorid260     2345
#> pwindow_as.factorid261     2155
#> pwindow_as.factorid262     2432
#> pwindow_as.factorid263     2436
#> pwindow_as.factorid264     2052
#> pwindow_as.factorid265     2377
#> pwindow_as.factorid266     2472
#> pwindow_as.factorid267     2021
#> pwindow_as.factorid268     2412
#> pwindow_as.factorid269     2430
#> pwindow_as.factorid270     2645
#> pwindow_as.factorid271     2357
#> pwindow_as.factorid272     2513
#> pwindow_as.factorid273     2724
#> pwindow_as.factorid274     2153
#> pwindow_as.factorid275     2314
#> pwindow_as.factorid276     2215
#> pwindow_as.factorid277     2410
#> pwindow_as.factorid278     2045
#> pwindow_as.factorid279     2472
#> pwindow_as.factorid280     2139
#> pwindow_as.factorid281     1970
#> pwindow_as.factorid282     2096
#> pwindow_as.factorid283     2515
#> pwindow_as.factorid284     2634
#> pwindow_as.factorid285     2112
#> pwindow_as.factorid286     2220
#> pwindow_as.factorid287     2338
#> pwindow_as.factorid288     2147
#> pwindow_as.factorid289     2224
#> pwindow_as.factorid290     2237
#> pwindow_as.factorid291     2333
#> pwindow_as.factorid292     2624
#> pwindow_as.factorid293     2483
#> pwindow_as.factorid294     2246
#> pwindow_as.factorid295     2534
#> pwindow_as.factorid296     2351
#> pwindow_as.factorid297     2640
#> pwindow_as.factorid298     2510
#> pwindow_as.factorid299     2497
#> pwindow_as.factorid300     2517
#> pwindow_as.factorid301     2604
#> pwindow_as.factorid302     2199
#> pwindow_as.factorid303     2445
#> pwindow_as.factorid304     2496
#> pwindow_as.factorid305     2165
#> pwindow_as.factorid306     2174
#> pwindow_as.factorid307     2491
#> pwindow_as.factorid308     2354
#> pwindow_as.factorid309     2310
#> pwindow_as.factorid310     2322
#> pwindow_as.factorid311     2066
#> pwindow_as.factorid312     2361
#> pwindow_as.factorid313     2227
#> pwindow_as.factorid314     2328
#> pwindow_as.factorid315     2376
#> pwindow_as.factorid316     2293
#> pwindow_as.factorid317     2098
#> swindow_as.factorid1       2211
#> swindow_as.factorid2       2460
#> swindow_as.factorid3       2179
#> swindow_as.factorid4       2595
#> swindow_as.factorid5       2416
#> swindow_as.factorid6       2456
#> swindow_as.factorid7       2407
#> swindow_as.factorid8       2281
#> swindow_as.factorid9       2590
#> swindow_as.factorid10      2148
#> swindow_as.factorid11      2107
#> swindow_as.factorid12      2401
#> swindow_as.factorid13      2426
#> swindow_as.factorid14      2193
#> swindow_as.factorid15      2423
#> swindow_as.factorid16      1798
#> swindow_as.factorid17      2414
#> swindow_as.factorid18      2216
#> swindow_as.factorid19      2454
#> swindow_as.factorid20      2492
#> swindow_as.factorid21      2566
#> swindow_as.factorid22      2164
#> swindow_as.factorid23      2181
#> swindow_as.factorid24      2527
#> swindow_as.factorid25      2373
#> swindow_as.factorid26      2608
#> swindow_as.factorid27      2527
#> swindow_as.factorid28      2488
#> swindow_as.factorid29      2152
#> swindow_as.factorid30      2220
#> swindow_as.factorid31      2725
#> swindow_as.factorid32      2805
#> swindow_as.factorid33      2687
#> swindow_as.factorid34      2218
#> swindow_as.factorid35      2460
#> swindow_as.factorid36      2605
#> swindow_as.factorid37      2564
#> swindow_as.factorid38      2338
#> swindow_as.factorid39      2257
#> swindow_as.factorid40      2186
#> swindow_as.factorid41      2551
#> swindow_as.factorid42      2182
#> swindow_as.factorid43      2402
#> swindow_as.factorid44      2483
#> swindow_as.factorid45      2411
#> swindow_as.factorid46      2486
#> swindow_as.factorid47      2429
#> swindow_as.factorid48      2047
#> swindow_as.factorid49      2550
#> swindow_as.factorid50      2259
#> swindow_as.factorid51      2185
#> swindow_as.factorid52      2331
#> swindow_as.factorid53      2105
#> swindow_as.factorid54      2602
#> swindow_as.factorid55      2485
#> swindow_as.factorid56      2103
#> swindow_as.factorid57      2576
#> swindow_as.factorid58      2393
#> swindow_as.factorid59      2336
#> swindow_as.factorid60      2547
#> swindow_as.factorid61      2183
#> swindow_as.factorid62      2434
#> swindow_as.factorid63      2286
#> swindow_as.factorid64      2363
#> swindow_as.factorid65      2539
#> swindow_as.factorid66      2531
#> swindow_as.factorid67      2268
#> swindow_as.factorid68      1849
#> swindow_as.factorid69      2457
#> swindow_as.factorid70      2712
#> swindow_as.factorid71      2330
#> swindow_as.factorid72      2369
#> swindow_as.factorid73      2026
#> swindow_as.factorid74      2471
#> swindow_as.factorid75      2211
#> swindow_as.factorid76      2537
#> swindow_as.factorid77      2523
#> swindow_as.factorid78      2060
#> swindow_as.factorid79      2456
#> swindow_as.factorid80      2372
#> swindow_as.factorid81      2733
#> swindow_as.factorid82      2001
#> swindow_as.factorid83      2650
#> swindow_as.factorid84      2425
#> swindow_as.factorid85      2530
#> swindow_as.factorid86      2209
#> swindow_as.factorid87      2716
#> swindow_as.factorid88      2742
#> swindow_as.factorid89      1861
#> swindow_as.factorid90      2498
#> swindow_as.factorid91      2469
#> swindow_as.factorid92      2367
#> swindow_as.factorid93      2540
#> swindow_as.factorid94      2074
#> swindow_as.factorid95      2507
#> swindow_as.factorid96      2450
#> swindow_as.factorid97      2402
#> swindow_as.factorid98      2662
#> swindow_as.factorid99      2345
#> swindow_as.factorid100     1995
#> swindow_as.factorid101     2313
#> swindow_as.factorid102     2537
#> swindow_as.factorid103     2301
#> swindow_as.factorid104     2561
#> swindow_as.factorid105     2378
#> swindow_as.factorid106     2550
#> swindow_as.factorid107     2458
#> swindow_as.factorid108     2274
#> swindow_as.factorid109     2441
#> swindow_as.factorid110     2240
#> swindow_as.factorid111     2597
#> swindow_as.factorid112     2396
#> swindow_as.factorid113     2432
#> swindow_as.factorid114     2268
#> swindow_as.factorid115     2570
#> swindow_as.factorid116     2503
#> swindow_as.factorid117     2030
#> swindow_as.factorid118     1944
#> swindow_as.factorid119     2455
#> swindow_as.factorid120     2058
#> swindow_as.factorid121     2283
#> swindow_as.factorid122     2502
#> swindow_as.factorid123     2568
#> swindow_as.factorid124     2391
#> swindow_as.factorid125     2570
#> swindow_as.factorid126     2637
#> swindow_as.factorid127     2312
#> swindow_as.factorid128     2578
#> swindow_as.factorid129     2535
#> swindow_as.factorid130     2299
#> swindow_as.factorid131     2581
#> swindow_as.factorid132     2857
#> swindow_as.factorid133     2006
#> swindow_as.factorid134     2329
#> swindow_as.factorid135     2566
#> swindow_as.factorid136     2166
#> swindow_as.factorid137     2830
#> swindow_as.factorid138     2307
#> swindow_as.factorid139     2209
#> swindow_as.factorid140     2397
#> swindow_as.factorid141     2449
#> swindow_as.factorid142     2274
#> swindow_as.factorid143     2523
#> swindow_as.factorid144     2337
#> swindow_as.factorid145     2711
#> swindow_as.factorid146     1769
#> swindow_as.factorid147     2472
#> swindow_as.factorid148     2058
#> swindow_as.factorid149     2418
#> swindow_as.factorid150     2210
#> swindow_as.factorid151     2289
#> swindow_as.factorid152     2200
#> swindow_as.factorid153     2596
#> swindow_as.factorid154     2428
#> swindow_as.factorid155     2239
#> swindow_as.factorid156     2102
#> swindow_as.factorid157     2068
#> swindow_as.factorid158     2136
#> swindow_as.factorid159     2443
#> swindow_as.factorid160     2125
#> swindow_as.factorid161     2555
#> swindow_as.factorid162     2317
#> swindow_as.factorid163     2287
#> swindow_as.factorid164     2632
#> swindow_as.factorid165     1986
#> swindow_as.factorid166     2343
#> swindow_as.factorid167     1904
#> swindow_as.factorid168     2397
#> swindow_as.factorid169     2600
#> swindow_as.factorid170     2475
#> swindow_as.factorid171     2453
#> swindow_as.factorid172     2374
#> swindow_as.factorid173     2077
#> swindow_as.factorid174     2677
#> swindow_as.factorid175     2486
#> swindow_as.factorid176     2673
#> swindow_as.factorid177     2263
#> swindow_as.factorid178     2384
#> swindow_as.factorid179     2495
#> swindow_as.factorid180     2499
#> swindow_as.factorid181     2298
#> swindow_as.factorid182     2312
#> swindow_as.factorid183     2325
#> swindow_as.factorid184     2525
#> swindow_as.factorid185     2633
#> swindow_as.factorid186     2608
#> swindow_as.factorid187     2462
#> swindow_as.factorid188     2093
#> swindow_as.factorid189     2435
#> swindow_as.factorid190     2061
#> swindow_as.factorid191     2481
#> swindow_as.factorid192     2199
#> swindow_as.factorid193     2476
#> swindow_as.factorid194     2653
#> swindow_as.factorid195     2341
#> swindow_as.factorid196     2365
#> swindow_as.factorid197     2524
#> swindow_as.factorid198     2046
#> swindow_as.factorid199     2200
#> swindow_as.factorid200     2596
#> swindow_as.factorid201     2373
#> swindow_as.factorid202     2643
#> swindow_as.factorid203     2360
#> swindow_as.factorid204     2245
#> swindow_as.factorid205     2653
#> swindow_as.factorid206     2600
#> swindow_as.factorid207     2336
#> swindow_as.factorid208     2622
#> swindow_as.factorid209     2464
#> swindow_as.factorid210     2389
#> swindow_as.factorid211     2404
#> swindow_as.factorid212     2311
#> swindow_as.factorid213     2312
#> swindow_as.factorid214     2514
#> swindow_as.factorid215     2090
#> swindow_as.factorid216     1922
#> swindow_as.factorid217     2044
#> swindow_as.factorid218     2328
#> swindow_as.factorid219     2192
#> swindow_as.factorid220     2545
#> swindow_as.factorid221     2291
#> swindow_as.factorid222     2304
#> swindow_as.factorid223     2473
#> swindow_as.factorid224     2505
#> swindow_as.factorid225     2215
#> swindow_as.factorid226     2079
#> swindow_as.factorid227     2439
#> swindow_as.factorid228     2593
#> swindow_as.factorid229     2343
#> swindow_as.factorid230     2275
#> swindow_as.factorid231     2140
#> swindow_as.factorid232     2499
#> swindow_as.factorid233     1892
#> swindow_as.factorid234     2446
#> swindow_as.factorid235     2281
#> swindow_as.factorid236     2476
#> swindow_as.factorid237     2706
#> swindow_as.factorid238     2604
#> swindow_as.factorid239     2445
#> swindow_as.factorid240     2646
#> swindow_as.factorid241     2225
#> swindow_as.factorid242     2052
#> swindow_as.factorid243     2477
#> swindow_as.factorid244     2511
#> swindow_as.factorid245     2517
#> swindow_as.factorid246     2437
#> swindow_as.factorid247     2224
#> swindow_as.factorid248     2303
#> swindow_as.factorid249     2673
#> swindow_as.factorid250     2235
#> swindow_as.factorid251     2519
#> swindow_as.factorid252     2503
#> swindow_as.factorid253     2878
#> swindow_as.factorid254     2541
#> swindow_as.factorid255     2478
#> swindow_as.factorid256     2602
#> swindow_as.factorid257     2670
#> swindow_as.factorid258     2410
#> swindow_as.factorid259     2379
#> swindow_as.factorid260     2564
#> swindow_as.factorid261     2247
#> swindow_as.factorid262     2374
#> swindow_as.factorid263     2645
#> swindow_as.factorid264     2333
#> swindow_as.factorid265     2538
#> swindow_as.factorid266     2285
#> swindow_as.factorid267     2415
#> swindow_as.factorid268     2436
#> swindow_as.factorid269     2228
#> swindow_as.factorid270     2198
#> swindow_as.factorid271     2287
#> swindow_as.factorid272     2186
#> swindow_as.factorid273     2366
#> swindow_as.factorid274     1993
#> swindow_as.factorid275     2121
#> swindow_as.factorid276     2680
#> swindow_as.factorid277     2415
#> swindow_as.factorid278     2497
#> swindow_as.factorid279     2389
#> swindow_as.factorid280     1909
#> swindow_as.factorid281     2491
#> swindow_as.factorid282     2527
#> swindow_as.factorid283     1916
#> swindow_as.factorid284     2215
#> swindow_as.factorid285     2335
#> swindow_as.factorid286     2759
#> swindow_as.factorid287     2179
#> swindow_as.factorid288     2501
#> swindow_as.factorid289     2182
#> swindow_as.factorid290     2458
#> swindow_as.factorid291     2420
#> swindow_as.factorid292     2374
#> swindow_as.factorid293     1869
#> swindow_as.factorid294     2731
#> swindow_as.factorid295     2364
#> swindow_as.factorid296     2233
#> swindow_as.factorid297     2503
#> swindow_as.factorid298     2426
#> swindow_as.factorid299     2451
#> swindow_as.factorid300     2306
#> swindow_as.factorid301     2472
#> swindow_as.factorid302     2443
#> swindow_as.factorid303     2377
#> swindow_as.factorid304     2455
#> swindow_as.factorid305     2398
#> swindow_as.factorid306     2674
#> swindow_as.factorid307     2323
#> swindow_as.factorid308     2116
#> swindow_as.factorid309     1836
#> swindow_as.factorid310     2704
#> swindow_as.factorid311     2133
#> swindow_as.factorid312     2272
#> swindow_as.factorid313     1784
#> swindow_as.factorid314     2234
#> swindow_as.factorid315     2378
#> swindow_as.factorid316     2558
#> swindow_as.factorid317     2458
#> 
#> Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

## Analyses

This analysis in this repository has been implemented using the
[`targets`](https://docs.ropensci.org/targets/) package and associated
packages. The workflow is defined in
[`_targets.md`](https://github.com/parksw3/dynamicaltruncation/blob/main/_targets.md)
and can be explored interactively using
[`_targets.Rmd`](https://github.com/parksw3/dynamicaltruncation/blob/main/_targets.Rmd)
`Rmarkdown` document. The workflow can be visualised as the following
graph.

This complete analysis can be recreated using the following (note this
may take quite some time even with a fairly large amount of available
compute),

``` bash
bash bin/update-targets.sh
```

Alternative the following `targets` functions may be used to
interactively explore the workflow:

- Run the workflow sequentially.

``` r
targets::tar_make()
```

- Run the workflow using all available workers.

``` r
targets::tar_make_future(workers = future::availableCores())
```

- Explore a graph of the workflow.

``` r
targets::tar_visnetwork(targets_only = TRUE)
```

Watch the workflow as it runs in a `shiny` app.

``` r
targets::tar_watch(targets_only = TRUE)
```

To use our archived version of the interim results (and so avoid long
run times) use the following to download it. Note that this process has
not been rigorously tested across environments and so may not work
seamlessly).

``` r
source(here::here("R", "targets-archive.R"))
get_targets_archive()
```
