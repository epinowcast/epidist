
# Adjusting for common biases in infectious disease data when estimating distributions.

## A simple example

First load required packages and functions.

``` r
library(data.table)
library(purrr, quietly = TRUE)
library(ggplot2)
library(patchwork)
library(here)
library(brms)
functions <- list.files(here("R"), full.names = TRUE)
walk(functions, source)
```

### Simulate the data

Simulate data from an outbreak.

``` r
outbreak <- simulate_gillespie(seed = 101)
```

Define the secondary distribution to use in the simulation

``` r
secondary_dist <- data.table(
  meanlog = 1.8, sdlog = 0.5
) |>
  add_natural_scale_mean_sd()
```

Simulate an observation process during the growth phase for a secondary
event using a lognormal distribution, and finally simulate observing
this event.

``` r
obs <- outbreak |>
  simulate_secondary(
    meanlog = secondary_dist$meanlog[[1]],
    sdlog = secondary_dist$sdlog[[1]]
  ) |>
  observe_process()
```

Observe the outbreak after 25 days and take 100 samples.

``` r
truncated_obs <- obs |>
  filter_obs_by_obs_time(obs_time = 25) |>
  DT(sample(1:.N, 200, replace = FALSE))
```

Plot the outbreak, and empirical delay distribution.

### Models

First fit a naive lognormal model with no adjustment.

``` r
naive_fit <- naive_delay(data = truncated_obs, cores = 4, refresh = 0)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 finished in 0.3 seconds.
#> Chain 2 finished in 0.4 seconds.
#> Chain 3 finished in 0.4 seconds.
#> Chain 4 finished in 0.4 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 0.4 seconds.
#> Total execution time: 0.7 seconds.
```

Estimate the delay after filtering out the most recent data as crude
adjustement for right truncation.

``` r
filtered_fit <- filtered_naive_delay(
  data = truncated_obs, cores = 4, refresh = 0
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 finished in 0.2 seconds.
#> Chain 2 finished in 0.2 seconds.
#> Chain 3 finished in 0.2 seconds.
#> Chain 4 finished in 0.2 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 0.2 seconds.
#> Total execution time: 0.4 seconds.
```

Adjust for date censoring.

``` r
censored_fit <- censoring_adjusted_delay(
  data = truncated_obs, cores = 4, refresh = 0
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 2 finished in 1.0 seconds.
#> Chain 1 finished in 1.1 seconds.
#> Chain 3 finished in 1.0 seconds.
#> Chain 4 finished in 1.1 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 1.0 seconds.
#> Total execution time: 1.2 seconds.
```

Adjust for censoring and filter to crudely adjust for right truncation.

``` r
filtered_censored_fit <- filtered_censoring_adjusted_delay(
  data = truncated_obs, cores = 4, refresh = 0
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 finished in 0.6 seconds.
#> Chain 2 finished in 0.7 seconds.
#> Chain 3 finished in 0.8 seconds.
#> Chain 4 finished in 0.7 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 0.7 seconds.
#> Total execution time: 0.9 seconds.
```

Adjust for right truncation.

``` r
truncation_fit <- truncation_adjusted_delay(
  data = truncated_obs, cores = 4, refresh = 0
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 finished in 1.4 seconds.
#> Chain 2 finished in 1.5 seconds.
#> Chain 3 finished in 1.4 seconds.
#> Chain 4 finished in 1.6 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 1.5 seconds.
#> Total execution time: 1.8 seconds.
```

Adjust for right truncation and date censoring.

``` r
truncation_censoring_fit <- truncation_censoring_adjusted_delay(
  data = truncated_obs, cores = 4, refresh = 0
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 finished in 2.0 seconds.
#> Chain 2 finished in 2.0 seconds.
#> Chain 3 finished in 2.1 seconds.
#> Chain 4 finished in 2.1 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 2.1 seconds.
#> Total execution time: 2.2 seconds.
```

Adjust for right truncation and date censoring using a latent variable
approach.

``` r
latent_truncation_censoring_fit <- latent_truncation_censoring_adjusted_delay(
  data = truncated_obs, cores = 4, refresh = 0
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 finished in 16.6 seconds.
#> Chain 2 finished in 16.7 seconds.
#> Chain 3 finished in 17.2 seconds.
#> Chain 4 finished in 17.5 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 17.0 seconds.
#> Total execution time: 17.6 seconds.
```

### Summarise model posteriors and compare to known truth

Combine models into a named list.

``` r
models <- list(
  "Naive" = naive_fit,
  "Filtered" = filtered_fit,
  "Censoring adjusted" = censored_fit,
  "Filtered and censoring adjusted" = filtered_censored_fit,
  "Truncation adjusted" = truncation_fit,
  "Truncation and censoring adjusted" = truncation_censoring_fit,
  "Latent variable truncation and censoring adjusted" =
    latent_truncation_censoring_fit
)
```

Extract and summarise lognormal posterior estimates.

``` r
draws <- models |>
  map(extract_lognormal_draws) |>
  map(draws_to_long) |>
  rbindlist(idcol = "model") |>
  DT(, model := factor(model, levels = rev(names(models))))

summarised_draws <- summarise_lognormal_draws(draws, sf = 2)

knitr::kable(summarised_draws)
```

| model                                             | parameter | mean | median | q2.5 |   q5 |  q20 |  q35 |  q65 |  q80 |  q95 | q97.5 |
|:--------------------------------------------------|:----------|-----:|-------:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|------:|
| Naive                                             | meanlog   | 1.60 |   1.60 | 1.50 | 1.50 | 1.50 | 1.60 | 1.60 | 1.60 | 1.60 |  1.60 |
| Naive                                             | sdlog     | 0.49 |   0.49 | 0.44 | 0.45 | 0.47 | 0.48 | 0.50 | 0.51 | 0.53 |  0.54 |
| Naive                                             | mean      | 5.40 |   5.40 | 5.00 | 5.10 | 5.20 | 5.30 | 5.50 | 5.50 | 5.70 |  5.80 |
| Naive                                             | sd        | 2.80 |   2.80 | 2.40 | 2.50 | 2.60 | 2.70 | 2.90 | 3.00 | 3.20 |  3.30 |
| Filtered                                          | meanlog   | 1.70 |   1.70 | 1.60 | 1.70 | 1.70 | 1.70 | 1.70 | 1.80 | 1.80 |  1.80 |
| Filtered                                          | sdlog     | 0.45 |   0.45 | 0.40 | 0.41 | 0.43 | 0.44 | 0.46 | 0.48 | 0.50 |  0.51 |
| Filtered                                          | mean      | 6.30 |   6.20 | 5.80 | 5.80 | 6.00 | 6.10 | 6.40 | 6.50 | 6.70 |  6.80 |
| Filtered                                          | sd        | 3.00 |   3.00 | 2.50 | 2.60 | 2.70 | 2.90 | 3.10 | 3.20 | 3.50 |  3.60 |
| Censoring adjusted                                | meanlog   | 1.60 |   1.60 | 1.50 | 1.50 | 1.50 | 1.60 | 1.60 | 1.60 | 1.60 |  1.60 |
| Censoring adjusted                                | sdlog     | 0.45 |   0.45 | 0.40 | 0.41 | 0.43 | 0.44 | 0.46 | 0.47 | 0.49 |  0.50 |
| Censoring adjusted                                | mean      | 5.40 |   5.30 | 5.00 | 5.10 | 5.20 | 5.30 | 5.40 | 5.50 | 5.70 |  5.70 |
| Censoring adjusted                                | sd        | 2.50 |   2.50 | 2.20 | 2.20 | 2.40 | 2.50 | 2.60 | 2.70 | 2.90 |  3.00 |
| Filtered and censoring adjusted                   | meanlog   | 1.70 |   1.70 | 1.70 | 1.70 | 1.70 | 1.70 | 1.80 | 1.80 | 1.80 |  1.80 |
| Filtered and censoring adjusted                   | sdlog     | 0.42 |   0.42 | 0.37 | 0.37 | 0.40 | 0.41 | 0.43 | 0.45 | 0.48 |  0.49 |
| Filtered and censoring adjusted                   | mean      | 6.20 |   6.20 | 5.80 | 5.80 | 6.00 | 6.10 | 6.30 | 6.40 | 6.70 |  6.80 |
| Filtered and censoring adjusted                   | sd        | 2.80 |   2.70 | 2.30 | 2.30 | 2.50 | 2.60 | 2.80 | 3.00 | 3.30 |  3.40 |
| Truncation adjusted                               | meanlog   | 1.80 |   1.80 | 1.70 | 1.70 | 1.80 | 1.80 | 1.80 | 1.90 | 1.90 |  2.00 |
| Truncation adjusted                               | sdlog     | 0.57 |   0.56 | 0.49 | 0.50 | 0.53 | 0.55 | 0.58 | 0.60 | 0.64 |  0.65 |
| Truncation adjusted                               | mean      | 7.20 |   7.10 | 6.20 | 6.30 | 6.70 | 6.90 | 7.30 | 7.70 | 8.40 |  8.70 |
| Truncation adjusted                               | sd        | 4.50 |   4.30 | 3.30 | 3.50 | 3.80 | 4.10 | 4.60 | 5.00 | 5.80 |  6.20 |
| Truncation and censoring adjusted                 | meanlog   | 1.80 |   1.80 | 1.70 | 1.70 | 1.70 | 1.80 | 1.80 | 1.80 | 1.90 |  1.90 |
| Truncation and censoring adjusted                 | sdlog     | 0.51 |   0.50 | 0.44 | 0.45 | 0.47 | 0.49 | 0.52 | 0.54 | 0.58 |  0.59 |
| Truncation and censoring adjusted                 | mean      | 6.80 |   6.70 | 6.00 | 6.10 | 6.40 | 6.60 | 6.90 | 7.20 | 7.70 |  7.90 |
| Truncation and censoring adjusted                 | sd        | 3.70 |   3.60 | 2.80 | 2.90 | 3.20 | 3.40 | 3.80 | 4.10 | 4.70 |  5.00 |
| Latent variable truncation and censoring adjusted | meanlog   | 1.80 |   1.80 | 1.70 | 1.70 | 1.80 | 1.80 | 1.80 | 1.90 | 1.90 |  1.90 |
| Latent variable truncation and censoring adjusted | sdlog     | 0.47 |   0.47 | 0.42 | 0.42 | 0.45 | 0.46 | 0.49 | 0.50 | 0.53 |  0.54 |
| Latent variable truncation and censoring adjusted | mean      | 6.90 |   6.80 | 6.10 | 6.20 | 6.50 | 6.70 | 7.00 | 7.20 | 7.60 |  7.80 |
| Latent variable truncation and censoring adjusted | sd        | 3.50 |   3.40 | 2.70 | 2.80 | 3.10 | 3.20 | 3.60 | 3.80 | 4.30 |  4.50 |

Plot summarised posterior estimates from each model compared to the
ground truth.

``` r
draws |>
  make_relative_to_truth(secondary_dist) |>
  plot_relative_recovery(fill = model) +
  facet_wrap(vars(parameter), nrow = 1, scales = "free_x") +
  scale_fill_brewer(palette = "Dark2") +
  guides(fill = guide_none()) +
  labs(
    y = "Model", x = "Relative to ground truth"
  )
```

<img src="figures/README-unnamed-chunk-10-1.png" width="100%" />

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
