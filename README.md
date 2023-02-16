
# Adjusting for common biases in infectious disease data when estimating distributions.

## A simple example

First load our analysis (from GitHub or locally using
`devtools::load_all()`) package and other required packages.

``` r
library(dynamicaltruncation)
library(data.table)
library(purrr)
library(ggplot2)
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

Plot primary cases (columns), and secondary cases (dots) by the
observation time of their secondary events. This reflects would could be
observed in real-time.

``` r
truncated_cases <- construct_cases_by_obs_window(
  obs, windows = c(25), obs_type = "stime"
)

plot_cases_by_obs_window(truncated_cases)
```

<img src="figures/README-observed-cases-1.png" width="100%" />

We can alternatively plot the observed data based on primary events.
This corresponds to a retrospective cohort based view of the data.

``` r
truncated_cases <- construct_cases_by_obs_window(
  obs, windows = c(25), obs_type = "ptime"
)

plot_cases_by_obs_window(truncated_cases)
```

<img src="figures/README-cohort-observed-cases-1.png" width="100%" />

Plot the true continuous delay distribution and the empirical observed
distribution for each observation window.

``` r
combined_obs <- combine_obs(truncated_obs, obs)

plot_empirical_delay(
  combined_obs, meanlog = secondary_dist$meanlog[[1]],
  sdlog = secondary_dist$sdlog[[1]]
)
```

<img src="figures/README-empirical-dist-1.png" width="100%" />

Plot the mean difference between continuous and discrete event time:

``` r
censor_delay <- calculate_censor_delay(truncated_obs)
plot_censor_delay(censor_delay)
```

<img src="figures/README-censor_delay-1.png" width="100%" />

### Models

First fit a naive lognormal model with no adjustment.

``` r
naive_fit <- naive_delay(data = truncated_obs, cores = 4, refresh = 0)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 finished in 0.2 seconds.
#> Chain 2 finished in 0.2 seconds.
#> Chain 3 finished in 0.1 seconds.
#> Chain 4 finished in 0.1 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 0.1 seconds.
#> Total execution time: 0.4 seconds.
```

Estimate the delay after filtering out the most recent data as crude
adjustement for right truncation.

``` r
filtered_fit <- filtered_naive_delay(
  data = truncated_obs, cores = 4, refresh = 0
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 finished in 0.1 seconds.
#> Chain 2 finished in 0.1 seconds.
#> Chain 3 finished in 0.1 seconds.
#> Chain 4 finished in 0.1 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 0.1 seconds.
#> Total execution time: 0.2 seconds.
```

Adjust for date censoring.

``` r
censored_fit <- censoring_adjusted_delay(
  data = truncated_obs, cores = 4, refresh = 0
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 finished in 0.5 seconds.
#> Chain 2 finished in 0.6 seconds.
#> Chain 4 finished in 0.5 seconds.
#> Chain 3 finished in 0.6 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 0.6 seconds.
#> Total execution time: 0.8 seconds.
```

Adjust for censoring and filter to crudely adjust for right truncation.

``` r
filtered_censored_fit <- filtered_censoring_adjusted_delay(
  data = truncated_obs, cores = 4, refresh = 0
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 finished in 0.4 seconds.
#> Chain 2 finished in 0.3 seconds.
#> Chain 3 finished in 0.4 seconds.
#> Chain 4 finished in 0.3 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 0.3 seconds.
#> Total execution time: 0.4 seconds.
```

Adjust for right truncation.

``` r
truncation_fit <- truncation_adjusted_delay(
  data = truncated_obs, cores = 4, refresh = 0
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 finished in 0.7 seconds.
#> Chain 2 finished in 0.7 seconds.
#> Chain 3 finished in 0.7 seconds.
#> Chain 4 finished in 0.8 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 0.7 seconds.
#> Total execution time: 1.0 seconds.
```

Adjust for right truncation and date censoring.

``` r
truncation_censoring_fit <- truncation_censoring_adjusted_delay(
  data = truncated_obs, cores = 4, refresh = 0
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 finished in 1.1 seconds.
#> Chain 2 finished in 1.1 seconds.
#> Chain 3 finished in 1.1 seconds.
#> Chain 4 finished in 1.1 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 1.1 seconds.
#> Total execution time: 1.3 seconds.
```

Adjust for right truncation and date censoring using a latent variable
approach.

``` r
latent_truncation_censoring_fit <- latent_truncation_censoring_adjusted_delay(
  data = truncated_obs, cores = 4, refresh = 0
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 finished in 3.4 seconds.
#> Chain 3 finished in 3.4 seconds.
#> Chain 4 finished in 3.3 seconds.
#> Chain 2 finished in 3.4 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 3.4 seconds.
#> Total execution time: 3.6 seconds.
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
  rbindlist(idcol = "model") |>
  DT(, model := factor(model, levels = rev(names(models))))

summarised_draws <- draws |>
  draws_to_long() |>
  summarise_draws(sf = 2)

knitr::kable(summarised_draws[parameter %in% c("meanlog", "sdlog")])
```

| model                                             | parameter | mean | median | q2.5 |   q5 |  q20 |  q35 |  q65 |  q80 |  q95 | q97.5 |
| :------------------------------------------------ | :-------- | ---: | -----: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ----: |
| Naive                                             | meanlog   | 1.60 |   1.60 | 1.50 | 1.50 | 1.50 | 1.60 | 1.60 | 1.60 | 1.60 |  1.60 |
| Filtered                                          | meanlog   | 1.70 |   1.70 | 1.60 | 1.70 | 1.70 | 1.70 | 1.70 | 1.80 | 1.80 |  1.80 |
| Censoring adjusted                                | meanlog   | 1.60 |   1.60 | 1.50 | 1.50 | 1.50 | 1.60 | 1.60 | 1.60 | 1.60 |  1.60 |
| Filtered and censoring adjusted                   | meanlog   | 1.70 |   1.70 | 1.70 | 1.70 | 1.70 | 1.70 | 1.80 | 1.80 | 1.80 |  1.80 |
| Truncation adjusted                               | meanlog   | 1.90 |   1.80 | 1.70 | 1.70 | 1.80 | 1.80 | 1.90 | 1.90 | 2.00 |  2.00 |
| Truncation and censoring adjusted                 | meanlog   | 1.80 |   1.80 | 1.70 | 1.70 | 1.80 | 1.80 | 1.80 | 1.90 | 1.90 |  1.90 |
| Latent variable truncation and censoring adjusted | meanlog   | 1.80 |   1.80 | 1.70 | 1.70 | 1.70 | 1.80 | 1.80 | 1.80 | 1.90 |  1.90 |
| Naive                                             | sdlog     | 0.49 |   0.49 | 0.44 | 0.45 | 0.47 | 0.48 | 0.50 | 0.51 | 0.53 |  0.54 |
| Filtered                                          | sdlog     | 0.45 |   0.45 | 0.40 | 0.41 | 0.43 | 0.44 | 0.46 | 0.48 | 0.50 |  0.51 |
| Censoring adjusted                                | sdlog     | 0.45 |   0.45 | 0.40 | 0.41 | 0.43 | 0.44 | 0.46 | 0.47 | 0.49 |  0.51 |
| Filtered and censoring adjusted                   | sdlog     | 0.42 |   0.42 | 0.37 | 0.37 | 0.40 | 0.41 | 0.43 | 0.45 | 0.48 |  0.49 |
| Truncation adjusted                               | sdlog     | 0.56 |   0.55 | 0.49 | 0.50 | 0.52 | 0.54 | 0.57 | 0.59 | 0.63 |  0.65 |
| Truncation and censoring adjusted                 | sdlog     | 0.48 |   0.48 | 0.41 | 0.42 | 0.45 | 0.46 | 0.49 | 0.51 | 0.54 |  0.56 |
| Latent variable truncation and censoring adjusted | sdlog     | 0.54 |   0.53 | 0.46 | 0.47 | 0.50 | 0.52 | 0.55 | 0.57 | 0.61 |  0.63 |

Plot summarised posterior estimates from each model compared to the
ground truth.

``` r
draws |>
  draws_to_long() |>
  make_relative_to_truth(draws_to_long(secondary_dist)) |>
  plot_relative_recovery(y = model, fill = model) +
  facet_wrap(vars(parameter), nrow = 1, scales = "free_x") +
  scale_fill_brewer(palette = "Dark2") +
  guides(fill = guide_none()) +
  labs(
    y = "Model", x = "Relative to ground truth"
  )
```

<img src="figures/README-unnamed-chunk-10-1.png" width="100%" />

Finally, check the mean posterior predictions for each model against the
observed daily cohort mean.

``` r
truncated_draws <- draws |>
  calculate_truncated_means(
    obs_at = max(truncated_obs$stime_daily),
    ptime = range(truncated_obs$ptime_daily)
  ) |>
  summarise_variable(variable = "trunc_mean", by = c("obs_horizon", "model")) |>
  DT(, model := factor(model, levels = rev(names(models))))


truncated_draws |>
  plot_mean_posterior_pred(
    truncated_obs |>
      calculate_cohort_mean(
        type = "cohort", obs_at = max(truncated_obs$stime_daily)
      ),
    col = model, fill = model, mean = TRUE, ribbon = TRUE
  ) +
  guides(
    fill = guide_legend(title = "Model", nrow = 4),
    col = guide_legend(title = "Model", nrow = 4)
  ) +
  scale_fill_brewer(palette = "Dark2", aesthetics = c("fill", "colour")) +
  theme(legend.direction = "vertical")
```

<img src="figures/README-postcheck-1.png" width="100%" />

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

<!-- end list -->

``` r
targets::tar_make()
```

  - Run the workflow using all available workers.

<!-- end list -->

``` r
targets::tar_make_future(workers = future::availableCores())
```

  - Explore a graph of the workflow.

<!-- end list -->

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
