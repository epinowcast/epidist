# Add natural scale summaries of the delay distribution

Adds the mean and standard deviation of the delay distribution implied
by each draw of the distributional parameters, and quantiles of that
distribution if `probs` is given. Analytic solutions are used for the
families that have one. Every other family is summarised by simulating
delays from it, which works for any family `brms` can predict from.

## Usage

``` r
add_summaries(
  data,
  family = NULL,
  probs = NULL,
  method = c("auto", "analytic", "sample"),
  nsim = 1000
)
```

## Arguments

- data:

  A `data.frame` of draws of the distributional parameters, as returned
  by
  [`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md).

- family:

  A model fit with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md), a
  `brms` family, or the name of one, giving the delay distribution. If
  `NULL`, the default, the family is taken from `data`, which
  [`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md)
  records on it. Some `dplyr` verbs drop that record, so pass the fit or
  the family if `data` has been through one of them.

- probs:

  A numeric vector of probabilities to add quantiles of the delay
  distribution for. If `NULL`, the default, no quantiles are added.

- method:

  Either `"auto"`, the default, which uses the analytic solution when
  there is one and simulates otherwise, `"analytic"`, which errors when
  there is no analytic solution, or `"sample"`, which always simulates.

- nsim:

  The number of delays to simulate per row of `data`. Defaults to 1000.
  Only used when simulating.

## Value

The input with `mean` and `sd` columns added, and one column per element
of `probs`.

## Details

The summaries describe the delay distribution, not the posterior. A row
of `data` holds one draw of the distributional parameters, and the
columns added are the mean, standard deviation and quantiles of the
delay distribution those parameters define. Summarise the resulting
columns across draws to get posterior summaries of them.

Quantile columns are named as in
[`posterior::quantile2()`](https://mc-stan.org/posterior/reference/quantile2.html),
so `probs = 0.05` gives a `q5` column.

Simulation adds Monte Carlo error to the summaries. The standard error
of the mean is the delay standard deviation divided by the square root
of `nsim`. Simulation is also memory hungry, because it draws `nsim`
delays for every row of `data`. Reduce the number of rows with
[`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md),
or the number of draws with the `ndraws` argument of
[`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md),
if it is slow.

## See also

Other postprocess:
[`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md),
[`delay_summary_draws()`](https://epidist.epinowcast.org/reference/delay_summary_draws.md),
[`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md)

## Examples

``` r
draws <- data.frame(mu = c(1.8, 2.0), sigma = c(0.5, 0.4))
add_summaries(draws, family = "lognormal", probs = c(0.05, 0.95))
#>    mu sigma     mean       sd       q5      q95
#> 1 1.8   0.5 6.855149 3.653385 2.657998 13.76910
#> 2 2.0   0.4 8.004469 3.334232 3.826913 14.26689
```
