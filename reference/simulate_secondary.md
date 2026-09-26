# Simulate secondary events based on a delay distribution

This function simulates secondary events based on a given delay
distribution. The input dataset should have the primary event times in a
column named `ptime`.

## Usage

``` r
simulate_secondary(data, dist = rlnorm, ...)
```

## Arguments

- data:

  A data frame with the primary event times.

- dist:

  The delay distribution to be used. Defaults to
  [`rlnorm()`](https://rdrr.io/r/stats/Lognormal.html).

- ...:

  Arguments to be passed to the delay distribution function.

## Value

A `data.frame` that augments `data` with two new columns: `delay`
(secondary event latency) and `stime` (the time of the secondary event).

## See also

Other simulate:
[`simulate_dates()`](https://epidist.epinowcast.org/reference/simulate_dates.md),
[`simulate_exponential_cases()`](https://epidist.epinowcast.org/reference/simulate_exponential_cases.md),
[`simulate_gillespie()`](https://epidist.epinowcast.org/reference/simulate_gillespie.md),
[`simulate_study()`](https://epidist.epinowcast.org/reference/simulate_study.md),
[`simulate_uniform_cases()`](https://epidist.epinowcast.org/reference/simulate_uniform_cases.md)

## Examples

``` r
simulate_uniform_cases(sample_size = 5) |>
  simulate_secondary(meanlog = 1.8, sdlog = 0.5)
#>   case    ptime     delay    stime
#> 1    1 16.63335 10.291230 26.92458
#> 2    2 24.52047  3.874891 28.39536
#> 3    3 50.20348  5.788692 55.99217
#> 4    4 26.10319  3.892437 29.99563
#> 5    5 15.24673  4.528961 19.77569
```
