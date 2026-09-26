# Simulate cases from a stochastic SIR model

This function simulates cases from an stochastic SIR model. The user may
specify the initial epidemic growth rate \\r\\, the rate of recovery
gamma \\\gamma\\, the initial number of infected cases \\I_0\\, and the
total population size \\N\\.

## Usage

``` r
simulate_gillespie(r = 0.2, gamma = 1/7, I0 = 50, N = 10000, seed = NULL)
```

## Arguments

- r:

  The initial epidemic growth rate. Defaults to 0.2.

- gamma:

  The rate of recovery. Defaults to 1/7.

- I0:

  The initial number of infected people. Defaults to 50.

- N:

  The total population size. Defaults to 10000.

- seed:

  The random seed to be used in the simulation process. Defaults to
  `NULL`, which leaves the random number generator as it is.

## Value

A `data.frame` with two columns: `case` (case number) and `ptime`
(primary event time).

## See also

Other simulate:
[`simulate_dates()`](https://epidist.epinowcast.org/reference/simulate_dates.md),
[`simulate_exponential_cases()`](https://epidist.epinowcast.org/reference/simulate_exponential_cases.md),
[`simulate_secondary()`](https://epidist.epinowcast.org/reference/simulate_secondary.md),
[`simulate_study()`](https://epidist.epinowcast.org/reference/simulate_study.md),
[`simulate_uniform_cases()`](https://epidist.epinowcast.org/reference/simulate_uniform_cases.md)

## Examples

``` r
cases <- simulate_gillespie(
  r = 0.2, gamma = 1 / 7, I0 = 5, N = 50, seed = 101
)
head(cases)
#>   case     ptime
#> 1    1 0.5236446
#> 2    2 0.6808627
#> 3    3 3.0981564
#> 4    4 4.0658230
#> 5    5 4.2537418
#> 6    6 4.7222071
```
