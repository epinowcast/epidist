# Simulate exponential cases

This function simulates cases from an exponential distribution. The user
may specify the rate parameter `r`, the sample size, and the upper bound
of the survival time. If the rate parameter is 0, then this function
defaults to the uniform distribution.

## Usage

``` r
simulate_exponential_cases(r = 0.2, sample_size = 10000, seed, t = 30)
```

## Arguments

- r:

  The exponential growth rate parameter. Defaults to 0.2.

- sample_size:

  The number of cases to simulate. Defaults to 10000.

- seed:

  The random seed to be used in the simulation process.

- t:

  Upper bound of the survival time. Defaults to 30.

## Value

A `data.frame` with two columns: `case` (case number) and `ptime`
(primary event time).

## See also

Other simulate:
[`simulate_gillespie()`](https://epidist.epinowcast.org/dev/reference/simulate_gillespie.md),
[`simulate_secondary()`](https://epidist.epinowcast.org/dev/reference/simulate_secondary.md),
[`simulate_uniform_cases()`](https://epidist.epinowcast.org/dev/reference/simulate_uniform_cases.md)
