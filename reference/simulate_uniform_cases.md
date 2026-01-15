# Simulate cases from a uniform distribution

This function simulates cases from a uniform distribution, where the
primary event times are uniformly distributed between 0 and `t`.

## Usage

``` r
simulate_uniform_cases(sample_size = 1000, t = 60)
```

## Arguments

- sample_size:

  The number of cases to simulate.

- t:

  Upper bound of the uniform distribution to generate primary event
  times.

## Value

A `data.frame` with two columns: `case` (case number) and `ptime`
(primary event time).

## See also

Other simulate:
[`simulate_exponential_cases()`](https://epidist.epinowcast.org/reference/simulate_exponential_cases.md),
[`simulate_gillespie()`](https://epidist.epinowcast.org/reference/simulate_gillespie.md),
[`simulate_secondary()`](https://epidist.epinowcast.org/reference/simulate_secondary.md)
