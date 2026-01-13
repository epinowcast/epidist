# Simulate cases from a stochastic SIR model

This function simulates cases from an stochastic SIR model. The user may
specify the initial epidemic growth rate \\r\\, the rate of recovery
gamma \\\gamma\\, the initial number of infected cases \\I_0\\, and the
total population size \\N\\.

## Usage

``` r
simulate_gillespie(r = 0.2, gamma = 1/7, I0 = 50, N = 10000, seed)
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

  The random seed to be used in the simulation process.

## Value

A `data.frame` with two columns: `case` (case number) and `ptime`
(primary event time).

## See also

Other simulate:
[`simulate_exponential_cases()`](https://epidist.epinowcast.org/dev/reference/simulate_exponential_cases.md),
[`simulate_secondary()`](https://epidist.epinowcast.org/dev/reference/simulate_secondary.md),
[`simulate_uniform_cases()`](https://epidist.epinowcast.org/dev/reference/simulate_uniform_cases.md)
