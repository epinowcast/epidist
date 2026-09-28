# Analytic delay summaries for the families that have them

Each gives the `brms` parameters of the family and functions of them
returning the mean, the standard deviation, the quantile function and
the density of the delay distribution, built by
[`.analytic_family()`](https://epidist.epinowcast.org/reference/dot-analytic_family.md).

## Usage

``` r
.analytic_delay_summaries(name)
```

## Arguments

- name:

  The name of a delay distribution family.

## Value

A list of solutions, or `NULL` when the family has none.
