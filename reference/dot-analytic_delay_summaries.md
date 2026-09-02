# Analytic delay summaries for the families that have them

Each element gives the distributional parameters the solution needs and
functions of them returning the mean, the standard deviation and the
quantile function of the delay distribution. The parameters are the
`brms` parameters of the family.

## Usage

``` r
.analytic_delay_summaries(name)
```

## Arguments

- name:

  The name of a delay distribution family.

## Value

A list of solutions, or `NULL` when the family has none.
