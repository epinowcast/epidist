# The expected delay of the non-parametric family

The expected delay of the non-parametric family

## Usage

``` r
.np_epred(np)
```

## Arguments

- np:

  The `np` element of the family, holding the boundaries, the basis and
  its coefficients.

## Value

A function of `prep` returning a matrix of the mean delay with one row
per draw and one column per observation, as used by
[`brms::posterior_epred()`](https://mc-stan.org/rstantools/reference/posterior_epred.html).
