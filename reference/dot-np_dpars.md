# The distributional parameters of the hazard coefficients

The distributional parameters of the hazard coefficients

## Usage

``` r
.np_dpars(np)
```

## Arguments

- np:

  The `np` element of the family, holding the boundaries, the basis and
  its coefficients.

## Value

A character vector of parameter names, all but `mu`: the unpenalised
coefficients, then the standard deviations, then the standardised
penalised coefficients.
