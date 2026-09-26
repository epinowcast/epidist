# Bin hazards of the non-parametric family

Bin hazards of the non-parametric family

## Usage

``` r
.np_hazards(dpars, np)
```

## Arguments

- dpars:

  A named list of distributional parameter vectors of equal length,
  holding `mu` and the hazard coefficients.

- np:

  The `np` element of the family, holding the boundaries, the basis and
  its coefficients.

## Value

A matrix with one row per element of the vectors in `dpars` and one
column per bin, whose last column is 1. Mirrors `epidist_np_params()` in
Stan.
