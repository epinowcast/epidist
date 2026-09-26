# Bin probabilities of the non-parametric family

Each row is the probability mass at the right edge of each bin, from the
hazards \\h_k\\ as \\h_k \prod\_{j \< k} (1 - h_j)\\. Matches
[`primarycensored::hazards_to_pmf()`](https://primarycensored.epinowcast.org/reference/hazards_to_pmf.html).

## Usage

``` r
.np_pmf(dpars, np)
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
column per bin.
