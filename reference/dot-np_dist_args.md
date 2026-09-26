# Distribution parameters of the non-parametric family for each draw

Distribution parameters of the non-parametric family for each draw

## Usage

``` r
.np_dist_args(prep, i, np)
```

## Arguments

- prep:

  A `brms` prepared predictions object.

- i:

  The index of the observation.

- np:

  The `np` element of the family, holding the boundaries, the basis and
  its coefficients.

## Value

A list with one element per draw, each a list of `boundaries` and
`hazards` for
[`primarycensored::pdiscretehazard()`](https://primarycensored.epinowcast.org/reference/pdiscretehazard.html).
