# Natural scale bounds of a distributional parameter

Replaces `brms:::dpar_bounds()` for the non-mixture families that
`epidist` supports. Guarantees a list with character elements `lb` and
`ub` giving the lower and upper bound of `dpar` on the natural scale,
where `""` means unbounded. A custom family, such as
[`gengamma()`](https://epidist.epinowcast.org/reference/gengamma.md),
records its own bounds, which are returned as `brms` does. Unlike the
`brms` internal, an unrecognised parameter is an error rather than
`NULL`, because `epidist` cannot generate Stan code without a bound.

## Usage

``` r
.dpar_bounds(dpar, family = NULL)
```

## Arguments

- dpar:

  A character string naming a distributional parameter.

- family:

  A `brmsfamily` object, whose bounds are used when it is a custom
  family. Otherwise unused and kept so that the signature matches the
  `brms` internal this helper replaces.

## Value

A list with character elements `lb` and `ub`.
