# Natural scale bounds of a distributional parameter

Replaces `brms:::dpar_bounds()` for the non-mixture, non-custom families
that `epidist` supports. Guarantees a list with character elements `lb`
and `ub` giving the lower and upper bound of `dpar` on the natural
scale, where `""` means unbounded. Unlike the `brms` internal, an
unrecognised parameter is an error rather than `NULL`, because `epidist`
cannot generate Stan code without a bound.

## Usage

``` r
.dpar_bounds(dpar, family = NULL)
```

## Arguments

- dpar:

  A character string naming a distributional parameter.

- family:

  Unused. Kept so that the signature matches the `brms` internal this
  helper replaces.
