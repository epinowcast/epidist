# Whether a delay is certainly below the cut of the distribution function

Decided from a closed form bound on the parameters rather than from the
distribution function, because in Stan the distribution function must
not be evaluated where it underflows: its autodiff partial is then
`0 / 0`, and Stan's reverse pass chains every node on the stack, so the
`NaN` poisons the gradient even when the value is discarded. The bounds
are `Phi(z) < exp(-100)` for `z < -14` for the lognormal,
`P(a, x) <= x^a / Gamma(a + 1)` for the gamma and `1 - exp(-y) <= y` for
the weibull. Mirrors `meta_family_deep_tail()` in Stan.

## Usage

``` r
.meta_deep_tail(q, dist, args)
```

## Arguments

- q:

  A numeric vector of delays.

- dist:

  A `primarycensored` distribution function name.

- args:

  A named list of distribution parameters.

## Value

A logical vector.
