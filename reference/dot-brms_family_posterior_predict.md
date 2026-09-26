# Draw from a `brms` delay family

Replaces the `brms` internals `posterior_predict_<family>()` and
`rcontinuous()`. When observation `i` is truncated to lie between `lb`
and `ub` the delays are drawn by inverting the distribution function.

## Usage

``` r
.brms_family_posterior_predict(family, i, prep)
```

## Arguments

- family:

  The name of a `brms` family, for example `"lognormal"`.

- i:

  The index of the observation.

- prep:

  A `brms` prepared predictions object.

## Value

A delay for observation `i` for every draw.
