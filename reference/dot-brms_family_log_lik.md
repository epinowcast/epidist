# The log likelihood of a `brms` delay family

Replaces the `brms` internals `log_lik_<family>()`, `log_lik_censor()`
and `log_lik_truncate()`. Observation `i` may be left (`cens` of -1),
right (1) or interval (2) censored, with the upper end of the interval
in `rcens`, and truncated to lie between `lb` and `ub`.

## Usage

``` r
.brms_family_log_lik(family, i, prep)
```

## Arguments

- family:

  The name of a `brms` family, for example `"lognormal"`.

- i:

  The index of the observation.

- prep:

  A `brms` prepared predictions object.

## Value

The log likelihood of observation `i` for every draw.
