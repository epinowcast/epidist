# The `stats` distribution of a `brms` delay family

Replaces the family specific parts of the `brms` internals
`log_lik_<family>()` and `posterior_predict_<family>()` for the delay
families `brms` provides and `epidist` supports. `brms` gives `mu` as
the mean of the gamma, Weibull and exponential families and as the mean
of the log delay for the lognormal family.

## Usage

``` r
.brms_family_dist(family, prep, i)
```

## Arguments

- family:

  The name of a `brms` family, for example `"lognormal"`.

- prep:

  A `brms` prepared predictions object.

- i:

  The index of the observation.

## Value

A list with the `stats` distribution name `dist`, for example `"lnorm"`,
and its arguments `args` for observation `i`.
