# The `brms` post-processing functions of the [`gengamma()`](https://epidist.epinowcast.org/reference/gengamma.md) family

The `brms` post-processing functions of the
[`gengamma()`](https://epidist.epinowcast.org/reference/gengamma.md)
family

## Usage

``` r
.gengamma_log_lik(i, prep)

.gengamma_posterior_predict(i, prep, ...)

.gengamma_posterior_epred(prep)
```

## Arguments

- i:

  The observation index, or `NULL` for every observation.

- prep:

  A `brms` prep object.

- ...:

  Not used.

## Value

The log likelihood of observation `i` for every draw, a delay drawn for
observation `i` for every draw, and the mean of the delay for every draw
and observation.
