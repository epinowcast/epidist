# Central moments of the delay distributions with analytic summaries

Each returns the mean, the variance and the third and fourth central
moments. They mirror the Stan functions of the same name in
`inst/stan/meta_model/functions.stan`. The weibull and generalised gamma
share `.meta_moments_scaled()`, since their raw moments are \\E\[T^r\] =
scale^r g_r\\.

## Usage

``` r
.meta_moments_lognormal(args)

.meta_moments_gamma(args)

.meta_moments_scaled(scale, g)

.meta_moments_weibull(args)

.meta_moments_gengamma(args)

.meta_moments_np(args)
```

## Arguments

- args:

  A named list of distribution parameters.

- scale:

  The scale of the distribution.

- g:

  The ratios \\g_1, \dots, g_4\\ of the raw moments.

## Value

A numeric vector of length four.
