# The largest correlation the joint moment likelihood will use

The asymptotic correlation between a sample mean and a sample standard
deviation is the skewness over the square root of the excess kurtosis,
and \\\kappa \geq \gamma_1^2 + 1\\ holds for every distribution, so it
never leaves \\\[-1, 1\]\\. Moments taken from a discrete grid or from
quadrature can sit a little outside that bound, which would make the
covariance matrix singular, so the correlation is held inside it.

## Usage

``` r
.meta_max_correlation()
```

## Value

A correlation.

## Details

Matches the value hard coded in `inst/stan/meta_model/functions.stan` so
that the R and Stan implementations agree.
