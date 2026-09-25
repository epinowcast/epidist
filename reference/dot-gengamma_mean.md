# The mean and standard deviation of the generalised gamma distribution

From the raw moments \\E\[T^r\] = scale^r \Gamma(k + r / shape) /
\Gamma(k)\\ of the Stacy form.

## Usage

``` r
.gengamma_mean(scale, shape, k)

.gengamma_sd(scale, shape, k)
```

## Arguments

- scale, shape, k:

  Parameters of
  [`flexsurv::dgengamma.orig()`](http://chjackson.github.io/flexsurv-dev/reference/GenGamma.orig.md).

## Value

A numeric vector.
