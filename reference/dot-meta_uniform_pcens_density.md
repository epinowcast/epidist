# The density of a delay censored by a uniform primary window

Averaging the distribution function over a uniform primary window makes
the density of the primary censored delay the difference of the delay
distribution function across the window, divided by its width.

## Usage

``` r
.meta_uniform_pcens_density(y, dist, args, pwindow)
```

## Arguments

- y:

  The delay.

- dist:

  A `primarycensored` distribution function name.

- args:

  A named list of distribution parameters.

## Value

A density on the delay scale.
