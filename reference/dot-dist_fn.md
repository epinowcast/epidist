# The functions of a `primarycensored` distribution name

`.pdist()` gives the distribution function, `.ddist()` the density and
`.qdist()` the quantile function, all found by swapping the leading `p`
of the name.

## Usage

``` r
.dist_fn(dist, type)

.pdist(dist)

.ddist(dist)

.qdist(dist)
```

## Arguments

- dist:

  A `primarycensored` distribution function name, for example
  `"plnorm"`.

- type:

  One of `"p"`, `"d"` or `"q"`.

## Value

The function of that name from the package that provides the
distribution.
