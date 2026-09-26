# A random delay generator for the non-parametric family

A random delay generator for the non-parametric family

## Usage

``` r
.np_rdist(np)
```

## Arguments

- np:

  The `np` element of the family, holding the boundaries, the basis and
  its coefficients.

## Value

A function of `n`, `i` and `prep`, as
[`primarycensored::rpcens()`](https://primarycensored.epinowcast.org/reference/rprimarycensored.html)
calls it, returning `n` delays, the delay of each draw in turn, recycled
when `n` is more than the number of draws.
