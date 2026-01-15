# Read in an `epidist` Stan code chunk

This function is used to obtain Stan code chunks from the `stan/` folder
of the `epidist` package. It is used within the
[`epidist_stancode()`](https://epidist.epinowcast.org/reference/epidist_stancode.md)
function.

## Usage

``` r
.stan_chunk(path)
```

## Arguments

- path:

  The path within the `stan/` folder of the installed `epidist` package
  to the Stan code chunk of interest.

## Value

A character string containing the Stan code chunk of interest.
