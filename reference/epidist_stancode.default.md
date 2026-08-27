# Default method for defining model specific Stan code

Default method for defining model specific Stan code

## Usage

``` r
# Default S3 method
epidist_stancode(data, ...)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

- ...:

  Additional arguments passed to `fn` method.

## Value

A list of `stanvars` objects, or `NULL` when none are needed.

## See also

Other stan:
[`epidist_stancode()`](https://epidist.epinowcast.org/reference/epidist_stancode.md)
