# Define model specific Stan code

This function is used within
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md) to
create any custom Stan code which is injected into `brms` via the
`stanvars` argument. It is unlikely that as a user you will need this
function, but we export it nonetheless to be transparent about what
exactly is happening inside of a call to
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

## Usage

``` r
epidist_stancode(data, ...)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

- ...:

  Additional arguments passed to `fn` method.

## See also

Other stan:
[`epidist_stancode.default()`](https://epidist.epinowcast.org/reference/epidist_stancode.default.md)
