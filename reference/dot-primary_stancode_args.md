# The primary event distribution arguments for Stan

The primary event distribution arguments for Stan

## Usage

``` r
.primary_stancode_args(spec, empty = "primary_params")
```

## Arguments

- spec:

  A registry entry, as returned by
  [`.primary_spec()`](https://epidist.epinowcast.org/reference/dot-primary_spec.md).

- empty:

  The Stan expression to pass when the distribution takes no parameters.

## Value

The distribution id and its parameters, as Stan code.
