# Primary event arguments for post-processing

The post-processing functions read distributional parameters by name, so
without this a fit made with a non-uniform primary event would be
post-processed as though it were uniform.

## Usage

``` r
.primary_args(spec, prep, i, draw = NULL)
```

## Arguments

- spec:

  A registry entry, as returned by
  [`.primary_spec()`](https://epidist.epinowcast.org/reference/dot-primary_spec.md).

- prep:

  A `brms` prep object.

- i:

  The observation index.

- draw:

  The posterior draw index, or `NULL` for all draws.

## Value

A named list of arguments for `spec$ddist` and `spec$rdist`.
