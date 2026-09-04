# Fill in the optional columns of an `epidist_estimates_data` object

Applies the documented defaults for study metadata that was not
supplied, informing the user about each assumption made on their behalf.

## Usage

``` r
.fill_estimates_defaults(data)
```

## Arguments

- data:

  A `data.frame` containing at least `study`, `type` and `value`.

## Value

The input with all of `.estimates_required_cols()` present.
