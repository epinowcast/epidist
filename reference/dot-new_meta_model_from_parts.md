# Build an `epidist_meta_model` object from its individual and summary parts

Build an `epidist_meta_model` object from its individual and summary
parts

## Usage

``` r
.new_meta_model_from_parts(data, estimates, primary = "uniform")
```

## Arguments

- data:

  Individual level data prepared by
  [`.prepare_marginal_data()`](https://epidist.epinowcast.org/reference/dot-prepare_marginal_data.md),
  or `NULL`.

- estimates:

  An `epidist_estimates_data` object, or `NULL`.

- primary:

  The primary event distribution of the individual level rows,
  `"uniform"` or `"expgrowth"`.

## Value

An object of class `epidist_meta_model`.
