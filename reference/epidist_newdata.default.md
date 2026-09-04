# Default method for building `newdata`

Default method for building `newdata`

## Usage

``` r
# Default S3 method
epidist_newdata(data, ...)
```

## Arguments

- data:

  An `epidist` data object, such as one returned by
  [`as_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.md),
  [`as_epidist_latent_model()`](https://epidist.epinowcast.org/reference/as_epidist_latent_model.md),
  [`as_epidist_naive_model()`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.md)
  or
  [`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md).

- ...:

  Variables to expand into a grid, passed to
  [`tidyr::expand()`](https://tidyr.tidyverse.org/reference/expand.html).
  Supply the variables used in the model formula, such as `sex`. Each
  combination of their unique values becomes a row. Supply no variables
  to get a single row, which is what an intercept only model needs. A
  variable expanded here keeps its expanded values, so naming it as an
  argument of the method as well is an error.

## Value

This method errors. It is called when `data` is not an `epidist` model
data object.

## See also

Other newdata:
[`epidist_newdata()`](https://epidist.epinowcast.org/reference/epidist_newdata.md),
[`epidist_newdata.epidist_latent_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_latent_model.md),
[`epidist_newdata.epidist_marginal_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_marginal_model.md),
[`epidist_newdata.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_meta_model.md),
[`epidist_newdata.epidist_naive_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_naive_model.md)
