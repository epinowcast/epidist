# Build `newdata` for the naive model

The naive model accounts for neither censoring nor truncation, so the
only variable it uses beyond those in the model formula is the response.
This method adds it, set to `NA` because it is the quantity being
predicted.

## Usage

``` r
# S3 method for class 'epidist_naive_model'
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

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
of `newdata` ready to predict from.

## See also

Other naive_model:
[`as_epidist_naive_model()`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.md),
[`as_epidist_naive_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.epidist_aggregate_data.md),
[`as_epidist_naive_model.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.epidist_linelist_data.md),
[`epidist_formula_model.epidist_naive_model()`](https://epidist.epinowcast.org/reference/epidist_formula_model.epidist_naive_model.md),
[`epidist_transform_data_model.epidist_naive_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.epidist_naive_model.md),
[`is_epidist_naive_model()`](https://epidist.epinowcast.org/reference/is_epidist_naive_model.md),
[`new_epidist_naive_model()`](https://epidist.epinowcast.org/reference/new_epidist_naive_model.md)

Other newdata:
[`epidist_newdata()`](https://epidist.epinowcast.org/reference/epidist_newdata.md),
[`epidist_newdata.default()`](https://epidist.epinowcast.org/reference/epidist_newdata.default.md),
[`epidist_newdata.epidist_latent_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_latent_model.md),
[`epidist_newdata.epidist_marginal_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_marginal_model.md),
[`epidist_newdata.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_meta_model.md)

## Examples

``` r
prep_obs <- sierra_leone_ebola_data |>
  as_epidist_linelist_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  ) |>
  as_epidist_naive_model()
#> ℹ No primary event upper bound provided, using the primary event lower bound + 1 day as the assumed upper bound.
#> ℹ No secondary event upper bound provided, using the secondary event lower bound + 1 day as the assumed upper bound.
#> ℹ No observation time column provided, using 2015-09-14 as the observation date (the maximum of the secondary event upper bound).

# A row for each sex
epidist_newdata(prep_obs, sex)
#> # A tibble: 3 × 2
#>   sex    delay
#>   <chr>  <dbl>
#> 1 Female    NA
#> 2 Male      NA
#> 3 NA        NA
```
