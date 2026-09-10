# Prior distributions for the growth rate of summary rows

See
[`epidist_model_prior.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_model_prior.epidist_meta_model.md).

## Usage

``` r
.meta_growth_prior(data, formula, default = NULL)
```

## Arguments

- data:

  An `epidist_meta_model` object, optionally holding a `study` and a
  `growth_rate_sd` column.

- formula:

  A `brmsformula` object whose `family$dpars` say whether the model
  estimates `pgrowth`.

- default:

  The result of
  [`brms::default_prior()`](https://paulbuerkner.com/brms/reference/default_prior.html)
  on `data` and `formula`, or `NULL` to compute it here.

## Value

A `brmsprior` object, or `NULL` where no summary row estimates its
growth rate.
