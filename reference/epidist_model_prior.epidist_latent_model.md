# Model specific prior distributions for latent models

Defines prior distributions for the latent model parameters
`pwindow_raw` and `swindow_raw` which control the width of the
observation windows.

## Usage

``` r
# S3 method for class 'epidist_latent_model'
epidist_model_prior(data, formula, ...)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

- formula:

  An object of class
  [stats::formula](https://rdrr.io/r/stats/formula.html) or
  [brms::brmsformula](https://paulbuerkner.com/brms/reference/brmsformula.html)
  (or one that can be coerced to those classes). A symbolic description
  of the model to be fitted. A formula must be provided for the
  distributional parameter `mu`, and may optionally be provided for
  other distributional parameters.

- ...:

  Additional arguments passed to `fn` method.

## See also

Other latent_model:
[`as_epidist_latent_model()`](https://epidist.epinowcast.org/reference/as_epidist_latent_model.md),
[`as_epidist_latent_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_latent_model.epidist_aggregate_data.md),
[`as_epidist_latent_model.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_latent_model.epidist_linelist_data.md),
[`epidist_family_model.epidist_latent_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.epidist_latent_model.md),
[`epidist_formula_model.epidist_latent_model()`](https://epidist.epinowcast.org/reference/epidist_formula_model.epidist_latent_model.md),
[`is_epidist_latent_model()`](https://epidist.epinowcast.org/reference/is_epidist_latent_model.md),
[`new_epidist_latent_model()`](https://epidist.epinowcast.org/reference/new_epidist_latent_model.md)
