# Create the model-specific component of an `epidist` custom family

Create the model-specific component of an `epidist` custom family

## Usage

``` r
# S3 method for class 'epidist_latent_model'
epidist_family_model(data, family, ...)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

- family:

  Output of a call to
  [`brms::brmsfamily()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  with additional information as provided by
  [`.add_dpar_info()`](https://epidist.epinowcast.org/dev/reference/dot-add_dpar_info.md)

- ...:

  Additional arguments passed to method.

## See also

Other latent_model:
[`as_epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/as_epidist_latent_model.md),
[`as_epidist_latent_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/dev/reference/as_epidist_latent_model.epidist_aggregate_data.md),
[`as_epidist_latent_model.epidist_linelist_data()`](https://epidist.epinowcast.org/dev/reference/as_epidist_latent_model.epidist_linelist_data.md),
[`epidist_formula_model.epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/epidist_formula_model.epidist_latent_model.md),
[`epidist_model_prior.epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/epidist_model_prior.epidist_latent_model.md),
[`is_epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/is_epidist_latent_model.md),
[`new_epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/new_epidist_latent_model.md)
