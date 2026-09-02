# Class constructor for `epidist_latent_model` objects

Class constructor for `epidist_latent_model` objects

## Usage

``` r
new_epidist_latent_model(data, primary = .primary_choices(), ...)
```

## Arguments

- data:

  An object to be set with the class `epidist_latent_model`

- primary:

  The distribution of the primary event within its censoring window.
  `"uniform"`, the default, assumes it is equally likely at any point.
  `"expgrowth"` tilts it, with the growth rate estimated as the
  `pgrowth` distributional parameter.

- ...:

  Additional arguments passed to methods.

## Value

An object of class `epidist_latent_model`

## See also

Other latent_model:
[`as_epidist_latent_model()`](https://epidist.epinowcast.org/reference/as_epidist_latent_model.md),
[`as_epidist_latent_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_latent_model.epidist_aggregate_data.md),
[`as_epidist_latent_model.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_latent_model.epidist_linelist_data.md),
[`epidist_family_model.epidist_latent_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.epidist_latent_model.md),
[`epidist_formula_model.epidist_latent_model()`](https://epidist.epinowcast.org/reference/epidist_formula_model.epidist_latent_model.md),
[`epidist_model_prior.epidist_latent_model()`](https://epidist.epinowcast.org/reference/epidist_model_prior.epidist_latent_model.md),
[`epidist_newdata.epidist_latent_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_latent_model.md),
[`is_epidist_latent_model()`](https://epidist.epinowcast.org/reference/is_epidist_latent_model.md)
