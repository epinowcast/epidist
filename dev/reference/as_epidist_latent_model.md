# Convert an object to an `epidist_latent_model` object

Creates an `epidist_latent_model` object from various input formats.
This enables fitting latent variable models for epidemiological delays
using
[`epidist()`](https://epidist.epinowcast.org/dev/reference/epidist.md),
as described in Park et al. (2024) and Charniga et al. (2024) The latent
model approach accounts for double interval censoring and right
truncation in delay data.

## Usage

``` r
as_epidist_latent_model(data, ...)
```

## Arguments

- data:

  An object to be converted to the class `epidist_latent_model`

- ...:

  Additional arguments passed to methods.

## References

- [Park et al. (2024)](https://doi.org/10.1101/2024.01.12.24301247)

- [Charniga et al. (2024)](https://doi.org/10.1371/journal.pcbi.1012520)

## See also

Other latent_model:
[`as_epidist_latent_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/dev/reference/as_epidist_latent_model.epidist_aggregate_data.md),
[`as_epidist_latent_model.epidist_linelist_data()`](https://epidist.epinowcast.org/dev/reference/as_epidist_latent_model.epidist_linelist_data.md),
[`epidist_family_model.epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/epidist_family_model.epidist_latent_model.md),
[`epidist_formula_model.epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/epidist_formula_model.epidist_latent_model.md),
[`epidist_model_prior.epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/epidist_model_prior.epidist_latent_model.md),
[`is_epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/is_epidist_latent_model.md),
[`new_epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/new_epidist_latent_model.md)
