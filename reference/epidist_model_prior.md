# Model specific prior distributions

This function contains `brms` prior distributions which are specific to
particular `epidist` models e.g. the `latent_lognormal` model.

## Usage

``` r
epidist_model_prior(data, ...)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

- ...:

  Additional arguments passed to `fn` method.

## See also

Other prior:
[`epidist_family_prior()`](https://epidist.epinowcast.org/reference/epidist_family_prior.md),
[`epidist_family_prior.default()`](https://epidist.epinowcast.org/reference/epidist_family_prior.default.md),
[`epidist_family_prior.lognormal()`](https://epidist.epinowcast.org/reference/epidist_family_prior.lognormal.md),
[`epidist_model_prior.default()`](https://epidist.epinowcast.org/reference/epidist_model_prior.default.md),
[`epidist_prior()`](https://epidist.epinowcast.org/reference/epidist_prior.md)
