# epidist 0.1.0.1000

Development version of `epidist`. As part of this release we have moved from @athowes maintaining the package (who led the initial package development, implementation of the S3 infrastructure, implementation of the core models, and wrote the first versions of the getting started vignette, Ebola case study, FAQ section, and the approximate inference vignette) to @seabbs maintaining the package.

## Models

- Added a marginalised likelihood model based on `primarycensored`. This can be specified using `as_epidist_marginal_model()`. This is currently limited to Weibull, log-normal, and gamma distributions with uniform primary censoring but this will be generalised in future releases. See #426.
- Added a `weight` argument to `as_epidist_marginal_model()` to allow for weighted data (for example count data) to be used in the marginal model. See #509.
- Added a `epidist_aggregate_data` method to `as_epidist_marginal_model()` to allow straightforward use of the marginal model with aggregated data. See #510.
- Added user settable primary event priors to the latent model. See #474.
- Added a marginalised likelihood to the latent model. See #474.

## Package

- Remove the default method for `epidist()`. See #473.
- Added `enforce_presence` argument to `epidist_prior()` to allow for priors to be
  specified if they do not match existing parameters. See #474.
- Added a `merge` argument to `epidist_prior()` to allow for not merging user and package priors. See #474.
- Generalised the Stan reparametrisation feature to work across all distributions without manual specification by generating Stan code with `brms` and then extracting the reparameterisation. See #474.
- Added a `transform_data` S3 method to allow for data to be transformed for specific models. This is specifically useful for the marginal model at the moment as it allows reducing the data to its unique strata. See #474.
- Added new `epidist_aggregate_data` class to handle pre-aggregated line list data
- Added a `as_epidist_linelist_data()` method for `epidist_aggregate_data` objects to allow for easy conversion to linelist data. See #510.

## Documentation

- Brings the README into line with `epinowcast` standards. See #467.
- Switched over to using the marginal model as default in the documentation. See #426.
- Added a helper functions for new variables to avoid code duplication in vignettes. See #426.

## Bugs

- Switched to using a patched of `primarycensored` that doesn't make use of `size()`. This fixes some Mac compilation edge cases. See #524.

# epidist 0.1.0

This is the first minor release of `epidist` intended for early test users of the package.
As some features may change, the package is marked as experimental.
We expect to release a stable 1.0.0 version shortly.

The `epidist` package implements models for epidemiological delay distributions.
It uses [`brms`](http://paulbuerkner.com/brms/) to perform Bayesian inference.

One data format is currently available:

1. The [linelist data](https://epidist.epinowcast.org/reference/index.html#linelist-data) format

Two statistical models are currently available:

1. The [naive model](https://epidist.epinowcast.org/reference/index.html#naive-model): which models the delay directly using `brms`
2. The [latent model](https://epidist.epinowcast.org/reference/index.html#latent-model): which implements a latent variable model to correct for biases in the data

The package is readily extensible to additional models via an [S3](https://adv-r.hadley.nz/s3.html) class based system.
In particular, model fitting with [epidist()] is possible using S3 classes for custom:

1. [Families](https://epidist.epinowcast.org/reference/index.html#family)
2. [Formula](https://epidist.epinowcast.org/reference/index.html#formula)
3. [Prior distributions](https://epidist.epinowcast.org/reference/index.html#prior-distributions)
4. [Stan code](https://epidist.epinowcast.org/reference/index.html#stan-code)

We provide functionality for [post-processing](https://epidist.epinowcast.org/reference/index.html#postprocess).
Alternatively, users may directly use `tidybayes` for specific families.

Three vignettes are available.
There is also a [frequently asked questions](https://epidist.epinowcast.org/articles/faq.html) section.
