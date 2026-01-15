# Package index

## Data preparation

### Linelist data

Functions for preparing linelist data

- [`as_epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.md)
  : Create an epidist_linelist_data object

- [`as_epidist_linelist_data(`*`<data.frame>`*`)`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.data.frame.md)
  : Create an epidist_linelist_data object from a data frame with event
  dates

- [`as_epidist_linelist_data(`*`<default>`*`)`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.default.md)
  : Create an epidist_linelist_data object from vectors of event times

- [`as_epidist_linelist_data(`*`<epidist_aggregate_data>`*`)`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.epidist_aggregate_data.md)
  : Convert aggregate data to linelist format

- [`assert_epidist(`*`<epidist_linelist_data>`*`)`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_linelist_data.md)
  :

  Assert validity of `epidist_linelist_data` objects

- [`is_epidist_linelist_data()`](https://epidist.epinowcast.org/reference/is_epidist_linelist_data.md)
  :

  Check if data has the `epidist_linelist_data` class

- [`new_epidist_linelist_data()`](https://epidist.epinowcast.org/reference/new_epidist_linelist_data.md)
  :

  Class constructor for `epidist_linelist_data` objects

### Aggregate data

Functions for preparing aggregate data

- [`as_epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_aggregate_data.md)
  : Create an epidist_aggregate_data object

- [`as_epidist_aggregate_data(`*`<data.frame>`*`)`](https://epidist.epinowcast.org/reference/as_epidist_aggregate_data.data.frame.md)
  : Create an epidist_aggregate_data object from a data.frame

- [`as_epidist_aggregate_data(`*`<default>`*`)`](https://epidist.epinowcast.org/reference/as_epidist_aggregate_data.default.md)
  : Create an epidist_aggregate_data object from vectors of event times

- [`as_epidist_aggregate_data(`*`<epidist_linelist_data>`*`)`](https://epidist.epinowcast.org/reference/as_epidist_aggregate_data.epidist_linelist_data.md)
  : Convert linelist data to aggregate format

- [`assert_epidist(`*`<epidist_aggregate_data>`*`)`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_aggregate_data.md)
  :

  Assert validity of `epidist_aggregate_data` objects

- [`is_epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/is_epidist_aggregate_data.md)
  :

  Check if data has the `epidist_aggregate_data` class

- [`new_epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/new_epidist_aggregate_data.md)
  :

  Class constructor for `epidist_aggregate_data` objects

## Models

### Naive model

Specific methods for the naive model

- [`as_epidist_naive_model()`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.md)
  :

  Convert an object to an `epidist_naive_model` object

- [`as_epidist_naive_model(`*`<epidist_aggregate_data>`*`)`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.epidist_aggregate_data.md)
  :

  The naive model method for `epidist_aggregate_data` objects

- [`as_epidist_naive_model(`*`<epidist_linelist_data>`*`)`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.epidist_linelist_data.md)
  :

  The naive model method for `epidist_linelist_data` objects

- [`epidist_formula_model(`*`<epidist_naive_model>`*`)`](https://epidist.epinowcast.org/reference/epidist_formula_model.epidist_naive_model.md)
  :

  Define the model-specific component of an `epidist` custom formula for
  the naive model

- [`epidist_transform_data_model(`*`<epidist_naive_model>`*`)`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.epidist_naive_model.md)
  : Transform data for the naive model

- [`is_epidist_naive_model()`](https://epidist.epinowcast.org/reference/is_epidist_naive_model.md)
  :

  Check if data has the `epidist_naive_model` class

- [`new_epidist_naive_model()`](https://epidist.epinowcast.org/reference/new_epidist_naive_model.md)
  :

  Class constructor for `epidist_naive_model` objects

### Latent model

Specific methods for the latent model

- [`as_epidist_latent_model()`](https://epidist.epinowcast.org/reference/as_epidist_latent_model.md)
  :

  Convert an object to an `epidist_latent_model` object

- [`as_epidist_latent_model(`*`<epidist_aggregate_data>`*`)`](https://epidist.epinowcast.org/reference/as_epidist_latent_model.epidist_aggregate_data.md)
  :

  The latent model method for `epidist_aggregate_data` objects

- [`as_epidist_latent_model(`*`<epidist_linelist_data>`*`)`](https://epidist.epinowcast.org/reference/as_epidist_latent_model.epidist_linelist_data.md)
  :

  The latent model method for `epidist_linelist_data` objects

- [`epidist_family_model(`*`<epidist_latent_model>`*`)`](https://epidist.epinowcast.org/reference/epidist_family_model.epidist_latent_model.md)
  :

  Create the model-specific component of an `epidist` custom family

- [`epidist_formula_model(`*`<epidist_latent_model>`*`)`](https://epidist.epinowcast.org/reference/epidist_formula_model.epidist_latent_model.md)
  :

  Define the model-specific component of an `epidist` custom formula for
  the latent model

- [`epidist_model_prior(`*`<epidist_latent_model>`*`)`](https://epidist.epinowcast.org/reference/epidist_model_prior.epidist_latent_model.md)
  : Model specific prior distributions for latent models

- [`is_epidist_latent_model()`](https://epidist.epinowcast.org/reference/is_epidist_latent_model.md)
  :

  Check if data has the `epidist_latent_model` class

- [`new_epidist_latent_model()`](https://epidist.epinowcast.org/reference/new_epidist_latent_model.md)
  :

  Class constructor for `epidist_latent_model` objects

### Marginal model

Specific methods for the marginal model

- [`as_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.md)
  :

  Convert an object to an `epidist_marginal_model` object

- [`as_epidist_marginal_model(`*`<epidist_aggregate_data>`*`)`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.epidist_aggregate_data.md)
  :

  The marginal model method for `epidist_aggregate_data` objects

- [`as_epidist_marginal_model(`*`<epidist_linelist_data>`*`)`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.epidist_linelist_data.md)
  :

  The marginal model method for `epidist_linelist_data` objects

- [`epidist_family_model(`*`<epidist_marginal_model>`*`)`](https://epidist.epinowcast.org/reference/epidist_family_model.epidist_marginal_model.md)
  :

  Create the model-specific component of an `epidist` custom family

- [`epidist_formula_model(`*`<epidist_marginal_model>`*`)`](https://epidist.epinowcast.org/reference/epidist_formula_model.epidist_marginal_model.md)
  :

  Define the model-specific component of an `epidist` custom formula for
  the marginal model

- [`epidist_transform_data_model(`*`<epidist_marginal_model>`*`)`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.epidist_marginal_model.md)
  : Transform data for the marginal model

- [`is_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/is_epidist_marginal_model.md)
  :

  Check if data has the `epidist_marginal_model` class

- [`new_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/new_epidist_marginal_model.md)
  :

  Class constructor for `epidist_marginal_model` objects

## Model fitting

### Model fitting

Functions for fitting delay distribution models using `brms`

- [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md) :

  Fit epidemiological delay distributions using a `brms` interface

### Postprocess

Functions for postprocessing model output

- [`add_mean_sd()`](https://epidist.epinowcast.org/reference/add_mean_sd.md)
  : Add natural scale mean and standard deviation parameters
- [`add_mean_sd(`*`<default>`*`)`](https://epidist.epinowcast.org/reference/add_mean_sd.default.md)
  : Default method for add natural scale parameters
- [`add_mean_sd(`*`<gamma_samples>`*`)`](https://epidist.epinowcast.org/reference/add_mean_sd.gamma_samples.md)
  : Add natural scale mean and standard deviation parameters for a Gamma
  model
- [`add_mean_sd(`*`<lognormal_samples>`*`)`](https://epidist.epinowcast.org/reference/add_mean_sd.lognormal_samples.md)
  : Add natural scale mean and standard deviation parameters for a
  lognormal model
- [`add_mean_sd(`*`<weibull_samples>`*`)`](https://epidist.epinowcast.org/reference/add_mean_sd.weibull_samples.md)
  : Add natural scale mean and standard deviation parameters for a
  Weibull model
- [`predict_delay_parameters()`](https://epidist.epinowcast.org/reference/predict_delay_parameters.md)
  [`predict_dpar()`](https://epidist.epinowcast.org/reference/predict_delay_parameters.md)
  : Extract samples of the delay distribution parameters

### Diagnostic functions

- [`epidist_diagnostics()`](https://epidist.epinowcast.org/reference/epidist_diagnostics.md)
  :

  Diagnostics for `epidist_fit` models

## Model specification

### Family

Functions related to specifying custom `brms` families

- [`epidist_family()`](https://epidist.epinowcast.org/reference/epidist_family.md)
  :

  Define `epidist` family

- [`epidist_family_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.md)
  [`epidist_formula_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.md)
  :

  The model-specific parts of an
  [`epidist_family()`](https://epidist.epinowcast.org/reference/epidist_family.md)
  call

- [`epidist_family_model(`*`<default>`*`)`](https://epidist.epinowcast.org/reference/epidist_family_model.default.md)
  : Default method for defining a model specific family

- [`epidist_family_param()`](https://epidist.epinowcast.org/reference/epidist_family_param.md)
  :

  Reparameterise an `epidist` family to align `brms` and Stan

- [`epidist_family_param(`*`<default>`*`)`](https://epidist.epinowcast.org/reference/epidist_family_param.default.md)
  : Default method for families which do not require a
  reparameterisation

### Formula

Functions related to specifying custom `brms` formula

- [`epidist_family_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.md)
  [`epidist_formula_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.md)
  :

  The model-specific parts of an
  [`epidist_family()`](https://epidist.epinowcast.org/reference/epidist_family.md)
  call

- [`epidist_formula()`](https://epidist.epinowcast.org/reference/epidist_formula.md)
  : Define a model specific formula

- [`epidist_formula_model(`*`<default>`*`)`](https://epidist.epinowcast.org/reference/epidist_formula_model.default.md)
  : Default method for defining a model specific formula

### Prior distributions

Functions for specifying prior distributions

- [`epidist_family_prior()`](https://epidist.epinowcast.org/reference/epidist_family_prior.md)
  : Family specific prior distributions
- [`epidist_family_prior(`*`<default>`*`)`](https://epidist.epinowcast.org/reference/epidist_family_prior.default.md)
  : Default family specific prior distributions
- [`epidist_family_prior(`*`<lognormal>`*`)`](https://epidist.epinowcast.org/reference/epidist_family_prior.lognormal.md)
  : Family specific prior distributions for the lognormal family
- [`epidist_model_prior()`](https://epidist.epinowcast.org/reference/epidist_model_prior.md)
  : Model specific prior distributions
- [`epidist_model_prior(`*`<default>`*`)`](https://epidist.epinowcast.org/reference/epidist_model_prior.default.md)
  : Default model specific prior distributions
- [`epidist_prior()`](https://epidist.epinowcast.org/reference/epidist_prior.md)
  : Define custom prior distributions for epidist models

### Stan code

Functions for specifying custom Stan code to put into `brms`

- [`epidist_stancode()`](https://epidist.epinowcast.org/reference/epidist_stancode.md)
  : Define model specific Stan code
- [`epidist_stancode(`*`<default>`*`)`](https://epidist.epinowcast.org/reference/epidist_stancode.default.md)
  : Default method for defining model specific Stan code

## Package internals

### Transform data

Transform data using the formula and family information

- [`epidist_transform_data()`](https://epidist.epinowcast.org/reference/epidist_transform_data.md)
  : Transform data for an epidist model

- [`epidist_transform_data_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.md)
  :

  The model-specific parts of an
  [`epidist_transform_data()`](https://epidist.epinowcast.org/reference/epidist_transform_data.md)
  call

- [`epidist_transform_data_model(`*`<default>`*`)`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.default.md)
  : Default method for transforming data for a model

### Generator model functions

Generator functions for creating family specific model utilities.

- [`epidist_gen_log_lik()`](https://epidist.epinowcast.org/reference/epidist_gen_log_lik.md)
  : Create a function to calculate the marginalised log likelihood for
  double censored and truncated delay distributions
- [`epidist_gen_posterior_epred()`](https://epidist.epinowcast.org/reference/epidist_gen_posterior_epred.md)
  : Create a function to draw from the expected value of the posterior
  predictive distribution for a model
- [`epidist_gen_posterior_predict()`](https://epidist.epinowcast.org/reference/epidist_gen_posterior_predict.md)
  : Create a function to draw from the posterior predictive distribution
  for a double censored and truncated delay distribution

### Assert validity of objects

Functions used to assert the validity of package objects

- [`assert_epidist()`](https://epidist.epinowcast.org/reference/assert_epidist.md)
  : Validation for epidist objects

## Simulation and Data

### Simulation

Tools for simulating datasets

- [`simulate_exponential_cases()`](https://epidist.epinowcast.org/reference/simulate_exponential_cases.md)
  : Simulate exponential cases
- [`simulate_gillespie()`](https://epidist.epinowcast.org/reference/simulate_gillespie.md)
  : Simulate cases from a stochastic SIR model
- [`simulate_secondary()`](https://epidist.epinowcast.org/reference/simulate_secondary.md)
  : Simulate secondary events based on a delay distribution
- [`simulate_uniform_cases()`](https://epidist.epinowcast.org/reference/simulate_uniform_cases.md)
  : Simulate cases from a uniform distribution

### Data

Data included with the package

- [`sierra_leone_ebola_data`](https://epidist.epinowcast.org/reference/sierra_leone_ebola_data.md)
  : Ebola linelist data from Fang et al. (2016)

## Reexported functions

- [`reexports`](https://epidist.epinowcast.org/reference/reexports.md)
  [`lognormal`](https://epidist.epinowcast.org/reference/reexports.md)
  [`weibull`](https://epidist.epinowcast.org/reference/reexports.md)
  [`Gamma`](https://epidist.epinowcast.org/reference/reexports.md)
  [`bf`](https://epidist.epinowcast.org/reference/reexports.md) :
  Objects exported from other packages
