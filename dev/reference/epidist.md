# Fit epidemiological delay distributions using a `brms` interface

Fit epidemiological delay distributions using a `brms` interface

## Usage

``` r
epidist(
  data,
  formula = mu ~ 1,
  family = lognormal(),
  prior = NULL,
  merge_priors = TRUE,
  fn = brms::brm,
  ...
)
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

- family:

  A description of the response distribution and link function to be
  used in the model. Every family function has a link argument allowing
  users to specify the link function to be applied on the response
  variable. If not specified, default links are used. For details of all
  supported families see `brmsfamily()`. Commonly used, such as
  [`lognormal()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  are also reexported as part of `epidist`.

- prior:

  One or more `brmsprior` objects created by
  [`brms::set_prior()`](https://paulbuerkner.com/brms/reference/set_prior.html)
  or related functions. These priors are passed to
  [`epidist_prior()`](https://epidist.epinowcast.org/dev/reference/epidist_prior.md)
  in the `prior` argument. Some models have default priors that are
  automatically added (see
  [`epidist_model_prior()`](https://epidist.epinowcast.org/dev/reference/epidist_model_prior.md)).
  These can be merged with user-provided priors using the `merge_priors`
  argument.

- merge_priors:

  If `TRUE` then merge user priors with default priors, if `FALSE` only
  use user priors. Defaults to `TRUE`. This may be useful if the built
  in approaches for merging priors are not flexible enough for a
  particular use case.

- fn:

  The internal function to be called. By default this is
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)
  which performs inference for the specified model. Other options are
  [`brms::make_stancode()`](https://paulbuerkner.com/brms/reference/stancode.html)
  which returns the Stan code for the specified model, or
  [`brms::make_standata()`](https://paulbuerkner.com/brms/reference/standata.html)
  which returns the data passed to Stan. These two later options may be
  useful for model debugging and extensions.

- ...:

  Additional arguments passed to `fn` method.

## Examples

``` r
fit <- sierra_leone_ebola_data |>
  as_epidist_linelist_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  ) |>
  as_epidist_aggregate_data() |>
  as_epidist_marginal_model() |>
  epidist(chains = 2, cores = 2, refresh = ifelse(interactive(), 250, 0))
#> ℹ No primary event upper bound provided, using the primary event lower bound + 1 day as the assumed upper bound.
#> ℹ No secondary event upper bound provided, using the secondary event lower bound + 1 day as the assumed upper bound.
#> ℹ No observation time column provided, using 2015-09-14 as the observation date (the maximum of the secondary event upper bound).
#> ! Setting 2394 observation times beyond 98 (=2x max delay) to Inf. This
#>   improves model efficiency by reducing unique observation times while
#>   maintaining model accuracy as these times should have negligible impact.
#> Warning: Found infinite values in the data, which may cause issues for Stan.
#> ℹ Data summarised by unique combinations of:
#> * Model variables: delay bounds, observation time, and primary censoring window
#> ! Reduced from 2453 to 272 rows.
#> ℹ This should improve model efficiency with no loss of information.
#> Warning: Found infinite values in the data, which may cause issues for Stan.
#> Warning: Found infinite values in the data, which may cause issues for Stan.
#> Compiling Stan program...
#> Start sampling

summary(fit)
#>  Family: marginal_lognormal 
#>   Links: mu = identity; sigma = log 
#> Formula: delay_lwr | weights(n) + vreal(relative_obs_time, pwindow, swindow, delay_upr) ~ 1 
#>          sigma ~ 1
#>    Data: transformed_data (Number of observations: 272) 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 2000
#> 
#> Regression Coefficients:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept           1.62      0.01     1.60     1.63 1.00     1655     1082
#> sigma_Intercept    -0.53      0.01    -0.54    -0.51 1.00     1679     1130
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```
