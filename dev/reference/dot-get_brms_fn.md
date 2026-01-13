# Get a brms function by prefix and family

Helper function to get internal brms functions by constructing their
name from a prefix and family. Used to get functions like `log_lik_*`,
`posterior_predict_*` etc.

## Usage

``` r
.get_brms_fn(prefix, family)
```

## Arguments

- prefix:

  Character string prefix of the brms function to get (e.g. "log_lik")

- family:

  A description of the response distribution and link function to be
  used in the model. Every family function has a link argument allowing
  users to specify the link function to be applied on the response
  variable. If not specified, default links are used. For details of all
  supported families see `brmsfamily()`. Commonly used, such as
  [`lognormal()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  are also reexported as part of `epidist`.

## Value

The requested brms function
