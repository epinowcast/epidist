# Additional distributional parameter information for `brms` families

Includes additional information (link functions and parameter bound)
about the distributional parameters of a `brms` family which are not the
conditional mean `mu`.

## Usage

``` r
.add_dpar_info(family)
```

## Arguments

- family:

  A description of the response distribution and link function to be
  used in the model. Every family function has a link argument allowing
  users to specify the link function to be applied on the response
  variable. If not specified, default links are used. For details of all
  supported families see `brmsfamily()`. Commonly used, such as
  [`lognormal()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  are also reexported as part of `epidist`.
