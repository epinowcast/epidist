# The `primarycensored` distribution name for a family

Falls back to the lower cased family name if `primarycensored` does not
recognise it, so the caller can still report a name in a message.

## Usage

``` r
.pcd_family_dist_name(family)
```

## Arguments

- family:

  A description of the response distribution and link function to be
  used in the model. Every family function has a link argument allowing
  users to specify the link function to be applied on the response
  variable. If not specified, default links are used. For details of all
  supported families see
  [`brms::brmsfamily()`](https://paulbuerkner.com/brms/reference/brmsfamily.html).
  Commonly used, such as
  [`brms::lognormal()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  are also reexported as part of `epidist`.

## Value

A `primarycensored` distribution function name, for example `"plnorm"`.
