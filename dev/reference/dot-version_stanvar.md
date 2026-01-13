# Label a `epidist` Stan model with a version indicator

This function is used within
[`epidist_stancode()`](https://epidist.epinowcast.org/dev/reference/epidist_stancode.md)
to label the generated Stan code with the version of `epidist` used. To
view the full Stan code for any particular `epidist` model, we recommend
use of
[`brms::make_stancode()`](https://paulbuerkner.com/brms/reference/stancode.html).

## Usage

``` r
.version_stanvar()
```

## Value

A `brms` Stan chunk containing the `epidist` package version used to
build the Stan code.
