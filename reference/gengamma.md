# Generalised gamma delay distribution family

A `brms` custom family for the generalised gamma distribution in the
Prentice (1974) parameterisation of
[`flexsurv::dgengamma()`](http://chjackson.github.io/flexsurv-dev/reference/GenGamma.md).
`mu` and `sigma` are the location and scale of the log delay and `Q` is
a positive shape. The Weibull (`Q = 1`) and gamma (`Q = sigma`) families
are special cases and the lognormal is the limit as `Q` goes to zero. It
needs the `flexsurv` package and works with every `epidist` model.

## Usage

``` r
gengamma(link = "identity", link_sigma = "log", link_Q = "log")
```

## Arguments

- link, link_sigma, link_Q:

  The link functions of `mu`, `sigma` and `Q`.

## Value

A `brms` custom family object.

## References

Prentice, R. L. (1974). A log gamma model and its maximum likelihood
estimation. Biometrika, 61(3), 539-544.
[doi:10.1093/biomet/61.3.539](https://doi.org/10.1093/biomet/61.3.539)

## See also

Other family:
[`epidist_family()`](https://epidist.epinowcast.org/reference/epidist_family.md),
[`epidist_family_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.md),
[`epidist_family_model.default()`](https://epidist.epinowcast.org/reference/epidist_family_model.default.md),
[`epidist_family_param()`](https://epidist.epinowcast.org/reference/epidist_family_param.md),
[`epidist_family_param.default()`](https://epidist.epinowcast.org/reference/epidist_family_param.default.md),
[`epidist_family_param.gengamma()`](https://epidist.epinowcast.org/reference/epidist_family_param.gengamma.md)

## Examples

``` r
gengamma()
#> 
#> Custom family: gengamma 
#> Link function: identity 
#> Parameters: mu, sigma, Q 
#> 
```
