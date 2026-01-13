# Default method for families which do not require a reparameterisation

This function extracts the Stan parameterisation for a given brms family
by creating a dummy model and parsing its Stan code. It looks for the
log probability density function (lpdf) call in the Stan code and
extracts the parameter order used by Stan. This is needed because brms
and Stan may use different parameter orderings for the same
distribution.

## Usage

``` r
# Default S3 method
epidist_family_param(family, ...)
```

## Arguments

- family:

  A brms family object containing at minimum a `family` element
  specifying the distribution family name.

- ...:

  Additional arguments passed to methods (not used)

## Value

The input family object with an additional `param` element containing
the Stan parameter ordering as a string

## Details

The function works by:

1.  Creating a minimal dummy model using the specified family

2.  Extracting the Stan code for this model

3.  Finding the lpdf function call for the family

4.  Parsing out the parameter ordering used in Stan

5.  Adding this as the `param` element to the family object

## See also

Other family:
[`epidist_family()`](https://epidist.epinowcast.org/dev/reference/epidist_family.md),
[`epidist_family_model()`](https://epidist.epinowcast.org/dev/reference/epidist_family_model.md),
[`epidist_family_model.default()`](https://epidist.epinowcast.org/dev/reference/epidist_family_model.default.md),
[`epidist_family_param()`](https://epidist.epinowcast.org/dev/reference/epidist_family_param.md)
