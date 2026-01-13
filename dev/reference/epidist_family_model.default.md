# Default method for defining a model specific family

Default method for defining a model specific family

## Usage

``` r
# Default S3 method
epidist_family_model(data, family, ...)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

- family:

  Output of a call to
  [`brms::brmsfamily()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  with additional information as provided by
  [`.add_dpar_info()`](https://epidist.epinowcast.org/dev/reference/dot-add_dpar_info.md)

- ...:

  Additional arguments passed to `fn` method.

## See also

Other family:
[`epidist_family()`](https://epidist.epinowcast.org/dev/reference/epidist_family.md),
[`epidist_family_model()`](https://epidist.epinowcast.org/dev/reference/epidist_family_model.md),
[`epidist_family_param()`](https://epidist.epinowcast.org/dev/reference/epidist_family_param.md),
[`epidist_family_param.default()`](https://epidist.epinowcast.org/dev/reference/epidist_family_param.default.md)
