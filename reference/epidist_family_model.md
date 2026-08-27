# The model-specific parts of an `epidist_family()` call

The model-specific parts of an
[`epidist_family()`](https://epidist.epinowcast.org/reference/epidist_family.md)
call

## Usage

``` r
epidist_family_model(data, family, ...)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

- family:

  Output of a call to
  [`brms::brmsfamily()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  with additional information as provided by
  [`.add_dpar_info()`](https://epidist.epinowcast.org/reference/dot-add_dpar_info.md)

- ...:

  Additional arguments passed to `fn` method.

## Value

A `brms` custom family object.

## See also

Other family:
[`epidist_family()`](https://epidist.epinowcast.org/reference/epidist_family.md),
[`epidist_family_model.default()`](https://epidist.epinowcast.org/reference/epidist_family_model.default.md),
[`epidist_family_param()`](https://epidist.epinowcast.org/reference/epidist_family_param.md),
[`epidist_family_param.default()`](https://epidist.epinowcast.org/reference/epidist_family_param.default.md)
