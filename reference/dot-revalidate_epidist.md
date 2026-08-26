# Re-check a modified `epidist` object and drop any classes it fails

Checks `data` against each of its `epidist` classes using
[`.check_epidist_class()`](https://epidist.epinowcast.org/reference/dot-check_epidist_class.md)
and drops those it no longer meets the requirements of. Modifications
that leave the object unchanged are not checked. Used by the methods
documented in
[epidist_data](https://epidist.epinowcast.org/reference/epidist_data.md).

## Usage

``` r
.revalidate_epidist(data, original = NULL)
```

## Arguments

- data:

  A modified `epidist` object. An object with no columns is never valid,
  and is unclassed without a warning because it is usually the prototype
  `vctrs` takes rather than something the user asked for.

- original:

  The object before it was modified. Checking is skipped when the
  modification left the object unchanged.

## Value

`data` with any `epidist` class whose requirements it no longer meets
removed.
