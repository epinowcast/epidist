# Build the Stan functions block shared by the marginal and meta models

Both models read a `functions.stan` chunk with the same placeholders
(`family`, `dist_id`, `dpars_A`, `dpars_B`, and the pair
`primary_id, primary_params`), filled in with the target distribution's
details and the primary event distribution. Used within
[`epidist_stancode()`](https://epidist.epinowcast.org/reference/epidist_stancode.md)
methods for the marginal and meta models, which differ only in the chunk
path, the family name prefix, and any further placeholders they need
substituted.

## Usage

``` r
.family_functions_stanvar(
  chunk_path,
  family,
  family_prefix,
  primary = .primary_spec("uniform"),
  extra = character()
)
```

## Arguments

- chunk_path:

  Path within the `stan/` folder to the functions chunk.

- family:

  The `epidist` family object.

- family_prefix:

  The model specific prefix stripped from `family$name`, for example
  `"marginal_"` or `"meta_"`.

- primary:

  A primary event registry entry, as returned by
  [`.primary_spec()`](https://epidist.epinowcast.org/reference/dot-primary_spec.md).
  Defaults to the uniform primary event.

- extra:

  A named character vector of further placeholder substitutions, applied
  after the shared ones.

## Value

A `brms` `stanvars` object holding the substituted functions chunk.
