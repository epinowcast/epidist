# Replace `brms` prior distributions

This function takes an existing set of prior distributions and updates
them with new prior specifications. It matches priors based on their
parameter class, coefficient, group, response, distributional parameter,
and non-linear parameter. Any new priors that don't match existing ones
can optionally trigger a warning.

## Usage

``` r
.replace_prior(
  old_prior,
  prior,
  warn = FALSE,
  merge = TRUE,
  enforce_presence = TRUE
)
```

## Arguments

- old_prior:

  One or more prior distributions in the class `brmsprior` to be
  updated.

- prior:

  One or more prior distributions in the class `brmsprior` containing
  the new specifications. Can include custom set priors using the syntax
  `parameter ~ distribution`

- warn:

  If `TRUE` then a warning will be displayed if a prior in `prior` has
  no match in `old_prior`. Defaults to `FALSE`

- merge:

  If `TRUE` then merge new priors with existing ones, if `FALSE` only
  use new priors. Defaults to `TRUE`

- enforce_presence:

  If `TRUE` then only keep rows that have both old and new priors. If
  `FALSE` then keep all rows but use new priors where available,
  otherwise keep old priors. Defaults to `TRUE`.

## Details

Prior distributions can be specified in two ways:

1.  Using the standard `brms` prior specification format. These priors
    are replaced based on matching parameter metadata (class,
    coefficient, group, etc.).

2.  Using custom set priors with the syntax `parameter ~ distribution`.
    These will only remove existing custom priors for the same parameter
    name but will not affect priors set via the standard `brms`
    specification format. Custom priors are excluded from the
    metadata-based joining process.
