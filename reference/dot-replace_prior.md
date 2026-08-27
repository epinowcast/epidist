# Replace `brms` prior distributions

This function takes an existing set of prior distributions and updates
them with new prior specifications. It matches priors based on their
parameter class, coefficient, group, response, distributional parameter,
and non-linear parameter.

## Usage

``` r
.replace_prior(old_prior, prior, enforce_presence = TRUE)
```

## Arguments

- old_prior:

  One or more prior distributions in the class `brmsprior` to be
  updated.

- prior:

  One or more prior distributions in the class `brmsprior` containing
  the new specifications. Can include manually specified priors using
  the syntax `parameter ~ distribution`.

- enforce_presence:

  If `TRUE` then only keep rows that have both old and new priors. If
  `FALSE` then keep all rows but use new priors where available,
  otherwise keep old priors. Defaults to `TRUE`.

## Value

A `brmsprior` object containing the updated prior distributions.

## Details

Prior distributions can be specified in two ways:

1.  Using the standard `brms` prior specification format. These priors
    are replaced based on matching parameter metadata (class,
    coefficient, group, etc.).

2.  Using manually specified priors with the syntax
    `parameter ~ distribution`. These replace existing manual priors for
    the same parameter name and are otherwise left alone. Manual priors
    are excluded from the metadata based matching.
