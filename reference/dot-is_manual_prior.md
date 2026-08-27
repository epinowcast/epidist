# Identify manually specified `brms` priors

Manual priors are written using the `parameter ~ distribution` syntax
and are passed through to the Stan model block unchanged. They cannot be
matched on parameter metadata in the way that standard `brms` priors
can.

## Usage

``` r
.is_manual_prior(prior)
```

## Arguments

- prior:

  One or more prior distributions in the class `brmsprior`.

## Value

A logical vector flagging the manually specified priors.
