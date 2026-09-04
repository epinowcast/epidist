# Bind coerced contributions into one `epidist_estimates_data` object

Covariance matrices are keyed by `mvn_id`, so a key used by two
contributions is renamed before the rows are bound.

## Usage

``` r
.estimates_bind(parts)
```

## Arguments

- parts:

  A list of `epidist_estimates_data` objects.

## Value

An `epidist_estimates_data` object.
