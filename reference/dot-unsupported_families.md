# Response families that `epidist` does not support

Ordinal, categorical, mixture and Cox families need extra handling in
`brms` that the helpers in this file deliberately do not reproduce.
Mixture families are tracked in epidist issue 617.

## Usage

``` r
.unsupported_families()
```
