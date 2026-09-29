# The cores of each leave-one-out refit

`brms` does not store the cores a model was fitted with, so this gives
`cores`, else the `mc.cores` option, else one core per chain.

## Usage

``` r
.leave_one_out_cores(cores, chains)
```

## Arguments

- cores:

  The number of cores each refit uses. If `NULL`, the default, the
  `mc.cores` option where it is set, and otherwise one core per chain,
  so the chains of each refit run in parallel.

- chains:

  The number of chains of each refit.

## Value

An integer.
