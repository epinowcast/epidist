# The log kernel of a forward pass step at its expected size

The kernel of a step is \\\exp(d \lambda - \log d!)\\ at a step of \\d\\
counts, with \\\lambda\\ the log expected step, and is divided by its
value at the expected step so that no entry exceeds one. Matches
`meta_family_step_kernel_top()` in Stan.

## Usage

``` r
.meta_step_kernel_top(lambda_scaled)
```

## Arguments

- lambda_scaled:

  The log expected step from
  [`.meta_fixed_point()`](https://epidist.epinowcast.org/reference/dot-meta_fixed_point.md).

## Value

A number.
