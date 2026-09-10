# Whether the kernel of a forward pass step is in range at its likely size

The kernel falls away from the expected step like a Poisson mass, so it
underflows at a step hundreds of nats from the expected one. The boxes
force such a step only where the parameters are that far from fitting
the study, and the step then takes the slower path that sums on the log
scale. Matches `meta_family_step_kernel_alive()` in Stan.

## Usage

``` r
.meta_step_kernel_alive(d_star, lambda_scaled)
```

## Arguments

- d_star:

  The step of the most likely path.

- lambda_scaled:

  The log expected step from
  [`.meta_fixed_point()`](https://epidist.epinowcast.org/reference/dot-meta_fixed_point.md).

## Value

`TRUE` or `FALSE`.
