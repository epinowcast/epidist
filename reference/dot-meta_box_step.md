# One step of the forward pass over the cumulative counts

Carries the log probabilities of the counts in one band to the counts in
the next, through the binomial step of the chain. The step is a matrix
product of
[`.meta_step_kernel()`](https://epidist.epinowcast.org/reference/dot-meta_step_kernel.md)
with the exponentiated source vector, each scaled so that the arithmetic
stays in range, and a sum on the log scale over every pair of counts
where the kernel would underflow at the most likely step. Both give the
same probabilities. Matches `meta_family_box_step()` in Stan.

## Usage

``` r
.meta_box_step(alpha, a, b, a2, b2, r, study_n, m_prev, m, lg)
```

## Arguments

- alpha:

  The log probabilities of the source counts `a:b`.

- a, b:

  The first and last source count.

- a2, b2:

  The first and last target count.

- r:

  The step probability from
  [`.meta_step_prob()`](https://epidist.epinowcast.org/reference/dot-meta_step_prob.md).

- study_n:

  The number of delays the quantiles were computed from.

- m_prev, m:

  The most likely source and target counts from
  [`.meta_box_mode_path()`](https://epidist.epinowcast.org/reference/dot-meta_box_mode_path.md).

- lg:

  The log factorials, `lg[d + 1]` being `log(d!)`.

## Value

The log probabilities of the target counts `a2:b2`.
