# Build the basis of the hazard formula over the bins

Parametric terms come from
[`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) and
smooths from
[`mgcv::smoothCon()`](https://rdrr.io/pkg/mgcv/man/smoothCon.html) and
[`mgcv::smooth2random()`](https://rdrr.io/pkg/mgcv/man/smooth2random.html),
which split each smooth into unpenalised columns and penalised blocks
with one standard deviation each, as `brms` does. Each column is centred
over the bins, each unpenalised column is scaled to unit standard
deviation and each penalised block to unit root mean square.

## Usage

``` r
.np_basis(formula, boundaries)
```

## Arguments

- formula:

  A one sided formula for the logit hazards over the bins, see Details.
  The default, `NULL`, is a spline over the delay.

- boundaries:

  A numeric vector of at least four strictly increasing bin boundaries,
  \\b_0\\ to \\b_K\\. The default, `NULL`, is set by
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md)
  from the data, with a bin for every whole delay from 0 up to the
  longest delay in the data, and at least four bins, that is
  `seq(-1, max(3, max_delay))`. The last boundary must be at least as
  long as the longest observed delay.

## Value

A list holding `basis`, a matrix with one row per free bin and one
column per coefficient, and `coefficients`, a `data.frame` with one row
per column giving the `term` of the column, its parameter `dpar`, and
the standard deviation parameter `sd` of its penalised term, `NA` for an
unpenalised column.
