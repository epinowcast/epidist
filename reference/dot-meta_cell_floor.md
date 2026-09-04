# The smallest probability a multinomial cell is given

A cell whose implied probability underflows to zero while the study saw
delays in it would give a log likelihood of `-Inf`. Stan differences log
distribution functions and so keeps a tiny mass in such a cell, where R
differences them on the natural scale and gets exactly zero. Both floor
the cell here, so that a single badly misfitting draw is rejected in
practice but leaves `loo()` a finite value.

## Usage

``` r
.meta_cell_floor()
```

## Value

A probability.
