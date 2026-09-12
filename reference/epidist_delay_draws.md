# Keep the `epidist_delay_draws` class through `dplyr` verbs

The `epidist_delay_draws` class records the delay distribution family
and the variables that define the strata, which
[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md)
and
[`plot.epidist_delay_draws()`](https://epidist.epinowcast.org/reference/plot.epidist_delay_draws.md)
read. Most `dplyr` verbs build a new object rather than keeping the
class of their input, so the methods documented here put the class and
what it records back.

## Usage

``` r
# S3 method for class 'epidist_delay_draws'
x[...]

# S3 method for class 'epidist_delay_draws'
names(x) <- value

# S3 method for class 'epidist_delay_draws'
dplyr_reconstruct(data, template)

# S3 method for class 'epidist_delay_draws'
dplyr_row_slice(data, i, ...)

# S3 method for class 'epidist_delay_draws'
dplyr_col_modify(data, cols)

# S3 method for class 'epidist_delay_draws'
group_by(.data, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.data))

# S3 method for class 'epidist_delay_draws'
ungroup(x, ...)
```

## Arguments

- x, .data:

  An `epidist_delay_draws` object.

- ...:

  Passed to the underlying method.

- value:

  A replacement value.

- data, template:

  Passed to
  [`dplyr::dplyr_reconstruct()`](https://dplyr.tidyverse.org/reference/dplyr_extending.html).

- i, cols:

  Passed to
  [`dplyr::dplyr_row_slice()`](https://dplyr.tidyverse.org/reference/dplyr_extending.html)
  and
  [`dplyr::dplyr_col_modify()`](https://dplyr.tidyverse.org/reference/dplyr_extending.html).

- .add, .drop:

  Passed to
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).

## Value

The modified object with the `epidist_delay_draws` class, and the family
and stratum variables it records, put back.

## Details

Methods are provided for base subsetting and renaming, and for
[`dplyr::dplyr_reconstruct()`](https://dplyr.tidyverse.org/reference/dplyr_extending.html),
which verbs such as
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
and
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
use to restore the class of their input.
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
and
[`dplyr::ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html)
build a new tibble rather than restoring the class of their input, as do
the `grouped_df` methods for
[`dplyr::dplyr_row_slice()`](https://dplyr.tidyverse.org/reference/dplyr_extending.html)
and
[`dplyr::dplyr_col_modify()`](https://dplyr.tidyverse.org/reference/dplyr_extending.html),
so each has a method of its own that puts the class back. A grouped
object keeps the `epidist_delay_draws` class ahead of `grouped_df`, and
the `dplyr` verbs keep both.
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
builds a new object from the groups rather than modifying its input, so
its result does not carry the class.

[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
restores the class from its first argument, so combining draws keeps the
family and the stratum variables of the first set of draws. Combining
draws from two fits of different families therefore describes the result
by the family of the first. Pass the family to
[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md)
or
[plot()](https://epidist.epinowcast.org/reference/plot.epidist_delay_draws.md)
with the `family` argument when the draws combined are not all from the
same family.

## See also

Other postprocess:
[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md),
[`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md),
[`delay_summary_draws()`](https://epidist.epinowcast.org/reference/delay_summary_draws.md),
[`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md)

## Examples

``` r
draws <- data.frame(mu = c(1.8, 2.0), sigma = c(0.5, 0.4)) |>
  add_summaries(family = "lognormal")

# Adding a column keeps the class
class(dplyr::mutate(draws, model = "a"))
#> [1] "epidist_delay_draws" "data.frame"         

# Combining two sets of draws keeps the class
class(dplyr::bind_rows(draws, draws))
#> [1] "epidist_delay_draws" "data.frame"         
```
