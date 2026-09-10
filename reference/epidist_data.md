# Keep `epidist` objects in their class

Every `epidist` data object also carries the `epidist_data` class. The
methods documented here re-check an object after it has been modified
and drop any `epidist` class whose requirements the modified object no
longer meets, warning about what was dropped and why. An object that
still carries an `epidist` class is therefore a valid object of that
class, so functions which accept one do not need to re-check it.

## Usage

``` r
# S3 method for class 'epidist_data'
x[...]

# S3 method for class 'epidist_data'
x[...] <- value

# S3 method for class 'epidist_data'
x[[...]] <- value

# S3 method for class 'epidist_data'
x$... <- value

# S3 method for class 'epidist_data'
names(x) <- value

# S3 method for class 'epidist_data'
rbind(..., deparse.level = 1)

# S3 method for class 'epidist_data'
dplyr_reconstruct(data, template)

# S3 method for class 'epidist_data'
dplyr_row_slice(data, i, ...)

# S3 method for class 'epidist_data'
dplyr_col_modify(data, cols)

# S3 method for class 'epidist_data'
group_by(.data, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.data))

# S3 method for class 'epidist_data'
ungroup(x, ...)
```

## Arguments

- x, .data:

  An object with the `epidist_data` class.

- ...:

  Passed to the underlying method.

- value:

  A replacement value.

- deparse.level:

  Passed to [`base::rbind()`](https://rdrr.io/r/base/cbind.html).

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

The modified object with any `epidist` class whose requirements it no
longer meets removed.

## Details

Methods are provided for base subsetting and replacement, and for
[`dplyr::dplyr_reconstruct()`](https://dplyr.tidyverse.org/reference/dplyr_extending.html),
which `dplyr` verbs such as
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
and
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
use to restore the class of their input.
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
and
[`dplyr::ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html)
build a new tibble rather than restoring the class of their input, as do
the `grouped_df` methods for
[`dplyr::dplyr_row_slice()`](https://dplyr.tidyverse.org/reference/dplyr_extending.html)
and
[`dplyr::dplyr_col_modify()`](https://dplyr.tidyverse.org/reference/dplyr_extending.html),
so each has a method of its own that puts the classes back. A grouped
object keeps the `grouped_df` class after the `epidist` classes, and the
`dplyr` verbs keep both.
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
builds a new object from the groups rather than modifying its input, so
its result does not carry the `epidist` classes.

A result with no columns is unclassed without a warning. Such a result
is almost always the prototype `vctrs` takes internally, in
[`dplyr::bind_cols()`](https://dplyr.tidyverse.org/reference/bind_cols.html)
for example, rather than something the user asked for, and it cannot be
told apart from a deliberate empty selection.

## See also

Other epidist_data:
[`is_epidist_data()`](https://epidist.epinowcast.org/reference/is_epidist_data.md)

## Examples

``` r
linelist_data <- sierra_leone_ebola_data |>
  as_epidist_linelist_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  )
#> ℹ No primary event upper bound provided, using the primary event lower bound + 1 day as the assumed upper bound.
#> ℹ No secondary event upper bound provided, using the secondary event lower bound + 1 day as the assumed upper bound.
#> ℹ No observation time column provided, using 2015-09-14 as the observation date (the maximum of the secondary event upper bound).

# Subsetting rows keeps the class
class(linelist_data[1:10, ])
#> [1] "epidist_linelist_data" "epidist_data"          "tbl_df"               
#> [4] "tbl"                   "data.frame"           

# Dropping a required column drops the class
class(dplyr::select(linelist_data, -"obs_time"))
#> Warning: ! Dropping the <epidist_linelist_data> class because the object no longer meets
#>   its requirements:
#> ✖ Assertion on 'names(data)' failed: Names must include the elements
#>   {'ptime_lwr','ptime_upr','stime_lwr','stime_upr','obs_time'}, but is missing
#>   elements {'obs_time'}.
#> ℹ Use the matching `as_epidist_*()` function to recreate the object.
#> [1] "tbl_df"     "tbl"        "data.frame"

# Grouping keeps the class alongside the grouped_df class
class(dplyr::group_by(linelist_data, obs_time))
#> [1] "epidist_linelist_data" "epidist_data"          "grouped_df"           
#> [4] "tbl_df"                "tbl"                   "data.frame"           
```
