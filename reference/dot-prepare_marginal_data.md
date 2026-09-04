# Prepare linelist data for a marginal likelihood

Calculates the delay bounds, censoring windows and relative observation
times required by the marginal likelihood, adds weights and the left
truncation point, and sets observation times far beyond the maximum
delay to `Inf`. Shared by
[`as_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.md)
and
[`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md).

## Usage

``` r
.prepare_marginal_data(
  data,
  obs_time_threshold = 2,
  weight = NULL,
  delay_min = NULL
)
```

## Arguments

- data:

  An `epidist_linelist_data` object

- obs_time_threshold:

  Ratio used to determine threshold for setting relative observation
  times to Inf. Observation times greater than `obs_time_threshold`
  times the maximum delay will be set to Inf to improve model efficiency
  by reducing the number of unique observation times. Default is 2.

- weight:

  A column name containing counts of identical linelist items. When
  specified, the user is declaring that rows with the same values
  represent the same observation occurring multiple times. This allows
  for efficient data representation by storing unique patterns with
  their counts rather than repeating identical rows. The marginal model
  will further aggregate these counts based on the formula
  specification. Default is NULL, which assigns a count of 1 to each
  row. Internally this is used to define the 'n' column of the returned
  object.

- delay_min:

  Minimum delay (left truncation point). Can be:

  - `NULL` (default): uses a `delay_min` column from the data if
    present, otherwise defaults to 0 (no left truncation).

  - A numeric scalar: applied to all observations.

  - A column name string: looks up the named column in the data. This is
    passed as the `L` parameter to
    [`primarycensored::dpcens()`](https://primarycensored.epinowcast.org/reference/dprimarycensored.html).

## Value

The input data with the marginal likelihood columns added.
