# Advise on summary estimates the meta model will fit poorly

Runs the advisory checks on a freshly built `epidist_estimates_data`
object and messages about each study they flag. They run once, here,
when the object is first built, rather than in
[`assert_epidist()`](https://epidist.epinowcast.org/reference/assert_epidist.md),
so that passing the finished object on to
[`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md)
does not repeat them. The pointer to the Checks section of
[`as_epidist_estimates_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.data.frame.md),
which describes the checks, prints once after the messages when at least
one check fired.

## Usage

``` r
.estimates_advise(data)
```

## Arguments

- data:

  An `epidist_estimates_data` object.

## Value

`NULL`, invisibly, called for the messages it may raise.
