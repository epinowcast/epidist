# Extending epidist

## Why extend `epidist`?

`epidist` handles the parts of delay estimation that are tedious and
easy to get wrong. Censoring, truncation, the `brms` formula interface,
priors, and the Stan code that ties them together.

If you are building a model that shares those problems but has a
different likelihood, you might want to do so by extending `epidist`.

Two packages already do this, in two different ways.

- [`cfrnow`](https://github.com/epiforecasts/cfrnow) registers a new
  **model type**.
- [`tbl.now`](https://github.com/RodrigoZepeda/tbl.now) adds a new
  **data source**, by writing
  [`as_epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.md)
  and
  [`as_epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_aggregate_data.md)
  methods for its own class.

## Adding a data source

Write a method that turns your class into one `epidist` understands.

Code

``` r

#' @importFrom epidist as_epidist_linelist_data
#' @export
as_epidist_linelist_data.my_class <- function(data, ...) {
  return(as_epidist_linelist_data(
    as.data.frame(data),
    pdate_lwr = "onset_date",
    sdate_lwr = "report_date"
  ))
}
```

Code

``` r

my_data |>
  as_epidist_linelist_data() |>
  as_epidist_marginal_model() |>
  epidist(formula = mu ~ 1)
```

## Adding a model type

A model type is an S3 class plus methods for six generics.

Three you have to supply.

| Generic | What you supply | If you skip it |
|----|----|----|
| [`assert_epidist()`](https://epidist.epinowcast.org/reference/assert_epidist.md) | What columns your data must have and what must be true of them | Errors |
| [`epidist_family_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.md) | The `brms` custom family, including the likelihood name and its `vars` | Returns the family unchanged |
| [`epidist_stancode()`](https://epidist.epinowcast.org/reference/epidist_stancode.md) | The Stan code injected through `brms` `stanvars` | `NULL`, so no likelihood |

Three are optional.

| Generic | What you supply | If you skip it |
|----|----|----|
| [`epidist_formula_model()`](https://epidist.epinowcast.org/reference/epidist_formula_model.md) | How the user’s formula is rewritten, usually adding `vreal()` terms | Returns the formula unchanged |
| [`epidist_transform_data_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.md) | Reshaping before fitting | Returns the data unchanged |
| [`epidist_model_prior()`](https://epidist.epinowcast.org/reference/epidist_model_prior.md) | Model specific priors | `NULL` |

You also write a constructor that sets your class, by convention
`as_epidist_<name>_model()`.

The skeleton looks like this.

Code

``` r

as_epidist_my_model <- function(data) {
  class(data) <- c("epidist_my_model", class(data))
  assert_epidist(data)
  return(data)
}

#' @importFrom epidist assert_epidist
#' @export
assert_epidist.epidist_my_model <- function(data, ...) {
  checkmate::assert_names(names(data), must.include = c("delay", "n"))
  return(invisible(NULL))
}

#' @importFrom epidist epidist_family_model
#' @export
epidist_family_model.epidist_my_model <- function(data, family, ...) {
  return(brms::custom_family(
    paste0("my_model_", family$family),
    dpars = family$dpars,
    vars = "vreal1",
    loop = FALSE
  ))
}

#' @importFrom epidist epidist_formula_model
#' @export
epidist_formula_model.epidist_my_model <- function(data, formula, ...) {
  return(stats::update(formula, delay | vreal(n) ~ .))
}

#' @importFrom epidist epidist_stancode
#' @export
epidist_stancode.epidist_my_model <- function(data, family, formula, ...) {
  return(brms::stanvar(block = "functions", scode = "// your lpdf here"))
}
```

Register the S3 methods in your `NAMESPACE` and import the generics from
`epidist`, as `cfrnow` does:

    importFrom(epidist,assert_epidist)
    importFrom(epidist,epidist_family_model)
    importFrom(epidist,epidist_formula_model)
    importFrom(epidist,epidist_stancode)
    S3method(assert_epidist,epidist_my_model)
    S3method(epidist_family_model,epidist_my_model)
    S3method(epidist_formula_model,epidist_my_model)
    S3method(epidist_stancode,epidist_my_model)

Users then call
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md)
exactly as they would for a built-in model.

Code

``` r

data |>
  as_epidist_my_model() |>
  epidist(formula = mu ~ 1)
```

## Exploring how cfrnow extends epidist

[`cfrnow`](https://github.com/epiforecasts/cfrnow) fits a mixture-cure
survival model to estimate a real-time case fatality ratio. Every method
it needs is in
[`R/cure_model.R`](https://github.com/epiforecasts/cfrnow/blob/main/R/cure_model.R).

| Method |  |
|----|----|
| [`as_epidist_cure_model()`](https://github.com/epiforecasts/cfrnow/blob/main/R/cure_model.R#L49) | Sets the class |
| [`assert_epidist()`](https://github.com/epiforecasts/cfrnow/blob/main/R/cure_model.R#L62) | Checks the columns its likelihood needs |
| [`epidist_family_model()`](https://github.com/epiforecasts/cfrnow/blob/main/R/cure_model.R#L93) | Declares the custom family and its `vars` |
| [`epidist_formula_model()`](https://github.com/epiforecasts/cfrnow/blob/main/R/cure_model.R#L116) | Adds the `vreal()` terms |
| [`epidist_transform_data_model()`](https://github.com/epiforecasts/cfrnow/blob/main/R/cure_model.R#L122) | Reshapes before fitting |
| [`epidist_model_prior()`](https://github.com/epiforecasts/cfrnow/blob/main/R/cure_model.R#L129) | Returns `NULL` |
| [`epidist_stancode()`](https://github.com/epiforecasts/cfrnow/blob/main/R/cure_model.R#L176) | Injects the mixture-cure likelihood |

`cfrnow` also exports its own frontend. Its users call
`prepare_cfr_data()`, `fit_cfr()` and `pp_check_cfr()` rather than
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

## Getting help

If you are building an extension and something in this interface is
awkward, please open an issue.
