as_direct_model <- function(data) {
  UseMethod("as_direct_model")
}

assert_direct_model_input <- function(data) {
  assert_data_frame(data)
  assert_names(names(data), must.include = c("case", "ptime", "stime"))
  assert_integer(data$case, lower = 0)
  assert_numeric(data$ptime, lower = 0)
  assert_numeric(data$stime, lower = 0)
}

as_direct_model.data.frame <- function(data) {
  assert_direct_model_input(data)
  class(data) <- c("epidist_direct_model", class(data))
  data <- data |>
    mutate(delay = .data$stime - .data$ptime)
  epidist_validate(data)
  return(data)
}

epidist_validate.epidist_direct_model <- function(data, ...) {
  assert_true(is_direct_model(data))
  assert_direct_model_input(data)
  assert_names(names(data), must.include = c("case", "ptime", "stime", "delay"))
  assert_numeric(data$delay, lower = 0)
}

is_direct_model <- function(data) {
  inherits(data, "epidist_direct_model")
}

epidist_formula_model.epidist_direct_model <- function(
    data, formula, ...
) {
  # data is only used to dispatch on
  formula <- stats::update(
    formula, delay ~ .
  )
  return(formula)
}
