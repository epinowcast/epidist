as_direct_model <- function(data) {
  UseMethod("direct_model")
}

assert_direct_model_input <- function(data) {
  # ...
}

as_direct_model.data.frame <- function(data) {
  # ...
}

epidist_validate.epidist_direct_model <- function(data, ...) {
  # ...
}

is_direct_model <- function(data) {
  inherits(data, "epidist_direct_model")
}
