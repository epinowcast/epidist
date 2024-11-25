#' Define `epidist` family
#'
#' This function is used within [epidist()] to create a model specific custom
#' `brms` family object. This custom family is passed to `brms`. It is unlikely
#' that as a user you will need this function, but we export it nonetheless to
#' be transparent about what happens inside of a call to [epidist()].
#'
#' @inheritParams epidist
#' @family family
#' @export
epidist_family <- function(data, family = lognormal(), ...) {
  assert_epidist(data)
  family <- brms:::validate_family(family) # nolint
  class(family) <- c(family$family, class(family))
  family <- .add_dpar_info(family)
  custom_family <- epidist_family_model(data, family, ...)
  class(custom_family) <- c(family$family, class(custom_family))
  custom_family <- epidist_family_param(custom_family)
  return(custom_family)
}

#' The model-specific parts of an `epidist_family()` call
#'
#' @inheritParams epidist
#' @param family Output of a call to `brms::brmsfamily()` with additional
#' information as provided by `.add_dpar_info()`
#' @rdname epidist_family_model
#' @family family
#' @export
epidist_family_model <- function(data, family, ...) {
  UseMethod("epidist_family_model")
}

#' Default method for defining a model specific family
#'
#' @inheritParams epidist_family_model
#' @family family
#' @export
epidist_family_model.default <- function(data, family, ...) {
  return(family)
}

#' Reparameterise an `epidist` family to align `brms` and Stan
#'
#' @inheritParams epidist_family
#' @rdname epidist_family_param
#' @family family
#' @export
epidist_family_param <- function(family, ...) {
  UseMethod("epidist_family_param")
}

#' Default method for families which do not require a reparameterisation
#'
#' This function extracts the Stan parameterisation for a given brms family by
#' creating a dummy model and parsing its Stan code. It looks for the log
#' probability density function (lpdf) call in the Stan code and extracts the
#' parameter order used by Stan. This is needed because brms and Stan may use
#' different parameter orderings for the same distribution.
#'
#' @param family A brms family object containing at minimum a `family` element
#' specifying the distribution family name
#' @param ... Additional arguments passed to methods (not used)
#'
#' @details
#' The function works by:
#' 1. Creating a minimal dummy model using the specified family
#' 2. Extracting the Stan code for this model
#' 3. Finding the lpdf function call for the family
#' 4. Parsing out the parameter ordering used in Stan
#' 5. Adding this as the `param` element to the family object
#'
#' @return The input family object with an additional `param` element containing
#' the Stan parameter ordering as a string
#'
#' @family family
#' @importFrom brms make_stancode
#' @importFrom cli cli_abort
#' @export
epidist_family_param.default <- function(family, ...) {
  dummy_mdl <- make_stancode(mpg ~ 1, data = mtcars, family = family$family)

  # Extract the Stan parameterisation from the dummy model code
  lpdf_pattern <- paste0(
    "target \\+= ", tolower(family$family), "_lpdf\\(Y \\| ([^)]+)\\)" # nolint
  )
  lpdf_match <- regexpr(lpdf_pattern, dummy_mdl)
  reparam <- if (lpdf_match > 0) {
    match_str <- regmatches(dummy_mdl, lpdf_match)[[1]]
    param <- sub(
      paste0(
        "target \\+= ", tolower(family$family), "_lpdf\\(Y \\| " # nolint
      ), "",
      match_str
    )
    param <- sub(")", "", param, fixed = TRUE)
    family$param <- param
  } else {
    cli_abort(
      "Unable to extract Stan parameterisation for {family$family}."
    )
  }
  return(family)
}
