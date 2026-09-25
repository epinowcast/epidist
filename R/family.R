#' Define `epidist` family
#'
#' This function is used within [epidist()] to create a model specific custom
#' `brms` family object. This custom family is passed to `brms`. It is unlikely
#' that as a user you will need this function, but we export it nonetheless to
#' be transparent about what happens inside of a call to [epidist()].
#'
#' The family may be any `brms` family of a positive response, such as
#' [brms::lognormal()], `Gamma(link = "log")` or [brms::weibull()], or a
#' family `epidist` defines itself, such as [gengamma()].
#'
#' @inheritParams epidist
#' @family family
#' @returns A `brms` custom family object.
#'
#' @export
epidist_family <- function(data, family = lognormal(), ...) {
  assert_epidist(data)
  family <- .validate_family(family)
  if (.is_nonparametric(family)) {
    family <- .np_resolve(family, data)
  }
  class(family) <- c(.family_name(family), class(family))
  family <- .add_dpar_info(family)
  custom_family <- epidist_family_model(data, family, ...)
  class(custom_family) <- c(.family_name(family), class(custom_family))
  custom_family <- epidist_family_param(custom_family)
  return(custom_family)
}

#' The model-specific parts of an `epidist_family()` call
#'
#' @inheritParams epidist
#'
#' @param family Output of a call to `brms::brmsfamily()` with additional
#' information as provided by `.add_dpar_info()`
#'
#' @rdname epidist_family_model
#' @family family
#' @returns A `brms` custom family object.
#'
#' @export
epidist_family_model <- function(data, family, ...) {
  UseMethod("epidist_family_model")
}

#' Default method for defining a model specific family
#'
#' @inheritParams epidist_family_model
#' @family family
#' @returns A `brms` custom family object.
#'
#' @export
epidist_family_model.default <- function(data, family, ...) {
  return(family)
}

#' Reparameterise an `epidist` family to align `brms` and Stan
#'
#' Called by [epidist_family()]. A custom model supplies its family through
#' [epidist_family_model()] rather than by calling this.
#'
#' @inheritParams epidist_family
#' @rdname epidist_family_param
#' @family family
#' @returns The family with a `param` element giving the Stan parameter
#'  ordering.
#'
#' @keywords internal
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
#'  specifying the distribution family name.
#'
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
#' @returns The input family object with an additional `param` element
#'  containing the Stan parameter ordering as a string
#'
#' @family family
#' @importFrom brms make_stancode
#' @importFrom cli cli_abort
#' @export
epidist_family_param.default <- function(family, ...) {
  data_dummy <- data.frame(y = c(1, 2))
  dummy_mdl <- make_stancode(
    y ~ 1,
    data = data_dummy,
    family = class(family)[1]
  )

  # get the lowered family name
  family_name <- tolower(class(family)[1])

  # Extract the Stan parameterisation from the dummy model code
  lpdf_pattern <- paste0(
    "target \\+= ", # nolint
    family_name,
    "_(lpdf|lpmf)\\(Y \\| (.+?)\\)" # nolint
  )
  lpdf_match <- regexpr(lpdf_pattern, dummy_mdl)
  if (lpdf_match > 0) {
    matches <- unlist(regmatches(dummy_mdl, lpdf_match))
    mu_matches <- matches[grepl("mu", matches, fixed = TRUE)]
    if (length(mu_matches) > 1) {
      cli_abort("Multiple Stan parameterisations found with 'mu' parameter.")
    } else if (length(mu_matches) == 0) {
      cli_abort("No Stan parameterisation found with 'mu' parameter.")
    }
    match_str <- mu_matches[1]
    param <- sub(
      paste0(
        "target \\+= ", # nolint
        family_name,
        "_(lpdf|lpmf)\\(Y \\| " # nolint
      ),
      "",
      match_str
    )
    param <- sub(")", "", param, fixed = TRUE)
    family$param <- param
  } else {
    cli_abort(
      "Unable to extract Stan parameterisation for {family_name}."
    )
  }
  return(family)
}

#' The families `epidist` defines itself
#'
#' `brms` names a custom family `"custom"` and keeps its own name in `name`,
#' so these are looked up by that name where `brms` would be asked for one of
#' its own families.
#'
#' @returns A named list of family constructors.
#'
#' @keywords internal
.epidist_families <- function() {
  return(list(gengamma = gengamma))
}

#' The name of a delay distribution family
#'
#' A family built with [brms::custom_family()], such as [gengamma()], is
#' named `"custom"` for `brms` to dispatch on and records its own name in
#' `name`. Every other family is named by `family`.
#'
#' @inheritParams epidist_family
#'
#' @returns A character string.
#'
#' @keywords internal
.family_name <- function(family) {
  if (identical(family$family, "custom")) {
    return(family$name)
  }
  return(family$family)
}

#' Stan functions a family defines itself
#'
#' A family `brms` does not have, such as [gengamma()], carries the Stan
#' density and distribution function `brms` and the latent model call. They
#' are read from the `stan/family/` folder of the installed package.
#'
#' @inheritParams epidist_family
#'
#' @returns A `brms` `stanvars` object, or `NULL` for a `brms` family.
#'
#' @keywords internal
.family_stanvars <- function(family) {
  name <- .delay_family(family)$name
  if (!name %in% names(.epidist_families())) {
    return(NULL)
  }
  return(brms::stanvar(
    block = "functions",
    scode = .stan_chunk(file.path("family", paste0(name, ".stan")))
  ))
}
