#' Define model specific Stan code
#'
#' This function is used within [epidist()] to create any custom Stan code which
#' is injected into `brms` via the `stanvars` argument. It is unlikely that
#' as a user you will need this function, but we export it nonetheless to be
#' transparent about what exactly is happening inside of a call to [epidist()].
#'
#' @inheritParams epidist
#' @rdname epidist_stancode
#' @family stan
#' @returns A list of `stanvars` objects, or `NULL` when none are needed.
#'
#' @export
#' @examples
#' data <- sierra_leone_ebola_data |>
#'   as_epidist_linelist_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested"
#'   ) |>
#'   as_epidist_aggregate_data() |>
#'   as_epidist_marginal_model()
#' family <- epidist_family(data, family = lognormal())
#' formula <- epidist_formula(data, family = family, formula = mu ~ 1)
#' epidist_stancode(data, family = family, formula = formula)
epidist_stancode <- function(data, ...) {
  UseMethod("epidist_stancode")
}

#' Default method for defining model specific Stan code
#'
#' @inheritParams epidist
#' @family stan
#' @returns A list of `stanvars` objects, or `NULL` when none are needed.
#'
#' @export
epidist_stancode.default <- function(data, ...) {
  return(NULL)
}
