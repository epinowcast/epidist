#' The default number of quadrature intervals for truncated continuous moments
#'
#' @returns An integer number of intervals.
#'
#' @keywords internal
.meta_n_quad_default <- function() {
  return(100L)
}

#' The smallest number of quadrature intervals used for a summary row
#'
#' Set with `options(epidist.meta_n_quad = )`, as an even number of at least
#' two. Each summary row carries its own number of intervals in its `n_quad`
#' slot, chosen by [.estimates_n_quad()] from the spread the study reported
#' so that the quadrature resolves the delay, and this is the floor of that
#' choice. Set it before building the model data, since the slot is filled
#' in then. It also lifts the cap of [.meta_n_quad_max()] when set above it.
#'
#' @returns An integer number of intervals.
#'
#' @keywords internal
.meta_n_quad <- function() {
  n_quad <- getOption("epidist.meta_n_quad", .meta_n_quad_default())
  assert_integerish(
    n_quad,
    lower = 2, len = 1, any.missing = FALSE,
    .var.name = "options(epidist.meta_n_quad)"
  )
  if (n_quad %% 2 != 0) {
    cli::cli_abort(paste0(
      "{.code options(epidist.meta_n_quad)} must be an even number of ",
      "intervals, because the quadrature uses Simpson's rule."
    ))
  }
  return(as.integer(n_quad))
}

#' The largest number of quadrature intervals chosen for a summary row
#'
#' Every quadrature node costs a distribution function evaluation on each
#' gradient, so the number of intervals [.estimates_n_quad()] chooses for a
#' study is capped here unless `options(epidist.meta_n_quad = )` is set
#' higher. [as_epidist_estimates_data()] warns about a study the cap leaves
#' unresolved.
#'
#' @returns An integer number of intervals.
#'
#' @keywords internal
.meta_n_quad_max <- function() {
  return(2000L)
}

#' The number of quadrature intervals a row's slots ask for
#'
#' Rows built by [as_epidist_meta_model()] carry it in their `n_quad` slot.
#' A slots list assembled by hand without one uses the floor.
#'
#' @param slots The output of [.meta_row_slots()].
#'
#' @returns An integer number of intervals.
#'
#' @keywords internal
.meta_slots_n_quad <- function(slots) {
  if (is.null(slots$n_quad)) {
    return(.meta_n_quad())
  }
  return(as.integer(slots$n_quad))
}

#' Implied summaries shared by meta model rows with the same study design
#'
#' Holds one entry per study design, each a list of the parameter draws it was
#' built from and the summaries they imply. It lives in the package namespace,
#' so it is never written into a fitted model object.
#' See [.meta_row_draw_moments()].
#'
#' @format An environment.
#'
#' @keywords internal
.meta_draws <- new.env(parent = emptyenv())

#' The largest number of entries the implied summary cache holds
#'
#' The cache is bounded so that it cannot grow without limit over a long
#' session. Passing the limit clears it rather than evicting one entry, which
#' keeps the bookkeeping to a single check. Each entry holds one summary vector
#' per posterior draw, so the limit is small.
#'
#' @returns An integer number of entries.
#'
#' @keywords internal
.meta_draw_cache_limit <- function() {
  return(8L)
}
