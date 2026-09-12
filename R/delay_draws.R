#' Keep the `epidist_delay_draws` class through `dplyr` verbs
#'
#' The `epidist_delay_draws` class records the delay distribution family and
#' the variables that define the strata, which [add_summaries()] and
#' [plot.epidist_delay_draws()] read. Most `dplyr` verbs build a new object
#' rather than keeping the class of their input, so the methods documented
#' here put the class and what it records back.
#'
#' Methods are provided for base subsetting and renaming, and for
#' [dplyr::dplyr_reconstruct()], which verbs such as [dplyr::mutate()] and
#' [dplyr::bind_rows()] use to restore the class of their input.
#' [dplyr::group_by()] and [dplyr::ungroup()] build a new tibble rather than
#' restoring the class of their input, as do the `grouped_df` methods for
#' [dplyr::dplyr_row_slice()] and [dplyr::dplyr_col_modify()], so each has a
#' method of its own that puts the class back. A grouped object keeps the
#' `epidist_delay_draws` class ahead of `grouped_df`, and the `dplyr` verbs
#' keep both. [dplyr::summarise()] builds a new object from the groups rather
#' than modifying its input, so its result does not carry the class.
#'
#' [dplyr::bind_rows()] restores the class from its first argument, so
#' combining draws keeps the family and the stratum variables of the first set
#' of draws. Combining draws from two fits of different families therefore
#' describes the result by the family of the first. Pass the family to
#' [add_summaries()] or [plot()][plot.epidist_delay_draws] with the `family`
#' argument when the draws combined are not all from the same family.
#'
#' @param x,.data An `epidist_delay_draws` object.
#'
#' @param data,template Passed to [dplyr::dplyr_reconstruct()].
#'
#' @param i,cols Passed to [dplyr::dplyr_row_slice()] and
#'  [dplyr::dplyr_col_modify()].
#'
#' @param .add,.drop Passed to [dplyr::group_by()].
#'
#' @param value A replacement value.
#'
#' @param ... Passed to the underlying method.
#'
#' @returns The modified object with the `epidist_delay_draws` class, and the
#'  family and stratum variables it records, put back.
#'
#' @family postprocess
#' @name epidist_delay_draws
#' @examples
#' draws <- data.frame(mu = c(1.8, 2.0), sigma = c(0.5, 0.4)) |>
#'   add_summaries(family = "lognormal")
#'
#' # Adding a column keeps the class
#' class(dplyr::mutate(draws, model = "a"))
#'
#' # Combining two sets of draws keeps the class
#' class(dplyr::bind_rows(draws, draws))
NULL

#' @rdname epidist_delay_draws
#' @method [ epidist_delay_draws
#' @export
`[.epidist_delay_draws` <- function(x, ...) {
  out <- NextMethod()
  return(.restore_delay_draws(out, x))
}

#' @rdname epidist_delay_draws
#' @method names<- epidist_delay_draws
#' @export
`names<-.epidist_delay_draws` <- function(x, value) {
  out <- NextMethod()
  return(.restore_delay_draws(out, x))
}

#' @rdname epidist_delay_draws
#' @method dplyr_reconstruct epidist_delay_draws
#' @importFrom dplyr dplyr_reconstruct
#' @export
dplyr_reconstruct.epidist_delay_draws <- function(data, template) {
  out <- NextMethod()
  return(.restore_delay_draws(out, template))
}

# The built-in `dplyr` methods for `data.frame` and `grouped_df` already
# reconstruct the object, calling `dplyr_reconstruct()` directly or, for a
# grouped object, rebuilding the grouping structure themselves, so these call
# the generic on the object without its class and put the class back once.

#' @rdname epidist_delay_draws
#' @method dplyr_row_slice epidist_delay_draws
#' @importFrom dplyr dplyr_row_slice
#' @export
dplyr_row_slice.epidist_delay_draws <- function(data, i, ...) {
  out <- dplyr_row_slice(.drop_delay_draws_class(data), i, ...)
  return(.restore_delay_draws(out, data))
}

#' @rdname epidist_delay_draws
#' @method dplyr_col_modify epidist_delay_draws
#' @importFrom dplyr dplyr_col_modify
#' @export
dplyr_col_modify.epidist_delay_draws <- function(data, cols) {
  out <- dplyr_col_modify(.drop_delay_draws_class(data), cols)
  return(.restore_delay_draws(out, data))
}

#' @rdname epidist_delay_draws
#' @method group_by epidist_delay_draws
#' @importFrom dplyr group_by
#' @export
group_by.epidist_delay_draws <- function(
  .data,
  ...,
  .add = FALSE,
  .drop = dplyr::group_by_drop_default(.data)
) {
  out <- NextMethod()
  return(.restore_delay_draws(out, .data))
}

#' @rdname epidist_delay_draws
#' @method ungroup epidist_delay_draws
#' @importFrom dplyr ungroup
#' @export
ungroup.epidist_delay_draws <- function(x, ...) {
  out <- NextMethod()
  return(.restore_delay_draws(out, x))
}

#' Put the `epidist_delay_draws` class of a template back on an object
#'
#' `dplyr` builds a new object rather than keeping the class of its input, so
#' the class, and the family and stratum variables it records, are lost
#' whenever the draws are modified. This puts them back, in front of the
#' classes `data` already has, so that a grouped result carries
#' `epidist_delay_draws` followed by `grouped_df`. Used by the methods
#' documented in [epidist_delay_draws].
#'
#' @param data A `data.frame` to put the class back on.
#'
#' @param template The object to take the class and its records from.
#'
#' @returns `data` with the `epidist_delay_draws` class, or `data` unchanged
#'  when it is not a `data.frame` or `template` does not have the class.
#'
#' @keywords internal
.restore_delay_draws <- function(data, template) {
  if (!is.data.frame(data) || !inherits(template, "epidist_delay_draws")) {
    return(data)
  }
  return(.new_delay_draws(
    data,
    attr(template, "epidist_family"),
    attr(template, "epidist_vars")
  ))
}

#' Drop the `epidist_delay_draws` class from an object
#'
#' @param data An object to drop the class from.
#'
#' @returns `data` without the `epidist_delay_draws` class.
#'
#' @keywords internal
.drop_delay_draws_class <- function(data) {
  class(data) <- setdiff(class(data), "epidist_delay_draws")
  return(data)
}
