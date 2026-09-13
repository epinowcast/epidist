#' Keep `epidist` objects in their class
#'
#' Every `epidist` data object also carries the `epidist_data` class. The
#' methods documented here re-check an object after it has been modified and
#' drop any `epidist` class whose requirements the modified object no longer
#' meets, warning about what was dropped and why. An object that still carries
#' an `epidist` class is therefore a valid object of that class, so functions
#' which accept one do not need to re-check it.
#'
#' Methods are provided for base subsetting and replacement, and for
#' [dplyr::dplyr_reconstruct()], which `dplyr` verbs such as [dplyr::mutate()],
#' [dplyr::filter()] and [dplyr::select()] use to restore the class of their
#' input. [dplyr::group_by()] and [dplyr::ungroup()] build a new tibble rather
#' than restoring the class of their input, as do the `grouped_df` methods for
#' [dplyr::dplyr_row_slice()] and [dplyr::dplyr_col_modify()], so each has a
#' method of its own that puts the classes back. A grouped object keeps the
#' `grouped_df` class after the `epidist` classes, and the `dplyr` verbs keep
#' both. [dplyr::summarise()] builds a new object from the groups rather than
#' modifying its input, so its result does not carry the `epidist` classes.
#'
#' A result with no columns is unclassed without a warning. Such a result is
#' almost always the prototype `vctrs` takes internally, in [dplyr::bind_cols()]
#' for example, rather than something the user asked for, and it cannot be told
#' apart from a deliberate empty selection.
#'
#' @param x,.data An object with the `epidist_data` class.
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
#' @param deparse.level Passed to [base::rbind()].
#'
#' @param ... Passed to the underlying method.
#'
#' @returns The modified object with any `epidist` class whose requirements it
#'  no longer meets removed.
#'
#' @name epidist_data
#' @family epidist_data
#' @examples
#' linelist_data <- sierra_leone_ebola_data |>
#'   as_epidist_linelist_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested"
#'   )
#'
#' # Subsetting rows keeps the class
#' class(linelist_data[1:10, ])
#'
#' # Dropping a required column drops the class
#' class(dplyr::select(linelist_data, -"obs_time"))
#'
#' # Grouping keeps the class alongside the grouped_df class
#' class(dplyr::group_by(linelist_data, obs_time))
NULL

#' @rdname epidist_data
#' @method [ epidist_data
#' @export
`[.epidist_data` <- function(x, ...) {
  out <- NextMethod()
  return(.revalidate_epidist(out, x))
}

#' @rdname epidist_data
#' @method [<- epidist_data
#' @export
`[<-.epidist_data` <- function(x, ..., value) {
  out <- NextMethod()
  return(.revalidate_epidist(out, x))
}

#' @rdname epidist_data
#' @method [[<- epidist_data
#' @export
`[[<-.epidist_data` <- function(x, ..., value) {
  out <- NextMethod()
  return(.revalidate_epidist(out, x))
}

#' @rdname epidist_data
#' @method $<- epidist_data
#' @export
`$<-.epidist_data` <- function(x, ..., value) {
  out <- NextMethod()
  return(.revalidate_epidist(out, x))
}

#' @rdname epidist_data
#' @method names<- epidist_data
#' @export
`names<-.epidist_data` <- function(x, value) {
  out <- NextMethod()
  return(.revalidate_epidist(out, x))
}

#' @rdname epidist_data
#' @method rbind epidist_data
#' @export
rbind.epidist_data <- function(..., deparse.level = 1) {
  out <- base::rbind.data.frame(..., deparse.level = deparse.level)
  return(.revalidate_epidist(out))
}

#' @rdname epidist_data
#' @method dplyr_reconstruct epidist_data
#' @importFrom dplyr dplyr_reconstruct
#' @export
dplyr_reconstruct.epidist_data <- function(data, template) {
  out <- NextMethod()
  return(.revalidate_epidist(out, template))
}

# The built-in `dplyr` methods for `data.frame` and `grouped_df` already
# reconstruct the object, calling `dplyr_reconstruct()` directly or, for a
# grouped object, rebuilding the grouping structure themselves, so these call
# the generic on the object without its `epidist` classes and check the
# result once.

#' @rdname epidist_data
#' @method dplyr_row_slice epidist_data
#' @importFrom dplyr dplyr_row_slice
#' @export
dplyr_row_slice.epidist_data <- function(data, i, ...) {
  out <- dplyr_row_slice(.drop_epidist_class(data), i, ...)
  return(.revalidate_epidist(out, data))
}

#' @rdname epidist_data
#' @method dplyr_col_modify epidist_data
#' @importFrom dplyr dplyr_col_modify
#' @export
dplyr_col_modify.epidist_data <- function(data, cols) {
  out <- dplyr_col_modify(.drop_epidist_class(data), cols)
  return(.revalidate_epidist(out, data))
}

#' @rdname epidist_data
#' @method group_by epidist_data
#' @importFrom dplyr group_by
#' @export
group_by.epidist_data <- function(
  .data,
  ...,
  .add = FALSE,
  .drop = dplyr::group_by_drop_default(.data)
) {
  out <- NextMethod()
  return(.revalidate_epidist(out, .data))
}

#' @rdname epidist_data
#' @method ungroup epidist_data
#' @importFrom dplyr ungroup
#' @export
ungroup.epidist_data <- function(x, ...) {
  out <- NextMethod()
  return(.revalidate_epidist(out, x))
}

#' Check if data has the `epidist_data` class
#'
#' All `epidist` data objects carry this class in addition to their specific
#' class. See [epidist_data] for the methods it provides.
#'
#' @param data An object.
#'
#' @returns A logical, `TRUE` if `data` inherits from `epidist_data` and
#'  `FALSE` otherwise.
#'
#' @family epidist_data
#' @export
#' @examples
#' sierra_leone_ebola_data |>
#'   as_epidist_linelist_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested"
#'   ) |>
#'   is_epidist_data()
is_epidist_data <- function(data) {
  return(inherits(data, "epidist_data"))
}

#' Set the class of an `epidist` data object
#'
#' Adds `class` to `data` along with the shared `epidist_data` class, which is
#' placed after all of the `epidist` classes so that dispatch on the specific
#' classes takes precedence. Used by the `new_epidist_*()` constructors.
#'
#' @param data A `data.frame` to set the class of.
#'
#' @param class A character string giving the `epidist` class to add.
#'
#' @returns `data` with `class` and `epidist_data` added to its class.
#'
#' @keywords internal
.new_epidist_data <- function(data, class) {
  classes <- c(class, setdiff(class(data), c(class, "epidist_data")))
  epidist_classes <- which(startsWith(classes, "epidist_"))
  classes <- append(classes, "epidist_data", after = max(epidist_classes))
  class(data) <- classes
  return(data)
}

#' Put the `epidist` classes of a template back on an object
#'
#' `dplyr` builds a new grouped tibble rather than keeping the class of its
#' input, so the `epidist` classes are lost whenever a grouped object is
#' modified. This puts them back, in front of the classes `data` already has,
#' so that a grouped result carries the `epidist` classes followed by
#' `grouped_df`. The result still needs checking, which
#' [.revalidate_epidist()] does after calling this.
#'
#' @param data A `data.frame` to add the classes to.
#'
#' @param template The object whose `epidist` classes to add.
#'
#' @returns `data` with the `epidist` classes of `template`, or `data`
#'  unchanged if it is not a `data.frame` or `template` has none.
#'
#' @keywords internal
.restore_epidist_class <- function(data, template) {
  classes <- .epidist_classes(template)
  if (!is.data.frame(data) || length(classes) == 0) {
    return(data)
  }
  class(data) <- c(
    classes,
    "epidist_data",
    setdiff(class(data), c(classes, "epidist_data"))
  )
  return(data)
}

#' Drop `epidist` classes from an object
#'
#' Removes `class` from `data`, and also removes the shared `epidist_data`
#' class once no specific `epidist` class is left.
#'
#' @param data An object to drop classes from.
#'
#' @param class A character vector of `epidist` classes to drop. Defaults to
#'  `NULL`, in which case all `epidist` classes are dropped.
#'
#' @returns `data` with the requested classes removed.
#'
#' @keywords internal
.drop_epidist_class <- function(data, class = NULL) {
  if (is.null(class)) {
    class <- .epidist_classes(data)
  }
  classes <- setdiff(class(data), class)
  if (!any(startsWith(setdiff(classes, "epidist_data"), "epidist_"))) {
    classes <- setdiff(classes, "epidist_data")
  }
  class(data) <- classes
  return(data)
}

#' Extract the specific `epidist` classes of an object
#'
#' @param data An object.
#'
#' @returns A character vector of the `epidist` classes of `data`, excluding
#'  the shared `epidist_data` class.
#'
#' @keywords internal
.epidist_classes <- function(data) {
  classes <- grep("^epidist_", class(data), value = TRUE)
  return(setdiff(classes, "epidist_data"))
}

#' Check an object against the requirements of a single `epidist` class
#'
#' @param data An object to check.
#'
#' @param class A character string giving the `epidist` class to check against.
#'
#' @returns `NULL` if `data` meets the requirements of `class`, or if `class`
#'  has no [assert_epidist()] method, and otherwise the message explaining why
#'  it does not.
#'
#' @keywords internal
#' @importFrom utils getS3method
.check_epidist_class <- function(data, class) {
  method <- getS3method("assert_epidist", class, optional = TRUE)
  if (is.null(method)) {
    return(NULL)
  }
  outcome <- tryCatch(method(data), error = function(e) e)
  if (inherits(outcome, "error")) {
    return(conditionMessage(outcome))
  }
  return(NULL)
}

#' Re-check a modified `epidist` object and drop any classes it fails
#'
#' Puts the `epidist` classes of `original` back on `data` with
#' [.restore_epidist_class()], then checks `data` against each of them using
#' [.check_epidist_class()] and drops those it no longer meets the
#' requirements of. Modifications that leave the object unchanged are not
#' checked. Used by the methods documented in [epidist_data].
#'
#' @param data A modified `epidist` object. An object with no columns is never
#'  valid, and is unclassed without a warning because it is usually the
#'  prototype `vctrs` takes rather than something the user asked for.
#'
#' @param original The object before it was modified. Checking is skipped when
#'  the modification left the object unchanged.
#'
#' @returns `data` with any `epidist` class whose requirements it no longer
#'  meets removed.
#'
#' @keywords internal
#' @importFrom cli cli_warn
#' @importFrom stats setNames
.revalidate_epidist <- function(data, original = NULL) {
  if (!is.data.frame(data) || identical(data, original)) {
    return(data)
  }
  # A zero column object is never a valid `epidist` object. `vctrs` takes one
  # as a prototype of the object in, for example, `dplyr::bind_cols()`, so drop
  # the classes without warning rather than warning about an object the user
  # never sees.
  if (ncol(data) == 0) {
    return(.drop_epidist_class(data))
  }
  data <- .restore_epidist_class(data, original)
  classes <- .epidist_classes(data)
  problems <- lapply(classes, .check_epidist_class, data = data)
  names(problems) <- classes
  problems <- problems[!vapply(problems, is.null, logical(1))]
  if (length(problems) == 0) {
    return(data)
  }
  dropped <- names(problems)
  reasons <- setNames(
    .escape_braces(unlist(problems, use.names = FALSE)),
    rep("x", length(problems))
  )
  cli_warn(c(
    "!" = "Dropping the {.cls {dropped}} class{?es} because the object no
           longer meets {?its/their} requirements:",
    reasons,
    i = "Use the matching {.code as_epidist_*()} function to recreate the
         object."
  ))
  return(.drop_epidist_class(data, dropped))
}

#' Escape braces so that `cli` does not interpolate them
#'
#' Messages from `checkmate` contain braces, which `cli` would otherwise treat
#' as glue interpolation.
#'
#' @param x A character vector.
#'
#' @returns `x` with each brace doubled.
#'
#' @keywords internal
.escape_braces <- function(x) {
  x <- gsub("{", "{{", x, fixed = TRUE)
  return(gsub("}", "}}", x, fixed = TRUE))
}
