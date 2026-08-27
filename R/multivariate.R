#' Create an `epidist_multivariate` object
#'
#' Summarises draws of a set of parameters by their mean vector and covariance
#' matrix. The result is a multivariate normal approximation to whatever the
#' draws describe, and it is the only route by which a covariance between
#' reported quantities reaches [as_epidist_meta_model()].
#'
#' The draws may be of any parameters. Posterior draws of a delay mean and
#' standard deviation, of a `meanlog` and `sdlog`, or of a `shape` and a
#' `scale` all give the same object. Nothing here checks that a parameter is
#' something a study could report, because that only matters when the object is
#' converted with [as_epidist_estimates_data()].
#'
#' Draws may also cover a trajectory, with the parameters varying over an
#' `index`. The elements are then ordered index major and parameter minor, so a
#' `mean` and an `sd` over two index points give `mean[1]`, `sd[1]`, `mean[2]`,
#' `sd[2]`. The order is fixed because the covariance is indexed by it.
#'
#' There is no method for a vector of individual delays. Resampling delays
#' manufactures a covariance the study never estimated, which is not what this
#' represents.
#'
#' @param draws Draws of the parameters. See the methods for supported formats.
#'
#' @param ... Additional arguments passed to methods.
#'
#' @family multivariate
#' @export
as_epidist_multivariate <- function(draws, ...) {
  UseMethod("as_epidist_multivariate")
}

#' Create an `epidist_multivariate` object from a data frame of draws
#'
#' [predict_delay_parameters()] returns exactly this shape, with a `draw`
#' column, an `index` column and one column per parameter, so its output can be
#' passed straight in.
#'
#' @inheritParams as_epidist_multivariate
#'
#' @param params A character vector of the columns holding the parameter draws.
#'  Defaults to every numeric column other than `draw` and `index`. Supply it
#'  wherever the draws carry more columns than the parameters being reported,
#'  because parameters that are functions of each other have a singular
#'  covariance.
#'
#' @param index A string giving the column that identifies the trajectory
#'  point. Defaults to `"index"` where such a column exists, and otherwise to
#'  `NULL`, meaning the draws describe a single point.
#'
#' @param draw A string giving the column that identifies the draw. Defaults to
#'  `"draw"` where such a column exists, and otherwise to `NULL`, meaning row
#'  order.
#'
#' @param ... Not used in this method.
#'
#' @method as_epidist_multivariate data.frame
#'
#' @family multivariate
#' @importFrom checkmate assert_data_frame assert_string assert_character
#' @export
#' @examples
#' set.seed(1)
#' draws <- data.frame(
#'   mean = rnorm(500, 7.5, 0.3), sd = rnorm(500, 3.6, 0.2)
#' )
#' as_epidist_multivariate(draws)
as_epidist_multivariate.data.frame <- function(
  draws,
  params = NULL,
  index = NULL,
  draw = NULL,
  ...
) {
  assert_data_frame(draws, min.rows = 2)
  draws <- tibble::as_tibble(unclass(draws))
  index <- .multivariate_column(index, draws, "index")
  draw <- .multivariate_column(draw, draws, "draw")
  params <- .multivariate_params(params, draws, c(index, draw))
  index_values <- if (is.null(index)) {
    1
  } else {
    as.numeric(sort(unique(draws[[index]])))
  }
  wide <- .multivariate_wide(draws, params, index, draw, index_values)
  return(.multivariate_from_matrix(wide, params, index_values))
}

#' Create an `epidist_multivariate` object from a matrix of draws
#'
#' Rows are draws and columns are parameters, describing a single point.
#'
#' @inheritParams as_epidist_multivariate
#'
#' @param params A character vector naming the columns of `draws`. Defaults to
#'  the column names of `draws`.
#'
#' @param ... Not used in this method.
#'
#' @method as_epidist_multivariate matrix
#'
#' @family multivariate
#' @importFrom checkmate assert_matrix
#' @export
#' @examples
#' set.seed(1)
#' draws <- cbind(mean = rnorm(500, 7.5, 0.3), sd = rnorm(500, 3.6, 0.2))
#' as_epidist_multivariate(draws)
as_epidist_multivariate.matrix <- function(draws, params = NULL, ...) {
  assert_matrix(draws, mode = "numeric", min.rows = 2, min.cols = 1)
  if (is.null(params)) {
    params <- colnames(draws)
  }
  if (is.null(params)) {
    cli::cli_abort(paste0(
      "{.var params} must name the columns of {.var draws} when the matrix ",
      "has no column names."
    ))
  }
  assert_character(
    params,
    len = ncol(draws), any.missing = FALSE, unique = TRUE,
    .var.name = "params"
  )
  colnames(draws) <- params
  return(.multivariate_from_matrix(draws, params, 1))
}

#' Class constructor for `epidist_multivariate` objects
#'
#' Use this where a study published a mean vector and a covariance matrix
#' directly, rather than draws they can be computed from. Converting the object
#' with a `family`, which needs the draws themselves, is then unavailable.
#'
#' @param value A named numeric vector of the mean of each element.
#'
#' @param vcov The covariance matrix of `value`.
#'
#' @param params A character vector of the parameter names, in order.
#'
#' @param index The trajectory points, in order. Defaults to a single point.
#'
#' @param n_draws The number of draws `value` and `vcov` were computed from, or
#'  `NA` where they were published directly.
#'
#' @param draws A matrix of draws with one row per draw and one column per
#'  element of `value`, or `NULL`.
#'
#' @returns An object of class `epidist_multivariate`.
#'
#' @family multivariate
#' @export
#' @examples
#' new_epidist_multivariate(
#'   value = c(mean = 7.5, sd = 3.6),
#'   vcov = matrix(c(0.09, 0.02, 0.02, 0.04), nrow = 2),
#'   params = c("mean", "sd")
#' )
new_epidist_multivariate <- function(
  value,
  vcov,
  params,
  index = 1,
  n_draws = NA_integer_,
  draws = NULL
) {
  object <- list(
    value = value,
    vcov = vcov,
    params = params,
    index = index,
    n_draws = n_draws,
    draws = draws
  )
  class(object) <- c("epidist_multivariate", "list")
  assert_epidist(object)
  return(object)
}

#' Check if an object has the `epidist_multivariate` class
#'
#' @param x An object.
#'
#' @returns A logical.
#'
#' @family multivariate
#' @export
#' @examples
#' is_epidist_multivariate(1)
is_epidist_multivariate <- function(x) {
  return(inherits(x, "epidist_multivariate"))
}

#' Assert validity of `epidist_multivariate` objects
#'
#' @param data An object to check for validity.
#'
#' @param ... Additional arguments.
#'
#' @method assert_epidist epidist_multivariate
#'
#' @family multivariate
#' @importFrom checkmate assert_numeric assert_matrix
#' @export
assert_epidist.epidist_multivariate <- function(data, ...) {
  assert_character(data$params, any.missing = FALSE, unique = TRUE)
  assert_numeric(data$index, any.missing = FALSE, unique = TRUE)
  size <- length(data$params) * length(data$index)
  assert_numeric(
    data$value,
    len = size, any.missing = FALSE, finite = TRUE, names = "unique"
  )
  assert_matrix(data$vcov, mode = "numeric", nrows = size, ncols = size)
  if (!isTRUE(all.equal(
    data$vcov, t(data$vcov), tolerance = 1e-8, check.attributes = FALSE
  ))) {
    cli::cli_abort("{.var vcov} must be symmetric.")
  }
  .assert_multivariate_definite(data$vcov)
  if (!is.null(data$draws)) {
    assert_matrix(data$draws, mode = "numeric", ncols = size)
  }
  return(invisible(NULL))
}

#' Print an `epidist_multivariate` object
#'
#' @param x An `epidist_multivariate` object.
#'
#' @param ... Not used.
#'
#' @returns The input, invisibly.
#'
#' @family multivariate
#' @export
#' @examples
#' print(new_epidist_multivariate(
#'   value = c(mean = 7.5, sd = 3.6),
#'   vcov = matrix(c(0.09, 0.02, 0.02, 0.04), nrow = 2),
#'   params = c("mean", "sd")
#' ))
print.epidist_multivariate <- function(x, ...) {
  cli::cli_inform(paste0(
    "A multivariate representation of {length(x$params)} parameter{?s} at ",
    "{length(x$index)} index point{?s}."
  ))
  print(x$value)
  print(x$vcov)
  return(invisible(x))
}

#' The covariance matrix of an `epidist_multivariate` object
#'
#' @param object An `epidist_multivariate` object.
#'
#' @param ... Not used.
#'
#' @returns The covariance matrix.
#'
#' @family multivariate
#' @importFrom stats vcov
#' @export
#' @examples
#' vcov(new_epidist_multivariate(
#'   value = c(mean = 7.5, sd = 3.6),
#'   vcov = matrix(c(0.09, 0.02, 0.02, 0.04), nrow = 2),
#'   params = c("mean", "sd")
#' ))
vcov.epidist_multivariate <- function(object, ...) {
  return(object$vcov)
}

#' Resolve an optional column name of a data frame of draws
#'
#' @param supplied The column name the user gave, or `NULL`.
#'
#' @param draws A data frame of draws.
#'
#' @param default The column name to fall back on where the data frame has one.
#'
#' @returns A column name, or `NULL`.
#'
#' @keywords internal
.multivariate_column <- function(supplied, draws, default) {
  if (is.null(supplied)) {
    return(if (hasName(draws, default)) default else NULL)
  }
  assert_string(supplied, .var.name = default)
  if (!hasName(draws, supplied)) {
    cli::cli_abort(
      "{.var draws} has no column {.val {supplied}}."
    )
  }
  return(supplied)
}

#' The parameter columns of a data frame of draws
#'
#' @param params The columns the user gave, or `NULL`.
#'
#' @param draws A data frame of draws.
#'
#' @param structural The index and draw columns.
#'
#' @returns A character vector of column names.
#'
#' @keywords internal
.multivariate_params <- function(params, draws, structural) {
  if (is.null(params)) {
    numeric_col <- vapply(draws, is.numeric, logical(1))
    params <- setdiff(names(draws)[numeric_col], structural)
    if (length(params) == 0) {
      cli::cli_abort(
        "{.var draws} holds no numeric column to take as a parameter."
      )
    }
    return(params)
  }
  assert_character(params, any.missing = FALSE, unique = TRUE, min.len = 1)
  missing_col <- setdiff(params, names(draws))
  if (length(missing_col) > 0) {
    cli::cli_abort(
      "{.var draws} has no column{?s} {.val {missing_col}}."
    )
  }
  overlap <- intersect(params, structural)
  if (length(overlap) > 0) {
    cli::cli_abort(paste0(
      "{.val {overlap}} identif{?ies/y} the draws rather than holding a ",
      "parameter, so {?it/they} cannot be in {.var params}."
    ))
  }
  return(params)
}

#' Widen a data frame of draws into one column per element
#'
#' @param draws A data frame of draws.
#'
#' @param params The parameter columns.
#'
#' @param index The index column, or `NULL`.
#'
#' @param draw The draw column, or `NULL`.
#'
#' @param index_values The trajectory points, in order.
#'
#' @returns A numeric matrix with one row per draw and one column per element,
#'  ordered index major and parameter minor.
#'
#' @keywords internal
.multivariate_wide <- function(draws, params, index, draw, index_values) {
  non_numeric <- params[!vapply(draws[params], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    cli::cli_abort(
      "{.var {non_numeric}} {?is/are} not numeric."
    )
  }
  if (is.null(index)) {
    wide <- as.matrix(draws[params])
    colnames(wide) <- .multivariate_names(params, index_values)
    return(wide)
  }
  draw_id <- if (is.null(draw)) {
    stats::ave(seq_len(nrow(draws)), draws[[index]], FUN = seq_along)
  } else {
    draws[[draw]]
  }
  blocks <- lapply(index_values, function(point) {
    rows <- draws[[index]] == point
    block <- as.matrix(draws[rows, params, drop = FALSE])
    return(block[order(draw_id[rows]), , drop = FALSE])
  })
  sizes <- vapply(blocks, nrow, integer(1))
  if (length(unique(sizes)) != 1) {
    cli::cli_abort(paste0(
      "Every index point must have the same number of draws, but they range ",
      "from {min(sizes)} to {max(sizes)}."
    ))
  }
  wide <- do.call(cbind, blocks)
  colnames(wide) <- .multivariate_names(params, index_values)
  return(wide)
}

#' The element names of a multivariate representation
#'
#' @param params The parameter names, in order.
#'
#' @param index The trajectory points, in order.
#'
#' @returns A character vector of element names, ordered index major and
#'  parameter minor.
#'
#' @keywords internal
.multivariate_names <- function(params, index) {
  if (length(index) == 1) {
    return(params)
  }
  return(as.vector(vapply(
    index,
    function(point) {
      return(paste0(params, "[", point, "]"))
    },
    character(length(params))
  )))
}

#' Build an `epidist_multivariate` object from a matrix of draws
#'
#' @param wide A numeric matrix with one row per draw and one column per
#'  element.
#'
#' @param params The parameter names, in order.
#'
#' @param index The trajectory points, in order.
#'
#' @returns An object of class `epidist_multivariate`.
#'
#' @keywords internal
.multivariate_from_matrix <- function(wide, params, index) {
  assert_numeric(as.vector(wide), any.missing = FALSE, finite = TRUE)
  if (nrow(wide) <= ncol(wide)) {
    cli::cli_abort(paste0(
      "{.var draws} holds {nrow(wide)} draw{?s} of {ncol(wide)} ",
      "quantit{?y/ies}, so their covariance matrix is singular. More draws ",
      "than reported quantities are needed."
    ))
  }
  covariance <- stats::cov(wide)
  dimnames(covariance) <- NULL
  return(new_epidist_multivariate(
    value = colMeans(wide),
    vcov = covariance,
    params = params,
    index = index,
    n_draws = nrow(wide),
    draws = unname(wide)
  ))
}

#' Check that a covariance matrix has a Cholesky factor
#'
#' A set of quantities that are deterministic functions of fewer underlying
#' parameters has a covariance of the rank of those parameters, not of its own
#' dimension. Five summaries of a two parameter fit are the common case. The
#' message says so, because the alternative reading, that a column is constant
#' or repeated, is usually not what happened.
#'
#' @param vcov A covariance matrix.
#'
#' @returns `NULL`, invisibly.
#'
#' @keywords internal
.assert_multivariate_definite <- function(vcov) {
  values <- eigen(vcov, symmetric = TRUE, only.values = TRUE)$values
  tolerance <- sqrt(.Machine$double.eps) * max(values)
  if (min(values) > tolerance) {
    return(invisible(NULL))
  }
  size <- sum(values > tolerance)
  return(cli::cli_abort(c(
    paste0(
      "The covariance over {ncol(vcov)} quantit{?y/ies} has rank {size}, so ",
      "it has no Cholesky factor and defines no multivariate normal."
    ),
    i = paste0(
      "Quantities that are deterministic functions of fewer underlying ",
      "parameters carry only as many degrees of freedom as those ",
      "parameters. Report at most {size} of them, or fewer index points."
    )
  )))
}
