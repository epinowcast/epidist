#' Read in an `epidist` Stan code chunk
#'
#' This function is used to obtain Stan code chunks from the `stan/` folder of
#' the `epidist` package. It is used within the [epidist_stancode()] function.
#'
#' @param path The path within the `stan/` folder of the installed `epidist`
#'  package to the Stan code chunk of interest.
#'
#' @return A character string containing the Stan code chunk of interest.
#'
#' @keywords internal
.stan_chunk <- function(path) {
  local_path <- system.file("stan", path, package = "epidist")
  return(paste(readLines(local_path), collapse = "\n"))
}

#' Label a `epidist` Stan model with a version indicator
#'
#' This function is used within [epidist_stancode()] to label the generated Stan
#' code with the version of `epidist` used. To view the full Stan code for any
#' particular `epidist` model, we recommend use of [brms::make_stancode()].
#'
#' @return A `brms` Stan chunk containing the `epidist` package version used to
#'  build the Stan code.
#'
#' @keywords internal
.version_stanvar <- function() {
  pkg_version <- utils::packageVersion("epidist")
  version_comment <- paste0(
    "// code chunks used from epidist ",
    pkg_version,
    "\n"
  )
  return(brms::stanvar(scode = version_comment, block = "functions"))
}

#' Round to the nearest multiple
#'
#' This function rounds an input `x` down to the nearest multiple of some number
#' `f`. For example, if `f = 0.2` and `x = 1.5` then the output would be 1.4.
#' If `f = 1` then `floor_mult` behaves as `floor`. If `f = 0` then `x` is
#' returned.
#'
#' @param x A number to be rounded down.
#'
#' @param f A positive number specifying the multiple to be rounded down to
#'
#' @keywords internal
.floor_mult <- function(x, f = 1) {
  assert_numeric(f, lower = 0)
  return(ifelse(f == 0, x, floor(x / f) * f))
}

#' Replace `brms` prior distributions
#'
#' This function takes an existing set of prior distributions and updates them
#' with new prior specifications. It matches priors based on their parameter
#' class, coefficient, group, response, distributional parameter, and non-linear
#' parameter. Any new priors that don't match existing ones can optionally
#' trigger a warning.
#'
#' Prior distributions can be specified in two ways:
#' 1. Using the standard `brms` prior specification format. These priors are
#'    replaced based on matching parameter metadata (class, coefficient, group,
#'    etc.).
#' 2. Using custom set priors with the syntax `parameter ~ distribution`. These
#'    will only remove existing custom priors for the same parameter name but
#'    will not affect priors set via the standard `brms` specification format.
#'    Custom priors are excluded from the metadata-based joining process.
#'
#' @param old_prior One or more prior distributions in the class `brmsprior` to
#'   be updated.
#'
#' @param prior One or more prior distributions in the class `brmsprior`
#'   containing the new specifications. Can include custom set priors using the
#'   syntax `parameter ~ distribution`
#'
#' @param warn If `TRUE` then a warning will be displayed if a prior in `prior`
#'   has no match in `old_prior`. Defaults to `FALSE`
#'
#' @param merge If `TRUE` then merge new priors with existing ones, if `FALSE`
#'   only use new priors. Defaults to `TRUE`
#'
#' @param enforce_presence If `TRUE` then only keep rows that have both old and
#'   new priors. If `FALSE` then keep all rows but use new priors where
#'   available, otherwise keep old priors. Defaults to `TRUE`.
#'
#' @keywords internal
#' @importFrom dplyr full_join filter select mutate bind_rows
#' @importFrom brms as.brmsprior
#' @autoglobal
.replace_prior <- function(
    old_prior,
    prior,
    warn = FALSE,
    merge = TRUE,
    enforce_presence = TRUE) {
  if (!isTRUE(merge)) {
    return(prior)
  }

  if (is.null(prior)) {
    return(old_prior)
  }

  if (is.null(old_prior)) {
    return(prior)
  }

  # Find priors defined with ~ in prior column
  tilde_priors <- prior[grepl("~", prior$prior, fix, fixed = TRUE), ]
  if (nrow(tilde_priors) > 0) {
    # Extract parameter names from left side of ~
    param_names <- gsub("\\s*~.*$", "", tilde_priors$prior)

    # Remove matching parameter priors from old_prior
    old_prior <- old_prior[
      !grepl(paste(param_names, collapse = "|"), old_prior$prior),
    ]
  }

  # Hold out manual priors
  hold_prior <- prior[grepl("~", prior$prior, fixed = TRUE), ]
  hold_prior_old <- old_prior[grepl("~", old_prior$prior, fixed = TRUE), ]

  prior <- prior[!grepl("~", prior$prior, fixed = TRUE), ]
  old_prior <- old_prior[!grepl("~", old_prior$prior, fixed = TRUE), ]

  join_cols <- c("class", "coef", "group", "resp", "dpar", "nlpar")
  cols <- c(join_cols, "lb", "ub")

  if (nrow(prior) == 0 && nrow(old_prior) == 0) {
    # If no non-manual priors found, just combine manual priors
    cli::cli_inform("No non-manual priors found, only combining manual priors")
    prior <- bind_rows(hold_prior, hold_prior_old)
  } else {
    prior <- dplyr::full_join(
      old_prior,
      prior,
      by = join_cols,
      suffix = c("_old", "_new")
    )

    if (anyNA(prior$prior_old)) {
      missing_prior <- utils::capture.output(
        print(
          as.data.frame(filter(prior, is.na(.data$prior_old)))
        )
      )
      if (warn) {
        msg <- c(
          "!" = "One or more priors have no match in existing parameters:",
          missing_prior,
          "i" = "To remove this warning consider changing prior specification." # nolint
        )
        cli_warn(message = msg)
      }
    }

    # use new lb and ub if prior_new is present
    prior <- mutate(
      prior,
      lb = ifelse(!is.na(.data$prior_new), .data$lb_new, .data$lb_old),
      ub = ifelse(!is.na(.data$prior_new), .data$ub_new, .data$ub_old),
      prior = ifelse(
        !is.na(.data$prior_new),
        .data$prior_new,
        .data$prior_old
      ),
      source = ifelse(
        !is.na(.data$prior_new),
        .data$source_new,
        .data$source_old
      )
    )

    # only keep rows that have both old and new priors
    if (isTRUE(enforce_presence)) {
      prior <- filter(prior, !is.na(.data$prior_old), !is.na(.data$prior_new))
    }

    prior <- prior |>
      select(prior, dplyr::all_of(cols), source) |>
      bind_rows(hold_prior, hold_prior_old)
  }
  return(as.brmsprior(prior))
}

#' Additional distributional parameter information for `brms` families
#'
#' Includes additional information (link functions and parameter bound) about
#' the distributional parameters of a `brms` family which are not the
#' conditional mean `mu`.
#'
#' @inheritParams epidist_family
#'
#' @keywords internal
.add_dpar_info <- function(family) {
  other_links <- family[[paste0("link_", setdiff(family$dpars, "mu"))]] # nolint
  other_bounds <- lapply(
    family$dpars[-1],
    brms:::dpar_bounds, # nolint
    family = family$family
  )
  family$other_links <- other_links
  family$other_bounds <- other_bounds
  return(family)
}

#' Include implicit intercepts in `brms` formula as explicit
#'
#' This function detects the distributional parameters in a `brms` formula
#' object, and alters to formula to include explicit intercept parameters for
#' them i.e. `~ 1`.
#'
#' @param formula A `brms` formula object.
#'
#' @keywords internal
.make_intercepts_explicit <- function(formula) {
  other_dpars <- setdiff(formula$family$dpars, "mu")
  fixed_dpars <- names(formula$pfix)
  formula_dpars <- names(formula$pforms)
  replace_dpars <- setdiff(other_dpars, c(fixed_dpars, formula_dpars))
  for (dpar in replace_dpars) {
    new_formula <- as.formula(paste0(dpar, " ~ 1"))
    formula$pforms[[dpar]] <- new_formula
  }
  return(formula)
}

#' Extract distributional parameter terms from a brms formula
#'
#' This function extracts all unique terms from the right-hand side of all
#' distributional parameters in a brms formula.
#'
#' @param formula A `brms` formula object.
#'
#' @return A character vector of unique terms.
#'
#' @keywords internal
.extract_dpar_terms <- function(formula) {
  formula_terms <- brms::brmsterms(formula)
  # Extract all terms from the right hand side of all dpars
  dpar_terms <- purrr::map(formula_terms$dpars, \(x) all.vars(x$allvars))
  dpar_terms <- unique(unlist(dpar_terms))
  return(dpar_terms)
}

#' Summarise data by grouping variables and count occurrences
#'
#' @param data A `data.frame` to summarise which must contain a `n` column
#' which is a count of occurrences.
#'
#' @param by Character vector of column names to group by.
#'
#' @param formula Optional `brms` formula object to extract additional grouping
#'  terms from.
#'
#' @return A `data.frame` summarised by the grouping variables with counts.
#'
#' @keywords internal
#' @importFrom dplyr group_by summarise across
.summarise_n_by_formula <- function(data, by = character(), formula = NULL) {
  if (!is.null(formula)) {
    formula_terms <- .extract_dpar_terms(formula)
    by <- c(by, formula_terms)
  }
  # Remove duplicates
  by <- unique(by)

  sum_data <- tibble::as_tibble(data)
  sum_data <- summarise(sum_data, n = sum(.data$n), .by = dplyr::all_of(by))
  return(sum_data)
}

#' Inform users about data summarisation
#'
#' This function informs users when data has been summarised by unique
#' combinations of variables, providing information about the variables used and
#' the reduction in number of rows.
#'
#' @param data The original data before summarisation
#'
#' @param trans_data The transformed/summarised data
#'
#' @param required_cols Character vector of required column names
#' @return Nothing, called for side effects only
#'
#' @keywords internal
.inform_data_summarised <- function(data, trans_data, required_cols) {
  n_rows_before <- nrow(data)
  n_rows_after <- nrow(trans_data)

  if (n_rows_before > n_rows_after) {
    cli::cli_inform(c(
      "i" = "Data summarised by unique combinations of:" # nolint
    ))

    formula_vars <- setdiff(names(trans_data), c(required_cols))
    if (length(formula_vars) > 0) {
      cli::cli_inform(c(
        "*" = "Formula variables: {.code {formula_vars}}"
      ))
    }

    cli::cli_inform(paste0(
      "* Model variables: delay bounds, observation time, ",
      "and primary censoring window"
    ))

    cli::cli_inform(c(
      "!" = paste("Reduced from", n_rows_before, "to", n_rows_after, "rows."),
      "i" = "This should improve model efficiency with no loss of information." # nolint
    ))
  }
  return(invisible(NULL))
}

#' Rename the columns of a `data.frame`
#'
#' @param data A `data.frame` to rename the columns of.
#'
#' @param new_names A character vector of new column names.
#'
#' @param old_names A character vector of old column names.
#'
#' @keywords internal
#' @importFrom stats setNames
.rename_columns <- function(data, new_names, old_names) {
  are_valid <- is.character(new_names) & is.character(old_names)

  valid_new_names <- new_names[are_valid]
  valid_old_names <- old_names[are_valid]

  # Check if old names exist in dataframe
  missing_cols <- setdiff(valid_old_names, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(paste0(
      "The following columns are not present in the data: ",
      toString(missing_cols)
    ))
  }

  if (length(valid_new_names) > 0) {
    rename_map <- setNames(valid_old_names, valid_new_names)
    data <- dplyr::rename(data, !!!rename_map)
  }

  return(data)
}

#' Get a brms function by prefix and family
#'
#' Helper function to get internal brms functions by constructing their name
#' from a prefix and family. Used to get functions like `log_lik_*`,
#' `posterior_predict_*` etc.
#'
#' @param prefix Character string prefix of the brms function to get (e.g.
#'  "log_lik")
#'
#' @inheritParams epidist_family
#'
#' @return The requested brms function
#'
#' @keywords internal
.get_brms_fn <- function(prefix, family) {
  return(get(
    paste0(prefix, "_", tolower(family$family)),
    asNamespace("brms")
  ))
}

#' Add weights to a data frame
#'
#' Helper function to add weights to a data frame, either from an existing
#' column or defaulting to 1.
#'
#' @param data A data frame to add weights to
#'
#' @param weight A column name to use for weighting the data in the
#'  likelihood. Default is NULL. Internally this is used to define the 'n'
#'  column of the returned object.
#'
#' @return The data frame with an added 'n' column containing the weights
#'
#' @keywords internal
.add_weights <- function(data, weight = NULL) {
  if (!is.null(weight)) {
    assert_names(names(data), must.include = weight)
    data$n <- data[[weight]]
  } else {
    data$n <- 1
  }
  return(data)
}
