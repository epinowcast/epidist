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

#' Build the Stan functions block shared by the marginal and meta models
#'
#' Both models read a `functions.stan` chunk with the same placeholders
#' (`family`, `dist_id`, `dpars_A`, `dpars_B`, `primary_id`), filled in with
#' the target distribution's details. Used within
#' [epidist_stancode()] methods for the marginal and meta models, which
#' differ only in the chunk path, the family name prefix, and any further
#' placeholders they need substituted.
#'
#' @param chunk_path Path within the `stan/` folder to the functions chunk.
#'
#' @param family The `epidist` family object.
#'
#' @param family_prefix The model specific prefix stripped from
#'  `family$name`, for example `"marginal_"` or `"meta_"`.
#'
#' @param extra A named character vector of further placeholder
#'  substitutions, applied after the shared ones.
#'
#' @returns A `brms` `stanvars` object holding the substituted functions
#'  chunk.
#'
#' @keywords internal
.family_functions_stanvar <- function(
  chunk_path,
  family,
  family_prefix,
  extra = character()
) {
  stanvars_functions <- brms::stanvar(
    block = "functions",
    scode = .stan_chunk(chunk_path)
  )

  family_name <- gsub(family_prefix, "", family$name, fixed = TRUE)
  dist_id <- primarycensored::pcd_stan_dist_id(family_name)

  substitutions <- c(
    family = family_name,
    dist_id = as.character(dist_id),
    dpars_A = toString(paste0("real ", family$dpars)),
    dpars_B = family$param,
    primary_id = "1",
    extra
  )

  for (placeholder in names(substitutions)) {
    stanvars_functions[[1]]$scode <- gsub(
      placeholder,
      substitutions[[placeholder]],
      stanvars_functions[[1]]$scode,
      fixed = TRUE
    )
  }

  return(stanvars_functions)
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

#' Identify manually specified `brms` priors
#'
#' Manual priors are written using the `parameter ~ distribution` syntax and
#' are passed through to the Stan model block unchanged. They cannot be
#' matched on parameter metadata in the way that standard `brms` priors can.
#'
#' @param prior One or more prior distributions in the class `brmsprior`.
#'
#' @returns A logical vector flagging the manually specified priors.
#'
#' @keywords internal
.is_manual_prior <- function(prior) {
  return(grepl("~", prior$prior, fixed = TRUE))
}

#' Extract the parameter name of a manually specified `brms` prior
#'
#' @param prior One or more manually specified prior distributions in the
#'   class `brmsprior`.
#'
#' @returns A character vector of parameter names.
#'
#' @keywords internal
.manual_prior_parameter <- function(prior) {
  return(trimws(sub("~.*$", "", prior$prior)))
}

#' The columns used to match `brms` prior distributions
#'
#' @returns A character vector of column names.
#'
#' @keywords internal
.prior_match_cols <- function() {
  return(c("class", "coef", "group", "resp", "dpar", "nlpar"))
}

#' Choose between the new and the old value of a prior column
#'
#' Keeps the result a character vector when there are no priors to choose
#' between, so that the result can still be combined with other priors.
#'
#' @param updated A logical vector flagging where a new prior was supplied.
#'
#' @param new The values from the new priors.
#'
#' @param old The values from the old priors.
#'
#' @returns A character vector the same length as `updated`.
#'
#' @keywords internal
.pick_prior_col <- function(updated, new, old) {
  return(as.character(ifelse(updated, new, old)))
}

#' Replace `brms` prior distributions
#'
#' This function takes an existing set of prior distributions and updates them
#' with new prior specifications. It matches priors based on their parameter
#' class, coefficient, group, response, distributional parameter, and non-linear
#' parameter.
#'
#' Prior distributions can be specified in two ways:
#' 1. Using the standard `brms` prior specification format. These priors are
#'    replaced based on matching parameter metadata (class, coefficient, group,
#'    etc.).
#' 2. Using manually specified priors with the syntax
#'    `parameter ~ distribution`. These replace existing manual priors for the
#'    same parameter name and are otherwise left alone. Manual priors are
#'    excluded from the metadata based matching.
#'
#' @param old_prior One or more prior distributions in the class `brmsprior` to
#'   be updated.
#'
#' @param prior One or more prior distributions in the class `brmsprior`
#'   containing the new specifications. Can include manually specified priors
#'   using the syntax `parameter ~ distribution`.
#'
#' @param enforce_presence If `TRUE` then only keep rows that have both old and
#'   new priors. If `FALSE` then keep all rows but use new priors where
#'   available, otherwise keep old priors. Defaults to `TRUE`.
#'
#' @returns A `brmsprior` object containing the updated prior distributions.
#'
#' @keywords internal
#' @importFrom dplyr full_join filter select mutate bind_rows
#' @importFrom brms as.brmsprior
#' @autoglobal
.replace_prior <- function(old_prior, prior, enforce_presence = TRUE) {
  if (is.null(prior)) {
    return(old_prior)
  }

  if (is.null(old_prior)) {
    return(prior)
  }

  new_manual <- .is_manual_prior(prior)
  old_manual <- .is_manual_prior(old_prior)

  # Manual priors replace old manual priors for the same parameter and are
  # otherwise carried through untouched
  replaced <- .manual_prior_parameter(old_prior[old_manual, ]) %in%
    .manual_prior_parameter(prior[new_manual, ])
  manual <- bind_rows(prior[new_manual, ], old_prior[old_manual, ][!replaced, ])

  standard <- full_join(
    old_prior[!old_manual, ],
    prior[!new_manual, ],
    by = .prior_match_cols(),
    suffix = c("_old", "_new")
  )

  # Use the new prior, and its bounds, where one has been supplied
  standard <- mutate(
    standard,
    updated = !is.na(.data$prior_new),
    lb = .pick_prior_col(.data$updated, .data$lb_new, .data$lb_old),
    ub = .pick_prior_col(.data$updated, .data$ub_new, .data$ub_old),
    prior = .pick_prior_col(.data$updated, .data$prior_new, .data$prior_old),
    source = .pick_prior_col(
      .data$updated, .data$source_new, .data$source_old
    )
  )

  if (isTRUE(enforce_presence)) {
    standard <- filter(standard, !is.na(.data$prior_old), .data$updated)
  }

  standard <- select(
    standard,
    prior,
    dplyr::all_of(c(.prior_match_cols(), "lb", "ub")),
    source
  )

  return(as.brmsprior(bind_rows(standard, manual)))
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
    .dpar_bounds,
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
#' @returns Nothing, called for side effects only
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
#' @param weight A column name to use for weighting. If NULL, weights default
#'  to 1. Internally this is used to define the 'n' column of the returned
#'  object.
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

#' Add delay_min column to data
#'
#' Resolves the `delay_min` argument into a column on the data frame.
#' If NULL, uses an existing `delay_min` column or defaults to 0.
#' If numeric, uses that scalar. If character, looks up the named
#' column.
#'
#' @param data A data frame
#' @param delay_min NULL, a numeric scalar, or a column name string
#' @return The data frame with a `delay_min` column
#' @keywords internal
.add_delay_min <- function(data, delay_min = NULL) {
  if (is.null(delay_min)) {
    if (!"delay_min" %in% names(data)) {
      data$delay_min <- 0
    }
  } else if (is.character(delay_min)) {
    assert_character(delay_min, len = 1)
    assert_names(names(data), must.include = delay_min)
    data$delay_min <- data[[delay_min]]
  } else if (is.numeric(delay_min)) {
    assert_numeric(delay_min, lower = 0, len = 1, any.missing = FALSE)
    data$delay_min <- delay_min
  } else {
    cli::cli_abort(
      "{.var delay_min} must be NULL, a column name, or a numeric scalar."
    )
  }
  return(data)
}

#' Capture the environment variables `rstan` leaks when compiling a model
#'
#' The `rstan` backend compiles through `inline::cxxfunction()`, which sets
#' `PKG_CPPFLAGS` and `PKG_LIBS` and never restores them. The leaked flags make
#' the next `pkgbuild::has_build_tools()` check fail, which prints a spurious
#' compiler error before the model compiles and fits successfully. Restore them
#' with [.restore_compile_env()] after a fit.
#'
#' @returns A named character vector of the current values, `NA` where unset.
#'
#' @keywords internal
.capture_compile_env <- function() {
  return(Sys.getenv(c("PKG_CPPFLAGS", "PKG_LIBS"), names = TRUE, unset = NA))
}

#' Restore environment variables captured by [.capture_compile_env()]
#'
#' @param vars A named character vector as returned by
#'  [.capture_compile_env()], with `NA` for variables that were unset.
#'
#' @returns Nothing, called for side effects only
#'
#' @keywords internal
.restore_compile_env <- function(vars) {
  unset <- is.na(vars)
  if (any(unset)) {
    Sys.unsetenv(names(vars)[unset])
  }
  if (!all(unset)) {
    do.call(Sys.setenv, as.list(vars[!unset]))
  }
  return(invisible(NULL))
}
