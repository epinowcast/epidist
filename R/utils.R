#' Read in an `epidist` Stan code chunk
#'
#' This function is used to obtain Stan code chunks from the `stan/` folder of
#' the `epidist` package. It is used within the [epidist_stancode()] function.
#'
#' @param path The path within the `stan/` folder of the installed `epidist`
#' package to the Stan code chunk of interest.
#' @return A character string containing the Stan code chunk of interest.
#' @keywords internal
.stan_chunk <- function(path) {
  local_path <- system.file("stan", path, package = "epidist")
  paste(readLines(local_path), collapse = "\n")
}

#' Label a `epidist` Stan model with a version indicator
#'
#' This function is used within [epidist_stancode()] to label the generated Stan
#' code with the version of `epidist` used. To view the full Stan code for any
#' particular `epidist` model, we recommend use of [brms::make_stancode()].
#'
#' @return A `brms` Stan chunk containing the `epidist` package version used to
#' build the Stan code.
#' @keywords internal
.version_stanvar <- function() {
  version <- utils::packageVersion("epidist")
  comment <- paste0("// code chunks used from epidist ", version, "\n")
  brms::stanvar(scode = comment, block = "functions")
}

#' Round to the nearest multiple
#'
#' This function rounds an input `x` down to the nearest multiple of some number
#' `f`. For example, if `f = 0.2` and `x = 1.5` then the output would be 1.4.
#' If `f = 1` then `floor_mult` behaves as `floor`. If `f = 0` then `x` is
#' returned.
#'
#' @param x A number to be rounded down
#' @param f A positive number specifying the multiple to be rounded down to
#' @keywords internal
.floor_mult <- function(x, f = 1) {
  assert_numeric(f, lower = 0)
  ifelse(f == 0, x, floor(x / f) * f)
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
#' 1. Using the standard `brms` prior specification format which will update
#'    matching priors based on parameter metadata. These priors are joined based
#'    on matching parameter metadata.
#' 2. Using custom set priors with the syntax `parameter ~ distribution`. These
#'    will only remove existing custom priors for the same parameter name but
#'    will not affect priors set via the standard `brms` specification format.
#'    Custom priors are excluded from the metadata-based joining process.
#'
#' @param old_prior One or more prior distributions in the class `brmsprior` to
#'   be updated
#' @param prior One or more prior distributions in the class `brmsprior`
#'   containing the new specifications. Can include custom set priors using the
#'   syntax `parameter ~ distribution`
#' @param warn If `TRUE` then a warning will be displayed if a prior in `prior`
#'   has no match in `old_prior`. Defaults to `FALSE`
#' @param merge If `TRUE` then merge new priors with existing ones, if `FALSE`
#'   only use new priors. Defaults to `TRUE`
#' @autoglobal
#' @importFrom dplyr full_join filter select mutate bind_rows
#' @importFrom brms as.brmsprior
#' @keywords internal
.replace_prior <- function(old_prior, prior, warn = FALSE, merge = TRUE) {
  if (!isTRUE(merge)) {
    return(prior)
  }

  if (is.null(prior)) {
    return(old_prior)
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

  cols <- c("class", "coef", "group", "resp", "dpar", "nlpar", "lb", "ub")
  prior <- dplyr::full_join(
    old_prior, prior,
    by = cols, suffix = c("_old", "_new")
  )

  if (anyNA(prior$prior_old)) {
    missing_prior <- utils::capture.output(print(
      prior |>
        filter(is.na(.data$prior_old)) |>
        select(prior = prior_new, dplyr::all_of(cols), source = source_new)
    ))
    if (warn) {
      msg <- c(
        "!" = "One or more priors have no match in existing parameters:",
        missing_prior,
        "i" = "To remove this warning consider changing prior specification." # nolint
      )
      cli_warn(message = msg)
    }
  }

  # Keep all rows but use new prior where available, otherwise keep old prior
  prior <- prior |>
    mutate(prior = ifelse(
      !is.na(.data$prior_new), .data$prior_new, .data$prior_old
    )) |>
    mutate(source = ifelse(
      !is.na(.data$prior_new), .data$source_new, .data$source_old
    )) |>
    select(prior, dplyr::all_of(cols), source)

  prior <- bind_rows(prior, hold_prior, hold_prior_old)
  return(as.brmsprior(prior))
}

#' Additional distributional parameter information for `brms` families
#'
#' Includes additional information (link functions and parameter bound) about
#' the distributional parameters of a `brms` family which are not the
#' conditional mean `mu`.
#'
#' @inheritParams epidist_family
#' @keywords internal
.add_dpar_info <- function(family) {
  other_links <- family[[paste0("link_", setdiff(family$dpars, "mu"))]] # nolint
  other_bounds <- lapply(
    family$dpars[-1], brms:::dpar_bounds, # nolint
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
#' @param formula ...
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

#' Rename the columns of a `data.frame`
#'
#' @param df ...
#' @param new_names ...
#' @param old_names ...
#' @keywords internal
#' @importFrom stats setNames
.rename_columns <- function(df, new_names, old_names) {
  are_valid <- is.character(new_names) & is.character(old_names)

  valid_new_names <- new_names[are_valid]
  valid_old_names <- old_names[are_valid]

  # Check if old names exist in dataframe
  missing_cols <- setdiff(valid_old_names, names(df))
  if (length(missing_cols) > 0) {
    cli::cli_abort(paste0(
      "The following columns are not present in the data: ",
      toString(missing_cols)
    ))
  }

  if (length(valid_new_names) > 0) {
    rename_map <- setNames(valid_old_names, valid_new_names)
    df <- dplyr::rename(df, !!!rename_map)
  }

  return(df)
}

#' Get a brms function by prefix and family
#'
#' Helper function to get internal brms functions by constructing their name
#' from a prefix and family. Used to get functions like `log_lik_*`,
#' `posterior_predict_*` etc.
#'
#' @param prefix Character string prefix of the brms function to get (e.g.
#' "log_lik")
#'
#' @inheritParams epidist_family
#' @return The requested brms function
#' @keywords internal
.get_brms_fn <- function(prefix, family) {
  get(
    paste0(prefix, "_", tolower(family$family)),
    asNamespace("brms")
  )
}
