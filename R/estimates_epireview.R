#' Report studies from an `epireview` parameter table
#'
#' Maps a table of published delay estimates from the
#' [epireview](https://mrc-ide.github.io/epireview/) package, one row per
#' reported estimate, to the long format [as_epidist_estimates_data()] takes,
#' and returns the `epidist_estimates_data` object it builds.
#' `epireview` collates the parameter estimates gathered by the Pathogen
#' Epidemiology Review Group.
#' It is not on CRAN, and is installed from the
#' [mrc-ide r-universe](https://mrc-ide.r-universe.dev).
#' Filter the `params` table of `epireview::load_epidata()` to one delay
#' before passing it here, because the rows of one object all describe the
#' same delay.
#'
#' # What each record becomes
#'
#' A record is mapped by its `parameter_value_type`.
#'
#' * A `"Mean"` becomes a `"mean"` row.
#'   A `"Standard Deviation"` in `parameter_uncertainty_singe_type` becomes a
#'   matching `"sd"` row, as does the second parameter of a fitted
#'   distribution that `epireview` records as a mean and a standard deviation.
#'   A `"Standard Error"` becomes the `se` of the mean row instead, and so does
#'   a 95% confidence or credible interval of the mean, as its width over
#'   `2 * qnorm(0.975)`.
#' * A `"Median"` becomes a `"quantile"` row at `p = 0.5`, with further rows at
#'   `p = 0.25` and `p = 0.75` where `parameter_uncertainty_type` is `"IQR"`.
#' * A record whose `distribution_type` is a gamma, a Weibull or a lognormal
#'   reported by its natural parameters (a shape with a scale or a rate, or a
#'   meanlog with a sdlog) becomes the mean and standard deviation the fitted
#'   distribution implies over the delays the study could have seen, as
#'   [epidist_estimates_parameters()] computes them.
#'   Its reported value and spread are not used again, because they are
#'   functions of the same parameters.
#'   `epireview` records no uncertainty for the parameters, so the rows take
#'   their sampling uncertainty from the sample size.
#'
#' A range is the smallest and largest delay a study saw rather than a summary
#' of the distribution, and is not used.
#' A spread that does not match the value type, such as a standard deviation
#' reported alongside a median, is not used either, because the two kinds of
#' summary from a study that reported integer date differences are fitted as
#' though they were independent.
#'
#' Records that cannot be mapped are dropped with a message naming them.
#' These are records reporting an inverse rate, units other than days, a value
#' with a scaling exponent, no mean or median and no usable distribution
#' parameters, and records left with no sample size and no standard error.
#' Fill a missing sample size through `metadata`.
#'
#' Several records may share a study, for example estimates stratified by
#' outbreak or by group, and they are then fitted as separate summaries of the
#' same study.
#'
#' # Study metadata
#'
#' `epireview` does not record how a study handled censoring or right
#' truncation, its censoring windows or its observation time.
#' Give the metadata that applies to every record through `...`, and the
#' metadata that differs by study through `metadata`, a data frame with one
#' row per study whose values replace those of `...` for that study's records.
#' Anything given for neither is assumed as [as_epidist_estimates_data()]
#' assumes it, with the same messages, and a study that `metadata` leaves
#' blank for a column it holds is assumed in the same way and named.
#' The Checks section of [as_epidist_estimates_data.data.frame()] says which
#' assumptions matter most.
#'
#' The result is an `epidist_estimates_data` object, so metadata learnt later
#' can be edited in with [dplyr::mutate()] or `[<-`.
#' The object is checked again after each change and drops its class if the
#' change breaks a requirement, so make related changes, such as setting
#' `trunc_adjusted` to `FALSE` and giving the `relative_obs_time` it then
#' needs, in one call.
#' See [epidist_data].
#'
#' @param data A `data.frame` of `epireview` delay estimates, such as the
#'  `params` element of `epireview::load_epidata()` filtered to one delay.
#'  It needs the `parameter_value`, `parameter_value_type` and
#'  `population_sample_size` columns and the column named by `study`.
#'  The other `parameter_*` and `distribution_*` columns are used where
#'  present.
#'
#' @param study A string naming the column of `data` that identifies the
#'  study.
#'  Defaults to `"article_label"`.
#'
#' @param metadata A `data.frame` of study metadata, or `NULL`.
#'  It needs a `study` column holding values of the `study` column of `data`,
#'  and takes any of `n`, `pwindow`, `swindow`, `relative_obs_time`,
#'  `trunc_adjusted`, `trunc_design`, `cens_adjusted`, `delay_min` and
#'  `growth_rate`, as documented in [as_epidist_estimates_data.data.frame()].
#'  Each value replaces the value of that column for the study's records, and
#'  an `NA` leaves it as it is.
#'
#' @param keep A character vector of columns of `data` to carry onto the rows
#'  of the result, for use as covariates.
#'  Defaults to `NULL`.
#'
#' @param advise Whether to run the advisory checks of
#'  [as_epidist_estimates_data.data.frame()].
#'  Defaults to `TRUE`.
#'
#' @param ... Study metadata applied to every record, each a single value, as
#'  documented in [as_epidist_estimates_data.data.frame()].
#'  Any of `pwindow`, `swindow`, `relative_obs_time`, `trunc_adjusted`,
#'  `trunc_design`, `cens_adjusted`, `delay_min` and `growth_rate`.
#'
#' @returns An `epidist_estimates_data` object.
#'
#' @family estimates_data
#' @importFrom checkmate assert_data_frame assert_string assert_character
#'  assert_flag assert_names
#' @importFrom tibble tibble as_tibble
#' @export
#' @examples
#' # A table with the columns epireview uses
#' records <- data.frame(
#'   article_label = c("A 2015", "B 2016", "B 2016"),
#'   parameter_value = c(10.6, 14, 8.9),
#'   parameter_value_type = c("Mean", "Median", "Mean"),
#'   parameter_unit = "Days",
#'   parameter_uncertainty_single_value = c(3.2, NA, NA),
#'   parameter_uncertainty_singe_type = c("Standard Deviation", NA, NA),
#'   parameter_uncertainty_lower_value = c(NA, 11, NA),
#'   parameter_uncertainty_upper_value = c(NA, 15, NA),
#'   parameter_uncertainty_type = c(NA, "IQR", NA),
#'   population_sample_size = c(76, 20, 92)
#' )
#' epidist_estimates_epireview(
#'   records,
#'   metadata = data.frame(
#'     study = "A 2015", relative_obs_time = 60, trunc_adjusted = FALSE
#'   ),
#'   trunc_adjusted = TRUE,
#'   cens_adjusted = 0
#' )
#' @examplesIf requireNamespace("epireview", quietly = TRUE)
#' ebola <- suppressMessages(epireview::load_epidata("ebola"))$params
#' onset_to_death <- ebola[
#'   ebola$parameter_type_short == "delay_onset_to_death",
#' ]
#' epidist_estimates_epireview(
#'   onset_to_death,
#'   trunc_adjusted = TRUE,
#'   cens_adjusted = 0,
#'   keep = "method_moment_value"
#' )
epidist_estimates_epireview <- function(
  data,
  study = "article_label",
  metadata = NULL,
  keep = NULL,
  advise = TRUE,
  ...
) {
  assert_data_frame(data, min.rows = 1)
  assert_string(study)
  assert_character(keep, any.missing = FALSE, null.ok = TRUE)
  assert_flag(advise)
  assert_names(
    names(data),
    must.include = c(
      study, "parameter_value", "parameter_value_type",
      "population_sample_size", keep
    )
  )
  shared <- .epireview_shared_metadata(...)
  data <- as_tibble(data)
  if (hasName(data, "parameter_type")) {
    delays <- unique(data$parameter_type)
    if (length(delays) > 1) {
      cli::cli_abort(c(
        paste0(
          "{.var data} holds {length(delays)} kinds of estimate, ",
          "{.val {delays}}, but the rows of one object must describe the ",
          "same delay."
        ),
        i = "Filter {.var data} to one {.var parameter_type} first."
      ))
    }
  }
  resolved <- .epireview_metadata(
    as.character(data[[study]]), as.numeric(data$population_sample_size),
    metadata, shared
  )
  meta <- resolved$meta
  rows <- .epireview_rows(data, meta)
  kept <- seq_len(nrow(data)) %in% rows$.record
  .epireview_report_gaps(meta, resolved$filled, kept)
  rows <- cbind(
    rows[c("type", "value", "se", "p")],
    meta[rows$.record, ],
    data[rows$.record, keep, drop = FALSE]
  )
  return(as_epidist_estimates_data(as_tibble(rows), advise = advise))
}

#' The metadata columns `epidist_estimates_epireview()` takes
#'
#' @returns A character vector of column names.
#'
#' @keywords internal
.epireview_metadata_cols <- function() {
  return(setdiff(.estimates_metadata_cols(), c("n", "max_delay")))
}

#' Check the shared study metadata passed to `epidist_estimates_epireview()`
#'
#' @param ... Study metadata applied to every record.
#'
#' @returns A named list with one value per metadata column given.
#'
#' @keywords internal
.epireview_shared_metadata <- function(...) {
  shared <- list(...)
  if (length(shared) == 0) {
    return(shared)
  }
  assert_names(
    names(shared),
    type = "unique", subset.of = .epireview_metadata_cols(),
    .var.name = "..."
  )
  sizes <- lengths(shared)
  if (any(sizes != 1)) {
    cli::cli_abort(c(
      paste0(
        "{.var {names(shared)[sizes != 1]}} must be a single value, ",
        "because it applies to every record."
      ),
      i = "Use {.var metadata} for values that differ by study."
    ))
  }
  return(shared)
}

#' Resolve the study metadata of each `epireview` record
#'
#' Starts from the metadata shared by every record, replaces it with the
#' values `metadata` gives for each study, and fills the studies a column
#' leaves blank with the default [as_epidist_estimates_data()] would assume.
#' A column no study has a value for is left out, so that
#' [as_epidist_estimates_data()] assumes it and messages once.
#' The studies filled are reported by [.epireview_report_gaps()] once the
#' records kept are known, so that a dropped record is not named.
#'
#' @param studies A character vector naming the study of each record.
#'
#' @param n A numeric vector of the sample size of each record.
#'
#' @param metadata A `data.frame` of study metadata, or `NULL`.
#'
#' @param shared A named list of metadata applied to every record.
#'
#' @returns A list holding `meta`, a `tibble` with one row per record and the
#'  `study`, `n` and metadata columns that were given, and `filled`, a list
#'  with one logical vector per column filled marking the records filled.
#'
#' @keywords internal
.epireview_metadata <- function(studies, n, metadata, shared) {
  meta <- tibble(study = studies, n = n)
  for (col in names(shared)) {
    meta[[col]] <- rep(shared[[col]], length(studies))
  }
  if (!is.null(metadata)) {
    assert_data_frame(metadata)
    assert_names(
      names(metadata),
      type = "unique", must.include = "study",
      subset.of = c("study", "n", .epireview_metadata_cols()),
      .var.name = "metadata"
    )
    key <- as.character(metadata$study)
    if (anyDuplicated(key) > 0) {
      cli::cli_abort(paste0(
        "{.var metadata} must have one row per study, but ",
        "{.val {unique(key[duplicated(key)])}} {?appears/appear} more than ",
        "once."
      ))
    }
    unknown <- setdiff(key, studies)
    if (length(unknown) > 0) {
      cli::cli_abort(paste0(
        "{.var metadata} names {.val {unknown}}, which {?is/are} not ",
        "{?a study/studies} in {.var data}."
      ))
    }
    index <- match(studies, key)
    for (col in setdiff(names(metadata), "study")) {
      values <- metadata[[col]][index]
      if (!hasName(meta, col)) {
        meta[[col]] <- rep(NA, length(studies))
      }
      meta[[col]][!is.na(values)] <- values[!is.na(values)]
    }
  }
  filled <- list()
  for (col in .epireview_metadata_cols()) {
    if (!hasName(meta, col)) {
      next
    }
    gap <- is.na(meta[[col]])
    if (all(gap)) {
      meta[[col]] <- NULL
    } else if (any(gap)) {
      meta[[col]][gap] <- .epireview_default(col, meta)[gap]
      filled[[col]] <- gap
    }
  }
  return(list(meta = meta, filled = filled))
}

#' The default `as_epidist_estimates_data()` assumes for a metadata column
#'
#' Reads from [.estimates_default_values] so the fallback matches
#' [.fill_estimates_defaults()] without restating each value here.
#'
#' @param col The column.
#'
#' @param meta The metadata of each record, as built by
#'  [.epireview_metadata()].
#'
#' @returns A vector with one default per record.
#'
#' @keywords internal
.epireview_default <- function(col, meta) {
  if (col == "trunc_adjusted") {
    obs_time <- rep(Inf, nrow(meta))
    if (hasName(meta, "relative_obs_time")) {
      obs_time <- meta$relative_obs_time
    }
    return(is.infinite(obs_time))
  }
  return(rep(.estimates_default_values[[col]], nrow(meta)))
}

#' Message about the studies a metadata column left blank
#'
#' A study assumed to have adjusted for right truncation is warned about, as
#' [as_epidist_estimates_data()] warns.
#'
#' @param meta The metadata of each record, as built by
#'  [.epireview_metadata()].
#'
#' @param filled A list with one logical vector per column filled, as built by
#'  [.epireview_metadata()].
#'
#' @param kept A logical vector marking the records kept.
#'
#' @returns `NULL`, invisibly, called for the messages it may raise.
#'
#' @keywords internal
.epireview_report_gaps <- function(meta, filled, kept) {
  for (col in names(filled)) {
    rows <- filled[[col]] & kept
    if (!any(rows)) {
      next
    }
    if (col == "trunc_adjusted") {
      assumed <- unique(meta$study[rows & meta$trunc_adjusted])
      if (length(assumed) > 0) {
        cli::cli_warn(c(
          "!" = paste0(
            "No trunc_adjusted given for {.val {assumed}}, so {?it is/they ",
            "are} assumed to have adjusted for right truncation, having no ",
            "finite relative_obs_time."
          ),
          .estimates_checks_pointer()
        ))
      }
      not_assumed <- unique(meta$study[rows & !meta$trunc_adjusted])
      if (length(not_assumed) > 0) {
        cli::cli_inform(c(
          i = paste0(
            "No trunc_adjusted given for {.val {not_assumed}}, so {?it is/",
            "they are} assumed not to have adjusted for right truncation, ",
            "having a finite relative_obs_time."
          )
        ))
      }
    } else {
      default <- unique(meta[[col]][rows])
      cli::cli_inform(c(
        i = paste0(
          "No {col} given for {.val {unique(meta$study[rows])}}, assuming ",
          "{.val {default}} as for a study with no {col} column."
        )
      ))
    }
  }
  return(invisible(NULL))
}

#' Map each `epireview` record to the rows of the long format
#'
#' @param data A `tibble` of `epireview` records.
#'
#' @param meta The metadata of each record, as built by
#'  [.epireview_metadata()].
#'
#' @returns A `data.frame` with a `.record` column indexing the records and
#'  the `type`, `value`, `se` and `p` of each row.
#'
#' @keywords internal
#' @importFrom dplyr bind_rows
.epireview_rows <- function(data, meta) {
  value <- as.numeric(data$parameter_value)
  value_type <- as.character(data$parameter_value_type)
  spread <- .epireview_column(data, "parameter_uncertainty_single_value")
  spread_type <- .epireview_column(
    data,
    c("parameter_uncertainty_singe_type", "parameter_uncertainty_single_type")
  )
  interval_type <- .epireview_column(data, "parameter_uncertainty_type")
  lower <- .epireview_column(data, "parameter_uncertainty_lower_value")
  upper <- .epireview_column(data, "parameter_uncertainty_upper_value")
  parameters <- .epireview_parameters(data)

  reason <- rep(NA_character_, nrow(data))
  inverse <- as.logical(.epireview_column(data, "inverse_param"))
  reason[which(inverse)] <- "an inverse rate"
  if (hasName(data, "parameter_unit")) {
    unit <- as.character(data$parameter_unit)
    other_unit <- is.na(unit) | unit != "Days"
    reason[is.na(reason) & other_unit] <- "units other than days"
  }
  exponent <- .epireview_column(data, "exponent")
  reason[is.na(reason) & !exponent %in% c(0, NA)] <- "a scaling exponent"
  has_parameters <- !vapply(parameters, is.null, logical(1))
  has_summary <- value_type %in% c("Mean", "Median") & !is.na(value)
  reason[is.na(reason) & !has_parameters & !has_summary] <-
    "no mean or median and no usable distribution parameters"

  rows <- lapply(which(is.na(reason)), function(i) {
    if (has_parameters[i]) {
      support <- do.call(.estimates_reported_support, as.list(
        meta[i, intersect(
          names(meta), c("relative_obs_time", "trunc_adjusted", "delay_min")
        )]
      ))
      implied <- .estimates_parameter_summary(
        parameters[[i]]$family, parameters[[i]]$parameters,
        moments = c("mean", "sd"), probs = numeric(0),
        lower = support$lower, cutoff = support$cutoff
      )
      return(.epireview_row(i, c("mean", "sd"), implied))
    }
    if (value_type[i] == "Mean") {
      se <- NA_real_
      if (isTRUE(spread_type[i] == "Standard Error")) {
        se <- spread[i]
      } else if (
        interval_type[i] %in% c("95% CI", "95% CrI") &&
          !is.na(lower[i]) && !is.na(upper[i])
      ) {
        se <- (upper[i] - lower[i]) / (2 * stats::qnorm(0.975))
      }
      out <- .epireview_row(i, "mean", value[i], se = se)
      reported_sd <- .epireview_sd(data, i, spread, spread_type)
      if (!is.na(reported_sd) && reported_sd > 0) {
        out <- rbind(out, .epireview_row(i, "sd", reported_sd))
      }
      return(out)
    }
    p <- 0.5
    quantiles <- value[i]
    if (
      isTRUE(interval_type[i] == "IQR") && !is.na(lower[i]) && !is.na(upper[i])
    ) {
      p <- c(p, 0.25, 0.75)
      quantiles <- c(quantiles, lower[i], upper[i])
    }
    return(.epireview_row(i, "quantile", quantiles, p = p))
  })
  rows <- bind_rows(rows)
  if (nrow(rows) > 0) {
    no_uncertainty <- is.na(meta$n[rows$.record]) & is.na(rows$se)
    rows <- rows[!no_uncertainty, ]
  }
  unused <- is.na(reason) & !seq_len(nrow(data)) %in% rows$.record
  reason[unused] <- "no sample size and no standard error"
  .epireview_report_dropped(meta$study, reason)
  if (nrow(rows) == 0) {
    cli::cli_abort("No record of {.var data} could be mapped.")
  }
  return(rows)
}

#' The rows of the long format one `epireview` record contributes
#'
#' @param record The index of the record.
#'
#' @param type The summary type of each row.
#'
#' @param value The reported value of each row.
#'
#' @param se The reported standard error of each row.
#'
#' @param p The probability of each quantile row.
#'
#' @returns A `data.frame` with one row per value.
#'
#' @keywords internal
.epireview_row <- function(record, type, value, se = NA_real_, p = NA_real_) {
  return(data.frame(
    .record = record, type = type, value = unname(value), se = se, p = p,
    stringsAsFactors = FALSE
  ))
}

#' The standard deviation an `epireview` record reports alongside its mean
#'
#' @param data A `tibble` of `epireview` records.
#'
#' @param i The record.
#'
#' @param spread The single uncertainty value of each record.
#'
#' @param spread_type The single uncertainty type of each record.
#'
#' @returns The standard deviation, or `NA` where none is reported.
#'  A standard deviation of zero is treated as not reported by the caller,
#'  because [as_epidist_estimates_data()] rejects it.
#'
#' @keywords internal
.epireview_sd <- function(data, i, spread, spread_type) {
  if (isTRUE(spread_type[i] == "Standard Deviation")) {
    return(as.numeric(spread[i]))
  }
  par1_type <- .epireview_parameter_name(
    .epireview_column(data, "distribution_par1_type")[i]
  )
  par2_type <- .epireview_parameter_name(
    .epireview_column(data, "distribution_par2_type")[i]
  )
  if (isTRUE(par1_type == "mean") && isTRUE(par2_type == "sd")) {
    return(as.numeric(.epireview_column(data, "distribution_par2_value")[i]))
  }
  return(NA_real_)
}

#' The natural parameters of the distribution an `epireview` record fitted
#'
#' @param data A `tibble` of `epireview` records.
#'
#' @returns A list with one element per record, `NULL` where the record does
#'  not report a supported family by a supported set of parameters, and
#'  otherwise a list holding the `family` and the named `parameters`.
#'
#' @keywords internal
.epireview_parameters <- function(data) {
  families <- .epireview_family(.epireview_column(data, "distribution_type"))
  names1 <- .epireview_parameter_name(
    .epireview_column(data, "distribution_par1_type")
  )
  names2 <- .epireview_parameter_name(
    .epireview_column(data, "distribution_par2_type")
  )
  values1 <- as.numeric(.epireview_column(data, "distribution_par1_value"))
  values2 <- as.numeric(.epireview_column(data, "distribution_par2_value"))
  sets <- .estimates_parameter_sets()
  return(lapply(seq_len(nrow(data)), function(i) {
    if (is.na(families[i]) || is.na(values1[i]) || is.na(values2[i])) {
      return(NULL)
    }
    par_names <- c(names1[i], names2[i])
    supported <- Filter(
      function(set) {
        return(setequal(set, par_names))
      },
      sets[[families[i]]]
    )
    if (length(supported) == 0) {
      return(NULL)
    }
    parameters <- stats::setNames(c(values1[i], values2[i]), par_names)
    positive <- .estimates_parameter_positive(par_names)
    if (any(parameters[positive] <= 0)) {
      return(NULL)
    }
    return(list(
      family = families[i], parameters = parameters[supported[[1]]]
    ))
  }))
}

#' The family `epidist` calls a distribution `epireview` names
#'
#' @param type A character vector of `epireview` distribution types.
#'
#' @returns A character vector of families, `NA` where the family is not one
#'  [epidist_estimates_parameters()] supports.
#'
#' @keywords internal
.epireview_family <- function(type) {
  type <- gsub("[^a-z]", "", tolower(as.character(type)))
  families <- rep(NA_character_, length(type))
  families[which(type == "gamma")] <- "gamma"
  families[which(type == "weibull")] <- "weibull"
  families[type %in% c("lognormal", "normallog")] <- "lognormal"
  return(families)
}

#' The parameter name `epidist` uses for a parameter type `epireview` names
#'
#' @param type A character vector of `epireview` parameter types.
#'
#' @returns A character vector of parameter names, `NA` where the type is not
#'  recognised.
#'  A mean is `"mean"` and a standard deviation `"sd"`, which are the moments
#'  a fit is often recorded by rather than natural parameters.
#'
#' @keywords internal
.epireview_parameter_name <- function(type) {
  type <- gsub("[^a-z]", "", tolower(as.character(type)))
  name <- rep(NA_character_, length(type))
  name[which(type == "shape")] <- "shape"
  name[which(type == "scale")] <- "scale"
  name[which(type == "rate")] <- "rate"
  name[type %in% c("meanlog", "logmean")] <- "meanlog"
  name[type %in% c("sdlog", "logsd")] <- "sdlog"
  name[which(type == "mean")] <- "mean"
  name[type %in% c("meansd", "standarddeviation", "sd")] <- "sd"
  return(name)
}

#' A column of an `epireview` table, or `NA` where it is absent
#'
#' @param data A `tibble` of `epireview` records.
#'
#' @param names The names the column may have, tried in order.
#'
#' @returns The first of the columns present, or a vector of `NA` with one
#'  entry per record where none is.
#'
#' @keywords internal
.epireview_column <- function(data, names) {
  present <- intersect(names, names(data))
  if (length(present) == 0) {
    return(rep(NA, nrow(data)))
  }
  return(data[[present[1]]])
}

#' Message about the `epireview` records that were dropped
#'
#' @param studies A character vector naming the study of each record.
#'
#' @param reason A character vector giving the reason each record was dropped,
#'  `NA` for a record that was kept.
#'
#' @returns `NULL`, invisibly, called for the message it may raise.
#'
#' @keywords internal
.epireview_report_dropped <- function(studies, reason) {
  dropped <- !is.na(reason)
  if (!any(dropped)) {
    return(invisible(NULL))
  }
  bullets <- vapply(
    unique(reason[dropped]),
    function(why) {
      rows <- dropped & reason == why
      records <- sprintf("\"%s\" (row %d)", studies[rows], which(rows))
      verb <- if (sum(rows) == 1) "reports" else "report"
      return(paste0(toString(records), " ", verb, " ", why, "."))
    },
    character(1)
  )
  cli::cli_inform(c(
    i = paste0(
      "Dropped {sum(dropped)} record{?s} that cannot be mapped to a ",
      "summary of the delay distribution:"
    ),
    stats::setNames(.escape_braces(bullets), rep("*", length(bullets)))
  ))
  return(invisible(NULL))
}
