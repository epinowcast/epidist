# nolint start: line_length_linter.
#' Non-parametric delay distribution family
#'
#' @description
#' A delay distribution with no parametric form, for use with the marginal
#' and meta models. Pass it as the `family` argument of [epidist()] in the
#' same way as [brms::lognormal()].
#'
#' The delay sits on a grid of \eqn{K} bins with boundaries
#' \eqn{b_0 < b_1 < \dots < b_K}. All the probability of bin \eqn{k} is
#' placed at its right edge \eqn{b_k}, so the delay distribution function is
#' a step function. This is the discrete hazard distribution of
#' [primarycensored::pdiscretehazard()], and the likelihood is the
#' `primarycensored` one for that distribution.
#'
#' The distribution is written in terms of the discrete time hazard of each
#' bin, the probability that the delay ends in bin \eqn{k} given that it did
#' not end before it. The hazard of the last bin is 1, so all the delays end
#' by \eqn{b_K}. The logit hazards of the other bins are a linear predictor
#' over the bins,
#' \deqn{\mathrm{logit}(h_k) = \mu + \sum_q B_{kq} \theta_q,}
#' where the basis \eqn{B} comes from `formula` evaluated on the bins.
#'
#' @details
#' # The hazard formula
#'
#' `formula` is a one sided formula in two variables of the bins: `delay`,
#' the right edge \eqn{b_k} of each bin, and `bin`, a factor with one level
#' per bin. It takes the terms of a `brms` formula that make sense on the
#' bins:
#'
#' * parametric terms such as `delay`, `I(delay^2)` or `bin`,
#' * smooths such as `s(delay)`, `s(delay, bs = "ps")` or `t2(delay)`, built
#'   with [mgcv::smoothCon()] as `brms` builds them,
#' * a random intercept per bin, `(1 | bin)`, which is the smooth
#'   `s(bin, bs = "re")`.
#'
#' `~ 1` gives the same hazard in every bin, a geometric delay. The default,
#' `NULL`, is `~ s(delay, k = min(10, K - 1))`, a thin plate regression
#' spline over the delay, which smooths the hazard over neighbouring bins.
#' With fewer than three free bins it is `~ (1 | bin)`.
#'
#' The intercept of the formula is dropped, because \eqn{\mu} is the
#' intercept. Each column of the basis is centred over the bins, so
#' \eqn{\mu} is the mean logit hazard, and scaled, so that each coefficient
#' is on the scale of the logit hazard.
#'
#' # Parameters
#'
#' Each coefficient \eqn{\theta_q} is a distributional parameter, so it
#' appears in the output of [epidist()] and takes a `brms` formula and prior
#' in the same way as `mu`:
#'
#' * `h<i>b` is the coefficient of the \eqn{i}th unpenalised column, such as
#'   a parametric term or the linear part of a spline.
#' * `h<j>sd` is the standard deviation of the \eqn{j}th penalised term, such
#'   as the wiggly part of a spline or the random intercept per bin.
#' * `h<i>z` is the \eqn{i}th standardised coefficient of the penalised
#'   terms, so the coefficient is `h<j>sd * h<i>z` for the term \eqn{j} the
#'   column belongs to. This is the non-centred form `brms` uses for smooths
#'   and random effects.
#'
#' The `coefficients` element of the `np` element of the family built by
#' [epidist_family()] names the term of each coefficient.
#'
#' # Covariates
#'
#' A covariate in the `mu` formula shifts the logit hazard of every bin by
#' the same amount, which is a proportional odds model for the hazard. A
#' covariate in the formula of a coefficient changes the shape of the
#' hazard over the bins, a non-proportional effect. For example with
#' `formula = ~ delay`, `bf(mu ~ age_group, h1b ~ age_group)` gives each age
#' group its own intercept and slope of the logit hazard over the delay, as
#' `delay * age_group` would.
#'
#' # Priors
#'
#' The default priors are `normal(logit(1 / K), 1.5)` on the intercept of
#' `mu`, centred on the hazard of the first bin when every bin is equally
#' likely, `normal(0, 2)` on each `h<i>b`, `normal(0, 1)` on the log of each
#' `h<j>sd`, as `primarycensored` uses for the spread of the logit hazards,
#' and `std_normal()` on each `h<i>z`. Set others with the `prior` argument
#' of [epidist()].
#'
#' @param formula A one sided formula for the logit hazards over the bins,
#'  see Details. The default, `NULL`, is a spline over the delay.
#'
#' @param boundaries A numeric vector of at least four strictly increasing
#'  bin boundaries, \eqn{b_0} to \eqn{b_K}. The default, `NULL`, is set by
#'  [epidist()] from the data, with a bin for every whole delay from 0 up to
#'  the longest delay in the data, and at least four bins, that is
#'  `seq(-1, max(3, max_delay))`. The last boundary must be at least as long
#'  as the longest observed delay.
#'
#' @returns A `brmsfamily` object for use as the `family` argument of
#'  [epidist()].
#'
#' @seealso The non-parametric section of `vignette("model")` for the
#'  model, `vignette("nonparametric")` for a worked example, and the
#'  `primarycensored` article on fitting non-parametric delays
#'  (\url{https://primarycensored.epinowcast.org/articles/fitting-nonparametric-delays.html})
#'  for the censored likelihood.
#' @family family
#' @export
#' @examples
#' nonparametric(boundaries = -1:10)
#' nonparametric(~ (1 | bin), boundaries = -1:10)
#' nonparametric(~ s(delay, bs = "ps", k = 6), boundaries = c(-1:5, 7, 10))
nonparametric <- function(formula = NULL, boundaries = NULL) {
  if (!is.null(formula)) {
    formula <- .np_check_formula(formula)
  }
  out <- list(
    family = "nonparametric",
    link = "identity",
    dpars = "mu",
    ybounds = c(-Inf, Inf),
    np = list(boundaries = NULL, formula = formula)
  )
  class(out) <- c("brmsfamily", "family")
  if (!is.null(boundaries)) {
    out <- .np_set_boundaries(out, boundaries)
  }
  return(out)
}
# nolint end

#' Check the hazard formula of the non-parametric family
#'
#' @inheritParams nonparametric
#'
#' @returns The formula, with any `(1 | bin)` term written as
#'  `s(bin, bs = "re")`.
#'
#' @keywords internal
.np_check_formula <- function(formula) {
  if (!inherits(formula, "formula") || length(formula) != 2) {
    cli_abort(
      "{.arg formula} must be a one sided formula, such as
       {.code ~ s(delay)}."
    )
  }
  formula <- .np_bars_to_smooths(formula)
  vars <- setdiff(all.vars(formula), c("delay", "bin"))
  if (length(vars) > 0) {
    cli_abort(c(
      "{.arg formula} of {.fn nonparametric} can only use the bin variables
       {.var delay} and {.var bin}, not {.var {vars}}.",
      i = "Put covariates in the formula of {.var mu} or of a hazard
           coefficient, see {.fn nonparametric}."
    ))
  }
  return(formula)
}

#' Write random intercepts per bin as random effect smooths
#'
#' @inheritParams nonparametric
#'
#' @returns The formula with each `(1 | g)` term replaced by
#'  `s(g, bs = "re")`.
#'
#' @keywords internal
.np_bars_to_smooths <- function(formula) {
  formula_terms <- stats::terms(formula)
  term_labels <- attr(formula_terms, "term.labels")
  bars <- grepl("|", term_labels, fixed = TRUE)
  if (!any(bars)) {
    return(formula)
  }
  term_labels[bars] <- vapply(term_labels[bars], function(label) {
    parts <- trimws(strsplit(label, "||", fixed = TRUE)[[1]])
    if (length(parts) == 1) {
      parts <- trimws(strsplit(label, "|", fixed = TRUE)[[1]])
    }
    if (length(parts) != 2 || parts[1] != "1") {
      cli_abort(
        "{.arg formula} of {.fn nonparametric} only takes random
         intercepts, such as {.code (1 | bin)}, not {.code ({label})}."
      )
    }
    return(sprintf("s(%s, bs = \"re\")", parts[2]))
  }, character(1))
  return(stats::reformulate(
    term_labels,
    intercept = attr(formula_terms, "intercept") == 1,
    env = environment(formula)
  ))
}

#' Set the bin boundaries of the non-parametric family
#'
#' Fixes the number of bins, and so the basis of the hazard formula, the
#' distributional parameters, their links and their bounds.
#'
#' @param family A family built by [nonparametric()].
#'
#' @inheritParams nonparametric
#'
#' @returns The family with its boundaries, basis and parameters set.
#'
#' @keywords internal
.np_set_boundaries <- function(family, boundaries) {
  boundaries <- .assert_np_boundaries(boundaries)
  hazard_formula <- family$np$formula
  if (is.null(hazard_formula)) {
    hazard_formula <- .np_default_formula(length(boundaries) - 1)
  }
  basis <- .np_basis(hazard_formula, boundaries)
  family$np <- c(
    list(boundaries = boundaries, formula = hazard_formula), basis
  )
  family$dpars <- c("mu", .np_dpars(family$np))
  links <- ifelse(
    family$dpars[-1] %in% basis$coefficients$sd, "log", "identity"
  )
  family$other_links <- links
  family$other_bounds <- lapply(links, function(link) {
    return(list(lb = ifelse(link == "log", "0", ""), ub = ""))
  })
  return(family)
}

#' The default hazard formula of the non-parametric family
#'
#' @param n_bins The number of bins, \eqn{K}.
#'
#' @returns A one sided formula.
#'
#' @keywords internal
.np_default_formula <- function(n_bins) {
  free <- n_bins - 1
  if (free < 3) {
    return(~ s(bin, bs = "re"))
  }
  return(stats::as.formula(
    sprintf("~ s(delay, k = %d)", min(10L, free)),
    env = globalenv()
  ))
}

#' The bins the hazard formula is evaluated on
#'
#' @inheritParams nonparametric
#'
#' @returns A `data.frame` with one row per bin whose hazard is free, all
#'  but the last, holding the right edge of the bin as `delay` and its
#'  index as the factor `bin`.
#'
#' @keywords internal
.np_bins <- function(boundaries) {
  free <- length(boundaries) - 2
  return(data.frame(
    delay = boundaries[seq_len(free) + 1],
    bin = factor(seq_len(free))
  ))
}

#' Build the basis of the hazard formula over the bins
#'
#' Parametric terms come from [stats::model.matrix()] and smooths from
#' [mgcv::smoothCon()] and [mgcv::smooth2random()], which split each smooth
#' into unpenalised columns and penalised blocks with one standard deviation
#' each, as `brms` does. Each column is centred over the bins, each
#' unpenalised column is scaled to unit standard deviation and each
#' penalised block to unit root mean square.
#'
#' @inheritParams nonparametric
#'
#' @returns A list holding `basis`, a matrix with one row per free bin and
#'  one column per coefficient, and `coefficients`, a `data.frame` with one
#'  row per column giving the `term` of the column, its parameter `dpar`,
#'  and the standard deviation parameter `sd` of its penalised term, `NA`
#'  for an unpenalised column.
#'
#' @keywords internal
.np_basis <- function(formula, boundaries) {
  bins <- .np_bins(boundaries)
  gam <- mgcv::interpret.gam(formula)
  parametric <- stats::terms(gam$pf)
  attr(parametric, "intercept") <- 1L
  fixed <- stats::model.matrix(parametric, bins)
  fixed_terms <- attr(parametric, "term.labels")[attr(fixed, "assign")]
  keep <- colnames(fixed) != "(Intercept)"
  fixed <- fixed[, keep, drop = FALSE]
  penalised <- list()
  penalised_terms <- character()
  for (spec in gam$smooth.spec) {
    smooths <- mgcv::smoothCon(
      spec,
      data = bins, absorb.cons = TRUE, diagonal.penalty = TRUE
    )
    for (smooth in smooths) {
      random <- mgcv::smooth2random(smooth, names(bins), type = 2)
      fixed <- cbind(fixed, random$Xf)
      fixed_terms <- c(fixed_terms, rep(smooth$label, ncol(random$Xf)))
      penalised <- c(penalised, unname(random$rand))
      penalised_terms <- c(
        penalised_terms, rep(smooth$label, length(random$rand))
      )
    }
  }
  fixed <- scale(fixed, center = TRUE, scale = FALSE)
  fixed <- sweep(fixed, 2, sqrt(colMeans(fixed^2)), "/")
  penalised <- lapply(penalised, function(block) {
    block <- scale(block, center = TRUE, scale = FALSE)
    return(block / sqrt(mean(rowSums(block^2))))
  })
  basis <- do.call(cbind, c(list(fixed), penalised))
  dimnames(basis) <- NULL
  # Penalised terms are identified by their prior, as in brms, so only the
  # unpenalised columns need to be of full rank.
  if (!all(is.finite(basis)) ||
    qr(cbind(1, fixed))$rank != ncol(fixed) + 1) {
    cli_abort(c(
      "The {.arg formula} of {.fn nonparametric} gives unpenalised terms
       that are not identified on the bins.",
      i = "Remove terms that repeat each other, or use more bins."
    ))
  }
  n_fixed <- ncol(fixed)
  n_penalised <- vapply(penalised, ncol, integer(1))
  sd_index <- rep(seq_along(penalised), n_penalised)
  coefs <- data.frame(
    term = c(fixed_terms, rep(penalised_terms, n_penalised)),
    dpar = c(
      sprintf("h%db", seq_len(n_fixed)),
      sprintf("h%dz", seq_len(sum(n_penalised)))
    ),
    sd = c(rep(NA_character_, n_fixed), sprintf("h%dsd", sd_index)),
    stringsAsFactors = FALSE
  )
  return(list(basis = basis, coefficients = coefs))
}

#' Stan parameterisation of the non-parametric family
#'
#' The delay distribution parameters are the boundaries followed by the bin
#' hazards, built from `mu`, the basis and the coefficients by the Stan
#' function `epidist_np_params()`. The boundaries and the basis are written
#' into the Stan code as functions returning them, see [.np_stanvars()].
#'
#' @inheritParams epidist_family_param
#' @method epidist_family_param nonparametric
#' @family family
#' @returns The family with a `param` element holding the Stan expression
#'  for the parameter array.
#'
#' @export
epidist_family_param.nonparametric <- function(family, ...) {
  family$param <- paste0(
    "epidist_np_params(epidist_np_boundaries(), mu, epidist_np_basis(), ",
    .np_stan_coefficients(family$np$coefficients), ")"
  )
  return(family)
}

#' The Stan expression of the hazard coefficients
#'
#' @param coefs The `coefficients` element of [.np_basis()].
#'
#' @returns A character string holding a Stan vector expression.
#'
#' @keywords internal
.np_stan_coefficients <- function(coefs) {
  if (nrow(coefs) == 0) {
    return("rep_vector(0, 0)")
  }
  values <- ifelse(
    is.na(coefs$sd),
    coefs$dpar,
    paste0(coefs$sd, " * ", coefs$dpar)
  )
  return(paste0("[", toString(values), "]'"))
}

#' Write numbers exactly as Stan real literals
#'
#' @param x A numeric vector.
#'
#' @returns A character vector such as `c("-1.0", "0.25")`.
#'
#' @keywords internal
.np_stan_reals <- function(x) {
  values <- sprintf("%.17g", x)
  whole <- !grepl("[.e]", values)
  values[whole] <- paste0(values[whole], ".0")
  return(values)
}

#' Stan functions of the non-parametric family
#'
#' `epidist_np_params()`, and the functions `epidist_np_boundaries()` and
#' `epidist_np_basis()`, which return the boundaries and the basis of the
#' family as constants.
#'
#' @param family The `epidist` family object.
#'
#' @returns A `brms` `stanvars` object, or `NULL` for any other family.
#'
#' @keywords internal
.np_stanvars <- function(family) {
  if (!.is_nonparametric(family)) {
    return(NULL)
  }
  np <- family$np
  basis <- np$basis
  if (ncol(basis) == 0) {
    basis_code <- sprintf("rep_matrix(0, %d, 0)", nrow(basis))
  } else {
    rows <- apply(basis, 1, function(row) {
      return(paste0("[", toString(.np_stan_reals(row)), "]"))
    })
    basis_code <- paste0(
      "[\n      ", paste(rows, collapse = ",\n      "),
      "\n    ]"
    )
  }
  constants <- paste0(
    "  array[] real epidist_np_boundaries() {\n",
    "    return {", toString(.np_stan_reals(np$boundaries)), "};\n",
    "  }\n",
    "  matrix epidist_np_basis() {\n",
    "    return ", basis_code, ";\n",
    "  }\n"
  )
  return(brms::stanvar(
    block = "functions",
    scode = paste0(
      .stan_chunk(file.path("nonparametric", "functions.stan")), "\n",
      constants
    )
  ))
}

#' Family specific prior distributions for the non-parametric family
#'
#' The intercept of `mu`, the mean logit hazard, gets a normal prior with a
#' standard deviation of 1.5 centred on \eqn{\mathrm{logit}(1 / K)}, the
#' hazard of the first of \eqn{K} bins when every bin is equally likely. A
#' prior centred on a hazard of a half would put half the prior mass on the
#' first bin, so the prior mean delay would be about a day whatever the bins.
#' Each unpenalised coefficient `h<i>b` gets `normal(0, 2)`, on the logit
#' scale since the basis is scaled. The intercept of each standard deviation
#' `h<j>sd` gets `normal(0, 1)` on the log scale, the prior `primarycensored`
#' uses for the spread of the logit hazards. Each standardised coefficient
#' `h<i>z` gets `std_normal()`, which makes the penalised terms non-centred.
#'
#' @inheritParams epidist
#' @method epidist_family_prior nonparametric
#' @family prior
#' @returns A `brmsprior` object.
#'
#' @export
epidist_family_prior.nonparametric <- function(family, formula, ...) {
  n_bins <- length(family$np$boundaries) - 1
  centre <- round(stats::qlogis(1 / n_bins), 2)
  prior <- set_prior(sprintf("normal(%s, 1.5)", centre), class = "Intercept")
  coefs <- family$np$coefficients
  for (dpar in .np_dpars(family$np)) {
    dpar_prior <- "std_normal()"
    if (dpar %in% coefs$sd) {
      dpar_prior <- "normal(0, 1)"
    } else if (dpar %in% coefs$dpar[is.na(coefs$sd)]) {
      dpar_prior <- "normal(0, 2)"
    }
    prior <- prior + set_prior(dpar_prior, class = "Intercept", dpar = dpar)
  }
  return(prior)
}

#' Is a family the non-parametric hazard family?
#'
#' @param family A family object, or a list recording a delay family as
#'  returned by `.delay_family()`.
#'
#' @returns A logical scalar.
#'
#' @keywords internal
.is_nonparametric <- function(family) {
  return(is.list(family) && !is.null(family$np))
}

#' Check the bin boundaries of the non-parametric family
#'
#' @inheritParams nonparametric
#'
#' @returns The boundaries as a numeric vector, invisibly.
#'
#' @keywords internal
.assert_np_boundaries <- function(boundaries) {
  assert_numeric(boundaries, any.missing = FALSE, finite = TRUE)
  if (length(boundaries) < 4) {
    cli_abort(c(
      "{.arg boundaries} must hold at least four values, so at least three
       bins.",
      i = "It holds {length(boundaries)}."
    ))
  }
  if (any(diff(boundaries) <= 0)) {
    cli_abort("{.arg boundaries} must be strictly increasing.")
  }
  return(invisible(as.numeric(boundaries)))
}

#' The distributional parameters of the hazard coefficients
#'
#' @param np The `np` element of the family, holding the boundaries, the
#'  basis and its coefficients.
#'
#' @returns A character vector of parameter names, all but `mu`: the
#'  unpenalised coefficients, then the standard deviations, then the
#'  standardised penalised coefficients.
#'
#' @keywords internal
.np_dpars <- function(np) {
  coefs <- np$coefficients
  return(c(
    coefs$dpar[is.na(coefs$sd)], unique(coefs$sd[!is.na(coefs$sd)]),
    coefs$dpar[!is.na(coefs$sd)]
  ))
}

#' Resolve the non-parametric family against the model data
#'
#' Checks that the model supports the family, sets the default boundaries
#' from the data where none were given, and checks that the boundaries reach
#' the longest observed delay.
#'
#' @inheritParams epidist_family
#'
#' @param family A family built by [nonparametric()].
#'
#' @returns The family with its boundaries set.
#'
#' @keywords internal
.np_resolve <- function(family, data) {
  if (!inherits(data, c("epidist_marginal_model", "epidist_meta_model"))) {
    cli_abort(c(
      "The {.fn nonparametric} family is only supported by the marginal and
       meta models.",
      i = "Use {.fn as_epidist_marginal_model} or
           {.fn as_epidist_meta_model}."
    ))
  }
  if (is.null(family$np$boundaries)) {
    longest <- .np_longest_delay(data)
    family <- .np_set_boundaries(family, seq(-1, max(3, ceiling(longest))))
  }
  if (inherits(data, "epidist_meta_model")) {
    .np_check_meta(data)
  }
  top <- max(family$np$boundaries)
  observed <- .np_longest_observed(data)
  if (top < observed) {
    cli_abort(c(
      "The last of {.arg boundaries} must be at least the longest observed
       delay.",
      i = "It is {top} and the longest delay is {observed}."
    ))
  }
  return(family)
}

#' Check that a meta model only uses summaries the non-parametric family has
#'
#' The non-parametric family puts its probability at bin edges, so the
#' continuous delay a study that fully adjusted for censoring targets
#' (`cens_adjusted` 1) is a step function with no density. Its mean and
#' standard deviation over the whole distribution are exact sums over the
#' bins. Its quantiles on the delay scale, and its moments when truncated,
#' would need a density or quadrature over a step, which the meta model does
#' not have. Every other censoring adjustment convolves the steps with a
#' censoring window, which gives a continuous distribution function, and is
#' supported.
#'
#' @param data An `epidist_meta_model` object.
#'
#' @returns `NULL`, invisibly. Errors naming the studies whose summaries are
#'  not supported.
#'
#' @keywords internal
.np_check_meta <- function(data) {
  members <- .meta_members(data)
  quantile_member <- vapply(seq_len(nrow(data)), function(row) {
    if (data$obs_type[row] != 7L) {
      return(FALSE)
    }
    rows <- seq(data$group_start[row], length.out = data$group_len[row])
    return(any(members$type[rows] == 3L))
  }, logical(1))
  moments_only <- data$obs_type %in% c(2L, 3L, 5L) |
    (data$obs_type == 7L & !quantile_member)
  unsupported <- data$obs_type != 1L & data$cens_adjusted == 1L &
    !(moments_only & data$trunc_adjusted == 1L & data$delay_min == 0)
  if (any(unsupported)) {
    studies <- paste("row", which(unsupported))
    if (hasName(data, "study")) {
      studies <- unique(as.character(data$study[unsupported]))
    }
    cli_abort(c(
      "The {.fn nonparametric} family does not support some summaries of
       studies with {.code cens_adjusted = 1}: {.val {studies}}.",
      i = "It puts its probability at bin edges, so the continuous delay
           these studies target has no density. Only a mean or standard
           deviation of the whole distribution, from a study that adjusted
           for right truncation and counted every delay, is supported.",
      "*" = "Use a parametric family, or drop these summaries."
    ))
  }
  return(invisible(NULL))
}

#' The longest delay observed in individual level data
#'
#' @inheritParams epidist_family
#'
#' @returns The largest lower delay bound of an individual level row, or
#'  `-Inf` where there are none.
#'
#' @keywords internal
.np_longest_observed <- function(data) {
  rows <- .np_individual_rows(data)
  if (!any(rows)) {
    return(-Inf)
  }
  return(max(data$delay_lwr[rows]))
}

#' The longest delay the data could hold
#'
#' The upper delay bound of the individual level rows, and for a meta model
#' the observation time of each summary row, which bounds the delays the
#' study saw. Both are always finite: a summary row whose study adjusted for
#' right truncation carries its grid cutoff as its observation time.
#'
#' @inheritParams epidist_family
#'
#' @returns A number.
#'
#' @keywords internal
.np_longest_delay <- function(data) {
  rows <- .np_individual_rows(data)
  delays <- data$delay_upr[rows]
  if (inherits(data, "epidist_meta_model")) {
    delays <- c(delays, data$relative_obs_time[!rows])
  }
  return(max(delays[is.finite(delays)]))
}

#' Which rows of the model data are individual level delays
#'
#' @inheritParams epidist_family
#'
#' @returns A logical vector.
#'
#' @keywords internal
.np_individual_rows <- function(data) {
  if (inherits(data, "epidist_meta_model")) {
    return(data$obs_type == 1L)
  }
  return(rep(TRUE, nrow(data)))
}


#' Bin hazards of the non-parametric family
#'
#' @param dpars A named list of distributional parameter vectors of equal
#'  length, holding `mu` and the hazard coefficients.
#'
#' @inheritParams .np_dpars
#'
#' @returns A matrix with one row per element of the vectors in `dpars` and
#'  one column per bin, whose last column is 1. Mirrors
#'  `epidist_np_params()` in Stan.
#'
#' @keywords internal
.np_hazards <- function(dpars, np) {
  n <- length(dpars$mu)
  coefs <- np$coefficients
  logit <- matrix(rep_len(dpars$mu, n), nrow = n, ncol = nrow(np$basis))
  for (q in seq_len(nrow(coefs))) {
    theta <- rep_len(dpars[[coefs$dpar[q]]], n)
    if (!is.na(coefs$sd[q])) {
      theta <- theta * rep_len(dpars[[coefs$sd[q]]], n)
    }
    logit <- logit + outer(theta, np$basis[, q])
  }
  return(cbind(stats::plogis(logit), 1))
}

#' Bin probabilities of the non-parametric family
#'
#' Each row is the probability mass at the right edge of each bin, from the
#' hazards \eqn{h_k} as \eqn{h_k \prod_{j < k} (1 - h_j)}. Matches
#' [primarycensored::hazards_to_pmf()].
#'
#' @inheritParams .np_hazards
#'
#' @returns A matrix with one row per element of the vectors in `dpars` and
#'  one column per bin.
#'
#' @keywords internal
.np_pmf <- function(dpars, np) {
  hazards <- .np_hazards(dpars, np)
  n_bins <- ncol(hazards)
  log_surv <- matrix(0, nrow = nrow(hazards), ncol = n_bins)
  for (j in seq_len(n_bins)[-1]) {
    log_surv[, j] <- log_surv[, j - 1] + log1p(-hazards[, j - 1])
  }
  return(hazards * exp(log_surv))
}

#' A random delay generator for the non-parametric family
#'
#' @inheritParams .np_dist_args
#'
#' @returns A function of `n`, `i` and `prep`, as
#'  [primarycensored::rpcens()] calls it, returning `n` delays, the delay of
#'  each draw in turn, recycled when `n` is more than the number of draws.
#'
#' @keywords internal
.np_rdist <- function(np) {
  return(function(n, i, prep, ...) {
    dist_args <- .np_dist_args(prep, i, np)
    draws <- rep_len(seq_along(dist_args), n)
    return(vapply(draws, function(draw) {
      return(primarycensored::rdiscretehazard(
        1,
        boundaries = dist_args[[draw]]$boundaries,
        hazards = dist_args[[draw]]$hazards
      ))
    }, numeric(1)))
  })
}

#' The expected delay of the non-parametric family
#'
#' @inheritParams .np_dist_args
#'
#' @returns A function of `prep` returning a matrix of the mean delay with
#'  one row per draw and one column per observation, as used by
#'  [brms::posterior_epred()].
#'
#' @keywords internal
.np_epred <- function(np) {
  return(function(prep) {
    edges <- np$boundaries[-1]
    means <- vapply(seq_len(prep$nobs), function(i) {
      dist_args <- .np_dist_args(prep, i, np)
      return(vapply(dist_args, function(arg) {
        return(sum(edges * primarycensored::hazards_to_pmf(arg$hazards)))
      }, numeric(1)))
    }, numeric(prep$ndraws))
    return(matrix(means, nrow = prep$ndraws))
  })
}

#' Distribution parameters of the non-parametric family for each draw
#'
#' @param prep A `brms` prepared predictions object.
#'
#' @param i The index of the observation.
#'
#' @inheritParams .np_dpars
#'
#' @returns A list with one element per draw, each a list of `boundaries`
#'  and `hazards` for [primarycensored::pdiscretehazard()].
#'
#' @keywords internal
.np_dist_args <- function(prep, i, np) {
  dpar_names <- c("mu", .np_dpars(np))
  dpars <- lapply(dpar_names, function(dpar) {
    return(brms::get_dpar(prep, dpar, i = i))
  })
  names(dpars) <- dpar_names
  hazards <- .np_hazards(dpars, np)
  return(lapply(seq_len(nrow(hazards)), function(draw) {
    return(list(boundaries = np$boundaries, hazards = hazards[draw, ]))
  }))
}

#' Analytic delay summaries of the non-parametric family
#'
#' @inheritParams .np_dist_args
#'
#' @returns A list in the form of `.analytic_delay_summaries()`.
#'
#' @keywords internal
.np_delay_summaries <- function(np) {
  if (is.null(np$boundaries)) {
    cli_abort(c(
      "The {.fn nonparametric} family has no {.arg boundaries}.",
      i = "Pass the fit as {.arg family}, or set {.arg boundaries}."
    ))
  }
  edges <- np$boundaries[-1]
  widths <- diff(np$boundaries)
  pmf <- function(d) {
    return(.np_pmf(d, np))
  }
  delay_mean <- function(d) {
    return(as.vector(pmf(d) %*% edges))
  }
  return(list(
    dpars = c("mu", .np_dpars(np)),
    mean = delay_mean,
    sd = function(d) {
      second <- as.vector(pmf(d) %*% edges^2)
      return(sqrt(pmax(second - delay_mean(d)^2, 0)))
    },
    quantile = function(d, p) {
      mass <- pmf(d)
      cum <- mass
      for (j in seq_len(ncol(mass))[-1]) {
        cum[, j] <- cum[, j - 1] + mass[, j]
      }
      reached <- cum >= p - sqrt(.Machine$double.eps)
      reached[, ncol(reached)] <- TRUE
      return(edges[max.col(reached, ties.method = "first")])
    },
    density = function(d, x) {
      bin <- findInterval(x, np$boundaries, left.open = TRUE)
      if (bin < 1 || bin > length(edges)) {
        return(rep(0, length(d$mu)))
      }
      return(pmf(d)[, bin] / widths[bin])
    }
  ))
}

#' Simulate delays from each draw of the non-parametric family
#'
#' @inheritParams .np_dist_args
#'
#' @inheritParams .simulate_delays
#'
#' @returns A matrix with one row per element of the vectors in `dpars` and
#'  `nsim` columns.
#'
#' @keywords internal
.np_simulate_delays <- function(np, dpars, nsim = 1000) {
  mass <- .np_pmf(dpars, np)
  edges <- np$boundaries[-1]
  samples <- lapply(seq_len(nrow(mass)), function(row) {
    return(sample(edges, nsim, replace = TRUE, prob = mass[row, ]))
  })
  return(matrix(unlist(samples), nrow = nrow(mass), byrow = TRUE))
}
