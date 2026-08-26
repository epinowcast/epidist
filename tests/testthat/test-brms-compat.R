# These compare against unexported `brms` internals, which CRAN checks should
# not depend on. They exist to catch drift during development.
skip_on_cran()
skip_if_not_installed("brms")

# The `brms` internals that the helpers in `R/brms-compat.R` replace. They
# are fetched with `getFromNamespace()` so that no `:::` call appears in the
# package sources.
brms_validate_family <- utils::getFromNamespace("validate_family", "brms")
brms_validate_formula <- utils::getFromNamespace("validate_formula", "brms")
brms_validate_data <- utils::getFromNamespace("validate_data", "brms")
brms_dpar_bounds <- utils::getFromNamespace("dpar_bounds", "brms")
brms_log_lik_weight <- utils::getFromNamespace("log_lik_weight", "brms")

# Closures and formula environments differ between two otherwise identical
# objects, so replace them before comparing.
normalise <- function(x) {
  if (is.function(x)) {
    return("<function>")
  }
  if (inherits(x, "formula")) {
    attr(x, ".Environment") <- NULL
    return(x)
  }
  if (is.list(x)) {
    x[] <- lapply(x, normalise)
    return(x)
  }
  return(x)
}

# Families that `epidist` can be asked to use for a delay distribution,
# covering every supported input form: a family function, a called family
# function, a `stats` family, a character string, and a character vector
# with an explicit link.
family_inputs <- list(
  "called lognormal" = brms::lognormal(),
  "called gamma" = brms::brmsfamily("gamma"),
  "called weibull" = brms::weibull(),
  "called exponential" = brms::exponential(),
  "called frechet" = brms::frechet(),
  "called inverse gaussian" = brms::brmsfamily("inverse.gaussian"),
  "called gaussian" = stats::gaussian(),
  "called student" = brms::student(),
  "called skew normal" = brms::skew_normal(),
  "stats Gamma" = stats::Gamma(),
  "stats gaussian log link" = stats::gaussian(link = "log"),
  "family function" = brms::lognormal,
  "character lognormal" = "lognormal",
  "character gamma" = "gamma",
  "character weibull" = "weibull",
  "character with link" = c("gamma", "log")
)

test_that(".validate_family matches brms::validate_family for every family epidist supports", { # nolint: line_length_linter.
  for (nm in names(family_inputs)) {
    expect_identical(
      normalise(.validate_family(family_inputs[[nm]])),
      normalise(brms_validate_family(family_inputs[[nm]])),
      info = nm
    )
  }
})

test_that(".validate_family returns brmsfamily objects unchanged", {
  family <- brms::lognormal()
  expect_identical(.validate_family(family), family)
})

test_that(".validate_family keeps the link fields that brms keeps", {
  # `brms::brmsfamily()` fills in default links for all distributional
  # parameters whereas the internal constructor does not. `.add_dpar_info()`
  # depends on this difference.
  expect_identical(.validate_family(brms::lognormal())$link_sigma, "log")
  expect_null(.validate_family("lognormal")$link_sigma)
  expect_null(brms_validate_family("lognormal")$link_sigma)
})

test_that(".validate_family errors on invalid input like brms does", {
  expect_error(.validate_family(1))
  expect_error(brms_validate_family(1))
  expect_error(.validate_family(list(a = 1)))
  expect_error(brms_validate_family(list(a = 1)))
})

test_that(".dpar_bounds matches brms::dpar_bounds for the parameters of every supported family", { # nolint: line_length_linter.
  families <- c(
    "lognormal", "gamma", "weibull", "exponential", "frechet",
    "inverse.gaussian", "gaussian", "student", "skew_normal",
    "gen_extreme_value", "beta", "von_mises", "asym_laplace",
    "zero_inflated_poisson", "hurdle_gamma", "zero_one_inflated_beta"
  )
  for (fam_name in families) {
    family <- brms::brmsfamily(fam_name)
    for (dpar in setdiff(family$dpars, "mu")) {
      expect_identical(
        .dpar_bounds(dpar, family = family$family),
        brms_dpar_bounds(dpar, family = family$family),
        info = paste(fam_name, dpar)
      )
    }
  }
})

test_that(".dpar_bounds errors where brms returns NULL", {
  expect_null(brms_dpar_bounds("not_a_dpar", family = "lognormal"))
  expect_error(
    .dpar_bounds("not_a_dpar", family = "lognormal"),
    "are unknown"
  )
})

test_that(".add_dpar_info gives the same result via .validate_family as via brms", { # nolint: line_length_linter.
  # `.add_dpar_info()` only handles families with a single distributional
  # parameter besides `mu`, which is what `epidist` supports.
  for (nm in names(family_inputs)) {
    ours <- .validate_family(family_inputs[[nm]])
    if (length(setdiff(ours$dpars, "mu")) != 1) {
      next
    }
    theirs <- brms_validate_family(family_inputs[[nm]])
    expect_identical(
      .add_dpar_info(ours)$other_links,
      .add_dpar_info(theirs)$other_links,
      info = nm
    )
    expect_identical(
      .add_dpar_info(ours)$other_bounds,
      .add_dpar_info(theirs)$other_bounds,
      info = nm
    )
  }
})

test_that(".log_lik_weight matches brms::log_lik_weight with and without weights", { # nolint: line_length_linter.
  lpdf <- c(-1.5, -2.5, -0.5)
  prep_unweighted <- list(data = list(Y = c(1, 2, 3)))
  prep_weighted <- list(data = list(Y = c(1, 2, 3), weights = c(2, 3, 4)))
  for (i in seq_len(3)) {
    expect_identical(
      .log_lik_weight(lpdf, i = i, prep = prep_unweighted),
      brms_log_lik_weight(lpdf, i = i, prep = prep_unweighted)
    )
    expect_identical(
      .log_lik_weight(lpdf, i = i, prep = prep_weighted),
      brms_log_lik_weight(lpdf, i = i, prep = prep_weighted)
    )
  }
  expect_identical(
    .log_lik_weight(lpdf, i = 2, prep = prep_weighted),
    lpdf * 3
  )
})

test_that(".validate_formula matches brms::validate_formula for the formula shapes epidist supports", { # nolint: line_length_linter.
  prep_obs <- as_epidist_latent_model(sim_obs)
  family <- epidist_family(prep_obs, family = brms::lognormal())
  formulas <- list(
    "plain formula" = mu ~ 1,
    brmsformula = brms::bf(mu ~ 1),
    "explicit sigma" = brms::bf(mu ~ 1, sigma ~ 1),
    "fixed sigma" = brms::bf(mu ~ 1, sigma = 1),
    covariate = brms::bf(mu ~ 1 + ptime_lwr),
    "random effect" = brms::bf(mu ~ 1 + (1 | ptime_lwr)),
    "both dpars with covariate" = brms::bf(
      mu ~ 1 + ptime_lwr, sigma ~ 1 + ptime_lwr
    )
  )
  for (nm in names(formulas)) {
    expect_identical(
      normalise(
        .validate_formula(formulas[[nm]], family = family, data = prep_obs)
      ),
      normalise(
        brms_validate_formula(
          formulas[[nm]],
          family = family, data = prep_obs
        )
      ),
      info = nm
    )
  }
})

test_that(".validate_formula matches brms for other supported families", {
  prep_obs <- as_epidist_latent_model(sim_obs_gamma)
  for (fam_name in c("gamma", "weibull", "lognormal")) {
    family <- epidist_family(prep_obs, family = brms::brmsfamily(fam_name))
    formula <- brms::bf(mu ~ 1)
    expect_identical(
      normalise(.validate_formula(formula, family = family, data = prep_obs)),
      normalise(
        brms_validate_formula(formula, family = family, data = prep_obs)
      ),
      info = fam_name
    )
  }
})

test_that(".validate_formula expands a dot in the same way as brms", {
  data <- data.frame(y = 1:5, a = 6:10, b = 11:15)
  formula <- brms::bf(y ~ .)
  expect_identical(
    normalise(
      .validate_formula(formula, family = brms::lognormal(), data = data)
    ),
    normalise(
      brms_validate_formula(
        formula,
        family = brms::lognormal(), data = data
      )
    )
  )
  expect_identical(
    all.vars(
      .validate_formula(
        formula,
        family = brms::lognormal(), data = data
      )$formula
    ),
    c("y", "a", "b")
  )
})

test_that(".validate_formula keeps a family already set on the formula", {
  formula <- brms::bf(y ~ 1, family = brms::lognormal())
  out <- .validate_formula(formula, family = brms::weibull())
  expect_identical(out$family$family, "lognormal")
  expect_identical(
    out$family$family,
    brms_validate_formula(formula, family = brms::weibull())$family$family
  )
})

test_that(".validate_formula rejects families epidist does not support", {
  expect_error(
    .validate_formula(brms::bf(y ~ 1), family = brms::cumulative()),
    "not a supported"
  )
  expect_error(
    .validate_formula(
      brms::bf(y ~ 1),
      family = brms::mixture(
        brms::lognormal(), brms::lognormal(),
        order = "mu"
      )
    ),
    "not a supported"
  )
})

test_that(".validate_data accepts the same data as brms::validate_data", {
  prep_obs <- as_epidist_latent_model(sim_obs)
  formula <- epidist_formula(
    prep_obs,
    family = epidist_family(prep_obs, family = brms::lognormal()),
    formula = brms::bf(mu ~ 1)
  )
  bterms <- brms::brmsterms(formula)
  expect_silent(.validate_data(prep_obs, bterms))
  expect_silent(brms_validate_data(prep_obs, bterms))
})

test_that(".validate_data errors on the same problems as brms::validate_data", { # nolint: line_length_linter.
  bterms <- brms::brmsterms(brms::bf(y ~ a + b))
  good <- data.frame(y = 1:5, a = 6:10, b = 11:15)
  expect_silent(.validate_data(good, bterms))
  expect_silent(brms_validate_data(good, bterms))

  no_rows <- good[0, ]
  expect_error(.validate_data(no_rows, bterms))
  expect_error(brms_validate_data(no_rows, bterms))

  missing_var <- good[, c("y", "a")]
  expect_error(.validate_data(missing_var, bterms))
  expect_error(brms_validate_data(missing_var, bterms))

  all_na <- good
  all_na$a <- NA_integer_
  expect_error(suppressWarnings(.validate_data(all_na, bterms)))
  expect_error(suppressWarnings(brms_validate_data(all_na, bterms)))

  bad_names <- data.frame(y = 1:5, a = 6:10, b = 11:15, c__d = 1:5)
  bad_bterms <- brms::brmsterms(brms::bf(y ~ a + b + c__d))
  expect_error(.validate_data(bad_names, bad_bterms))
  expect_error(brms_validate_data(bad_names, bad_bterms))
})

test_that(".validate_data warns about infinite values like brms does", {
  bterms <- brms::brmsterms(brms::bf(y ~ a))
  data <- data.frame(y = c(1, 2, Inf), a = c(1, 2, 3))
  expect_warning(.validate_data(data, bterms), "infinite")
  expect_warning(brms_validate_data(data, bterms), "infinite")
})

test_that(".validate_data returns the data invisibly", {
  bterms <- brms::brmsterms(brms::bf(y ~ a))
  data <- data.frame(y = 1:3, a = 4:6)
  expect_identical(.validate_data(data, bterms), data)
})
