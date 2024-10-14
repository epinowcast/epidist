prep_obs <- as_latent_individual(sim_obs)
prep_obs_gamma <- as_latent_individual(sim_obs_gamma)

family_lognormal <- epidist_family(prep_obs, family = brms::lognormal())

as_string_formula <- function(formula) {
  form <- paste(deparse(formula), collapse = " ")
  form <- gsub("\\s+", " ", form, perl = FALSE)
  return(form)
}

test_that("epidist_formula with default settings produces a brmsformula with the correct intercept only formula", { # nolint: line_length_linter.
  form <- epidist_formula(
    prep_obs, family = family_lognormal, formula = brms::bf(mu ~ 1, sigma ~ 1)
  )
  expect_s3_class(form, "brmsformula")
  expect_equal(
    as_string_formula(form$formula),
    "delay | vreal(relative_obs_time, pwindow, swindow) ~ 1"
  )
  expect_equal(
    as_string_formula(form$pforms$sigma),
    "sigma ~ 1"
  )
})

test_that("epidist_formula with custom formulas produces a brmsformula with correct custom formulas", { # nolint: line_length_linter.
  prep_obs$sex <- rbinom(n = nrow(prep_obs), size = 1, prob = 0.5)
  form_sex <- epidist_formula(
    prep_obs,
    family = family_lognormal,
    formula = brms::bf(mu ~ 1 + sex, sigma ~ 1 + sex)
  )
  expect_s3_class(form_sex, "brmsformula")
  expect_equal(
    as_string_formula(form_sex$formula),
    "delay | vreal(relative_obs_time, pwindow, swindow) ~ sex"
  )
  expect_equal(
    as_string_formula(form_sex$pforms$sigma),
    "sigma ~ 1 + sex"
  )
})

test_that("epidist_formula with custom formulas errors for incorrect custom formulas", { # nolint: line_length_linter.
  expect_error(
    epidist_formula(
      prep_obs, family = family_lognormal,
      formula = brms::bf(mu ~ 1 + age, sigma ~ 1)
    )
  )
  expect_error(
    epidist_formula(
      prep_obs, family = family_lognormal,
      formula = brms::bf(mu ~ 1, sigma ~ 1 + age)
    )
  )
  expect_error(
    epidist_formula(
      prep_obs, family = family_lognormal,
      formula = brms::bf(list(), sigma ~ 1)
    )
  )
  expect_error(
    epidist_formula(
      prep_obs, family = family_lognormal,
      formula = brms::bf(mu ~ 1, shape ~ 1)
    )
  )
})
