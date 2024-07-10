as_string_formula <- function(formula) {
  form <- paste(deparse(formula), collapse = " ")
  form <- gsub("\\s+", " ", form, perl = FALSE)
  return(form)
}

test_that("as_latent_individual.data.frame with default settings an object with the correct classes", { # nolint: line_length_linter.
  prep_obs <- as_latent_individual(sim_obs)
  expect_s3_class(prep_obs, "data.table")
  expect_s3_class(prep_obs, "data.frame")
  expect_s3_class(prep_obs, "epidist_latent_individual")
})

test_that("as_latent_individual.data.frame errors when passed incorrect inputs", { # nolint: line_length_linter.
  expect_error(as_latent_individual(list()))
  expect_error(as_latent_individual(sim_obs[, 1]))
  expect_error({
    sim_obs$case <- paste("case_", seq_len(nrow(sim_obs)))
    as_latent_individual(sim_obs)
  })
})

# Make this data available for other tests
prep_obs <- as_latent_individual(sim_obs)

test_that("is_latent_individual returns TRUE for correct input", { # nolint: line_length_linter.
  expect_true(is_latent_individual(prep_obs))
  expect_true({
    x <- list()
    class(x) <- "epidist_latent_individual"
    is_latent_individual(x)
  })
})

test_that("is_latent_individual returns FALSE for incorrect input", { # nolint: line_length_linter.
  expect_false(is_latent_individual(list()))
  expect_false({
    x <- list()
    class(x) <- "epidist_latent_individual_extension"
    is_latent_individual(x)
  })
})

test_that("validate_latent_individual doesn't produce an error for correct input", { # nolint: line_length_linter.
  expect_no_error(validate_latent_individual(prep_obs))
})

test_that("validate_latent_individual returns FALSE for incorrect input", { # nolint: line_length_linter.
  expect_error(validate_latent_individual(list()))
  expect_error(validate_latent_individual(prep_obs[, 1]))
  expect_error({
    x <- list()
    class(x) <- "epidist_latent_individual"
    validate_latent_individual(x)
  })
})

test_that("epidist_formula.epidist_latent_individual with default settings produces a brmsformula with the correct intercept only formula", { # nolint: line_length_linter.
  form <- epidist_formula(prep_obs)
  expect_s3_class(form, "brmsformula")
  expect_equal(
    as_string_formula(form$formula),
    "delay_central | vreal(obs_t, pwindow_upr, swindow_upr) ~ 1"
  )
  expect_equal(
    as_string_formula(form$pforms$sigma),
    "sigma ~ 1"
  )
})

test_that("epidist_formula.epidist_latent_individual with custom formulas produces a brmsformula with correct custom formulas", { # nolint: line_length_linter.
  prep_obs$sex <- rbinom(n = nrow(prep_obs), size = 1, prob = 0.5)
  form_sex <- epidist_formula(prep_obs, delay_central = ~ 1 + sex,
                              sigma = ~ 1 + sex)
  expect_s3_class(form_sex, "brmsformula")
  expect_equal(
    as_string_formula(form_sex$formula),
    "delay_central | vreal(obs_t, pwindow_upr, swindow_upr) ~ 1 + sex"
  )
  expect_equal(
    as_string_formula(form_sex$pforms$sigma),
    "sigma ~ 1 + sex"
  )
})

test_that("epidist_formula.epidist_latent_individual with custom formulas errors for incorrect custom formulas", { # nolint: line_length_linter.
  expect_error(epidist_formula(prep_obs, delay_central = ~ 1 + age))
  expect_error(epidist_formula(prep_obs, sigma = ~ 1 + age))
  expect_error(epidist_formula(prep_obs, delay_central = 1))
  expect_error(epidist_formula(prep_obs, sigma = 1))
})

test_that("epidist_family.epidist_latent_individual with default settings produces an object of the right class", { # nolint: line_length_linter.
  family <- epidist_family(prep_obs)
  expect_s3_class(family, "customfamily")
  expect_s3_class(family, "brmsfamily")
  expect_s3_class(family, "family")
})

test_that("the family argument in epidist_family.epidist_latent_individual passes as expected", { # nolint: line_length_linter.
  family_gamma <- epidist_family(prep_obs, family = "gamma")
  expect_equal(family_gamma$name, "latent_gamma")
})

test_that("epidist_prior.epidist_latent_individual with default settings produces an object of the right class", { # nolint: line_length_linter.
  prior <- epidist_prior(prep_obs)
  expect_s3_class(prior, "brmsprior")
  expect_s3_class(prior, "data.frame")
})
