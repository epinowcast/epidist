as_string_formula <- function(formula) {
  form <- paste(deparse(formula), collapse = " ")
  form <- gsub("\\s+", " ", form, perl = FALSE)
  return(form)
}

# Generate observation data in correct format for the latent_individual model
prep_obs <- epidist_prepare(sim_obs, model = "latent_individual")

test_that("epidist_prepare.epidist_latent_individual with default settings an object with the correct classes", { # nolint: line_length_linter.
  expect_s3_class(prep_obs, "data.table")
  expect_s3_class(prep_obs, "data.frame")
  expect_s3_class(prep_obs, "epidist_latent_individual")
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
