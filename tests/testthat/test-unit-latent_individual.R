# Assumes that tests/testthat/setup.R has been run

as_string_formula <- function(formula) {
  form <- paste(deparse(formula), collapse = " ")
  form <- gsub("\\s+", " ", form, perl = FALSE)
  return(form)
}

# Generate observation data in correct format for the latent_individual model
prep_obs <- epidist_prepare(sim_obs, model = "latent_individual")

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

# epidist_formula success when there is the appropriate column (custom formula)
# epidist_formula failure when there is not the appropriate column (custom formula)

test_that("epidist_family.epidist_latent_individual with default settings produces an object of the right class", { # nolint: line_length_linter.
  family <- epidist_family(prep_obs)
  expect_s3_class(family, "customfamily")
  expect_s3_class(family, "brmsfamily")
  expect_s3_class(family, "family")
})

priors <- epidist_priors(prep_obs)
# TODO: add tests for priors: waiting on prior PR being merged for this
priors

test_that("epidist_stancode.epidist_latent_individual", { # nolint: line_length_linter.
  stancode <- epidist_stancode(prep_obs)
  expect_equal(1, 1)
})

# Could also test how default methods work
x <- list()
epidist_prepare(x)
epidist_family(x)
epidist_formula(x)
epidist_priors(x)
epidist_stancode(x)