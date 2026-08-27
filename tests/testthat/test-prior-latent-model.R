test_that("epidist_prior warns or errors for latent model non-uniform priors", {
  # Mock data object
  data <- list(
    relative_obs_time = 1,
    pwindow = 1,
    woverlap = 1,
    swindow = 1,
    delay = 1,
    .row_id = 1
  )
  class(data) <- c("epidist_latent_model", "data.frame")

  # Mock brmsprior object creator
  mock_prior <- function(prior_str, class = "b", dpar = "", ...) {
    return(data.frame(
      prior = prior_str,
      class = class,
      dpar = dpar,
      stringsAsFactors = FALSE
    ))
  }
  class(mock_prior) <- "brmsprior"

  # Since we cannot easily run epidist_prior because it depends on
  # brms::default_prior and other things we will test the checking function
  # directly.

  # Case 1: swindow_raw non-uniform -> Error
  p1 <- mock_prior("normal(0,1)", dpar = "swindow_raw")
  expect_error(.check_model_prior(data, p1), "secondary event")

  # Case 2: swindow_raw uniform -> No error
  p2 <- mock_prior("uniform(0,1)", dpar = "swindow_raw")
  expect_no_error(.check_model_prior(data, p2))

  # Case 3: pwindow_raw non-uniform -> Warning
  p3 <- mock_prior("normal(0,1)", dpar = "pwindow_raw")
  expect_warning(.check_model_prior(data, p3), "primary event")

  # Case 4: pwindow_raw uniform -> No warning
  p4 <- mock_prior("uniform(0,1)", dpar = "pwindow_raw")
  expect_no_warning(.check_model_prior(data, p4))

  # Case 5: mixed
  p5 <- rbind(p1, p3)
  expect_error(.check_model_prior(data, p5), "secondary event")
})

test_that("epidist_prior rejects a non-uniform secondary event window prior", {
  data <- as_epidist_latent_model(sim_obs)
  family <- lognormal()
  formula <- bf(mu ~ 1, sigma ~ 1)
  epidist_family <- epidist_family(data, family)
  epidist_formula <- epidist_formula(
    data = data, family = epidist_family, formula = formula
  )

  user_prior <- prior("swindow_raw ~ normal(0, 1);", check = FALSE)
  expect_error(
    epidist_prior(data, epidist_family, epidist_formula, prior = user_prior),
    "secondary event"
  )
})
