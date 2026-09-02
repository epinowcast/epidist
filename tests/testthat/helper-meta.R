# Helpers for evaluating the Stan meta model log likelihood outside a fit.
#
# cmdstanr::expose_functions() cannot be relied on for the meta model, so the
# Stan log likelihood is evaluated through a fixed parameter generated
# quantities program that calls meta_lognormal_lpmf with the parameters
# passed as data, one row of the model data per column and one parameter
# draw per row.

meta_slot_names <- function() {
  return(c(paste0("vint", 1:9), paste0("vreal", 1:8)))
}

# Compile the generated quantities program for a lognormal meta model.
meta_log_lik_program <- function(meta) {
  meta_family <- epidist_family(meta, family = lognormal())
  meta_formula <- epidist_formula(meta, meta_family, formula = bf(mu ~ 1))
  stanvars <- epidist_stancode(
    meta, family = meta_family, formula = meta_formula
  )
  standata <- suppressMessages(epidist(meta, fn = brms::make_standata))
  slots <- meta_slot_names()
  mod <- cmdstanr::cmdstan_model(cmdstanr::write_stan_file(paste0(
    "functions {\n", stanvars[[3]]$scode, "\n", stanvars[[2]]$scode, "\n}\n",
    "data {\n  int N;\n  array[N] int Y;\n",
    paste0("  array[N] int ", slots[1:9], ";\n", collapse = ""),
    paste0("  array[N] real ", slots[10:17], ";\n", collapse = ""),
    "  int<lower=0> N_meta_group;\n",
    "  vector[N_meta_group] meta_group_value;\n",
    "  array[N_meta_group] int meta_group_count;\n",
    "  array[N_meta_group] int meta_group_type;\n",
    "  vector[N_meta_group] meta_group_p;\n",
    "  int<lower=0> N_meta_chol;\n",
    "  vector[N_meta_chol] meta_group_chol;\n",
    "  int D;\n  array[D] real mu;\n  array[D] real sigma;\n}\n",
    "generated quantities {\n  array[0] real primary_params;\n",
    "  matrix[D, N] log_lik;\n  for (d in 1:D) {\n    for (n in 1:N) {\n",
    "      log_lik[d, n] = meta_lognormal_lpmf(Y[n] | mu[d], sigma[d], ",
    paste0(slots, "[n]", collapse = ", "),
    ", meta_group_value, meta_group_count, meta_group_type, meta_group_p",
    ", meta_group_chol, primary_params);\n    }\n  }\n}\n"
  )))
  stan_data <- c(
    list(N = length(standata$Y), Y = as.integer(standata$Y)),
    lapply(standata[slots[1:9]], as.integer),
    lapply(standata[slots[10:17]], as.numeric),
    list(
      N_meta_group = standata$N_meta_group,
      meta_group_value = as.array(standata$meta_group_value),
      meta_group_count = as.array(standata$meta_group_count),
      meta_group_type = as.array(standata$meta_group_type),
      meta_group_p = as.array(standata$meta_group_p),
      N_meta_chol = standata$N_meta_chol,
      meta_group_chol = as.array(standata$meta_group_chol)
    )
  )
  return(list(mod = mod, data = stan_data, standata = standata))
}

# The Stan log likelihood, one row per draw and one column per model row.
meta_stan_log_lik <- function(program, mu, sigma) {
  fit <- program$mod$sample(
    data = c(
      program$data,
      list(D = length(mu), mu = as.array(mu), sigma = as.array(sigma))
    ),
    fixed_param = TRUE, chains = 1, iter_sampling = 1, iter_warmup = 0,
    refresh = 0, show_messages = FALSE, sig_figs = 18
  )
  draws <- posterior::as_draws_matrix(fit$draws("log_lik"))
  n <- program$data$N
  return(t(vapply(
    seq_along(mu),
    function(d) {
      return(as.numeric(draws[1, paste0("log_lik[", d, ",", seq_len(n), "]")]))
    },
    numeric(n)
  )))
}

# Every element within a relative tolerance, scaled by at least one so that
# a log likelihood near zero is compared absolutely.
expect_rows_close <- function(actual, expected, tolerance) {
  gap <- abs(actual - expected) / pmax(abs(expected), 1)
  return(testthat::expect_lt(max(gap), tolerance))
}

# The R log likelihood on the same layout.
meta_r_log_lik <- function(program, mu, sigma) {
  prep <- list(data = program$data, ndraws = 1)
  n <- program$data$N
  return(t(vapply(
    seq_along(mu),
    function(d) {
      dist_args <- list(meanlog = mu[d], sdlog = sigma[d])
      return(vapply(
        seq_len(n),
        function(i) {
          return(.meta_row_log_lik(
            .meta_row_slots(i, prep), "plnorm", dist_args
          ))
        },
        numeric(1)
      ))
    },
    numeric(n)
  )))
}
