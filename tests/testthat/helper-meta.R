# Helpers for evaluating the Stan meta model log likelihood outside a fit.
#
# Exposing the Stan functions to R cannot be relied on for the meta model, so
# the Stan log likelihood is evaluated through a fixed parameter generated
# quantities program that calls the meta family lpmf, meta_lognormal_lpmf by
# default, with the distributional parameters passed as data, one row of the
# model data per column and one parameter draw per row. The draws are given
# in the order of the family's distributional parameters, so a model whose
# summary rows estimate their growth rate takes pgrowth as a further draw.

meta_slot_names <- function() {
  return(c(paste0("vint", 1:10), paste0("vreal", 1:8)))
}

# Compile the generated quantities program for a meta model of a family.
meta_log_lik_program <- function(meta, family = lognormal()) {
  meta_family <- epidist_family(meta, family = family)
  dpars <- meta_family$dpars
  name <- .delay_family(meta_family)$name
  meta_formula <- epidist_formula(meta, meta_family, formula = bf(mu ~ 1))
  stanvars <- epidist_stancode(
    meta,
    family = meta_family, formula = meta_formula
  )
  standata <- suppressMessages(epidist(meta, fn = brms::make_standata))
  slots <- meta_slot_names()
  growth <- "pgrowth" %in% dpars
  mod <- rstan::stan_model(model_code = paste0(
    "functions {\n", stanvars[[3]]$scode, "\n", stanvars[[2]]$scode, "\n}\n",
    "data {\n  int N;\n  array[N] int Y;\n",
    paste0("  array[N] int ", slots[1:10], ";\n", collapse = ""),
    paste0("  array[N] real ", slots[11:18], ";\n", collapse = ""),
    "  int<lower=0> N_meta_group;\n",
    "  vector[N_meta_group] meta_group_value;\n",
    "  array[N_meta_group] int meta_group_count;\n",
    "  array[N_meta_group] int meta_group_lower;\n",
    "  array[N_meta_group] int meta_group_type;\n",
    "  vector[N_meta_group] meta_group_p;\n",
    "  int<lower=0> N_meta_chol;\n",
    "  vector[N_meta_chol] meta_group_chol;\n",
    "  int D;\n",
    paste0("  array[D] real ", dpars, ";\n", collapse = ""),
    "}\n",
    "generated quantities {\n  array[0] real primary_params;\n",
    "  matrix[D, N] log_lik;\n  for (d in 1:D) {\n    for (n in 1:N) {\n",
    "      log_lik[d, n] = meta_", name, "_lpmf(Y[n] | ",
    paste0(dpars, "[d]", collapse = ", "), ", ",
    paste0(slots, "[n]", collapse = ", "),
    ", meta_group_value, meta_group_count, meta_group_lower, meta_group_type",
    ", meta_group_p",
    ", meta_group_chol, primary_params);\n    }\n  }\n}\n"
  ))
  stan_data <- c(
    list(N = length(standata$Y), Y = as.integer(standata$Y)),
    lapply(standata[slots[1:10]], as.integer),
    lapply(standata[slots[11:18]], as.numeric),
    list(
      N_meta_group = standata$N_meta_group,
      meta_group_value = as.array(standata$meta_group_value),
      meta_group_count = as.array(standata$meta_group_count),
      meta_group_lower = as.array(standata$meta_group_lower),
      meta_group_type = as.array(standata$meta_group_type),
      meta_group_p = as.array(standata$meta_group_p),
      N_meta_chol = standata$N_meta_chol,
      meta_group_chol = as.array(standata$meta_group_chol)
    )
  )
  return(list(
    mod = mod, data = stan_data, standata = standata, dpars = dpars,
    dist = .pcd_family_dist_name(meta_family), growth = growth
  ))
}

# Name the parameter draws after the family's distributional parameters, each
# recycled to the number of draws.
meta_dpar_draws <- function(program, ...) {
  draws <- list(...)
  names(draws) <- program$dpars
  n_draws <- max(lengths(draws))
  return(lapply(draws, rep_len, n_draws))
}

# The primarycensored arguments of one draw. Any parameter of the model that
# is not one of the distribution's, pgrowth, is left out.
meta_dist_args <- function(dist, dpars) {
  return(switch(dist,
    plnorm = list(meanlog = dpars$mu, sdlog = dpars$sigma),
    pgengamma.orig = list(shape = dpars$shape, scale = dpars$mu, k = dpars$k)
  ))
}

# The Stan log likelihood, one row per draw and one column per model row.
meta_stan_log_lik <- function(program, ...) {
  draws <- meta_dpar_draws(program, ...)
  fit <- rstan::sampling(
    program$mod,
    data = c(
      program$data,
      list(D = length(draws[[1]])),
      lapply(draws, as.array)
    ),
    algorithm = "Fixed_param", chains = 1, iter = 1, warmup = 0, refresh = 0
  )
  log_lik <- posterior::as_draws_matrix(fit)
  n <- program$data$N
  return(t(vapply(
    seq_along(draws[[1]]),
    function(d) {
      return(as.numeric(
        log_lik[1, paste0("log_lik[", d, ",", seq_len(n), "]")]
      ))
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

# The R log likelihood on the same layout. The prep is a brmsprep in name
# only, holding pgrowth as a draw by row matrix so that .meta_row_slots()
# can read it through brms::get_dpar().
meta_r_log_lik <- function(program, ...) {
  draws <- meta_dpar_draws(program, ...)
  n <- program$data$N
  return(t(vapply(
    seq_along(draws[[1]]),
    function(d) {
      dpars <- lapply(draws, `[`, d)
      prep <- structure(
        list(data = program$data, ndraws = 1, dpars = list()),
        class = "brmsprep"
      )
      if (program$growth) {
        prep$dpars$pgrowth <- matrix(dpars$pgrowth, nrow = 1, ncol = n)
      }
      dist_args <- meta_dist_args(program$dist, dpars)
      return(vapply(
        seq_len(n),
        function(i) {
          return(.meta_row_log_lik(
            .meta_row_slots(i, prep), program$dist, dist_args
          ))
        },
        numeric(1)
      ))
    },
    numeric(n)
  )))
}
